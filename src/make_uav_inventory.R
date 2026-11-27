# Build a portal metadata CSV from a folder of drone lidar files (.las / .laz).
# Reads file headers only, plus ~1,000 points per file to get the flight date from GPS time.
#
# install.packages(c("lidR", "sf", "dplyr", "purrr", "readr", "fs", "units"))

library(lidR)
library(sf)
library(dplyr)
library(purrr)
library(readr)
library(fs)

# ---- One file -> one row --------------------------------------------------------
read_one <- function(f, default_epsg = NA) {
  hdr <- readLASheader(f)
  phb <- hdr@PHB
  
  xmin <- phb[["Min X"]]; xmax <- phb[["Max X"]]
  ymin <- phb[["Min Y"]]; ymax <- phb[["Max Y"]]
  n    <- phb[["Number of point records"]]
  
  # -- CRS: from the file, otherwise the default you pass in
  crs <- tryCatch(st_crs(hdr), error = function(e) NA_crs_)
  if (is.na(crs) && !is.na(default_epsg)) crs <- st_crs(default_epsg)
  
  # -- Bounding box in decimal degrees
  west <- south <- east <- north <- area_ha <- NA_real_
  if (!is.na(crs)) {
    box <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = crs))
    if (!st_is_longlat(crs)) {
      # add vertices along the edges so the box stays correct after reprojection
      box <- st_segmentize(box, dfMaxLength = max(xmax - xmin, ymax - ymin) / 20)
      area_ha <- as.numeric(units::set_units(st_area(box), "ha"))
    }
    b <- st_bbox(st_transform(box, 4326))
    west <- b[["xmin"]]; south <- b[["ymin"]]; east <- b[["xmax"]]; north <- b[["ymax"]]
  }
  
  # -- Flight date: GPS time if the file uses Adjusted Standard GPS time,
  #    otherwise the header's creation date (often the processing date: check it)
  date <- NA_character_; date_source <- NA_character_
  if (isTRUE(phb[["Global Encoding"]][["GPS Time Type"]]) && n > 0) {
    nth <- max(1, floor(n / 1000))
    pts <- readLAS(f, select = "t", filter = paste("-keep_every_nth", nth))
    t <- if (!is.empty(pts)) median(pts@data$gpstime, na.rm = TRUE) else NA
    if (is.finite(t) && t != 0) {
      date <- format(as.Date(as.POSIXct(t + 1e9, origin = "1980-01-06", tz = "UTC")))
      date_source <- "GPS time"
    }
  }
  if (is.na(date)) {
    yr <- phb[["File Creation Year"]]; doy <- phb[["File Creation Day of Year"]]
    if (isTRUE(yr > 0) && isTRUE(doy > 0)) {
      date <- format(as.Date(doy - 1, origin = paste0(yr, "-01-01")))
      date_source <- "file creation date (check)"
    }
  }
  
  tibble(
    Flight_ID     = path_ext_remove(path_file(f)),
    File_name     = path_file(f),
    File_path     = as.character(f),
    Flight_date   = date,
    Date_source   = date_source,
    West          = round(west, 6),
    South         = round(south, 6),
    East          = round(east, 6),
    North         = round(north, 6),
    Area_ha       = round(area_ha, 2),
    Point_count   = n,
    Point_density = if (isTRUE(area_ha > 0)) round(n / (area_ha * 1e4), 1) else NA_real_,
    CRS_EPSG      = if (is.na(crs)) NA_integer_ else crs$epsg,
    LAS_version   = paste0(phb[["Version Major"]], ".", phb[["Version Minor"]]),
    File_size_MB  = round(as.numeric(file_size(f)) / 1e6, 1)
  )
}

# ---- Folder -> CSV -----------------------------------------------------------------
las_inventory <- function(folder,
                          out_csv      = path(folder, "uav_inventory.csv"),
                          recursive    = TRUE,
                          default_epsg = NA,    # e.g. 2958 (NAD83(CSRS) / UTM 17N) when files lack a CRS
                          project_id   = NA,    # pre-fill Project_IDs for every new flight
                          path_prefix  = NULL)  # e.g. "/lab/data/uav": write File_path as on the server
{
  files <- dir_ls(folder, regexp = "\\.la[sz]$", recurse = recursive, ignore.case = TRUE)
  if (!length(files)) stop("No .las/.laz files found in ", folder)
  message("Reading ", length(files), " file(s)...")
  
  rows <- map(files, \(f) tryCatch(
    read_one(f, default_epsg),
    error = function(e) {
      warning(path_file(f), ": ", conditionMessage(e), call. = FALSE)
      tibble(Flight_ID = path_ext_remove(path_file(f)), File_name = path_file(f),
             File_path = as.character(f), Error = conditionMessage(e))
    }), .progress = TRUE) |>
    list_rbind() |>
    mutate(Project_IDs = project_id, .after = Flight_date)
  
  # File paths relative to the folder, optionally under a server prefix
  rows$File_path <- path_rel(rows$File_path, folder)
  if (!is.null(path_prefix)) rows$File_path <- path(path_prefix, rows$File_path)
  
  # Rerunning keeps what was typed by hand (Project_IDs, Site, notes...) for known flights
  if (file_exists(out_csv)) {
    old <- read_csv(out_csv, col_types = cols(.default = col_character()), na = "")
    manual <- setdiff(names(old), names(rows))
    rows <- rows |>
      left_join(select(old, Flight_ID, any_of(manual), Project_IDs_old = any_of("Project_IDs")),
                by = "Flight_ID") |>
      mutate(Project_IDs = coalesce(Project_IDs_old, as.character(Project_IDs))) |>
      select(-any_of("Project_IDs_old"))
    message("Kept hand-entered columns from the existing file: ",
            if (length(manual)) toString(manual) else "(none besides Project_IDs)")
  }
  
  dups <- rows$Flight_ID[duplicated(rows$Flight_ID)]
  if (length(dups)) warning("Same Flight_ID in several folders: ", toString(unique(dups)),
                            " (rename files or use unique_names: false)", call. = FALSE)
  if (anyNA(rows$West)) warning(sum(is.na(rows$West)), " file(s) have no CRS, so no bounding box. ",
                                "Pass default_epsg = ... if you know it.", call. = FALSE)
  
  write_csv(rows, out_csv, na = "")
  message("Wrote ", nrow(rows), " row(s) to ", out_csv)
  invisible(rows)
}

# ---- Example -------------------------------------------------------------------------
# las_inventory("D:/UAV/2025_Graham_Road",
#               out_csv     = "data/datatypes/uav_lidar/2025_graham.csv",
#               project_id  = "graham",
#               path_prefix = "/lab/data/uav/2025_Graham_Road")