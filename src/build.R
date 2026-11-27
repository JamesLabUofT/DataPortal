# R/build.R ------------------------------------------------------------------
# Reads data/, checks it, and writes everything the site needs to _generated/.
# Run from the project root:   Rscript R/build.R
# Quarto also runs it before each render (see _quarto.yml).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(yaml)
  library(jsonlite)
  library(glue)
  library(cli)
  library(fs)
})

DATA_DIR    <- "data"
OUT_DIR     <- "_generated"   # rebuilt on every run

# Rough box for valid coordinates (Canada). Catches flipped signs and typos.
BBOX <- list(lat = c(41, 70), lon = c(-141, -52))

SHARED_KEYS    <- c("datapoint_name", "plot", "site", "lat", "lon", "date", "project_id")
JOIN_KEYS      <- c("plot", "year", "datapoint_name")
COLUMN_TYPES   <- c("category", "text", "number", "date", "boolean", "path")
FILTERS        <- c("select", "multiselect", "range", "search", "checkbox", "none")
VISIBILITY     <- c("public", "hidden")
PARAM_TYPES    <- c("select", "text", "number", "checkbox")
DEFAULT_FILTER <- c(category = "multiselect", text = "search", number = "range",
                    date = "range", boolean = "checkbox", path = "none")

# Text that means "missing" in columns that must hold a number, date, coordinate or ID.
# In category and text columns, "NA" is kept as a value in its own right.
MISSING_TOKENS <- c("NA", "N/A")

BBOX_OUT <- c("bbox_west", "bbox_south", "bbox_east", "bbox_north")

# ---- 1. Issue log and helpers ----------------------------------------------
issues <- tibble(level = character(), where = character(), msg = character())

log_issue <- function(level, where, msg) {
  issues <<- add_row(issues, level = level, where = as.character(where),
                     msg = as.character(msg))
  invisible(NULL)
}
err  <- function(where, msg) log_issue("error", where, msg)
warn <- function(where, msg) log_issue("warning", where, msg)
note <- function(where, msg) log_issue("note", where, msg)

# ' Did you mean "x"?' for typos, case mismatches and prefixes (plot_name -> plot)
suggest <- function(x, choices) {
  choices <- unique(na.omit(as.character(choices)))
  if (!length(choices) || is.na(x)) return("")
  lx <- tolower(x); lc <- tolower(choices)
  hit <- choices[lc == lx]
  if (!length(hit)) hit <- choices[startsWith(lx, lc) | startsWith(lc, lx)]
  if (!length(hit)) {
    d <- adist(x, choices, ignore.case = TRUE)[1, ]
    if (min(d) <= max(2, nchar(x) %/% 4)) hit <- choices[which.min(d)]
  }
  if (length(hit)) glue(' Did you mean "{hit[1]}"?') else ""
}

# "file.csv row 14, file.csv row 20 (+3 more)"
rows_at <- function(d, idx, n = 5) {
  idx <- which(idx)
  s <- glue("{d$.file[idx]} row {d$.row[idx]}")
  more <- if (length(s) > n) glue(" (+{length(s) - n} more)") else ""
  paste0(paste(head(s, n), collapse = ", "), more)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

read_yml <- function(path) {
  tryCatch(read_yaml(path), error = function(e) {
    err(path, glue("could not be read as YAML: {conditionMessage(e)}"))
    NULL
  })
}

# Read every CSV matching `pattern` in `dir` as text and stack them.
read_csv_folder <- function(dir, pattern = "*.csv") {
  files <- dir_ls(dir, glob = pattern, type = "file")
  if (!length(files)) return(NULL)

  tables <- map(files, \(f) {
    d <- read_csv(f, col_types = cols(.default = col_character()),
                  na = "", trim_ws = TRUE,
                  name_repair = "minimal", progress = FALSE)
    names(d) <- str_squish(names(d))   # " Year" -> "Year"
    d$.file <- path_file(f)
    d$.row  <- seq_len(nrow(d)) + 1L   # +1 for the header line
    d
  })

  # All CSVs in one folder must have the same columns
  ref <- setdiff(names(tables[[1]]), c(".file", ".row"))
  iwalk(tables[-1], \(d, f) {
    cols <- setdiff(names(d), c(".file", ".row"))
    if (!setequal(cols, ref)) err(f, glue(
      "columns differ from {path_file(names(tables)[1])}. ",
      "Missing: {toString(setdiff(ref, cols))}. Extra: {toString(setdiff(cols, ref))}."))
  })
  bind_rows(tables)
}

# "2022-07-14" / "2022-07" / "2022" -> date_start, date_end, date_precision
parse_partial_dates <- function(x) {
  prec <- case_when(
    is.na(x)                                ~ NA_character_,
    str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$") ~ "day",
    str_detect(x, "^\\d{4}-\\d{2}$")        ~ "month",
    str_detect(x, "^\\d{4}$")               ~ "year",
    TRUE                                    ~ "invalid"
  )
  start <- as.Date(case_when(
    prec == "day"   ~ x,
    prec == "month" ~ paste0(x, "-01"),
    prec == "year"  ~ paste0(x, "-01-01")
  ), format = "%Y-%m-%d")
  end <- case_when(
    prec == "day"   ~ start,
    prec == "month" ~ as.Date(format(start + 31, "%Y-%m-01"), format = "%Y-%m-%d") - 1,
    prec == "year"  ~ as.Date(paste0(substr(x, 1, 4), "-12-31"), format = "%Y-%m-%d")
  )
  prec[prec %in% c("day", "month", "year") & is.na(start)] <- "invalid"  # e.g. 2022-13-40
  tibble(date_start = start, date_end = end, date_precision = prec)
}

# Turn lat/lon into numbers and check that they fall inside BBOX
coerce_coords <- function(d, where) {
  for (k in c("lat", "lon")) {
    if (!k %in% names(d)) { d[[k]] <- NA_real_; next }
    num <- suppressWarnings(as.numeric(d[[k]]))
    bad <- !is.na(d[[k]]) & is.na(num)
    if (any(bad)) err(where, glue("{k} is not a number (use decimal degrees, e.g. 49.78): {rows_at(d, bad)}"))
    d[[k]] <- num
  }
  out <- (!is.na(d$lat) & (d$lat < BBOX$lat[1] | d$lat > BBOX$lat[2])) |
         (!is.na(d$lon) & (d$lon < BBOX$lon[1] | d$lon > BBOX$lon[2]))
  flipped <- out & !is.na(d$lon) & d$lon > 0
  if (any(flipped)) err(where, glue("longitude is positive; west of Greenwich it should be negative: {rows_at(d, flipped)}"))
  if (any(out & !flipped)) err(where, glue("coordinates outside the expected area: {rows_at(d, out & !flipped)}"))
  d
}

blank_tokens <- function(d, cols) {
  for (col in intersect(cols, names(d))) d[[col]][d[[col]] %in% MISSING_TOKENS] <- NA
  d
}

strict_cols <- function(colspec) colspec$column[colspec$type %in% c("number", "date", "boolean")]

# geometry: bbox: [west, south, east, north] -> named c(bbox_west = "West", ...)
check_bbox <- function(g, d, yml_path) {
  cols <- unlist(g$bbox)
  if (is.null(cols)) return(character())
  if (length(cols) != 4) {
    err(yml_path, "geometry.bbox needs 4 columns: [west, south, east, north]")
    return(character())
  }
  miss <- setdiff(cols, names(d))
  for (m in miss) err(yml_path, glue('geometry.bbox: column "{m}" is not in the CSV.{suggest(m, names(d))}'))
  if (length(miss)) return(character())
  setNames(cols, BBOX_OUT)
}

# Numbers, all-or-nothing, west < east, south < north, inside the expected area
coerce_bbox <- function(d, where) {
  for (k in BBOX_OUT) {
    if (!k %in% names(d)) { d[[k]] <- NA_real_; next }
    num <- suppressWarnings(as.numeric(d[[k]]))
    bad <- !is.na(d[[k]]) & is.na(num)
    if (any(bad)) err(where, glue("{k} is not a number: {rows_at(d, bad)}"))
    d[[k]] <- num
  }
  n_set <- rowSums(!is.na(as.matrix(d[BBOX_OUT])))
  partial <- n_set > 0 & n_set < 4
  if (any(partial)) err(where, glue("bounding box needs all four values: {rows_at(d, partial)}"))

  ok <- n_set == 4
  flipped <- ok & (d$bbox_west >= d$bbox_east | d$bbox_south >= d$bbox_north)
  if (any(flipped)) err(where, glue("bounding box has west >= east or south >= north: {rows_at(d, flipped)}"))
  outside <- ok & (d$bbox_west < BBOX$lon[1] | d$bbox_east > BBOX$lon[2] |
                   d$bbox_south < BBOX$lat[1] | d$bbox_north > BBOX$lat[2])
  if (any(outside)) err(where, glue("bounding box outside the expected area (check CRS or signs): {rows_at(d, outside)}"))
  d
}

# ---- 2. Column specs ---------------------------------------------------------
infer_type <- function(x) {
  v <- x[!is.na(x)]
  if (!length(v)) return("text")
  if (all(!is.na(suppressWarnings(as.numeric(v))))) return("number")
  if (all(str_detect(v, "^\\d{4}(-\\d{2}){0,2}$"))) return("date")
  if (n_distinct(v) <= 30) "category" else "text"
}

# Combine the yml's `columns:` section with inferred defaults.
# `exclude` = CSV headers already used as shared or join keys.
build_column_spec <- function(d, spec, exclude, where) {
  data_cols <- setdiff(names(d), c(exclude, ".file", ".row"))

  for (col in setdiff(names(spec), c(data_cols, exclude))) {
    err(where, glue('columns: "{col}" is not a column in the CSV.{suggest(col, data_cols)}'))
  }

  empty <- tibble(column = character(), label = character(), type = character(),
                  filter = character(), visibility = character(), units = character(),
                  description = character(), values = list())
  if (!length(data_cols)) return(empty)

  map(data_cols, \(col) {
    s <- spec[[col]] %||% list()

    type <- s$type %||% infer_type(d[[col]])
    if (!type %in% COLUMN_TYPES) {
      err(where, glue('columns: "{col}" has type "{type}". Use one of: {toString(COLUMN_TYPES)}.'))
      type <- "text"
    }
    flt <- s$filter %||% DEFAULT_FILTER[[type]]
    if (!flt %in% FILTERS) {
      err(where, glue('columns: "{col}" has filter "{flt}". Use one of: {toString(FILTERS)}.'))
      flt <- "none"
    }
    vis <- s$visibility %||% if (type == "path") "hidden" else "public"
    if (!vis %in% VISIBILITY) {
      err(where, glue('columns: "{col}" has visibility "{vis}". Use one of: {toString(VISIBILITY)}.'))
      vis <- "hidden"   # when unsure, don't show
    }
    if (type == "path" && vis != "hidden") {
      warn(where, glue('"{col}" is a path with visibility "{vis}", so it will be displayed on the website. Use `hidden` to hide it'))
    }

    tibble(column = col, label = s$label %||% col, type = type, filter = flt,
           visibility = vis, units = s$units %||% NA_character_,
           description = s$description %||% NA_character_, values = list(s$values))
  }) |> list_rbind()
}

check_column_values <- function(d, colspec, where) {
  for (i in seq_len(nrow(colspec))) {
    col <- colspec$column[i]
    x   <- d[[col]]

    if (colspec$type[i] == "number") {
      bad <- !is.na(x) & is.na(suppressWarnings(as.numeric(x)))
      if (any(bad)) err(where, glue('"{col}" must be a plain number (no units or text): {rows_at(d, bad)}'))
    }

    allowed <- colspec$values[[i]]
    if (!is.null(allowed)) {
      bad <- !is.na(x) & !x %in% allowed
      for (v in unique(x[bad])) {
        err(where, glue('"{col}" value "{v}" is not one of [{toString(allowed)}].',
                        '{suggest(v, allowed)} ({rows_at(d, bad & x == v)})'))
      }
    }
  }
}

# Check a `shared:` or `join_on:` mapping against the valid keys and the CSV headers.
# Returns only the usable part of the mapping (portal name = CSV header).
check_mapping <- function(m, valid_keys, d, yml_path, section) {
  m <- unlist(m %||% list())
  for (k in setdiff(names(m), valid_keys)) {
    err(yml_path, glue("{section}: unknown key `{k}`.{suggest(k, valid_keys)} Valid keys: {toString(valid_keys)}."))
  }
  m <- m[names(m) %in% valid_keys]
  for (k in names(m)) if (!m[[k]] %in% names(d)) {
    err(yml_path, glue('{section}: `{k}` points to column "{m[[k]]}", which is not in the CSV.{suggest(m[[k]], names(d))}'))
  }
  m[m %in% names(d)]
}

# ---- 3. Plot registry ---------------------------------------------------------
read_plots <- function() {
  dir      <- path(DATA_DIR, "plots")
  yml_path <- path(dir, "plots.yml")
  s <- read_yml(yml_path) %||% list()
  d <- read_csv_folder(dir)
  if (is.null(d)) { err(dir, "no plots CSV found"); return(NULL) }

  smap <- check_mapping(s$shared, c("plot", "site", "lat", "lon"), d, yml_path, "shared")
  if (!all(c("plot", "lat", "lon") %in% names(smap))) {
    err(yml_path, "shared: `plot`, `lat` and `lon` are required")
    return(NULL)
  }

  colspec <- build_column_spec(d, s$columns %||% list(), unname(smap), yml_path)
  d <- blank_tokens(d, c(unname(smap), strict_cols(colspec))) 
  check_column_values(d, colspec, dir)

  plots <- d |> rename(all_of(smap))
  if (!"site" %in% names(plots)) plots$site <- NA_character_
  plots <- coerce_coords(plots, dir)

  blank <- is.na(plots$plot)
  if (any(blank)) err(dir, glue("blank plot name: {rows_at(plots, blank)}"))
  dup <- !blank & (duplicated(plots$plot) | duplicated(plots$plot, fromLast = TRUE))
  if (any(dup)) err(dir, glue("duplicate plot names ({toString(unique(plots$plot[dup]))}): {rows_at(plots, dup)}"))

  attr(plots, "colspec")  <- colspec
  plots
}

# ---- 4. Data types ------------------------------------------------------------
check_id <- function(s, dir, yml_path) {
  id <- s$id %||% path_file(dir)
  if (is.null(s$id)) {
    err(yml_path, "`id` is missing")
  } else if (tolower(s$id) != tolower(path_file(dir))) {
    warn(yml_path, glue('id "{s$id}" differs from the folder name "{path_file(dir)}"'))
  }
  if (!str_detect(id, "^[A-Za-z0-9_]+$")) err(yml_path, glue('id "{id}" may only contain letters, numbers and _'))
  id <- tolower(id)

  if (is.null(s$name)) err(yml_path, "`name` is missing")
  id
}

# shared: datapoint_name may be one column, a list of columns, or
#   {columns: [...], sep: "_", label: "..."}.
# A list is joined into a new column, which then acts as the datapoint name.
compose_datapoint_name <- function(s, d, yml_path) {
  spec <- s$shared$datapoint_name
  if (is.null(spec) || (is.character(spec) && length(spec) == 1)) {
    return(list(s = s, d = d, desc = NULL))
  }
  
  cols  <- unlist(if (is.list(spec) && !is.null(spec$columns)) spec$columns else spec)
  sep   <- (if (is.list(spec)) spec$sep)   %||% "_"
  label <- (if (is.list(spec)) spec$label) %||% "Datapoint ID"
  
  problems <- FALSE
  if (!is.character(cols) || length(cols) < 2) {
    err(yml_path, "shared: datapoint_name needs one column name, or a list of at least two")
    problems <- TRUE
  }
  for (m in setdiff(cols, names(d))) {
    err(yml_path, glue('shared: datapoint_name uses column "{m}", which is not in the CSV.{suggest(m, names(d))}'))
    problems <- TRUE
  }
  if (label %in% names(d)) {
    err(yml_path, glue('shared: datapoint_name label "{label}" is already a CSV column; choose another `label`'))
    problems <- TRUE
  }
  if (problems) {
    s$shared$datapoint_name <- NULL
    return(list(s = s, d = d, desc = NULL))
  }
  
  # "NA"/"N/A" in a part counts as missing: an ID should never contain them
  parts <- lapply(cols, \(c) replace(d[[c]], d[[c]] %in% MISSING_TOKENS, NA))
  incomplete <- Reduce(`|`, lapply(parts, is.na))
  id <- do.call(paste, c(parts, sep = sep))
  id[incomplete] <- NA          # treated like any blank name further down
  
  d[[label]] <- id
  s$shared$datapoint_name <- label
  list(s = s, d = d, desc = glue("{label} ({paste(cols, collapse = ' + ')})"))
}

read_datatype <- function(dir, plots) {
  yml_path <- path(dir, "schema.yml")
  if (!file_exists(yml_path)) { err(dir, "no schema.yml"); return(NULL) }
  s <- read_yml(yml_path); if (is.null(s)) return(NULL)
  id <- check_id(s, dir, yml_path)

  d <- read_csv_folder(dir, s$files %||% "*.csv")
  if (is.null(d)) { warn(dir, "no CSV files yet; data type skipped"); return(NULL) }

  d <- read_csv_folder(dir, s$files %||% "*.csv")
  if (is.null(d)) { warn(dir, "no CSV files yet; data type skipped"); return(NULL) }
  
  # -- multi-column datapoint names
  composed <- compose_datapoint_name(s, d, yml_path)
  s <- composed$s
  d <- composed$d
  
  # -- shared mapping
  smap <- check_mapping(s$shared, SHARED_KEYS, d, yml_path, "shared")
  
  bbox_cols <- check_bbox(s$geometry, d, yml_path)
  if (!"plot" %in% names(smap) && !all(c("lat", "lon") %in% names(smap)) && !length(bbox_cols)) {
    err(yml_path, "shared: map `plot`, or `lat` and `lon`, or add `geometry: bbox:`")
  }

  for (k in c("datapoint_name", "date", "project_id")) {
    if (!k %in% names(smap)) err(yml_path, glue("shared: `{k}` is required (and must match a CSV column)"))
  }
  if (!all(c("datapoint_name", "date", "project_id") %in% names(smap))) return(NULL)

  # -- type-specific columns
  colspec <- build_column_spec(d, s$columns %||% list(), c(unname(smap), unname(bbox_cols)), yml_path)
  d <- blank_tokens(d, c(unname(smap), unname(bbox_cols), strict_cols(colspec)))
  check_column_values(d, colspec, dir)

  d <- d |> rename(all_of(smap))
  if (length(bbox_cols)) d <- d |> rename(all_of(bbox_cols))
  for (k in setdiff(SHARED_KEYS, names(d))) d[[k]] <- NA_character_

  # -- datapoint names
  nm <- smap[["datapoint_name"]]
  if (!is.null(composed$desc)) nm <- composed$desc   # messages name the parts
  unique_names <- s$unique_names %||% TRUE
  if (!is.logical(unique_names) || length(unique_names) != 1) {
    err(yml_path, "`unique_names` must be true or false")
    unique_names <- TRUE
  }
  
  blank <- is.na(d$datapoint_name)
  if (any(blank)) {
    if (unique_names) {
      err(dir, glue('blank "{nm}": {rows_at(d, blank)}'))
    } else {
      # No name: generate a readable one, e.g. "freezer_samples-1"
      generated <- paste0(id, "-", seq_len(sum(blank)))
      clash <- generated %in% d$datapoint_name[!blank]
      if (any(clash)) {
        err(dir, glue('generated name(s) {toString(generated[clash])} already exist as "{nm}" values; ',
                      "rename those rows or fill in the blank names"))
      }
      d$datapoint_name[blank] <- generated
      note(dir, glue('{sum(blank)} row(s) without a "{nm}" were named {id}-1 to {id}-{sum(blank)}'))
    }
  }
  
  dup <- !is.na(d$datapoint_name) &
    (duplicated(d$datapoint_name) | duplicated(d$datapoint_name, fromLast = TRUE))
  if (any(dup)) {
    if (unique_names) {
      err(dir, glue('duplicate "{nm}" ({toString(head(unique(d$datapoint_name[dup]), 10))}): {rows_at(d, dup)}. ',
                    "If names may repeat for this data type, add `unique_names: false` to schema.yml."))
    } else {
      note(dir, glue('{n_distinct(d$datapoint_name[dup])} "{nm}" value(s) are shared by more than one row (unique_names: false)'))
    }
  }
  # -- dates
  d <- bind_cols(d, parse_partial_dates(d$date))
  bad <- d$date_precision %in% "invalid"
  if (any(bad)) err(dir, glue('"{smap[["date"]]}" must be YYYY-MM-DD, YYYY-MM or YYYY: {rows_at(d, bad)}'))
  if (any(is.na(d$date))) warn(dir, glue("no date, so these rows can't be matched to project years: {rows_at(d, is.na(d$date))}"))

  # -- coordinates: from plots.csv first, then the row's own lat/lon
  d <- coerce_coords(d, dir)
  d <- coerce_bbox(d, dir)   # also adds empty bbox columns for data types without boxes
  d <- d |> mutate(
    lat = coalesce(lat, (bbox_south + bbox_north) / 2),
    lon = coalesce(lon, (bbox_west  + bbox_east)  / 2)
  )
  if ("plot" %in% names(smap) && !is.null(plots)) {
    d <- d |>
      left_join(select(plots, plot, reg_site = site, reg_lat = lat, reg_lon = lon), by = "plot")
    differs <- (!is.na(d$lat) & !is.na(d$reg_lat) &
                (abs(d$lat - d$reg_lat) > 0.001 | abs(d$lon - d$reg_lon) > 0.001)) %in% TRUE
    if (any(differs)) warn(dir, glue("coordinates differ from plots.csv by more than ~100 m; using plots.csv: {rows_at(d, differs)}"))

    unreg <- !is.na(d$plot) & is.na(d$reg_lat) & !d$plot %in% plots$plot
    own   <- unreg & !is.na(d$lat)
    if (any(own)) note(dir, glue("{n_distinct(d$plot[own])} plot(s) are not in plots.csv; using the coordinates in the CSV for them"))

    d <- d |>
      mutate(lat  = coalesce(reg_lat, lat),
             lon  = coalesce(reg_lon, lon),
             site = coalesce(reg_site, site)) |>
      select(-starts_with("reg_"))
  }
  nomap <- is.na(d$lat) | is.na(d$lon)
  if (any(nomap)) {
    ps <- unique(na.omit(d$plot[nomap]))
    ex <- if (length(ps)) paste0(" Plots: ", toString(head(ps, 10)), if (length(ps) > 10) ", ..." else "") else ""
    warn(dir, glue("{sum(nomap)} row(s) have no coordinates and won't appear on the map.{ex}"))
  }

  # -- project links (one row per datapoint x project)
  # uid must be unique even when names aren't: repeated names get #1, #2, ...
  occurrence <- ave(seq_along(d$datapoint_name), d$datapoint_name, FUN = seq_along)
  repeated   <- d$datapoint_name %in% d$datapoint_name[duplicated(d$datapoint_name)]
  d <- d |> mutate(
    datatype = id,
    uid = if_else(repeated,
                  paste0(id, "::", datapoint_name, "#", occurrence),
                  paste0(id, "::", datapoint_name))
  )
  links <- d |>
    select(uid, datatype, project_id, .file, .row) |>
    separate_longer_delim(project_id, delim = ";") |>
    mutate(project_id = tolower(str_trim(project_id))) |>
    filter(!is.na(project_id), project_id != "")
  noproj <- !d$uid %in% links$uid
  if (any(noproj)) warn(dir, glue("no project ID: {rows_at(d, noproj)}"))

  # -- display, templates and parameters
  known <- c(names(smap), unname(smap), colspec$column)
  for (col in s$display$table %||% character()) {
    if (!col %in% known) err(yml_path, glue('display.table: "{col}" is not a column.{suggest(col, known)}'))
  }
  for (ph in str_match_all(s$display$popup %||% "", "\\{\\{\\s*([^}#^/]+?)\\s*\\}\\}")[[1]][, 2]) {
    if (!ph %in% known) err(yml_path, glue('display.popup: placeholder "{ph}" is not a column.{suggest(ph, known)}'))
  }
  for (t in s$access$templates %||% list()) {
    if (is.null(t$file) || !file_exists(path(dir, t$file))) {
      err(yml_path, glue('access.templates: file "{t$file %||% "?"}" not found in {dir}'))
    }
  }
  
  fz <- s$display$freezer
  if (!is.null(fz)) {
    for (k in c("drawer", "rack", "shelf", "position")) {
      if (is.null(fz[[k]])) err(yml_path, glue("display.freezer: `{k}` is required"))
    }
    for (k in names(fz)) for (col in unlist(fz[[k]])) {
      if (!col %in% known) err(yml_path, glue('display.freezer.{k}: "{col}" is not a column.{suggest(col, known)}'))
    }
  }
  params <- s$access$params %||% list()
  for (p in names(params)) {
    pr <- params[[p]]
    if (!(pr$type %||% "") %in% PARAM_TYPES) {
      err(yml_path, glue("access.params.{p}: type must be one of {toString(PARAM_TYPES)}"))
    }
    if (identical(pr$type, "select") && !is.null(pr$default) && !pr$default %in% pr$options) {
      err(yml_path, glue('access.params.{p}: default "{pr$default}" is not one of the options'))
    }
  }

  list(id = id, dir = dir, schema = s, smap = smap, data = d,
       colspec = colspec, links = links)
}

# ---- 5. Projects ----------------------------------------------------------------
read_project <- function(dir) {
  yml_path <- path(dir, "project.yml")
  if (!file_exists(yml_path)) { err(dir, "no project.yml"); return(NULL) }
  s <- read_yml(yml_path); if (is.null(s)) return(NULL)
  id <- check_id(s, dir, yml_path)

  out <- list(id = id, dir = dir, schema = s, data = NULL, colspec = NULL,
              join = NULL, mode = "exact")
  d <- read_csv_folder(dir)
  if (is.null(d)) return(out)   # a project without its own columns is fine

  join <- check_mapping(s$join_on, JOIN_KEYS, d, yml_path, "join_on")
  if (!length(join)) { err(yml_path, "has CSV files but no usable `join_on`"); return(out) }

  mode <- s$join_mode %||% "exact"
  if (!mode %in% c("exact", "as_of")) { err(yml_path, 'join_mode must be "exact" or "as_of"'); mode <- "exact" }
  if (mode == "as_of" && !"year" %in% names(join)) { err(yml_path, "join_mode as_of needs `year` in join_on"); mode <- "exact" }

   colspec <- build_column_spec(d, s$columns %||% list(), unname(join), yml_path)
  d <- blank_tokens(d, c(unname(join), strict_cols(colspec)))
  check_column_values(d, colspec, dir)

  d <- d |> rename(all_of(join))
  if ("year" %in% names(d)) {
    yr  <- suppressWarnings(as.integer(d$year))
    bad <- is.na(yr) | !str_detect(d$year %|% "", "^\\d{4}$")
    if (any(bad)) err(dir, glue("year must be a 4-digit year: {rows_at(d, bad)}"))
    d$year <- yr
  }

  key <- names(join)
  dup <- duplicated(d[key]) | duplicated(d[key], fromLast = TRUE)
  if (any(dup)) err(dir, glue("the same {toString(key)} appears more than once: {rows_at(d, dup)}"))

  modifyList(out, list(data = d, colspec = colspec, join = key, mode = mode))
}

`%|%` <- function(a, b) ifelse(is.na(a), b, a)

join_project <- function(p, datapoints, links) {
  rows <- datapoints |>
    semi_join(filter(links, project_id == p$id), by = "uid") |>
    mutate(year = as.integer(format(date_start, "%Y")))
  if (is.null(p$data)) return(rows)

  pd <- select(p$data, -.file, -.row)

  if (p$mode == "as_of") {
    # Match the most recent project row from the same year or earlier
    other <- setdiff(p$join, "year")
    by <- eval(parse(text = glue(
      "dplyr::join_by({paste(other, collapse = ', ')}, closest(year >= year))")))
  } else {
    by <- p$join
    orphans <- anti_join(p$data, rows, by = p$join)
    if (nrow(orphans)) warn(p$dir, glue(
      '{nrow(orphans)} row(s) match no datapoint tagged "{p$id}" ',
      "(typo, wrong year, or data not uploaded yet): ",
      "{rows_at(orphans, rep(TRUE, nrow(orphans)))}"))
  }

  left_join(rows, pd, by = by, relationship = "many-to-one")
}
# ---- 6. Publications -------------------------------------------------------------
read_publications <- function(project_ids, datatype_ids, links) {
  files <- dir_ls(path(DATA_DIR, "publications"), glob = "*.yml")

  pubs <- map(files, \(f) {
    p <- read_yml(f); if (is.null(p)) return(NULL)
    p$id <- path_ext_remove(path_file(f))

    if (is.null(p$citation) || !nzchar(str_trim(p$citation))) {
      err(f, "`citation` is required")
      return(NULL)
    }

    if (!is.null(p$doi)) {
      p$doi <- str_remove(str_trim(p$doi), "^(https?://)?(dx\\.)?doi\\.org/")
      if (!str_detect(p$doi, "^10\\.\\d{4,9}/\\S+$")) err(f, glue('"{p$doi}" does not look like a DOI'))
    }

    # Year for sorting: from `year:`, otherwise the first 19xx/20xx in the citation
    yr <- p$year %||% str_extract(p$citation, "\\b(19|20)\\d{2}\\b")
    p$year <- suppressWarnings(as.integer(yr))
    if (!is.null(yr) && is.na(p$year)) warn(f, glue('year "{yr}" is not a number'))
    p$projects <- tolower(p$projects %||% character())
    for (x in p$projects %||% character()) {
      if (!x %in% project_ids) err(f, glue('projects: unknown project "{x}".{suggest(x, project_ids)}'))
    }
    if (identical(p$datatypes, "inherit")) {
      p$datatypes <- links |> filter(project_id %in% p$projects) |> pull(datatype) |> unique()
    }
    for (x in p$datatypes %||% character()) {
      if (!x %in% datatype_ids) err(f, glue('datatypes: unknown data type "{x}".{suggest(x, datatype_ids)}'))
    }
    if (!length(c(p$projects, p$datatypes))) warn(f, "not linked to any project or data type")
    p
  }) |> compact() |> unname()

  dois <- unlist(map(pubs, "doi"))
  for (x in unique(dois[duplicated(dois)])) {
    err(path(DATA_DIR, "publications"), glue("DOI {x} is listed in more than one file"))
  }
  pubs
}

# One Markdown list item:  - Citation [doi:...](...) · [link](...)
#                            *note*
format_publication <- function(p) {
  links <- c(
    if (!is.null(p$doi)) glue("[doi:{p$doi}](https://doi.org/{p$doi})"),
    if (!is.null(p$url)) glue("[link]({p$url})")
  )
  line <- paste0("- ", str_squish(p$citation),
                 if (length(links)) paste0(" ", paste(links, collapse = " · ")) else "")
  if (!is.null(p$note)) line <- paste0(line, "  \n  *", str_squish(p$note), "*")
  line
}

write_publications <- function(pubs, project_ids, datatype_ids) {
  out <- path(OUT_DIR, "publications")
  dir_create(out)

  write_list <- function(sel, file) {
    sel <- sel[order(map_int(sel, "year"), decreasing = TRUE)]   # newest first, no year last
    txt <- if (length(sel)) paste(map_chr(sel, format_publication), collapse = "\n")
           else "*No publications yet.*"
    write_file(paste0(txt, "\n"), file)
  }

  for (id in project_ids)  write_list(keep(pubs, \(p) id %in% p$projects),  path(out, paste0("project_", id, ".md")))
  for (id in datatype_ids) write_list(keep(pubs, \(p) id %in% p$datatypes), path(out, paste0("datatype_", id, ".md")))
  write_list(pubs, path(out, "all.md"))   # for a portal-wide Publications page
}

# ---- 7. Outputs ----------------------------------------------------------------------
SHARED_OUT <- c("uid", "datatype", "datapoint_name", "plot", "site", "lat", "lon",
                "date", "date_start", "date_end", "date_precision", BBOX_OUT)

round_coords <- function(d, round_to) {
  if (identical(round_to, "exact") || is.null(round_to)) return(d)
  mutate(d, across(any_of(c("lat", "lon", BBOX_OUT)), \(x) round(x, round_to)))
}

datatype_meta <- function(x) {
  s <- x$schema; smap <- x$smap
  # display entries may use CSV headers; translate shared ones to portal names
  to_out <- function(cols) unname(ifelse(cols %in% smap, names(smap)[match(cols, smap)], cols))
  popup <- s$display$popup
  for (i in seq_along(smap)) {
    popup <- str_replace_all(popup %||% "", fixed(paste0("{{", smap[[i]], "}}")),
                             paste0("{{", names(smap)[i], "}}"))
  }
  templates <- map(s$access$templates %||% list(), \(t) {
    f <- path(x$dir, t$file %||% "")
    list(label = t$label %||% t$file, language = t$language %||% "bash",
         text = if (file_exists(f)) read_file(f) else NULL)
  })
  list(id = x$id, name = s$name, description = s$description, contact = s$contact,
       shared_labels = as.list(smap),
       columns = filter(x$colspec, visibility != "internal"),
       display = compact(list(table = to_out(s$display$table %||% character()),
                                      popup = popup,
                                      freezer = s$display$freezer)),
       params = s$access$params %||% list(), templates = templates)
}

write_navbar <- function(projects, datatypes) {
  item <- function(text, href) list(text = text, href = href)
  proj_menu <- c(list(item("All projects", "projects/index.qmd")),
                 unname(map(projects, \(p) item(p$schema$name %||% p$id, paste0("projects/", p$id, ".qmd")))))
  dt_menu   <- c(list(item("All data types", "datatypes/index.qmd")),
                 unname(map(datatypes, \(x) item(x$schema$name %||% x$id, paste0("datatypes/", x$id, ".qmd")))))
  nav <- list(website = list(navbar = list(left = list(
    item("Home", "index.qmd"),
    list(text = "Projects", menu = proj_menu),
    list(text = "Data types", menu = dt_menu),
    item("About", "about.qmd")
  ))))
  write_yaml(nav, path(OUT_DIR, "navbar.yml"))
}

# Fill _templates/*.qmd with {{id}} and {{name}}; skipped until the templates exist
write_pages <- function(projects, datatypes) {
  fill <- function(tpl, out, vars) {
    if (!file_exists(tpl)) return(invisible())
    txt <- read_file(tpl)
    for (k in names(vars)) txt <- str_replace_all(txt, fixed(paste0("{{", k, "}}")), vars[[k]])
    dir_create(path_dir(out))
    write_file(txt, out)
  }
  for (p in projects)  fill("_templates/project.qmd",  path("projects",  paste0(p$id, ".qmd")),
                            list(id = p$id, name = p$schema$name %||% p$id))
  for (x in datatypes) fill("_templates/datatype.qmd", path("datatypes", paste0(x$id, ".qmd")),
                            list(id = x$id, name = x$schema$name %||% x$id))
}

report_issues <- function() {
  if (!nrow(issues)) { cli_alert_success("No problems found"); return(invisible()) }
  for (w in unique(issues$where)) {
    cli_h3(w)
    for (i in which(issues$where == w)) {
      m <- issues$msg[i]   # passed as "{m}" so braces in messages aren't interpreted
      switch(issues$level[i],
             error   = cli_alert_danger("{m}"),
             warning = cli_alert_warning("{m}"),
             note    = cli_alert_info("{m}"))
    }
  }
  n <- table(factor(issues$level, c("error", "warning", "note")))
  cli_rule("{n[['error']]} error(s), {n[['warning']]} warning(s), {n[['note']]} note(s)")
}

# ---- 8. Run ---------------------------------------------------------------------------
cli_h1("Building the James Lab data portal")

plots <- read_plots()

dt_dirs   <- dir_ls(path(DATA_DIR, "datatypes"), type = "directory")
datatypes <- map(dt_dirs, read_datatype, plots = plots) |> compact() |> unname()
dt_ids    <- path_file(dt_dirs) |> unname()       # includes data types without CSVs yet

projects <- dir_ls(path(DATA_DIR, "projects"), type = "directory") |>
  map(read_project) |> compact() |> unname()
proj_ids <- map_chr(projects, "id")

links <- map(datatypes, "links") |> list_rbind()
for (pid in setdiff(unique(links$project_id), proj_ids)) {
  bad   <- links$project_id == pid
  where <- path(DATA_DIR, "datatypes", links$datatype[bad][1])
  
  is_probable_typo <- function(x, choices) {
    if (!length(choices)) return(FALSE)
    d <- adist(tolower(x), tolower(choices))[1, ]   # edits needed: insert, delete, replace
    max_d <- if (nchar(x) >= 8) 2 else 1            # short IDs: allow only 1 edit
    any(d <= max_d)
  }
  
  if (is_probable_typo(pid, proj_ids)) { 
    err(where, glue('project ID "{pid}" has no folder in data/projects/.',
                    '{suggest(pid, proj_ids)} ({rows_at(links, bad)})'))
  } else {
    # A project that hasn't been set up yet: skip it for now
    warn(where, glue('project ID "{pid}" is not configured yet (no data/projects/{pid}/ folder); ',
                     '{sum(bad)} datapoint(s) are not linked to it.{suggest(pid, proj_ids)}'))
  }
}

for (p in projects) {
  if (is.null(p$data) || !"plot" %in% p$join || is.null(plots)) next
  for (x in unique(p$data$plot[!p$data$plot %in% plots$plot])) {
    warn(p$dir, glue('plot "{x}" is not in plots.csv.{suggest(x, plots$plot)}'))
  }
}

datapoints <- map(datatypes, \(x) select(x$data, all_of(SHARED_OUT))) |> list_rbind()

joined <- map(projects, \(p) tryCatch(
  join_project(p, datapoints, links),
  error = function(e) { err(p$dir, glue("could not join project data: {conditionMessage(e)}")); NULL }
)) |> set_names(proj_ids)

pubs <- read_publications(proj_ids, dt_ids, links)

report_issues()
if (any(issues$level == "error")) cli_abort("Build stopped: fix the errors above, then run again.")

# -- write everything
dir_create(path(OUT_DIR, c("datatypes", "projects")))

write_csv(datapoints, path(OUT_DIR, "datapoints.csv"), na = "")
write_csv(select(links, uid, datatype, project_id), path(OUT_DIR, "project_links.csv"), na = "")

for (x in datatypes) {
  cols  <- x$colspec$column
  x$data |>
    select(all_of(SHARED_OUT), project_id, all_of(cols)) |>
    write_csv(path(OUT_DIR, "datatypes", paste0(x$id, ".csv")), na = "")
  write_json(datatype_meta(x), path(OUT_DIR, "datatypes", paste0(x$id, ".json")),
             auto_unbox = TRUE, pretty = TRUE, na = "null")
}

for (p in projects) {
  j <- joined[[p$id]]
  proj_cols <- p$colspec$column
  j |>
    select(any_of(c(SHARED_OUT, "year")), any_of(proj_cols)) |>
    write_csv(path(OUT_DIR, "projects", paste0(p$id, ".csv")), na = "")
  write_json(list(id = p$id, name = p$schema$name, description = p$schema$description,
                  contact = p$schema$contact, status = p$schema$status,
                  columns = p$colspec,
                  n_datapoints = nrow(j), datatypes = sort(unique(j$datatype))),
             path(OUT_DIR, "projects", paste0(p$id, ".json")),
             auto_unbox = TRUE, pretty = TRUE, na = "null")
}

overview <- map(projects, \(p) tibble(
  id           = p$id,
  name         = p$schema$name %||% p$id,
  description  = str_squish(p$schema$description %||% ""),
  n_datapoints = nrow(joined[[p$id]]),
  datatypes    = toString(sort(unique(joined[[p$id]]$datatype)))
)) |> list_rbind()
write_csv(overview, path(OUT_DIR, "projects_overview.csv"), na = "")

write_publications(pubs, proj_ids, dt_ids)
write_navbar(projects, datatypes)
write_pages(projects, datatypes)

cli_alert_success("Wrote {nrow(datapoints)} datapoints, {length(datatypes)} data type(s), {length(projects)} project(s) to {OUT_DIR}/")