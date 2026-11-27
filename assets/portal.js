// assets/portal.js
// Shared code for the data portal's data type and project pages.
// The page passes in its own d3, Inputs, html and DOM, so library versions always
// match the ones Quarto bundles.

export function makePortal({d3, Inputs, html, DOM, leaflet, Mustache}) {

  // ---- Helpers ---------------------------------------------------------------
  // jsonlite writes empty R lists as {} rather than [] or null, so normalise them
  const asArray    = v => Array.isArray(v) ? v : (v == null || typeof v === "object" ? [] : [v]);
  const str        = v => typeof v === "string" ? v : "";
  const num        = v => (v === "" || v == null) ? null : +v;
  const isDate     = v => v instanceof Date && !isNaN(v);
  const escapeHtml = s => String(s ?? "").replace(/[&<>"']/g,
    c => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;"}[c]));
  const withFlags  = rs => rs.map((r, i, a) => ({...r, first: i === 0, last: i === a.length - 1}));
  const unique     = (data, col) =>
    [...new Set(data.map(d => d[col]).filter(v => v != null && v !== ""))].sort();

  const PREVIEW_ROWS = 5;
  const CANADA = leaflet ? leaflet.latLngBounds([41, -141], [70, -52]) : null;

    // ---- Small input components --------------------------------------------------
  // Each returns an element with a `value` and fires "input" events, like Observable Inputs,
  // so they work with viewof and Inputs.form.
  const el   = (tag, props = {}) => Object.assign(document.createElement(tag), props);
  const emit = node => node.dispatchEvent(new Event("input", {bubbles: true}));
  const DAY  = 86400000;
  const isoDay = ms => new Date(ms).toISOString().slice(0, 10);

  // A plain button whose value counts clicks (for "Clear filters")
  function button(label) {
    const b = el("button", {type: "button", className: "pr-button", textContent: label});
    let clicks = 0;
    Object.defineProperty(b, "value", {get: () => clicks});
    b.onclick = () => { clicks++; emit(b); };
    return b;
  }

  // Toggle buttons that wrap to the available width; value = selected options
  function chips(options, {label, format = String} = {}) {
    const root = el("div", {className: "pr-field"});
    if (label) root.append(el("div", {className: "pr-label", textContent: label}));
    const wrap = el("div", {className: "pr-chips"});
    const selected = new Set();
    for (const o of options) {
      const b = el("button", {type: "button", className: "pr-chip", textContent: format(o)});
      b.setAttribute("aria-pressed", "false");
      b.onclick = () => {
        selected.has(o) ? selected.delete(o) : selected.add(o);
        b.setAttribute("aria-pressed", String(selected.has(o)));
        emit(root);
      };
      wrap.append(b);
    }
    root.append(wrap);
    Object.defineProperty(root, "value", {get: () => options.filter(o => selected.has(o))});
    return root;
  }

  // Toggle buttons for short lists, a scrolling multi-select for very long ones
  const choiceInput = (opts, o) => opts.length <= 40
    ? chips(opts, o)
    : Inputs.select(opts, {...o, multiple: true});

  // Two-handle slider with editable boxes underneath.
  // kind "number" or "date" (dates in ms, whole days). value = [lo, hi]:
  // each end is null while its handle is at the end of the range (= no filter on that side).
  function rangeSlider({label, min, max, step, kind = "number", units = ""}) {
    const dates = kind === "date";
    step = step ?? (dates ? DAY
      : Number.isInteger(min) && Number.isInteger(max) ? 1 : (max - min) / 200);
    const show = v => dates ? isoDay(v) : String(+(+v).toFixed(6));

    const root = el("div", {className: "pr-field"});
    root.append(el("div", {className: "pr-label", textContent: label + (units ? ` (${units})` : "")}));

    const box  = el("div", {className: "pr-range"});
    const fill = el("div", {className: "pr-fill"});
    const mk   = v => el("input", {type: "range", min, max, step, value: v});
    const lo = mk(min), hi = mk(max);
    box.append(el("div", {className: "pr-track"}), fill, lo, hi);

    const loBox = el("input", {type: dates ? "date" : "number"});
    const hiBox = el("input", {type: dates ? "date" : "number"});
    if (!dates) loBox.step = hiBox.step = "any";
    const readout = el("div", {className: "pr-readout"});
    readout.append(loBox, el("span", {textContent: "–"}), hiBox);
    root.append(box, readout);

    const atMin = v => v <= min + step / 2;
    const atMax = v => v >= max - step / 2;
    const pct   = v => ((v - min) / (max - min)) * 100;

    const sync = () => {
      const a = +lo.value, b = +hi.value;
      fill.style.left  = `${pct(a)}%`;
      fill.style.width = `${pct(b) - pct(a)}%`;
      loBox.value = show(a);
      hiBox.value = show(b);
      lo.style.zIndex = atMax(a) ? 3 : 2;   // keep the left handle grabbable when both sit at the right end
    };
    const changed = () => { sync(); emit(root); };

    lo.addEventListener("input", e => { e.stopPropagation(); if (+lo.value > +hi.value) lo.value = hi.value; changed(); });
    hi.addEventListener("input", e => { e.stopPropagation(); if (+hi.value < +lo.value) hi.value = lo.value; changed(); });

    // Typing in the boxes: applied on Enter or when leaving the box
    const parse = s => dates ? Date.parse(s) : parseFloat(s);   // "YYYY-MM-DD" parses as UTC midnight
    for (const [input, slider, other, isLo] of [[loBox, lo, hi, true], [hiBox, hi, lo, false]]) {
      input.addEventListener("input", e => e.stopPropagation());
      input.addEventListener("change", e => {
        e.stopPropagation();
        let v = parse(input.value);
        if (!Number.isFinite(v)) v = isLo ? min : max;
        v = Math.min(max, Math.max(min, v));
        v = isLo ? Math.min(v, +other.value) : Math.max(v, +other.value);
        slider.value = v;
        changed();
      });
    }

    sync();
    Object.defineProperty(root, "value", {get: () => {
      const a = +lo.value, b = +hi.value, out = v => dates ? new Date(v) : v;
      return [atMin(a) ? null : out(a), atMax(b) ? null : out(b)];
    }});
    return root;
  }

  // Inputs.form laid out as a vertical stack (labels above controls)
  const stack = inputs => Inputs.form(inputs, {
    template: ins => { const d = el("div", {className: "pr-filters"}); d.append(...Object.values(ins)); return d; }
  });

  // ---- Columns -----------------------------------------------------------------
  function normalizeColumns(columns) {
    const cols = asArray(columns).map(c => ({
      ...c, values: asArray(c.values), units: str(c.units), label: str(c.label) || c.column
    }));
    const publicCols = cols.filter(c => c.visibility === "public");
    const filterCols = publicCols.filter(c => c.filter && c.filter !== "none");
    return {cols, publicCols, filterCols};
  }

  // ---- Rows ----------------------------------------------------------------------
  // merge(r): extra fields placed *under* the row (e.g. the data type's full row)
  // text(r):  extra values to include in the free-text search
  function prepareRows(raw, cols, {merge = () => ({}), text = () => []} = {}) {
    const publicCols = cols.filter(c => c.visibility === "public");
    return raw.map(r => {
      const d = {
        ...merge(r),
        ...r,
        lat: num(r.lat), lon: num(r.lon),
        date_start: r.date_start ? new Date(r.date_start) : null,
        date_end:   r.date_end   ? new Date(r.date_end)   : null
      };
      for (const c of cols) if (c.type === "number") d[c.column] = num(r[c.column]);

      // Bounding box -> Leaflet [[south, west], [north, east]], or null
      const bb = ["bbox_south", "bbox_west", "bbox_north", "bbox_east"].map(k => num(r[k]));
      d.bbox = bb.every(Number.isFinite) ? [[bb[0], bb[1]], [bb[2], bb[3]]] : null;
      for (const c of cols) if (c.type === "number") d[c.column] = num(r[c.column]);
      d._text = [r.datapoint_name, r.plot, r.site, r.date,
                 ...publicCols.map(c => r[c.column]), ...text(r)]
        .map(v => v ?? "").join(" ").toLowerCase();
      return d;
    });
  }

    // ---- Filters -------------------------------------------------------------------
  // `extra` = page-specific inputs (project or data type), shown after the search box
  function filterForm(data, filterCols, extra = {}) {
    const inputs = {q: Inputs.text({label: "Search", placeholder: "Any column…"}), ...extra};

    const sites = unique(data, "site");
    if (sites.length > 1) inputs.site = choiceInput(sites, {label: "Site"});

    const starts = data.map(d => d.date_start).filter(isDate);
    const ends   = data.map(d => d.date_end).filter(isDate);
    if (starts.length && d3.max(ends) > d3.min(starts)) {
      inputs.date = rangeSlider({label: "Date", kind: "date", min: +d3.min(starts), max: +d3.max(ends)});
    }

    for (const c of filterCols) {
      const label = c.label + (c.units ? ` (${c.units})` : "");
      if (c.filter === "multiselect") {
        const opts = c.values.length ? c.values : unique(data, c.column);
        inputs[c.column] = choiceInput(opts, {label});
      } else if (c.filter === "select" || c.filter === "checkbox") {
        inputs[c.column] = Inputs.select([null, ...unique(data, c.column)], {label, format: v => v ?? "(any)"});
      } else if (c.filter === "search") {
        inputs[c.column] = Inputs.text({label, placeholder: "contains…"});
      } else if (c.filter === "range") {
        const v = data.map(d => d[c.column]).filter(Number.isFinite);
        const lo = d3.min(v), hi = d3.max(v);
        if (v.length && hi > lo) inputs[c.column] = rangeSlider({label: c.label, units: c.units, min: lo, max: hi});
      }
    }
    return stack(inputs);
  }

  function matches(d, f, filterCols) {
    if (f.q && !d._text.includes(f.q.toLowerCase())) return false;
    if (f.site?.length && !f.site.includes(d.site)) return false;

    // keep rows whose date range overlaps the chosen range (works for "2022" too)
    const [from, to] = f.date ?? [null, null];
    if (from && (!d.date_end   || d.date_end   < from)) return false;
    if (to   && (!d.date_start || d.date_start > to))   return false;

    for (const c of filterCols) {
      const v = d[c.column], fv = f[c.column];
      if (fv == null) continue;   // no control for this column (e.g. a single value)
      switch (c.filter) {
        case "multiselect":
          if (fv.length && !fv.includes(v)) return false; break;
        case "select": case "checkbox":
          if (v !== fv) return false; break;
        case "search":
          if (fv && !String(v ?? "").toLowerCase().includes(fv.toLowerCase())) return false; break;
        case "range": {
          const [lo, hi] = fv;
          if (lo != null && !(v >= lo)) return false;
          if (hi != null && !(v <= hi)) return false;
        }
      }
    }
    return true;
  }

  // ---- Map -----------------------------------------------------------------------
  function createMap({height = 460} = {}) {
    const el = html`<div style="height:${height}px;border-radius:6px;"></div>`;
    const map = leaflet.map(el, {preferCanvas: true, scrollWheelZoom: false, minZoom: 3});
    leaflet.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 18, attribution: "&copy; OpenStreetMap contributors"
    }).addTo(map);
    const layer = leaflet.layerGroup().addTo(map);

    const wrap = html`<div>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/leaflet@1.9.4/dist/leaflet.css">
      ${el}
    </div>`;
    wrap.map = map;
    wrap.layer = layer;

    // Remember the area to show; apply it only once the map has a real size
    wrap.fit = bounds => {
      wrap.target = bounds;
      if (el.clientWidth && el.clientHeight) map.fitBounds(bounds, {maxZoom: 13});
    };
    new ResizeObserver(() => {
      if (!el.clientWidth || !el.clientHeight) return;
      map.invalidateSize();
      if (wrap.target) map.fitBounds(wrap.target, {maxZoom: 13});
    }).observe(el);

    wrap.fit(CANADA);
    return wrap;
  }

  // Fill a display.popup template ("{{col}} · {{col}}") with a row's values, HTML-escaped
  function fillPopup(template, d) {
    const tpl = str(template);
    if (!tpl) return escapeHtml(d.datapoint_name);
    return tpl.replace(/\{\{\s*([^}]+?)\s*\}\}/g, (_, k) => escapeHtml(d[k]));
  }

    function drawMarkers(view, rows, selection, popupLine) {
    const {map, layer} = view;
    layer.clearLayers();
    const sel = new Set(selection.map(d => d.uid));

        // ---- Flights (rows with a bounding box): box + a point used only when zoomed out
    const boxStyle = on => ({
      color: on ? "#d95f02" : "#1b6f7a", weight: on ? 2.5 : 1.5,
      fillColor: on ? "#d95f02" : "#1b6f7a", fillOpacity: on ? 0.2 : 0.06
    });
    const boxes = rows.filter(d => d.bbox);
    const boxItems = boxes.map(d => {
      const on = sel.has(d.uid);
      const name = String(d.datapoint_name);
      const popup = `${popupLine(d)}${on ? " <b>✓</b>" : ""}`;
      const centre = [(d.bbox[0][0] + d.bbox[1][0]) / 2, (d.bbox[0][1] + d.bbox[1][1]) / 2];

      const rect = leaflet.rectangle(d.bbox, boxStyle(on))
        .bindTooltip(name).bindPopup(popup).addTo(layer);
      const dot = leaflet.circleMarker(centre, {
        radius: on ? 8 : 6, weight: 1, color: "#fff",
        fillColor: on ? "#d95f02" : "#1b6f7a", fillOpacity: 0.85
      }).bindTooltip(name).bindPopup(popup);      // same name and popup as the box

      return {bbox: d.bbox, dot};
    });

    // Show a flight's point only while its box is smaller than ~12 px on screen
    const toggleDots = () => {
      for (const b of boxItems) {
        const p1 = map.latLngToContainerPoint(b.bbox[0]);
        const p2 = map.latLngToContainerPoint(b.bbox[1]);
        const tiny = Math.abs(p2.x - p1.x) < 12 && Math.abs(p2.y - p1.y) < 12;
        if (tiny && !layer.hasLayer(b.dot)) b.dot.addTo(layer);
        if (!tiny && layer.hasLayer(b.dot)) layer.removeLayer(b.dot);
      }
    };
    if (view._toggleDots) map.off("zoomend", view._toggleDots);   // drop the previous redraw's handler
    view._toggleDots = toggleDots;
    map.on("zoomend", toggleDots);

    // ---- Point data: one marker per location (rows without a box)
    const pts = rows.filter(d => !d.bbox && d.lat != null && d.lon != null);
    const groups = d3.groups(pts, d => d.plot || `${d.lat.toFixed(5)},${d.lon.toFixed(5)}`);

    for (const [key, rs] of groups) {
      rs.sort((a, b) => (a.date_start ?? 0) - (b.date_start ?? 0));
      const n = rs.length;
      const anySel = rs.some(d => sel.has(d.uid));
      const first = rs[0];
      const items = rs.map(d => `<li>${popupLine(d)}${sel.has(d.uid) ? " <b>✓</b>" : ""}</li>`).join("");
      const popup = `<b>${escapeHtml(first.plot || first.datapoint_name)}</b>` +
        (first.site ? ` <small>(${escapeHtml(first.site)})</small>` : "") +
        `<br><small>${n} datapoint${n > 1 ? "s" : ""}</small>` +
        `<ul style="padding-left:1.1em;margin:.3em 0 0">${items}</ul>`;

      leaflet.circleMarker([first.lat, first.lon], {
        radius: (anySel ? 8 : 6) + Math.min(n - 1, 4),
        weight: n > 1 ? 2 : 1, color: "#fff",
        fillColor: anySel ? "#d95f02" : "#1b6f7a", fillOpacity: 0.85
      })
        .bindTooltip(n > 1 ? `${key} · ${n} datapoints` : key)
        .bindPopup(popup, {maxHeight: 260})
        .addTo(layer);
    }

    const corners = [...pts.map(d => [d.lat, d.lon]), ...boxes.flatMap(d => d.bbox)]
    .filter(ll => CANADA.contains(ll));
    view.fit(corners.length ? leaflet.latLngBounds(corners).pad(0.2) : CANADA);
    toggleDots();   // in case the zoom didn't change

    const shown = pts.length + boxes.length;
    const missing = rows.length - shown;
    return html`<small>${shown} of ${rows.length} datapoints on the map${
      groups.length ? ` (${groups.length} location(s)` : " ("}${
      boxes.length ? `${groups.length ? ", " : ""}${boxes.length} flight footprint(s)` : ""})${
      missing ? `, ${missing} without coordinates` : ""}.</small>`;
  }

  // ---- Code chunks ------------------------------------------------------------------
  function paramsForm(params) {
    const inputs = {};
    for (const [key, s] of Object.entries(params ?? {})) {
      const label = s.label ?? key;
      inputs[key] =
        s.type === "select"   ? Inputs.select(asArray(s.options), {label, value: s.default}) :
        s.type === "number"   ? Inputs.number({label, value: s.default}) :
        s.type === "checkbox" ? Inputs.toggle({label, value: !!s.default}) :
                                Inputs.text({label, value: s.default ?? ""});
    }
    return Inputs.form(inputs);
  }

  // One section per data type: parameters, language, preview, Copy, Download.
  // Built once, so parameter values survive filter changes; rows arrive via assignRows().
  function codePanels(ids, metaById, {headings = true, fileStem = id => id} = {}) {
    const wrap = html`<div></div>`;
    wrap.panels = new Map();

    for (const id of ids) {
      const m = metaById.get(id) ?? {};
      const templates = asArray(m.templates).filter(t => typeof t.text === "string" && t.text.trim() !== "");
      const form    = paramsForm(m.params);
      const tplPick = templates.length > 1
        ? Inputs.radio(templates.map(t => t.label), {label: "Language", value: templates[0].label})
        : null;

      const codeEl = html`<code></code>`;
      const pre    = html`<pre class="sourceCode" style="margin-top:.5rem">${codeEl}</pre>`;
      const status = html`<small></small>`;
      const dlSlot = html`<span></span>`;
      const copy   = html`<button class="btn btn-sm btn-outline-secondary">Copy</button>`;

      let rows = [];
      let full = "";

      const update = () => {
        if (!templates.length) {
          codeEl.textContent = "";
          status.textContent = "No code template for this data type yet.";
          dlSlot.replaceChildren();
          return;
        }
        const t = templates.find(t => t.label === tplPick?.value) ?? templates[0];
        const view = rs => ({datatype_id: id, n_rows: rows.length, ...form.value, rows: withFlags(rs)});

        full = Mustache.render(t.text, view(rows)).trim();
        const preview = rows.length <= PREVIEW_ROWS + 1 ? full
          : Mustache.render(t.text, view([
              ...rows.slice(0, PREVIEW_ROWS),
              {datapoint_name: `… ${(rows.length - PREVIEW_ROWS).toLocaleString()} more (included when you copy or download)`}
            ])).trim();
        codeEl.textContent = preview;

        const ext   = ({bash: "sh", r: "R", python: "py"})[t.language] ?? "txt";
        const fname = `${form.value.out || fileStem(id)}.${ext}`;
        dlSlot.replaceChildren(DOM.download(new Blob([full + "\n"], {type: "text/plain"}), fname, `Download ${fname}`));

        status.textContent = `${rows.length.toLocaleString()} datapoint(s)` +
          (preview !== full ? `. Preview shortened to ${PREVIEW_ROWS}; Copy and Download include all of them.` : "");
      };

      copy.onclick = async () => {
        await navigator.clipboard.writeText(full);
        copy.textContent = "Copied ✓";
        setTimeout(() => (copy.textContent = "Copy"), 1500);
      };
      form.addEventListener("input", update);
      tplPick?.addEventListener("input", update);

      const section = html`<section style="margin-bottom:2rem">
        ${headings ? html`<h3>${m.name ?? id}</h3>` : ""}
        ${form}
        ${tplPick ?? ""}
        <div>${copy} ${dlSlot} ${status}</div>
        ${templates.length ? pre : ""}
      </section>`;
      wrap.append(section);

      wrap.panels.set(id, {
        setRows: r => {
          rows = r;
          section.style.display = r.length ? "" : "none";   // hide data types with nothing selected
          update();
        }
      });
    }
    return wrap;
  }

  // Give each panel its rows; returns the rows grouped by data type
  function assignRows(panelsEl, rows) {
    const byType = d3.group(rows, d => d.datatype);
    for (const [id, panel] of panelsEl.panels) panel.setRows(byType.get(id) ?? []);
    return byType;
  }

  // ---- CSV download --------------------------------------------------------------------
  // format: {column: value => text} for columns that need converting (arrays, ids…)
  function csvDownload(rows, keys, labelOf, filename, format = {}) {
    const out = rows.map(d => Object.fromEntries(keys.map(k =>
      [labelOf(k), format[k] ? format[k](d[k], d) : (d[k] ?? "")])));
    return DOM.download(
      () => new Blob([d3.csvFormat(out)], {type: "text/csv"}),
      filename,
      `Download ${out.length} row(s) as CSV`
    );
  }

    // ---- Overview (home page and the "All …" pages) ---------------------------------
  // base = path from the current page to the site root: "" on the home page, "../" one level down

  function parseProjects(overview) {
    return overview.map(p => ({
      id: p.id,
      name: p.name || p.id,
      description: p.description ?? "",
      n: +p.n_datapoints || 0,
      datatypes: (p.datatypes ?? "").split(/,\s*/).filter(Boolean)
    }));
  }

  async function loadDatatypeMeta(ids, base = "") {
    const pairs = await Promise.all(ids.map(async id =>
      [id, await d3.json(`${base}_generated/datatypes/${id}.json`)]));
    return new Map(pairs);
  }

  function datatypeSummary(datapoints, dtMeta) {
    const counts = d3.rollup(datapoints, v => v.length, d => d.datatype);
    return [...dtMeta.keys()].map(id => ({
      id,
      name: dtMeta.get(id)?.name ?? id,
      n: counts.get(id) ?? 0,
      description: str(dtMeta.get(id)?.description).trim()
    }));
  }

  // Tables are built with DOM calls: text is always inserted as text, never as HTML
  const link = (href, text) => Object.assign(document.createElement("a"), {href, textContent: text});

  function joinNodes(nodes, sep = ", ") {
    const frag = document.createDocumentFragment();
    nodes.forEach((n, i) => { if (i) frag.append(sep); frag.append(n); });
    return frag;
  }

  function simpleTable(headers, rows, {numeric = []} = {}) {
    const table = document.createElement("table");
    table.className = "table table-sm table-hover";
    const head = table.createTHead().insertRow();
    headers.forEach((h, j) => {
      const th = document.createElement("th");
      th.textContent = h;
      if (numeric.includes(j)) th.style.textAlign = "right";
      head.append(th);
    });
    const body = table.createTBody();
    for (const r of rows) {
      const tr = body.insertRow();
      r.forEach((c, j) => {
        const td = tr.insertCell();
        td.append(c ?? "");
        if (numeric.includes(j)) td.style.textAlign = "right";
      });
    }
    return table;
  }

  function projectsTable(projects, dtName, base = "") {
    return simpleTable(
      ["Project", "Data types", "Datapoints", "Description"],
      projects.map(p => [
        link(`${base}projects/${p.id}.html`, p.name),
        joinNodes(p.datatypes.map(id => link(`${base}datatypes/${id}.html`, dtName(id)))),
        p.n.toLocaleString(),
        p.description
      ]),
      {numeric: [2]}
    );
  }

  function datatypesTable(types, base = "") {
    return simpleTable(
      ["Data type", "Datapoints", "Description"],
      types.map(t => [link(`${base}datatypes/${t.id}.html`, t.name), t.n.toLocaleString(), t.description]),
      {numeric: [1]}
    );
  }

  // ---- Freezer diagram -------------------------------------------------------------
  // spec = display.freezer: {drawer, rack, shelf, position, label} → column names.
  // Level order comes from each column's `values:` list in the schema.
  function freezerDiagram(rows, allRows, spec, cols, selection = []) {
    const levels = key => {
      const c = cols.find(c => c.column === spec[key]);
      return c?.values?.length ? c.values.map(String) : unique(allRows, spec[key]);
    };
    const D = levels("drawer"), R = levels("rack"), S = levels("shelf"), Pos = levels("position");
    const parts  = d => [spec.drawer, spec.rack, spec.shelf, spec.position].map(k => String(d[k] ?? ""));
    const keyOf  = d => parts(d).join("|");
    const placed = d => {
      const [dr, rk, sh, po] = parts(d);
      return D.includes(dr) && R.includes(rk) && S.includes(sh) && Pos.includes(po);
    };

    const total   = d3.rollup(allRows, v => v.length, keyOf);
    const colLabel   = c => cols.find(x => x.column === c)?.label ?? c;
    const boxCols    = asArray(spec.box_details).map(String).filter(c => allRows.some(d => d[c]));
    const sampleCols = asArray(spec.sample_details).map(String).filter(c => allRows.some(d => d[c]));

    // Box-level values: the first non-empty value among the box's samples
    const firstValue = (v, c) => v.find(d => d[c])?.[c] ?? "";
    const boxInfo = d3.rollup(allRows, v => ({
      label: firstValue(v, spec.label),
      byCol: Object.fromEntries(boxCols.map(c => [c, firstValue(v, c)]))
    }), keyOf);
    const matched = d3.group(rows.filter(placed), keyOf);
    const ticked  = new Set(selection.map(keyOf));
    const unplaced = rows.length - rows.filter(placed).length;

    const max   = d3.max([...matched.values()], v => v.length) ?? 1;
    const color = n => d3.interpolateBlues(0.3 + 0.7 * Math.sqrt(n / max));

    // ---- Geometry
    const cell = 13, rackGap = 10, drawerGap = 18, left = 78, top = 30;
    const rackW = Pos.length * cell, drawerH = S.length * cell;
    const width  = left + R.length * rackW + (R.length - 1) * rackGap + 6;
    const height = top + D.length * drawerH + (D.length - 1) * drawerGap + 6;

    const svg = d3.create("svg")
      .attr("viewBox", [0, 0, width, height])
      .attr("width", width)
      .style("max-width", "100%")
      .style("font", "11px sans-serif");

    R.forEach((rk, j) => svg.append("text")
      .attr("x", left + j * (rackW + rackGap) + rackW / 2).attr("y", top - 10)
      .attr("text-anchor", "middle").attr("fill", "#555").text(`Rack ${rk}`));

    // Custom tooltip (SVG <title> tooltips are slow or missing in some browsers)
    const holder = html`<div style="position:relative;display:inline-block;max-width:100%"></div>`;
    const tip = html`<div style="position:absolute;display:none;pointer-events:none;z-index:10;
      background:rgba(33,37,41,.92);color:#fff;padding:4px 8px;border-radius:4px;
      font-size:12px;line-height:1.35;white-space:pre"></div>`;

    const showTip = (event, text) => {
      tip.textContent = text;
      tip.style.display = "block";
      const b = holder.getBoundingClientRect();
      let x = event.clientX - b.left + 12;
      const y = event.clientY - b.top + 12;
      if (x + tip.offsetWidth > b.width) x = event.clientX - b.left - tip.offsetWidth - 12;  // flip near the right edge
      tip.style.left = `${x}px`;
      tip.style.top  = `${y}px`;
    };
    const hideTip = () => { tip.style.display = "none"; };

    D.forEach((dr, i) => {
      const y0 = top + i * (drawerH + drawerGap);
      svg.append("text").attr("x", 2).attr("y", y0 + drawerH / 2).attr("dy", "0.35em")
        .attr("font-weight", "bold").text(`Drawer ${dr}`);
      S.forEach((sh, k) => svg.append("text")
        .attr("x", left - 6).attr("y", y0 + k * cell + cell / 2).attr("dy", "0.35em")
        .attr("text-anchor", "end").attr("fill", "#888").text(sh));

      R.forEach((rk, j) => {
        const x0 = left + j * (rackW + rackGap);
        svg.append("rect").attr("x", x0 - 1).attr("y", y0 - 1)
          .attr("width", rackW + 1).attr("height", drawerH + 1)
          .attr("fill", "none").attr("stroke", "#adb5bd");

        S.forEach((sh, k) => Pos.forEach((po, m) => {
          const key = [dr, rk, sh, po].join("|");
          const n = matched.get(key)?.length ?? 0;
          const t = total.get(key) ?? 0;
          const isTicked = ticked.has(key);
          const r = svg.append("rect")
            .attr("x", x0 + m * cell).attr("y", y0 + k * cell)
            .attr("width", cell - 1).attr("height", cell - 1)
            .attr("fill", n ? color(n) : t ? "#e9ecef" : "#fff")
            .attr("stroke", isTicked ? "#d95f02" : t ? "#ced4da" : "#f1f3f5")
            .attr("stroke-width", isTicked ? 2 : 0.5);
          if (t) {
            const info = boxInfo.get(key);
            r.append("title").text([
              `Drawer ${dr} · Rack ${rk} · Shelf ${sh} · Position ${po}`,
              info.label || "(no box label)",
              ...boxCols.filter(c => info.byCol[c]).map(c => `${colLabel(c)}: ${info.byCol[c]}`),
              `${n} of ${t} sample(s) match`
            ].join("\n"));
          }
        }));
      });
    });

       // Sample ID plus its sample-level details, e.g. "T003_103 (Row 2, slot 5)"
    const sampleText = d => {
      const extra = sampleCols.map(c => d[c]).filter(Boolean).join(", ");
      return extra ? `${d.datapoint_name} (${extra})` : d.datapoint_name;
    };

    const order = (arr, v) => arr.indexOf(v);
    const boxes = [...matched].map(([key, v]) => {
      const [dr, rk, sh, po] = key.split("|");
      return {dr, rk, sh, po, info: boxInfo.get(key), n: v.length, ids: v.map(sampleText)};
    }).sort((a, b) =>
      order(D, a.dr) - order(D, b.dr) || order(R, a.rk) - order(R, b.rk) ||
      order(S, a.sh) - order(S, b.sh) || order(Pos, a.po) - order(Pos, b.po));

    const idHeader = sampleCols.length
      ? `Sample IDs (${sampleCols.map(colLabel).join(", ")})`
      : "Sample IDs";

    const pick = simpleTable(
      ["Drawer", "Rack", "Shelf", "Position", "Box", ...boxCols.map(colLabel), "Samples", idHeader],
      boxes.map(b => [
        b.dr, b.rk, b.sh, b.po,
        b.info.label,
        ...boxCols.map(c => b.info.byCol[c]),
        b.n.toLocaleString(),
        b.ids.slice(0, 8).join("; ") + (b.ids.length > 8 ? `; … (+${b.ids.length - 8})` : "")
      ]),
      {numeric: [5 + boxCols.length]}
    );

    holder.append(svg.node(), tip);

    return html`<div>
      ${holder}
      <div><small>Darker = more matching samples in that box · grey = box with no matching samples ·
        orange outline = box with ticked rows. In each rack: shelves A–D top to bottom,
        positions front (left) to back (right). Hover a box for details.</small></div>
      <h4 style="margin-top:1rem">Boxes to pull (${boxes.length})</h4>
      ${boxes.length
        ? html`<div style="max-height:400px;overflow:auto">${pick}</div>`
        : html`<p><i>No matching samples.</i></p>`}
      ${unplaced ? html`<small>${unplaced} matching sample(s) have no complete freezer location.</small>` : ""}
    </div>`;
  }

  return {
    asArray, str, num, isDate, escapeHtml, unique, CANADA, PREVIEW_ROWS,
    normalizeColumns, prepareRows, filterForm, matches,
    button, chips, rangeSlider, stack,
    createMap, fillPopup, drawMarkers, freezerDiagram,
    paramsForm, codePanels, assignRows, csvDownload,
    parseProjects, loadDatatypeMeta, datatypeSummary, projectsTable, datatypesTable
  };
}