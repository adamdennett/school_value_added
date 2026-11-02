# explore_js.R - The browser side of the area explorer (explore.qmd)
# ---------------------------------------------------------------------------
# Choose a level (region, combined or mayoral authority, local authority),
# then an area within it, then a measure. The map shows that area's schools
# coloured by the measure and zooms to them; a sortable table below lists
# them. All the data travel with the page.
# ---------------------------------------------------------------------------

EXPLORE_JS <- '
function(el, x, data) {
  var map = this;
  var S = data.schools, LEVELS = data.levels, MEASURES = data.measures, DEFAULT = data.default;
  var markers = L.layerGroup().addTo(map);
  var DIV = [[31,127,160],[232,230,238],[54,26,84]];
  function ramp(t) { t = Math.max(0, Math.min(1, t)); var i = Math.min(Math.floor(t * 2), 1), f = t * 2 - i;
    return "rgb(" + [0,1,2].map(function(k) { return Math.round(DIV[i][k] + f * (DIV[i+1][k] - DIV[i][k])); }).join(",") + ")"; }
  function fmt(v, d) { return v == null ? "" : (v > 0 ? "+" : "") + (+v).toFixed(d == null ? 1 : d); }
  function esc(s) { return s == null ? "" : String(s).replace(/[&<>]/g, function(c) { return {"&":"&amp;","<":"&lt;",">":"&gt;"}[c]; }); }

  // ---- controls above the map ----
  var bar = document.createElement("div");
  bar.className = "explore-bar";
  function sel(label, id) {
    var w = document.createElement("label"); w.className = "explore-ctl";
    w.innerHTML = "<span>" + label + "</span>";
    var s = document.createElement("select"); s.id = id; w.appendChild(s); bar.appendChild(w); return s;
  }
  var sLevel = sel("Level", "x-level"), sArea = sel("Area", "x-area"), sMeasure = sel("Colour by", "x-measure");
  LEVELS.forEach(function(l) { var o = document.createElement("option"); o.value = l.key; o.textContent = l.label; sLevel.appendChild(o); });
  MEASURES.forEach(function(m) { var o = document.createElement("option"); o.value = m.var; o.textContent = m.label; sMeasure.appendChild(o); });
  el.parentNode.insertBefore(bar, el);

  var summary = document.createElement("div"); summary.className = "explore-summary";
  var tableBox = document.createElement("div"); tableBox.className = "explore-table";
  el.parentNode.insertBefore(tableBox, el.nextSibling);
  el.parentNode.insertBefore(summary, el.nextSibling);

  // ---- legend ----
  var legDiv, leg = L.control({position: "bottomright"});
  leg.onAdd = function() { legDiv = L.DomUtil.create("div"); legDiv.className = "explore-legend"; return legDiv; };
  leg.addTo(map);

  function areasFor(level) {
    var seen = {}, out = [];
    S.forEach(function(d) { var a = d[level]; if (a && !seen[a]) { seen[a] = 1; out.push(a); } });
    return out.sort();
  }
  function fillAreas(keep) {
    var areas = areasFor(sLevel.value);
    sArea.innerHTML = "";
    areas.forEach(function(a) { var o = document.createElement("option"); o.value = a; o.textContent = a; sArea.appendChild(o); });
    if (keep && areas.indexOf(keep) >= 0) sArea.value = keep;
  }

  var sortKey = "va_shrunk_all", sortDesc = true;
  var COLS = [
    {key: "SCHNAME", label: "School", txt: true},
    {key: "LANAME", label: "Local authority", txt: true},
    {key: "va_shrunk_all", label: "All pupils", show: function(d) { return fmt(d.va_shrunk_all) + (d.va_lo_all == null ? "" :
      " <span class=\\"ci\\">(" + fmt(d.va_lo_all) + " to " + fmt(d.va_hi_all) + ")</span>"); }},
    {key: "va_shrunk_disadvantaged", label: "Disadv."}, {key: "va_shrunk_non_disadvantaged", label: "Non-disadv."},
    {key: "va_shrunk_girls", label: "Girls"}, {key: "va_shrunk_boys", label: "Boys"},
    {key: "abs_excess", label: "Absence vs intake"},
    {key: "ofsted_latest", label: "Ofsted latest", txt: true}];

  function drawTable(rows) {
    rows = rows.slice().sort(function(a, b) {
      var x = a[sortKey], y = b[sortKey];
      if (x == null) return 1; if (y == null) return -1;
      var c = (typeof x === "string") ? x.localeCompare(y) : x - y;
      return sortDesc ? -c : c; });
    var h = "<table class=\\"table table-sm table-striped\\"><thead><tr>" + COLS.map(function(c) {
      return "<th data-k=\\"" + c.key + "\\" class=\\"" + (c.txt ? "" : "num") + "\\">" + c.label +
        (c.key === sortKey ? (sortDesc ? " &#9660;" : " &#9650;") : "") + "</th>"; }).join("") + "</tr></thead><tbody>";
    rows.forEach(function(d) {
      h += "<tr>" + COLS.map(function(c) {
        var v = c.show ? c.show(d) : (c.txt ? esc(d[c.key]) : fmt(d[c.key]));
        var cls = c.txt ? "" : "num";
        if (c.key === "va_shrunk_all") cls += d.up ? " up" : (d.down ? " down" : "");
        return "<td class=\\"" + cls + "\\">" + v + "</td>"; }).join("") + "</tr>"; });
    tableBox.innerHTML = h + "</tbody></table>";
    tableBox.querySelectorAll("th").forEach(function(th) {
      th.addEventListener("click", function() { var k = th.getAttribute("data-k");
        if (k === sortKey) sortDesc = !sortDesc; else { sortKey = k; sortDesc = true; } drawTable(rows); }); });
  }

  function draw() {
    var level = sLevel.value, area = sArea.value;
    var m = MEASURES.filter(function(x) { return x.var === sMeasure.value; })[0];
    var rows = S.filter(function(d) { return d[level] === area; });
    var vals = rows.map(function(d) { return d[m.var]; }).filter(function(v) { return v != null; });
    var hi = Math.max.apply(null, vals.map(Math.abs).concat([1]));
    markers.clearLayers();
    var pts = [];
    rows.forEach(function(d) {
      if (d.lat == null) return;
      var v = d[m.var], t = v == null ? null : (v + hi) / (2 * hi);
      if (t != null && m.good === "low") t = 1 - t;
      L.circleMarker([d.lat, d.lon], {radius: Math.max(4, Math.sqrt(d.pupils || 900) / 4.5),
        fillColor: t == null ? "#ffffff" : ramp(t), fillOpacity: t == null ? 0.6 : 0.9,
        color: t == null ? "#8a8aa3" : "white", weight: 1})
        .bindTooltip("<b>" + esc(d.SCHNAME) + "</b> (" + esc(d.LANAME) + ")<br>All pupils " + fmt(d.va_shrunk_all) +
          ", disadvantaged " + fmt(d.va_shrunk_disadvantaged) + ", girls " + fmt(d.va_shrunk_girls) + ", boys " + fmt(d.va_shrunk_boys) +
          "<br>Absence vs intake " + fmt(d.abs_excess) + " pp<br>Ofsted: " + esc(d.ofsted_latest || "not known") +
          "<br><b>" + m.label + ": " + (v == null ? "no data" : fmt(v) + m.unit) + "</b>")
        .addTo(markers);
      pts.push([d.lat, d.lon]);
    });
    if (pts.length) map.fitBounds(pts, {padding: [24, 24], maxZoom: 13});
    legDiv.innerHTML = "<strong>" + m.label + "</strong><div class=\\"bar\\" style=\\"background:linear-gradient(to right," +
      (m.good === "low" ? "rgb(54,26,84),rgb(232,230,238),rgb(31,127,160)" : "rgb(31,127,160),rgb(232,230,238),rgb(54,26,84)") +
      ")\\"></div><span>" + fmt(-hi) + "</span><span style=\\"float:right\\">" + fmt(hi) + "</span><div class=\\"note\\">" +
      (m.good === "low" ? "purple below what the intake predicts, blue above" : "blue below expectation, purple above") + "</div>";
    var n = rows.length, va = rows.map(function(d) { return d.va_shrunk_all; }).filter(function(v) { return v != null; });
    var mean = va.reduce(function(a, b) { return a + b; }, 0) / Math.max(va.length, 1);
    var up = rows.filter(function(d) { return d.up; }).length, down = rows.filter(function(d) { return d.down; }).length;
    var ab = rows.map(function(d) { return d.abs_excess; }).filter(function(v) { return v != null; });
    var abm = ab.reduce(function(a, b) { return a + b; }, 0) / Math.max(ab.length, 1);
    var note = (data.notes[area] ? " <em>" + data.notes[area] + "</em>" : "");
    summary.innerHTML = "<strong>" + esc(area) + ":</strong> " + n + " schools. Value added for all pupils averages " +
      fmt(mean) + " points against England; " + up + " significantly above expectation, " + down + " below. " +
      "Absence runs " + fmt(abm) + " points against what intakes predict." + note;
    drawTable(rows);
  }

  sLevel.addEventListener("change", function() { fillAreas(); draw(); });
  sArea.addEventListener("change", draw);
  sMeasure.addEventListener("change", draw);
  sLevel.value = DEFAULT.level; fillAreas(DEFAULT.area); draw();
}
'
