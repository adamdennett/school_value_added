# explore_js.R - The browser side of the area explorer (explore.qmd)
# ---------------------------------------------------------------------------
# Choose a level (region, combined or mayoral authority, local authority),
# an area, a measure and a period. The period is either the four-year figure
# (shrunk, with intervals) or a moving average of yearly value added centred
# on a chosen year over 1, 3 or 5 years. The map colours the area's schools,
# a trajectory chart shows every school's yearly value added and the area's
# average, and a sortable table lists the schools with a sparkline each.
# All the data travel with the page.
# ---------------------------------------------------------------------------

EXPLORE_JS <- '
function(el, x, data) {
  var map = this;
  var S = data.schools, LEVELS = data.levels, MEASURES = data.measures, DEFAULT = data.default;
  var YEARS = data.years, PROV = data.provisional_year, CAPS = data.caps_ma;
  var GROUPS = ["all", "disadvantaged", "non_disadvantaged", "girls", "boys"];
  var GLAB = {all: "All pupils", disadvantaged: "Disadv.", non_disadvantaged: "Non-disadv.", girls: "Girls", boys: "Boys"};
  var markers = L.layerGroup().addTo(map);
  var DIV = [[31,127,160],[232,230,238],[54,26,84]];
  function ramp(t) { t = Math.max(0, Math.min(1, t)); var i = Math.min(Math.floor(t * 2), 1), f = t * 2 - i;
    return "rgb(" + [0,1,2].map(function(k) { return Math.round(DIV[i][k] + f * (DIV[i+1][k] - DIV[i][k])); }).join(",") + ")"; }
  function fmt(v, d) { if (v == null || isNaN(v)) return ""; var s = (+v).toFixed(d == null ? 1 : d);
    if (+s === 0) return s.replace("-", ""); return (v > 0 ? "+" : "") + s; }
  function esc(s) { return s == null ? "" : String(s).replace(/[&<>]/g, function(c) { return {"&":"&amp;","<":"&lt;",">":"&gt;"}[c]; }); }
  function yl(y) { return y.replace("-", "/") + (y === PROV ? " (prov.)" : ""); }

  // ---- controls ----
  var bar = document.createElement("div"); bar.className = "explore-bar";
  function sel(label, id) {
    var w = document.createElement("label"); w.className = "explore-ctl";
    w.innerHTML = "<span>" + label + "</span>";
    var s = document.createElement("select"); s.id = id; w.appendChild(s); bar.appendChild(w); return s;
  }
  function opts(s, list) { list.forEach(function(o) { var e = document.createElement("option");
    e.value = o[0]; e.textContent = o[1]; s.appendChild(e); }); }
  var sLevel = sel("Level", "x-level"), sArea = sel("Area", "x-area"), sMeasure = sel("Colour by", "x-measure");
  var sPeriod = sel("Period", "x-period"), sCentre = sel("Centred on", "x-centre"), sWin = sel("Window", "x-window");
  opts(sLevel, LEVELS.map(function(l) { return [l.key, l.label]; }));
  opts(sMeasure, MEASURES.map(function(m) { return [m.var, m.label]; }));
  opts(sPeriod, [["four", "Four years (shrunk)"], ["ma", "Moving average"]]);
  opts(sCentre, YEARS.map(function(y) { return [y, yl(y)]; }));
  opts(sWin, [["1", "1 year"], ["3", "3 years"], ["5", "5 years"]]);
  sCentre.value = YEARS[YEARS.length - 3] || YEARS[0]; sWin.value = "3";
  el.parentNode.insertBefore(bar, el);

  var summary = document.createElement("div"); summary.className = "explore-summary";
  var traj = document.createElement("div"); traj.className = "explore-traj";
  var tableBox = document.createElement("div"); tableBox.className = "explore-table";
  el.parentNode.insertBefore(tableBox, el.nextSibling);
  el.parentNode.insertBefore(traj, el.nextSibling);
  var distBox = document.createElement("div"); distBox.className = "explore-dist";
  el.parentNode.insertBefore(distBox, el.nextSibling);
  el.parentNode.insertBefore(summary, el.nextSibling);

  var legDiv, leg = L.control({position: "bottomright"});
  leg.onAdd = function() { legDiv = L.DomUtil.create("div"); legDiv.className = "explore-legend"; return legDiv; };
  leg.addTo(map);

  // ---- values for the chosen period ----
  function windowYears() {
    var i = YEARS.indexOf(sCentre.value), h = (parseInt(sWin.value, 10) - 1) / 2, out = [];
    for (var k = Math.max(0, i - h); k <= Math.min(YEARS.length - 1, i + h); k++) out.push(k);
    return out;
  }
  function val(d, g) {
    if (sPeriod.value === "four") return d["va_shrunk_" + g];
    var ys = d["y_" + g]; if (!ys) return null;
    var w = windowYears(), s = 0, n = 0;
    w.forEach(function(k) { if (ys[k] != null) { s += ys[k]; n++; } });
    return n ? s / n : null;
  }
  function measure() { return MEASURES.filter(function(x) { return x.var === sMeasure.value; })[0]; }
  function value(d, m) { return m.group ? val(d, m.group) : d[m.var]; }
  function cap(m) {
    if (!m.group || sPeriod.value === "four") return m.cap;
    return CAPS[m.group][sWin.value];
  }
  function periodText() {
    if (sPeriod.value === "four") return "four years, shrunk";
    var w = windowYears();
    return (w.length === 1 ? yl(YEARS[w[0]]) : "average of " + w.map(function(k) { return yl(YEARS[k]); }).join(", "));
  }

  function areasFor(level) {
    var seen = {}, out = [];
    S.forEach(function(d) { var a = d[level]; if (a && !seen[a]) { seen[a] = 1; out.push(a); } });
    return out.sort();
  }
  function fillAreas(keep) {
    var areas = areasFor(sLevel.value); sArea.innerHTML = "";
    opts(sArea, areas.map(function(a) { return [a, a]; }));
    if (keep && areas.indexOf(keep) >= 0) sArea.value = keep;
  }

  // ---- sparkline and trajectory chart ----
  function spark(ys, lim) {
    if (!ys) return "";
    var W = 92, H = 24, n = YEARS.length, px = function(k) { return 4 + k * (W - 8) / (n - 1); },
        py = function(v) { v = Math.max(-lim, Math.min(lim, v)); return H / 2 - v / lim * (H / 2 - 3); };
    var pts = [], s = "<svg width=\\"" + W + "\\" height=\\"" + H + "\\"><line x1=\\"0\\" x2=\\"" + W + "\\" y1=\\"" + H / 2 +
      "\\" y2=\\"" + H / 2 + "\\" stroke=\\"#cfc8dc\\"/>";
    ys.forEach(function(v, k) { if (v != null) pts.push([px(k), py(v), k]); });
    for (var i = 1; i < pts.length; i++) s += "<line x1=\\"" + pts[i-1][0] + "\\" y1=\\"" + pts[i-1][1] + "\\" x2=\\"" + pts[i][0] +
      "\\" y2=\\"" + pts[i][1] + "\\" stroke=\\"#361a54\\" stroke-width=\\"1.4\\"" + (YEARS[pts[i][2]] === PROV ? " stroke-dasharray=\\"2,2\\"" : "") + "/>";
    pts.forEach(function(p) { var prov = YEARS[p[2]] === PROV;
      s += "<circle cx=\\"" + p[0] + "\\" cy=\\"" + p[1] + "\\" r=\\"2\\" fill=\\"" + (prov ? "#fff" : "#361a54") + "\\" stroke=\\"#361a54\\"/>"; });
    return s + "</svg>";
  }
  function drawTraj(rows, g, area) {
    var W = 720, H = 260, L0 = 44, R0 = 44, T0 = 30, B0 = 30, n = YEARS.length, lim = CAPS[g]["1"];
    var px = function(k) { return L0 + k * (W - L0 - R0) / (n - 1); },
        py = function(v) { v = Math.max(-lim, Math.min(lim, v)); return T0 + (lim - v) / (2 * lim) * (H - T0 - B0); };
    var s = "<svg viewBox=\\"0 0 " + W + " " + H + "\\" width=\\"100%\\" role=\\"img\\" aria-label=\\"Value added by year\\">";
    if (sPeriod.value === "ma") { var w = windowYears(), x0 = px(w[0]) - 14, x1 = px(w[w.length - 1]) + 14;
      s += "<rect x=\\"" + x0 + "\\" y=\\"" + T0 + "\\" width=\\"" + (x1 - x0) + "\\" height=\\"" + (H - T0 - B0) + "\\" fill=\\"#eedeff\\"/>"; }
    [-lim, -lim / 2, 0, lim / 2, lim].forEach(function(v) {
      s += "<line x1=\\"" + L0 + "\\" x2=\\"" + (W - R0) + "\\" y1=\\"" + py(v) + "\\" y2=\\"" + py(v) + "\\" stroke=\\"" + (v === 0 ? "#6b6680" : "#ece8f3") + "\\"/>" +
        "<text x=\\"" + (L0 - 6) + "\\" y=\\"" + (py(v) + 4) + "\\" text-anchor=\\"end\\" font-size=\\"11\\" fill=\\"#6b6680\\">" + fmt(v, 0) + "</text>"; });
    YEARS.forEach(function(y, k) { s += "<text x=\\"" + px(k) + "\\" y=\\"" + (H - 10) + "\\" text-anchor=\\"middle\\" font-size=\\"11\\" fill=\\"#4a4060\\">" + yl(y) + "</text>"; });
    function path(ys, colour, width, op) {
      var out = "", prev = null;
      ys.forEach(function(v, k) { if (v == null) { prev = null; return; }
        if (prev) out += "<line x1=\\"" + px(prev[0]) + "\\" y1=\\"" + py(prev[1]) + "\\" x2=\\"" + px(k) + "\\" y2=\\"" + py(v) +
          "\\" stroke=\\"" + colour + "\\" stroke-width=\\"" + width + "\\" opacity=\\"" + op + "\\"" + (YEARS[k] === PROV ? " stroke-dasharray=\\"4,3\\"" : "") + "/>";
        prev = [k, v]; });
      return out; }
    rows.forEach(function(d) { if (d["y_" + g]) s += path(d["y_" + g], "#ba82ff", 1, rows.length > 60 ? 0.25 : 0.55); });
    var mean = YEARS.map(function(y, k) { var a = rows.map(function(d) { return d["y_" + g] ? d["y_" + g][k] : null; })
      .filter(function(v) { return v != null; }); return a.length ? a.reduce(function(p, q) { return p + q; }, 0) / a.length : null; });
    s += path(mean, "#361a54", 3.2, 1);
    mean.forEach(function(v, k) { if (v != null) s += "<circle cx=\\"" + px(k) + "\\" cy=\\"" + py(v) + "\\" r=\\"4\\" fill=\\"" +
      (YEARS[k] === PROV ? "#fff" : "#361a54") + "\\" stroke=\\"#361a54\\" stroke-width=\\"2\\"/>"; });
    s += "<text x=\\"" + L0 + "\\" y=\\"16\\" font-size=\\"13\\" font-weight=\\"700\\" fill=\\"#361a54\\">" + esc(area) + ": value added by year, " +
      GLAB[g].toLowerCase() + "</text></svg>";
    traj.innerHTML = s + "<div class=\\"note\\">Each school a thin line, the area average bold" +
      (sPeriod.value === "ma" ? "; shaded, the years averaged" : "") + ". Points beyond &plusmn;" + lim.toFixed(1) + " are drawn at the edge. " +
      yl(PROV) + " is provisional (dashed, hollow) and will be updated with the revised KS4 release in February 2027.</div>";
  }

  // ---- table ----
  var sortKey = "v_all", sortDesc = true;
  function drawTable(rows, m) {
    var g = m.group || "all", four = sPeriod.value === "four", lim = CAPS[g]["1"];
    var cols = [{key: "SCHNAME", label: "School", txt: true}, {key: "LANAME", label: "Local authority", txt: true}];
    GROUPS.forEach(function(gg) { cols.push({key: "v_" + gg, label: GLAB[gg]}); });
    cols.push({key: "abs_excess", label: "Absence vs intake"}, {key: "spark", label: "Trajectory, " + GLAB[g].toLowerCase(), nosort: true},
              {key: "ofsted_latest", label: "Ofsted latest", txt: true});
    rows.forEach(function(d) { GROUPS.forEach(function(gg) { d["v_" + gg] = val(d, gg); }); });
    rows = rows.slice().sort(function(a, b) { var x = a[sortKey], y = b[sortKey];
      if (x == null) return 1; if (y == null) return -1;
      var c = (typeof x === "string") ? x.localeCompare(y) : x - y; return sortDesc ? -c : c; });
    var h = "<table class=\\"table table-sm table-striped\\"><thead><tr>" + cols.map(function(c) {
      return "<th data-k=\\"" + c.key + "\\" class=\\"" + (c.txt || c.nosort ? "" : "num") + "\\">" + c.label +
        (c.key === sortKey ? (sortDesc ? " &#9660;" : " &#9650;") : "") + "</th>"; }).join("") + "</tr></thead><tbody>";
    rows.forEach(function(d) {
      h += "<tr>" + cols.map(function(c) {
        if (c.key === "spark") return "<td>" + spark(d["y_" + g], lim) + "</td>";
        if (c.txt) return "<td>" + esc(d[c.key]) + "</td>";
        var v = fmt(d[c.key]), cls = "num";
        if (c.key === "v_all" && four) { cls += d.up ? " up" : (d.down ? " down" : "");
          if (d.va_lo_all != null) v += " <span class=\\"ci\\">(" + fmt(d.va_lo_all) + " to " + fmt(d.va_hi_all) + ")</span>"; }
        return "<td class=\\"" + cls + "\\">" + v + "</td>"; }).join("") + "</tr>"; });
    tableBox.innerHTML = h + "</tbody></table>";
    tableBox.querySelectorAll("th").forEach(function(th) {
      var k = th.getAttribute("data-k"); if (k === "spark") return;
      th.addEventListener("click", function() { if (k === sortKey) sortDesc = !sortDesc; else { sortKey = k; sortDesc = true; }
        drawTable(rows, m); }); });
  }

  // How the values on the map are spread, under it, in the map colours.
  // Every measure here is diverging on a symmetric cap, so one path does it:
  // a dot each while the schools are few, bins once they are not.
  function drawDist(rows, m, hi) {
    var W = 720, L = 10, R = 10, T = 12, B = 20, H = 96, iw = W - L - R, ih = H - T - B;
    var vals = [];
    rows.forEach(function(d) { var v = value(d, m); if (v != null && !isNaN(v)) vals.push(v); });
    if (!vals.length) { distBox.innerHTML = ""; return; }
    var lo = -hi, span = 2 * hi;
    var tOf = function(v) { var t = (v - lo) / span; if (t < 0) t = 0; if (t > 1) t = 1; return t; };
    var col = function(v) { var t = tOf(v); return ramp(m.good === "low" ? 1 - t : t); };
    var xs = function(v) { return L + tOf(v) * iw; };
    var s = "<line x1=\\"" + L + "\\" y1=\\"" + (T + ih) + "\\" x2=\\"" + (L + iw) +
            "\\" y2=\\"" + (T + ih) + "\\" stroke=\\"#d8d8e0\\"></line>";
    var note;
    if (vals.length <= 120) {
      var sorted = vals.slice().sort(function(a, b) { return a - b; }), lanes = [];
      sorted.forEach(function(v) {
        var cx = xs(v), r = 0;
        while (lanes[r] != null && cx - lanes[r] < 7.6) r++;
        lanes[r] = cx;
        var cy = T + ih - 5.5 - r * 7.6; if (cy < T + 4) cy = T + 4;
        s += "<circle cx=\\"" + cx.toFixed(1) + "\\" cy=\\"" + cy.toFixed(1) +
             "\\" r=\\"3.4\\" fill=\\"" + col(v) + "\\" stroke=\\"#fff\\" stroke-width=\\"0.7\\"></circle>";
      });
      note = "One dot a school, " + vals.length + " of them, coloured as on the map";
    } else {
      var NB = 28, bins = [], i;
      for (i = 0; i < NB; i++) bins[i] = 0;
      vals.forEach(function(v) { var b = Math.floor(tOf(v) * NB); if (b > NB - 1) b = NB - 1; bins[b]++; });
      var mx = 0;
      bins.forEach(function(c) { if (c > mx) mx = c; });
      var bw = iw / NB;
      bins.forEach(function(c, b) {
        if (!c) return;
        var hgt = (c / mx) * (ih - 4);
        s += "<rect x=\\"" + (L + b * bw + 0.6).toFixed(1) + "\\" y=\\"" + (T + ih - hgt).toFixed(1) +
             "\\" width=\\"" + (bw - 1.2).toFixed(1) + "\\" height=\\"" + hgt.toFixed(1) +
             "\\" fill=\\"" + col(lo + (b + 0.5) * span / NB) + "\\"></rect>";
      });
      note = vals.length + " schools in " + NB + " bins, coloured as on the map";
    }
    s += "<line x1=\\"" + xs(0).toFixed(1) + "\\" y1=\\"" + T + "\\" x2=\\"" + xs(0).toFixed(1) +
         "\\" y2=\\"" + (T + ih + 4) + "\\" stroke=\\"#999\\" stroke-dasharray=\\"3 2\\"></line>";
    s += "<text x=\\"" + xs(0).toFixed(1) + "\\" y=\\"" + (T + ih + 15) +
         "\\" text-anchor=\\"middle\\" font-size=\\"10\\" fill=\\"#666\\">0</text>";
    s += "<text x=\\"" + L + "\\" y=\\"" + (T + ih + 15) + "\\" font-size=\\"10\\" fill=\\"#666\\">&le; " +
         fmt(-hi) + m.unit + "</text>";
    s += "<text x=\\"" + (L + iw) + "\\" y=\\"" + (T + ih + 15) +
         "\\" text-anchor=\\"end\\" font-size=\\"10\\" fill=\\"#666\\">&ge; " + fmt(hi) + m.unit + "</text>";
    distBox.innerHTML = "<svg viewBox=\\"0 0 " + W + " " + H + "\\" width=\\"100%\\" height=\\"" + H +
      "\\" preserveAspectRatio=\\"xMidYMin meet\\" role=\\"img\\" aria-label=\\"Spread of " + esc(m.label) +
      "\\">" + s + "</svg><div class=\\"note\\">" + note + "</div>";
  }

  function draw() {
    var level = sLevel.value, area = sArea.value, m = measure();
    var isVA = !!m.group;
    sCentre.disabled = sWin.disabled = (sPeriod.value === "four");
    sPeriod.disabled = !isVA;
    var rows = S.filter(function(d) { return d[level] === area; });
    var hi = cap(m) || 1;
    markers.clearLayers();
    var pts = [];
    rows.forEach(function(d) {
      if (d.lat == null) return;
      var v = value(d, m), t = v == null ? null : (v + hi) / (2 * hi);
      if (t != null && m.good === "low") t = 1 - t;
      L.circleMarker([d.lat, d.lon], {radius: Math.max(4, Math.sqrt(d.pupils || 900) / 4.5),
        fillColor: t == null ? "#ffffff" : ramp(t), fillOpacity: t == null ? 0.6 : 0.9,
        color: t == null ? "#8a8aa3" : "white", weight: 1})
        .bindTooltip("<b>" + esc(d.SCHNAME) + "</b> (" + esc(d.LANAME) + ")<br>Four years: all pupils " + fmt(d.va_shrunk_all) +
          ", disadvantaged " + fmt(d.va_shrunk_disadvantaged) + ", girls " + fmt(d.va_shrunk_girls) + ", boys " + fmt(d.va_shrunk_boys) +
          "<br>Absence vs intake " + fmt(d.abs_excess) + " pp<br>Ofsted: " + esc(d.ofsted_latest || "not known") +
          "<br><b>" + m.label + (isVA ? " (" + periodText() + ")" : "") + ": " + (v == null ? "no data" : fmt(v) + m.unit) + "</b>")
        .addTo(markers);
      pts.push([d.lat, d.lon]);
    });
    if (pts.length) map.fitBounds(pts, {padding: [24, 24], maxZoom: 13});
    legDiv.innerHTML = "<strong>" + m.label + "</strong>" + (isVA ? "<div class=\\"note\\">" + periodText() + "</div>" : "") +
      "<div class=\\"bar\\" style=\\"background:linear-gradient(to right," +
      (m.good === "low" ? "rgb(54,26,84),rgb(232,230,238),rgb(31,127,160)" : "rgb(31,127,160),rgb(232,230,238),rgb(54,26,84)") +
      ")\\"></div><span>&le; " + fmt(-hi) + "</span><span style=\\"float:right\\">&ge; " + fmt(hi) + "</span><div class=\\"note\\">" +
      (m.good === "low" ? "purple below what the intake predicts, blue above" : "blue below expectation, purple above") + "</div>";

    var g = m.group || "all";
    var vals = rows.map(function(d) { return val(d, g); }).filter(function(v) { return v != null; });
    var mean = vals.reduce(function(a, b) { return a + b; }, 0) / Math.max(vals.length, 1);
    var ab = rows.map(function(d) { return d.abs_excess; }).filter(function(v) { return v != null; });
    var abm = ab.reduce(function(a, b) { return a + b; }, 0) / Math.max(ab.length, 1);
    var counts = sPeriod.value === "four" ?
      rows.filter(function(d) { return d.up; }).length + " significantly above expectation, " +
      rows.filter(function(d) { return d.down; }).length + " below (all pupils)" :
      vals.filter(function(v) { return v > 0; }).length + " above zero, " + vals.filter(function(v) { return v < 0; }).length + " below";
    var provNote = (sPeriod.value === "ma" && windowYears().some(function(k) { return YEARS[k] === PROV; })) ?
      " <em>This window includes provisional " + PROV.replace("-", "/") + ", to be updated with the revised KS4 release in February 2027.</em>" : "";
    summary.innerHTML = "<strong>" + esc(area) + ":</strong> " + rows.length + " schools. Value added for " + GLAB[g].toLowerCase() +
      " (" + periodText() + ") averages " + fmt(mean) + " points against England; " + counts + ". " +
      "Absence runs " + fmt(abm) + " points against what intakes predict." + (data.notes[area] ? " <em>" + data.notes[area] + "</em>" : "") + provNote;
    drawDist(rows, m, hi);
    drawTraj(rows, g, area);
    drawTable(rows, m);
  }

  sLevel.addEventListener("change", function() { fillAreas(); draw(); });
  [sArea, sMeasure, sPeriod, sCentre, sWin].forEach(function(s) { s.addEventListener("change", draw); });
  sLevel.value = DEFAULT.level; fillAreas(DEFAULT.area); draw();
}
'
