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
  // Bad -> good, centred on zero: ColorBrewer PRGn by default, or red,
  // amber and green or red, yellow and blue from the Colours menu (shared
  // with the area maps)
  // Sequential versions (no pale middle) for levels with a good direction,
  // and a neutral grey ramp for measures with none (FSM, EAL, ...), as on
  // the area maps
  var PALS = {
    prgn: {name: "Purple, green (colour-blind safe)", good: "green", bad: "purple",
          c: [[118,42,131],[153,112,171],[194,165,207],[231,212,232],[247,247,247],[217,240,211],[166,219,160],[90,174,97],[27,120,55]],
          seq: [[118,42,131],[153,112,171],[194,165,207],[231,212,232],[166,219,160],[90,174,97],[27,120,55]]},
    rag: {name: "Red, amber, green", good: "green",
          c: [[215,48,39],[244,109,67],[253,174,97],[254,224,139],[255,255,191],[217,239,139],[166,217,106],[102,189,99],[26,152,80]],
          seq: [[215,48,39],[244,109,67],[253,174,97],[254,224,139],[217,239,139],[145,207,96],[26,152,80]]},
    rdylbu: {name: "Red, yellow, blue (colour-blind safe)", good: "blue",
          c: [[215,48,39],[244,109,67],[253,174,97],[254,224,144],[255,255,191],[224,243,248],[171,217,233],[116,173,209],[69,117,180]],
          seq: [[215,48,39],[244,109,67],[253,174,97],[254,224,144],[171,217,233],[116,173,209],[69,117,180]]}};
  var NEUTRAL = [[241,241,245],[158,158,176],[58,58,78]];
  var CATS = data.cats || {}, HEADS = data.heads || {}, NONE = data.none || {};
  var scheme = "prgn";
  try { var s0 = localStorage.getItem("va-pal"); if (PALS[s0]) scheme = s0; } catch (e) {}
  function DIV() { return PALS[scheme].c; }
  // Ofsted grades take the chosen scheme (good to bad); other categories keep their own colours
  function catsOf(n) { var o = (data.cats_s || {})[scheme]; return (o && o[n]) || CATS[n] || {}; }
  function badWord() { return PALS[scheme].bad || "red"; }
  function rampC(C, t) { var n = C.length - 1; t = Math.max(0, Math.min(1, t));
    var i = Math.min(Math.floor(t * n), n - 1), f = t * n - i;
    return "rgb(" + [0,1,2].map(function(k) { return Math.round(C[i][k] + f * (C[i+1][k] - C[i][k])); }).join(",") + ")"; }
  function ramp(t) { return rampC(DIV(), t); }
  function gradC(C, rev) { C = C.slice(); if (rev) C.reverse();
    return C.map(function(c) { return "rgb(" + c.join(",") + ")"; }).join(","); }
  function grad(rev) { return gradC(DIV(), rev); }
  function fmt(v, d) { if (v == null || isNaN(v)) return ""; var s = (+v).toFixed(d == null ? 1 : d);
    if (+s === 0) return s.replace("-", ""); return (v > 0 ? "+" : "") + s; }
  // a value as the measure shows it: signed for a diverging measure, plain otherwise
  function fmtM(v, m) {
    if (v == null || (m.scale !== "cat" && isNaN(v))) return m.scale === "cat" ? (NONE[m.var] || "not known") : "no data";
    if (m.scale === "cat") return String(v);
    if (m.scale === "div") return fmt(v, m.dp) + m.unit;
    return (+v).toFixed(m.dp) + m.unit;
  }
  // How a measure colours the schools of an area: a function of the value,
  // and the ends of its scale (a fixed national cap for diverging measures,
  // the range within the area for the rest)
  function scaleFor(rows, m) {
    if (m.scale === "cat") return {colour: function(v) { return (catsOf(m.var) || {})[v] || "#c9c9d6"; }};
    if (m.scale === "div") {
      var hi = cap(m) || 1;
      return {lo: -hi, hi: hi, colour: function(v) { var t = (v + hi) / (2 * hi);
        return ramp(m.good === "low" ? 1 - t : t); }};
    }
    var vs = rows.map(function(d) { return value(d, m); }).filter(function(v) { return v != null && !isNaN(v); });
    var lo = vs.length ? Math.min.apply(null, vs) : 0, hi2 = vs.length ? Math.max.apply(null, vs) : 1;
    var C = m.good ? PALS[scheme].seq : NEUTRAL;
    return {lo: lo, hi: hi2, colour: function(v) { var t = (v - lo) / ((hi2 - lo) || 1);
      return rampC(C, m.good === "low" ? 1 - t : t); }};
  }
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
  var og = null;
  MEASURES.forEach(function(m) {
    if (!og || og.label !== m.grp) { og = document.createElement("optgroup"); og.label = m.grp; sMeasure.appendChild(og); }
    var e = document.createElement("option"); e.value = m.var; e.textContent = m.label; og.appendChild(e);
  });
  opts(sPeriod, [["four", "Four years (shrunk)"], ["ma", "Moving average"]]);
  opts(sCentre, YEARS.map(function(y) { return [y, yl(y)]; }));
  opts(sWin, [["1", "1 year"], ["3", "3 years"], ["5", "5 years"]]);
  sCentre.value = YEARS[YEARS.length - 3] || YEARS[0]; sWin.value = "3";
  var sPal = sel("Colours", "x-colours");
  opts(sPal, Object.keys(PALS).map(function(k) { return [k, PALS[k].name]; }));
  sPal.value = scheme;
  el.parentNode.insertBefore(bar, el);

  var summary = document.createElement("div"); summary.className = "explore-summary";
  var traj = document.createElement("div"); traj.className = "explore-traj";
  var tableBox = document.createElement("div"); tableBox.className = "explore-table";
  el.parentNode.insertBefore(tableBox, el.nextSibling);
  el.parentNode.insertBefore(traj, el.nextSibling);
  el.parentNode.insertBefore(summary, el.nextSibling);
  // the spread of the mapped values sits straight under the map, as on the area pages
  var distBox = document.createElement("div"); distBox.className = "explore-dist";
  distBox.style.margin = "6px 0 14px";
  el.parentNode.insertBefore(distBox, el.nextSibling);

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
    cols.push({key: "abs_excess", label: "Absence vs intake"});
    // the measure on the map, when it is not already a column
    if (!m.group && m.var !== "abs_excess")
      cols.push({key: m.var, label: m.short || m.label, txt: m.scale === "cat", m: m});
    cols.push({key: "spark", label: "Trajectory, " + GLAB[g].toLowerCase(), nosort: true},
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
        if (c.m) return "<td class=\\"num\\">" + (d[c.key] == null ? "" : fmtM(d[c.key], c.m)) + "</td>";
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

  // How the values on the map are spread, under it, in the map colours: a
  // dot each while the schools are few, bins once they are not, and count
  // bars for a measure in categories.
  function drawDist(rows, m, sc, counts) {
    var W = 720, L = 10, R = 10, T = 12, B = 20, iw = W - L - R, s = "", H, ih, note;
    if (m.scale === "cat") {
      var keys = Object.keys(catsOf(m.var) || {}).filter(function(k) { return counts[k]; });
      if (!keys.length) { distBox.innerHTML = ""; return; }
      var bh = 15, gap = 4, lw = 230, mx0 = 0;
      ih = keys.length * (bh + gap) - gap; H = T + ih + B;
      keys.forEach(function(k) { if (counts[k] > mx0) mx0 = counts[k]; });
      keys.forEach(function(k, j) {
        var y = T + j * (bh + gap), w = (counts[k] / mx0) * (iw - lw - 34);
        s += "<rect x=\\"" + (L + lw) + "\\" y=\\"" + y + "\\" width=\\"" + w.toFixed(1) + "\\" height=\\"" + bh +
             "\\" rx=\\"2\\" fill=\\"" + catsOf(m.var)[k] + "\\"></rect>" +
             "<text x=\\"" + (L + lw - 7) + "\\" y=\\"" + (y + bh - 3.5) + "\\" text-anchor=\\"end\\" font-size=\\"11\\" fill=\\"#333\\">" + esc(k) + "</text>" +
             "<text x=\\"" + (L + lw + w + 5) + "\\" y=\\"" + (y + bh - 3.5) + "\\" font-size=\\"11\\" fill=\\"#666\\">" + counts[k] + "</text>";
      });
      note = "Schools in each category";
    } else {
      var vals = [];
      rows.forEach(function(d) { var v = value(d, m); if (v != null && !isNaN(v)) vals.push(v); });
      if (!vals.length) { distBox.innerHTML = ""; return; }
      H = 96; ih = H - T - B;
      var lo = sc.lo, span = (sc.hi - sc.lo) || 1;
      var tOf = function(v) { var t = (v - lo) / span; return t < 0 ? 0 : (t > 1 ? 1 : t); };
      var xs = function(v) { return L + tOf(v) * iw; };
      s = "<line x1=\\"" + L + "\\" y1=\\"" + (T + ih) + "\\" x2=\\"" + (L + iw) + "\\" y2=\\"" + (T + ih) + "\\" stroke=\\"#d8d8e0\\"></line>";
      if (vals.length <= 120) {
        var sorted = vals.slice().sort(function(a, b) { return a - b; }), lanes = [];
        sorted.forEach(function(v) {
          var cx = xs(v), r = 0;
          while (lanes[r] != null && cx - lanes[r] < 7.6) r++;
          lanes[r] = cx;
          var cy = T + ih - 5.5 - r * 7.6; if (cy < T + 4) cy = T + 4;
          s += "<circle cx=\\"" + cx.toFixed(1) + "\\" cy=\\"" + cy.toFixed(1) + "\\" r=\\"3.4\\" fill=\\"" + sc.colour(v) +
               "\\" stroke=\\"#fff\\" stroke-width=\\"0.7\\"></circle>";
        });
        note = "One dot a school, " + vals.length + " of them, coloured as on the map";
      } else {
        var NB = 28, bins = [], i, mx = 0;
        for (i = 0; i < NB; i++) bins[i] = 0;
        vals.forEach(function(v) { var b = Math.floor(tOf(v) * NB); if (b > NB - 1) b = NB - 1; bins[b]++; });
        bins.forEach(function(c) { if (c > mx) mx = c; });
        var bw = iw / NB;
        bins.forEach(function(c, b) {
          if (!c) return;
          var hgt = (c / mx) * (ih - 4);
          s += "<rect x=\\"" + (L + b * bw + 0.6).toFixed(1) + "\\" y=\\"" + (T + ih - hgt).toFixed(1) +
               "\\" width=\\"" + (bw - 1.2).toFixed(1) + "\\" height=\\"" + hgt.toFixed(1) +
               "\\" fill=\\"" + sc.colour(lo + (b + 0.5) * span / NB) + "\\"></rect>";
        });
        note = vals.length + " schools in " + NB + " bins, coloured as on the map";
      }
      if (m.scale === "div") s += "<line x1=\\"" + xs(0).toFixed(1) + "\\" y1=\\"" + T + "\\" x2=\\"" + xs(0).toFixed(1) +
        "\\" y2=\\"" + (T + ih + 4) + "\\" stroke=\\"#999\\" stroke-dasharray=\\"3 2\\"></line><text x=\\"" + xs(0).toFixed(1) +
        "\\" y=\\"" + (T + ih + 15) + "\\" text-anchor=\\"middle\\" font-size=\\"10\\" fill=\\"#666\\">0</text>";
      s += "<text x=\\"" + L + "\\" y=\\"" + (T + ih + 15) + "\\" font-size=\\"10\\" fill=\\"#666\\">" +
           (m.scale === "div" ? "&le; " : "") + fmtM(sc.lo, m) + "</text>" +
           "<text x=\\"" + (L + iw) + "\\" y=\\"" + (T + ih + 15) + "\\" text-anchor=\\"end\\" font-size=\\"10\\" fill=\\"#666\\">" +
           (m.scale === "div" ? "&ge; " : "") + fmtM(sc.hi, m) + "</text>";
    }
    distBox.innerHTML = "<div style=\\"font-size:12px;color:#444;margin-bottom:2px\\"><b>" + esc(m.label) +
      "</b> &mdash; how the schools on the map are spread</div>" +
      "<svg viewBox=\\"0 0 " + W + " " + H + "\\" width=\\"100%\\" height=\\"" + H +
      "\\" preserveAspectRatio=\\"xMidYMin meet\\" role=\\"img\\" aria-label=\\"Spread of " + esc(m.label) +
      "\\">" + s + "</svg><div style=\\"font-size:11px;color:#666\\">" + note + "</div>";
  }

  function legendFor(m, sc, counts, missing) {
    var h = "<strong>" + m.label + "</strong>" + (m.group ? "<div class=\\"note\\">" + periodText() + "</div>" : "");
    if (m.scale === "cat") {
      var cur = null;
      Object.keys(catsOf(m.var) || {}).forEach(function(k) {
        if (!counts[k]) return;
        var hd = HEADS[m.var] ? HEADS[m.var][k] : null;
        if (hd && hd !== cur) { h += "<div class=\\"note\\" style=\\"margin-top:4px\\">" + hd + "</div>"; cur = hd; }
        h += "<div><span style=\\"display:inline-block;width:11px;height:11px;border-radius:50%;margin-right:6px;vertical-align:-1px;background:" +
             catsOf(m.var)[k] + "\\"></span>" + esc(k) + " (" + counts[k] + ")</div>";
      });
    } else {
      var C = m.scale === "div" ? DIV() : (m.good ? PALS[scheme].seq : NEUTRAL);
      h += "<div class=\\"bar\\" style=\\"background:linear-gradient(to right," + gradC(C, m.good === "low") + ")\\"></div>" +
           "<span>" + (m.scale === "div" ? "&le; " : "") + fmtM(sc.lo, m) + "</span><span style=\\"float:right\\">" +
           (m.scale === "div" ? "&ge; " : "") + fmtM(sc.hi, m) + "</span>";
      if (m.scale === "div") h += "<div class=\\"note\\">" + (m.good === "low" ? badWord() + " above what the intake predicts, " + PALS[scheme].good + " below"
                                                                              : badWord() + " below expectation, " + PALS[scheme].good + " above") + "</div>";
      else if (m.good) h += "<div class=\\"note\\">" + PALS[scheme].good + " is better, " + badWord() + " worse</div>";
      else h += "<div class=\\"note\\">no better or worse: shading only</div>";
    }
    if (missing) h += "<div class=\\"note\\">" + (NONE[m.var] || "no data") + " (" + missing + ")</div>";
    return h;
  }

  function draw() {
    var level = sLevel.value, area = sArea.value, m = measure();
    var isVA = !!m.group;
    sCentre.disabled = sWin.disabled = (sPeriod.value === "four");
    sPeriod.disabled = !isVA;
    var rows = S.filter(function(d) { return d[level] === area; });
    var sc = scaleFor(rows, m);
    var catCounts = {}, missing = 0;
    markers.clearLayers();
    var pts = [];
    rows.forEach(function(d) {
      if (d.lat == null) return;
      var v = value(d, m), none = v == null || (m.scale !== "cat" && isNaN(v));
      if (none) missing++; else if (m.scale === "cat") catCounts[v] = (catCounts[v] || 0) + 1;
      L.circleMarker([d.lat, d.lon], {radius: Math.max(4, Math.sqrt(d.pupils || 900) / 4.5),
        fillColor: none ? "#ffffff" : sc.colour(v), fillOpacity: none ? 0.6 : 0.92,
        color: none ? "#8a8aa3" : "rgba(40,40,50,0.55)", weight: none ? 1 : 0.8})
        .bindTooltip("<b>" + esc(d.SCHNAME) + "</b> (" + esc(d.LANAME) + ")<br>Four years: all pupils " + fmt(d.va_shrunk_all) +
          ", disadvantaged " + fmt(d.va_shrunk_disadvantaged) + ", non-disadvantaged " + fmt(d.va_shrunk_non_disadvantaged) + ", girls " + fmt(d.va_shrunk_girls) + ", boys " + fmt(d.va_shrunk_boys) +
          "<br>Absence vs intake " + fmt(d.abs_excess) + " pp<br>Ofsted: " + esc(d.ofsted_latest || "not known") +
          "<br><b>" + m.label + (isVA ? " (" + periodText() + ")" : "") + ": " + esc(fmtM(v, m)) + "</b>")
        .addTo(markers);
      pts.push([d.lat, d.lon]);
    });
    if (pts.length) map.fitBounds(pts, {padding: [24, 24], maxZoom: 13});
    legDiv.innerHTML = legendFor(m, sc, catCounts, missing);

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
    // what the measure on the map says about the area, when it is not value added
    var mNote = "";
    if (!isVA && m.var !== "abs_excess") {
      if (m.scale === "cat") {
        mNote = " " + m.label + ": " + Object.keys(catCounts).sort(function(a, b) { return catCounts[b] - catCounts[a]; })
          .map(function(k) { return esc(k) + " " + catCounts[k]; }).join(", ") + ".";
      } else {
        var mv = rows.map(function(d) { return d[m.var]; }).filter(function(v) { return v != null && !isNaN(v); });
        var ev = S.map(function(d) { return d[m.var]; }).filter(function(v) { return v != null && !isNaN(v); });
        var avg = function(a) { return a.reduce(function(p, q) { return p + q; }, 0) / Math.max(a.length, 1); };
        if (mv.length) mNote = " " + m.label + " averages " + fmtM(avg(mv), m) + " across its schools (England " + fmtM(avg(ev), m) + ").";
      }
    }
    summary.innerHTML = "<strong>" + esc(area) + ":</strong> " + rows.length + " schools. Value added for " + GLAB[g].toLowerCase() +
      " (" + periodText() + ") averages " + fmt(mean) + " points against England; " + counts + ". " +
      "Absence runs " + fmt(abm) + " points against what intakes predict." + mNote +
      (data.notes[area] ? " <em>" + data.notes[area] + "</em>" : "") + provNote;
    drawDist(rows, m, sc, catCounts);
    drawTraj(rows, g, area);
    drawTable(rows, m);
  }

  sLevel.addEventListener("change", function() { fillAreas(); draw(); });
  [sArea, sMeasure, sPeriod, sCentre, sWin].forEach(function(s) { s.addEventListener("change", draw); });
  sPal.addEventListener("change", function() { scheme = sPal.value;
    try { localStorage.setItem("va-pal", scheme); } catch (e) {}
    draw(); });
  sLevel.value = DEFAULT.level; fillAreas(DEFAULT.area); draw();
}
'
