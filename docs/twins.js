// twins.js - Find a twin: schools most like a chosen one in intake and
// setting, and the look-alikes that do better on absence or results.
// Data: window.TWINS, written by twins.qmd from R/08_absence_causes.R.
(function () {
  "use strict";
  var D = window.TWINS, S = D.schools, root = document.getElementById("twins-app");
  if (!root || !S) return;

  // ---- what is matched on ---------------------------------------------------
  var VARS = [
    {k: "fsm", l: "Disadvantaged (FSM)", g: "Pupils", t: "num", u: "%", dp: 1},
    {k: "ks2", l: "KS2 prior attainment (centred)", g: "Pupils", t: "num", u: "", dp: 2},
    {k: "eal", l: "English as an additional language", g: "Pupils", t: "num", u: "%", dp: 1},
    {k: "wbri", l: "White British", g: "Pupils", t: "num", u: "%", dp: 1},
    {k: "mobile", l: "Joined mid-phase", g: "Pupils", t: "num", u: "%", dp: 1},
    {k: "girls", l: "Girls", g: "Pupils", t: "num", u: "%", dp: 0},
    {k: "sen", l: "SEN support", g: "Needs", t: "num", u: "%", dp: 1},
    {k: "ehcp", l: "EHC plans", g: "Needs", t: "num", u: "%", dp: 1},
    {k: "semh", l: "Social, emotional and mental health need", g: "Needs", t: "num", u: "%", dp: 1},
    {k: "l4z", l: "Neighbourhood education", g: "Neighbourhood and place", t: "num", u: "", dp: 2, show: "l4", su: "% level 4+"},
    {k: "place", l: "Rural or urban", g: "Neighbourhood and place", t: "cat"},
    {k: "size", l: "Size", g: "The school", t: "num", u: "", dp: 0, show: "pupils", su: " pupils"},
    {k: "adm", l: "Admissions", g: "The school", t: "cat"},
    {k: "sixth", l: "Sixth form", g: "The school", t: "cat"},
    {k: "faith", l: "Religious character", g: "The school", t: "cat"}
  ];
  var GROUPS = ["Pupils", "Needs", "Neighbourhood and place", "The school"];
  // ---- what twins are compared on ------------------------------------------
  var OUTS = [
    {k: "overall", l: "Overall absence", b: "low", u: "%", dp: 1},
    {k: "unauth", l: "Unauthorised absence", b: "low", u: "%", dp: 1},
    {k: "persistent", l: "Persistent absence (10%+)", b: "low", u: "%", dp: 1},
    {k: "severe", l: "Severe absence (50%+)", b: "low", u: "%", dp: 1},
    {k: "illness", l: "Illness absence", b: "low", u: "%", dp: 1},
    {k: "r_unauth", l: "Unauthorised absence against similar schools in its authority", b: "low", u: "%", dp: 0, sign: true},
    {k: "va_all", l: "Value added v2.1, all pupils", b: "high", u: "", dp: 1, sign: true},
    {k: "va_dis", l: "Value added v2.1, disadvantaged pupils", b: "high", u: "", dp: 1, sign: true},
    {k: "att8", l: "Attainment 8, all pupils (" + D.gy + ")", b: "high", u: "", dp: 1},
    {k: "combo", l: "Absence and value added together", b: "high", u: "", dp: 2, combo: true}
  ];
  var OUT = {}; OUTS.forEach(function (o) { OUT[o.k] = o; });

  // national means and SDs, for standardising
  var MEAN = {}, SD = {};
  function stats(k) {
    var v = S.map(function (s) { return s[k]; }).filter(function (x) { return x != null; });
    var m = v.reduce(function (a, b) { return a + b; }, 0) / v.length;
    var sd = Math.sqrt(v.reduce(function (a, b) { return a + (b - m) * (b - m); }, 0) / (v.length - 1));
    MEAN[k] = m; SD[k] = sd || 1;
  }
  VARS.forEach(function (v) { if (v.t === "num") stats(v.k); });
  ["overall", "va_all"].forEach(stats);

  // ---- helpers -------------------------------------------------------------
  function esc(x) { return String(x == null ? "" : x).replace(/[&<>"]/g, function (c) {
    return {"&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;"}[c]; }); }
  function fmt(v, dp, u, sign) {
    if (v == null || isNaN(v)) return "–";
    var s = (+v).toFixed(dp); if (sign && v > 0) s = "+" + s; return s + (u || ""); }
  function el(tag, attrs, html) { var e = document.createElement(tag);
    for (var a in attrs || {}) e.setAttribute(a, attrs[a]); if (html != null) e.innerHTML = html; return e; }
  // lower-case a label's first letter, unless it starts an abbreviation (KS2, EAL)
  function lc(s) { return /^[A-Z][A-Z0-9]/.test(s) ? s : s.charAt(0).toLowerCase() + s.slice(1); }
  function where(s) { return s.lad === s.la ? esc(s.la) : esc(s.lad) + ", " + esc(s.la); }
  function score(s, o) {
    if (!o.combo) return s[o.k];
    if (s.overall == null || s.va_all == null) return null;
    return -(s.overall - MEAN.overall) / SD.overall + (s.va_all - MEAN.va_all) / SD.va_all;
  }

  // ---- controls ------------------------------------------------------------
  var q = new URLSearchParams(location.search);
  var bar = el("div", {"class": "tw-bar"});
  function ctl(label, node) { var w = el("label", {"class": "tw-ctl"}); w.appendChild(el("span", {}, label)); w.appendChild(node); bar.appendChild(w); return node; }
  function select(opts, val) { var s = el("select"); opts.forEach(function (o) {
    var e = el("option", {value: o[0]}, esc(o[1])); if (o[0] === val) e.selected = true; s.appendChild(e); }); return s; }
  var sLevel = ctl("Area type", select([["district", "District"], ["la", "Local authority"], ["region", "Region"]], q.get("level") || "district"));
  var sArea = ctl("Area", el("select"));
  var sSchool = ctl("School", el("select"));
  var sMode = ctl("Show", select([["anti", "Anti-twins: similar, but doing better"], ["closest", "Closest twins"]], q.get("mode") || "anti"));
  var sOut = ctl("Better on", select(OUTS.map(function (o) { return [o.k, o.l]; }), q.get("on") || "unauth"));
  var sScope = ctl("Look in", select([["eng", "All of England"], ["notla", "England, outside its own authority"], ["rg", "Its own region"]], q.get("scope") || "eng"));
  var sPool = ctl("From the", select([["25", "25 most similar"], ["50", "50 most similar"], ["100", "100 most similar"]], q.get("pool") || "50"));
  var sN = ctl("Show", select([["5", "5 schools"], ["10", "10 schools"]], q.get("n") || "5"));
  var sWt = ctl("Weighting", select([["m", "By what matters for the measure"], ["eq", "Equal"]], q.get("wt") || "m"));
  // the header only a printed comparison carries
  var printHead = el("div", {"class": "tw-printhead"}); root.appendChild(printHead);
  root.appendChild(bar);

  var groupsOn = {}; var gq = q.get("match");
  GROUPS.forEach(function (g) { groupsOn[g] = gq ? gq.split("|").indexOf(g) >= 0 : true; });
  var gbar = el("div", {"class": "tw-groups"}, "<span>Match on:</span>");
  GROUPS.forEach(function (g) {
    var lab = el("label"); var cb = el("input", {type: "checkbox"}); cb.checked = groupsOn[g];
    cb.addEventListener("change", function () { groupsOn[g] = cb.checked; run(); });
    lab.appendChild(cb); lab.appendChild(document.createTextNode(" " + g)); gbar.appendChild(lab);
  });
  root.appendChild(gbar);
  var wbox = el("details", {"class": "tw-wts"}); root.appendChild(wbox);

  var tools = el("div", {"class": "tw-tools"});
  var printBtn = el("button", {type: "button", "class": "btn btn-sm tw-print-btn"}, "Print for governors");
  tools.appendChild(printBtn);
  tools.appendChild(el("span", {"class": "tw-note"}, " A clean copy of this comparison, with the fact cards, for a meeting or a report."));
  root.appendChild(tools);

  var summary = el("div", {"class": "tw-summary"}); root.appendChild(summary);
  var chart = el("div", {"class": "tw-chart"}); root.appendChild(chart);
  var mapWrap = el("div", {"class": "tw-mapwrap"}); root.appendChild(mapWrap);
  var mapNote = el("div", {"class": "tw-note"}); mapWrap.appendChild(mapNote);
  var mapDiv = el("div", {"class": "tw-map", role: "region", "aria-label": "Map of the chosen school and its twins"}); mapWrap.appendChild(mapDiv);
  var table = el("div", {"class": "tw-table"}); root.appendChild(table);
  var cards = el("div", {"class": "tw-cards"}); root.appendChild(cards);

  var areaKey = {region: "rg", la: "la", district: "lad"};
  function fillAreas(pref) {
    var list = D.areas[sLevel.value]; sArea.innerHTML = "";
    list.forEach(function (a) { var e = el("option", {value: a}, esc(a)); sArea.appendChild(e); });
    sArea.value = pref && list.indexOf(pref) >= 0 ? pref : (list.indexOf("Brighton and Hove") >= 0 ? "Brighton and Hove" : list[0]);
  }
  function fillSchools(pref) {
    var k = areaKey[sLevel.value];
    var list = S.filter(function (s) { return s[k] === sArea.value; }).sort(function (a, b) { return a.n.localeCompare(b.n); });
    sSchool.innerHTML = "";
    list.forEach(function (s) { sSchool.appendChild(el("option", {value: s.URN}, esc(s.n))); });
    if (pref && list.some(function (s) { return String(s.URN) === String(pref); })) sSchool.value = pref;
  }

  // ---- matching ------------------------------------------------------------
  // weighted Gower-type distance: each characteristic's gap (in SDs, capped
  // at three; 0 or 1 for a category) times its weight, over the weights used
  function weightsFor(o) {
    var w = {}, src = sWt.value === "eq" ? null : (D.weights || {})[o.k];
    VARS.forEach(function (v) { w[v.k] = src && src[v.k] != null ? src[v.k] : 1; });
    return w;
  }
  function distance(a, b, vars, wts) {
    var t = 0, n = 0;
    vars.forEach(function (v) {
      var x = a[v.k], y = b[v.k]; if (x == null || y == null) return;
      var w = wts[v.k];
      t += w * (v.t === "num" ? Math.min(Math.abs(x - y) / SD[v.k] / 3, 1) : (x === y ? 0 : 1)); n += w;
    });
    return n ? t / n : null;
  }
  function drawWeights(vars, wts, o) {
    var on = vars.slice().sort(function (a, b) { return wts[b.k] - wts[a.k]; });
    var mx = Math.max.apply(null, on.map(function (v) { return wts[v.k]; }));
    var top = on.slice(0, 3).map(function (v) { return lc(v.l); });
    var h = "<summary>" + (sWt.value === "eq" ? "Every characteristic counts equally" :
      "Weighted most on " + top.slice(0, -1).join(", ") + (top.length > 1 ? " and " : "") + top[top.length - 1] +
      ", for " + (o.combo ? "absence and value added" : lc(o.l))) +
      " <span class='tw-sub'>(show the weights)</span></summary><div class='tw-wbars'>";
    on.forEach(function (v) {
      h += "<div class='tw-wrow'><span class='tw-wl'>" + esc(v.l) + "</span><span class='tw-wb'><span style='width:" +
        (100 * wts[v.k] / mx).toFixed(0) + "%'></span></span><span class='tw-wv'>" + wts[v.k].toFixed(2) + "</span></div>";
    });
    wbox.innerHTML = h + "</div><p class='tw-note'>Weights average one across all fifteen characteristics. " +
      "See <a href='#sec-how'>how the matching works</a>.</p>";
  }

  var current = null, picked = null;
  function run() {
    var focal = S.find(function (s) { return String(s.URN) === sSchool.value; });
    if (!focal) return;
    var vars = VARS.filter(function (v) { return groupsOn[v.g]; });
    if (!vars.length) { summary.innerHTML = "<p>Choose at least one group to match on.</p>"; chart.innerHTML = table.innerHTML = cards.innerHTML = ""; return; }
    var o = OUT[sOut.value], scope = sScope.value, wts = weightsFor(o);
    drawWeights(vars, wts, o);
    var cand = S.filter(function (s) {
      if (s.URN === focal.URN) return false;
      if (scope === "notla" && s.la === focal.la) return false;
      if (scope === "rg" && s.rg !== focal.rg) return false;
      return true;
    }).map(function (s) { var d = distance(focal, s, vars, wts); return {s: s, sim: d == null ? null : 100 * (1 - d)}; })
      .filter(function (c) { return c.sim != null; })
      .sort(function (a, b) { return b.sim - a.sim; });
    var pool = cand.slice(0, +sPool.value), fv = score(focal, o), list;
    pool.forEach(function (c) { var v = score(c.s, o); c.val = v;
      c.gap = (v == null || fv == null) ? null : (o.b === "low" ? fv - v : v - fv); });
    if (sMode.value === "closest") list = pool.slice(0, +sN.value);
    else list = pool.filter(function (c) { return c.gap != null && c.gap > 0; })
                    .sort(function (a, b) { return b.gap - a.gap; }).slice(0, +sN.value);
    current = {focal: focal, pool: pool, list: list, o: o, vars: vars};
    picked = list.length ? list[0].s : null;
    // keep the choice in the address bar
    var p = new URLSearchParams({level: sLevel.value, area: sArea.value, urn: focal.URN, mode: sMode.value,
      on: o.k, scope: scope, pool: sPool.value, n: sN.value, wt: sWt.value,
      match: GROUPS.filter(function (g) { return groupsOn[g]; }).join("|")});
    try { history.replaceState(null, "", "?" + p.toString() + location.hash); } catch (e) {}
    draw(); drawMap(true);
  }
  // a click on a twin anywhere (chart, map, table) shows its card
  function pick(urn) {
    picked = S.find(function (x) { return String(x.URN) === String(urn); }) || picked;
    drawChart(); drawTable(); drawCards(); drawMap(false);
  }

  // ---- drawing -------------------------------------------------------------
  function draw() {
    var c = current, f = c.focal, o = c.o;
    var fv = score(f, o), better = c.pool.filter(function (x) { return x.gap != null && x.gap > 0; }).length;
    var simRange = c.pool.length ? Math.round(c.pool[c.pool.length - 1].sim) + "% to " + Math.round(c.pool[0].sim) + "%" : "";
    summary.innerHTML = "<p><strong>" + esc(f.n) + "</strong> (" + where(f) + "). Of the " +
      c.pool.length + " schools most like it" + (sWt.value === "eq" ? "" : " on what matters for this measure") +
      " (similarity " + simRange + "), <strong>" + better + "</strong> do better on " +
      lc(o.l) + (o.combo ? "" : " (it has " + fmt(fv, o.dp, o.u, o.sign) + ")") + ". " +
      (sMode.value === "anti" ? (c.list.length ? "The " + c.list.length + " furthest ahead are below." : "None does better.")
                              : "The " + c.list.length + " closest are below.") + "</p>";
    drawChart(); drawTable(); drawCards();
  }

  function drawChart() {
    var c = current, o = c.o, W = 720, H = 260, L = 54, R = 14, T = 14, B = 38, iw = W - L - R, ih = H - T - B;
    var pts = c.pool.filter(function (x) { return x.val != null; });
    var fv = score(c.focal, o);
    if (!pts.length) { chart.innerHTML = ""; return; }
    var xs = pts.map(function (p) { return p.sim; }), ys = pts.map(function (p) { return p.val; }).concat(fv == null ? [] : [fv]);
    var x0 = Math.min.apply(null, xs), x1 = Math.max.apply(null, xs), y0 = Math.min.apply(null, ys), y1 = Math.max.apply(null, ys);
    var pad = (y1 - y0) * 0.08 || 1; y0 -= pad; y1 += pad; if (x1 - x0 < 1) { x0 -= 0.5; x1 += 0.5; }
    var X = function (v) { return L + (v - x0) / (x1 - x0) * iw; }, Y = function (v) { return T + (1 - (v - y0) / (y1 - y0)) * ih; };
    var inList = {}; c.list.forEach(function (x) { inList[x.s.URN] = true; });
    var s = "";
    [0, 0.5, 1].forEach(function (t) { var v = y0 + t * (y1 - y0);
      s += "<line x1='" + L + "' x2='" + (W - R) + "' y1='" + Y(v) + "' y2='" + Y(v) + "' stroke='#ece8f3'/>" +
           "<text x='" + (L - 6) + "' y='" + (Y(v) + 3.5) + "' text-anchor='end' font-size='10' fill='#6b6680'>" + fmt(v, o.dp, o.u, o.sign) + "</text>"; });
    if (fv != null) s += "<line x1='" + L + "' x2='" + (W - R) + "' y1='" + Y(fv) + "' y2='" + Y(fv) + "' stroke='#361a54' stroke-dasharray='4 3'/>" +
      "<text x='" + (W - R) + "' y='" + (Y(fv) - 5) + "' text-anchor='end' font-size='10.5' fill='#361a54'>" + esc(c.focal.n) + "</text>";
    pts.forEach(function (p) {
      var hl = inList[p.s.URN], sel = picked && p.s.URN === picked.URN;
      s += "<circle data-urn='" + p.s.URN + "' cx='" + X(p.sim).toFixed(1) + "' cy='" + Y(p.val).toFixed(1) + "' r='" + (hl ? 6 : 4) +
        "' fill='" + (hl ? "#993bff" : "#d3cde0") + "' stroke='" + (sel ? "#1a1a2e" : "#fff") + "' stroke-width='" + (sel ? 2 : 1) +
        "' style='cursor:pointer'><title>" + esc(p.s.n) + " (" + esc(p.s.la) + "): similarity " + p.sim.toFixed(0) + "%, " +
        fmt(p.val, o.dp, o.u, o.sign) + "</title></circle>";
    });
    s += "<text x='" + (L + iw / 2) + "' y='" + (H - 6) + "' text-anchor='middle' font-size='11' fill='#6b6680'>Similarity to " +
      esc(c.focal.n) + " (%), the " + c.pool.length + " most similar schools</text>" +
      "<text x='" + L + "' y='" + (H - 20) + "' font-size='10' fill='#6b6680'>" + x0.toFixed(0) + "%</text>" +
      "<text x='" + (W - R) + "' y='" + (H - 20) + "' text-anchor='end' font-size='10' fill='#6b6680'>" + x1.toFixed(0) + "%</text>";
    chart.innerHTML = "<div class='tw-note'><strong>" + esc(o.l) + "</strong>: " + (o.b === "low" ? "lower is better" : "higher is better") +
      (o.combo ? " (overall absence, lower, and value added, higher, in national standard deviations)" : "") +
      ". Purple, the schools listed below<span class='tw-screen'>; click any point for its card</span>.</div>" +
      "<svg viewBox='0 0 " + W + " " + H + "' width='100%' role='img' aria-label='Similar schools by similarity and " + esc(o.l) + "'>" + s + "</svg>";
    chart.querySelectorAll("circle").forEach(function (n) { n.addEventListener("click", function () { pick(n.getAttribute("data-urn")); }); });
  }

  // ---- map: the chosen school, the listed twins (numbered as in the table),
  // and the rest of the similar schools, on the explorer's grey basemap ------
  var map = null, layer = null;
  function drawMap(fit) {
    if (!window.L) { mapWrap.style.display = "none"; return; }
    if (!map) {
      map = L.map(mapDiv, {scrollWheelZoom: false, minZoom: 5});
      L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
        {attribution: "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ", maxZoom: 16}).addTo(map);
      layer = L.layerGroup().addTo(map);
    }
    layer.clearLayers();
    var c = current, f = c.focal, o = c.o, num = {}, has = function (s) { return s.lat != null && s.lon != null; };
    c.list.forEach(function (x, i) { num[x.s.URN] = i + 1; });
    var tip = function (s, sim, v) { return "<b>" + esc(s.n) + "</b><br>" + where(s) +
      (sim != null ? "<br>Similarity " + sim.toFixed(0) + "%" : "") +
      "<br>" + esc(o.combo ? "Combined score" : o.l) + ": " + fmt(v, o.dp, o.u, o.sign); };
    var isSel = function (s) { return picked && s.URN === picked.URN; };
    // the rest of the pool, small and grey
    c.pool.forEach(function (p) {
      if (num[p.s.URN] || !has(p.s)) return;
      L.circleMarker([p.s.lat, p.s.lon], {radius: isSel(p.s) ? 6 : 4, fillColor: "#d3cde0", fillOpacity: 0.95,
        color: isSel(p.s) ? "#1a1a2e" : "#8a8aa3", weight: isSel(p.s) ? 2.5 : 0.8})
        .bindTooltip(tip(p.s, p.sim, p.val)).on("click", function () { pick(p.s.URN); }).addTo(layer);
    });
    // lines from the chosen school to each listed twin
    if (has(f)) c.list.forEach(function (x) { if (!has(x.s)) return;
      L.polyline([[f.lat, f.lon], [x.s.lat, x.s.lon]], {color: "#993bff", weight: 1.5, opacity: 0.45, dashArray: "4 5", interactive: false}).addTo(layer); });
    // the listed twins, numbered
    c.list.forEach(function (x, i) { if (!has(x.s)) return;
      L.marker([x.s.lat, x.s.lon], {zIndexOffset: 500 + (isSel(x.s) ? 100 : 0), keyboard: true, title: x.s.n,
        icon: L.divIcon({className: "tw-pin" + (isSel(x.s) ? " sel" : ""), html: String(i + 1), iconSize: [22, 22]})})
        .bindTooltip(tip(x.s, x.sim, x.val)).on("click", function () { pick(x.s.URN); }).addTo(layer);
    });
    // the chosen school, a dark diamond
    if (has(f)) L.marker([f.lat, f.lon], {zIndexOffset: 1000, title: f.n,
      icon: L.divIcon({className: "tw-home", iconSize: [14, 14]})})
      .bindTooltip("<b>" + esc(f.n) + "</b> (chosen)<br>" + where(f) + "<br>" + esc(o.combo ? "Combined score" : o.l) + ": " + fmt(score(f, o), o.dp, o.u, o.sign))
      .addTo(layer);
    var far = c.list.filter(function (x) { return has(x.s); }).map(function (x) { return [x.s.lat, x.s.lon]; });
    if (fit) {
      var pts = far.concat(has(f) ? [[f.lat, f.lon]] : []);
      if (pts.length > 1) map.fitBounds(pts, {padding: [36, 36], maxZoom: 11});
      else if (pts.length) map.setView(pts[0], 10);
    }
    var km = function (a) { var R = 6371, r = Math.PI / 180, dl = (a.lat - f.lat) * r, dn = (a.lon - f.lon) * r,
      h = Math.sin(dl / 2) * Math.sin(dl / 2) + Math.cos(f.lat * r) * Math.cos(a.lat * r) * Math.sin(dn / 2) * Math.sin(dn / 2);
      return 2 * R * Math.asin(Math.sqrt(h)); };
    var d = has(f) ? c.list.filter(function (x) { return has(x.s); }).map(function (x) { return km(x.s); }).sort(function (a, b) { return a - b; }) : [];
    mapNote.innerHTML = "<strong>Where they are.</strong> The diamond is " + esc(f.n) + "; the numbered points are the schools listed below" +
      (d.length ? " (" + (d.length === 1 ? Math.round(d[0]) + " km away" : Math.round(d[0]) + " to " + Math.round(d[d.length - 1]) + " km away") + ")" : "") +
      "; grey points are the rest of the " + c.pool.length + " most similar. Click any point for its card.";
    setTimeout(function () { map.invalidateSize(); }, 0);
  }

  function drawTable() {
    var c = current, o = c.o;
    if (!c.list.length) { table.innerHTML = ""; return; }
    var h = "<table class='table table-sm table-hover tw-t'><thead><tr><th>School</th><th>Authority</th><th class='r'>Similarity</th>" +
      "<th class='r'>" + esc(o.combo ? "Combined score" : o.l) + "</th><th class='r'>Better by</th>" +
      "<th class='r'>Overall absence</th><th class='r'>Unauthorised</th><th class='r'>Value added</th></tr></thead><tbody>";
    h += "<tr class='tw-focal'><td>" + esc(c.focal.n) + " <em>(chosen)</em></td><td>" + esc(c.focal.la) + "</td><td class='r'>–</td><td class='r'>" +
      fmt(score(c.focal, o), o.dp, o.u, o.sign) + "</td><td></td><td class='r'>" + fmt(c.focal.overall, 1, "%") + "</td><td class='r'>" +
      fmt(c.focal.unauth, 1, "%") + "</td><td class='r'>" + fmt(c.focal.va_all, 1, "", true) + "</td></tr>";
    c.list.forEach(function (x, i) {
      var sel = picked && x.s.URN === picked.URN;
      h += "<tr data-urn='" + x.s.URN + "' class='" + (sel ? "tw-sel" : "") + "' style='cursor:pointer'><td><span class='tw-num'>" + (i + 1) + "</span>" +
        esc(x.s.n) + "</td><td>" + esc(x.s.la) +
        "</td><td class='r'>" + x.sim.toFixed(0) + "%</td><td class='r'>" + fmt(x.val, o.dp, o.u, o.sign) + "</td><td class='r'>" +
        (x.gap == null ? "–" : (x.gap > 0 ? "" : "−") + Math.abs(x.gap).toFixed(o.dp) + (o.u === "%" && !o.sign ? " pts" : "")) +
        "</td><td class='r'>" + fmt(x.s.overall, 1, "%") + "</td><td class='r'>" + fmt(x.s.unauth, 1, "%") + "</td><td class='r'>" +
        fmt(x.s.va_all, 1, "", true) + "</td></tr>";
    });
    table.innerHTML = h + "</tbody></table>";
    table.querySelectorAll("tr[data-urn]").forEach(function (r) { r.addEventListener("click", function () { pick(r.getAttribute("data-urn")); }); });
  }

  // fact cards: the chosen school and the picked twin side by side
  var ROWS = [
    {h: "The school"},
    {k: "run_by", l: "Run by", t: "txt"}, {k: "ofsted", l: "Latest Ofsted", t: "txt"},
    {k: "pupils", l: "Pupils", dp: 0}, {k: "adm", l: "Admissions", t: "txt"}, {k: "sixth", l: "Sixth form", t: "txt"},
    {k: "place", l: "Rural or urban", t: "txt"}, {k: "faith", l: "Religious character", t: "txt"},
    {h: "Pupils and needs (January 2026)"},
    {k: "fsm", l: "Disadvantaged (FSM)", dp: 1, u: "%"}, {k: "ks2", l: "KS2 prior attainment", dp: 2},
    {k: "eal", l: "EAL", dp: 1, u: "%"}, {k: "wbri", l: "White British", dp: 1, u: "%"},
    {k: "mobile", l: "Joined mid-phase", dp: 1, u: "%"}, {k: "girls", l: "Girls", dp: 0, u: "%"},
    {k: "sen", l: "SEN support", dp: 1, u: "%"}, {k: "ehcp", l: "EHC plans", dp: 1, u: "%"},
    {k: "semh", l: "SEMH need", dp: 1, u: "%"}, {k: "l4", l: "Neighbourhood education", dp: 1, u: "% level 4+"},
    {h: "Absence (autumn and spring 2025/26)"},
    {k: "overall", l: "Overall", dp: 1, u: "%", b: "low"}, {k: "illness", l: "Illness", dp: 1, u: "%", b: "low"},
    {k: "unauth", l: "Unauthorised", dp: 1, u: "%", b: "low"}, {k: "persistent", l: "Persistent (10%+)", dp: 1, u: "%", b: "low"},
    {k: "severe", l: "Severe (50%+)", dp: 1, u: "%", b: "low"},
    {k: "r_unauth", l: "Unauthorised, against similar schools in its authority", dp: 0, u: "%", sign: true, b: "low"},
    {h: "Results"},
    {k: "att8", l: "Attainment 8 (" + D.gy + ")", dp: 1, b: "high"},
    {k: "va_all", l: "Value added v2.1, all pupils", dp: 1, sign: true, b: "high"},
    {k: "va_dis", l: "Value added v2.1, disadvantaged", dp: 1, sign: true, b: "high"}
  ];
  // a small chart of one measure over time: the chosen school (dark), the
  // twin (purple) and England (grey, dashed); hollow points are provisional
  function spark(ya, yb, ye, labs, opt) {
    var arr = function (x) { return Array.isArray(x) ? x : []; };
    ya = arr(ya); yb = arr(yb); ye = arr(ye);
    var all = ya.concat(yb, ye).filter(function (v) { return v != null; });
    if (!ya.concat(yb).some(function (v) { return v != null; })) return "<span class='tw-sub'>Not available</span>";
    if (opt.zero) all.push(0);
    var W = 460, H = 118, L = 44, R = 10, T = 8, B = 22, iw = W - L - R, ih = H - T - B;
    var y0 = Math.min.apply(null, all), y1 = Math.max.apply(null, all), pad = (y1 - y0) * 0.1 || 1; y0 -= pad; y1 += pad;
    var n = labs.length, X = function (i) { return L + (n === 1 ? iw / 2 : i / (n - 1) * iw); },
        Y = function (v) { return T + (1 - (v - y0) / (y1 - y0)) * ih; };
    var s = "";
    [y0 + pad, (y0 + y1) / 2, y1 - pad].forEach(function (v) {
      s += "<line x1='" + L + "' x2='" + (W - R) + "' y1='" + Y(v).toFixed(1) + "' y2='" + Y(v).toFixed(1) + "' stroke='#ece8f3'/>" +
        "<text x='" + (L - 5) + "' y='" + (Y(v) + 3.5).toFixed(1) + "' text-anchor='end' font-size='9.5' fill='#6b6680'>" + fmt(v, opt.dp, opt.u, opt.sign) + "</text>"; });
    if (opt.zero) s += "<line x1='" + L + "' x2='" + (W - R) + "' y1='" + Y(0).toFixed(1) + "' y2='" + Y(0).toFixed(1) + "' stroke='#8a8aa3' stroke-dasharray='4 3'/>";
    var line = function (ys, col, w, dash) { var d = "", pen = false;
      ys.forEach(function (v, i) { if (v == null) { pen = false; return; } d += (pen ? "L" : "M") + X(i).toFixed(1) + " " + Y(v).toFixed(1) + " "; pen = true; });
      return d ? "<path d='" + d + "' fill='none' stroke='" + col + "' stroke-width='" + w + "'" + (dash ? " stroke-dasharray='" + dash + "'" : "") + "/>" : ""; };
    var dots = function (ys, col, who) { var o = "";
      ys.forEach(function (v, i) { if (v == null) return; var pv = opt.prov && opt.prov[i];
        o += "<circle cx='" + X(i).toFixed(1) + "' cy='" + Y(v).toFixed(1) + "' r='3' fill='" + (pv ? "#fff" : col) + "' stroke='" + col +
          "' stroke-width='1.5'><title>" + esc(who) + ", " + labs[i] + ": " + fmt(v, opt.dp, opt.u, opt.sign) + (pv ? " (provisional)" : "") + "</title></circle>"; });
      return o; };
    if (ye.length) s += line(ye, "#8a8aa3", 1.5, "4 3");
    s += line(yb, "#993bff", 2) + line(ya, "#361a54", 2) + dots(yb, "#993bff", opt.nb || "Twin") + dots(ya, "#361a54", opt.na || "Chosen");
    labs.forEach(function (lb, i) { if (n > 6 && i % 2 && i !== n - 1) return;
      s += "<text x='" + (i === 0 ? X(i) - 8 : i === n - 1 ? X(i) + 8 : X(i)).toFixed(1) + "' y='" + (H - 6) + "' text-anchor='" + (i === 0 ? "start" : i === n - 1 ? "end" : "middle") + "' font-size='9.5' fill='#6b6680'>" + esc(lb) + "</text>"; });
    return "<svg class='tw-spark' viewBox='0 0 " + W + " " + H + "' width='100%' role='img' aria-label='" + esc(opt.title) + "'>" + s + "</svg>";
  }

  function drawCards() {
    var f = current.focal, t = picked;
    var h = "<table class='tw-card'><thead><tr><th></th><th>" + esc(f.n) + "<div class='tw-sub'>" + where(f) + "</div></th>" +
      (t ? "<th>" + esc(t.n) + "<div class='tw-sub'>" + where(t) + "</div></th>" : "") + "</tr></thead><tbody>";
    ROWS.forEach(function (r) {
      if (r.h) { h += "<tr class='tw-h'><td colspan='3'>" + r.h + "</td></tr>"; return; }
      var a = f[r.k], b = t ? t[r.k] : null, cls = "";
      if (t && r.b && a != null && b != null && a !== b) cls = ((r.b === "low") === (b < a)) ? "tw-better" : "tw-worse";
      var show = function (v) { return r.t === "txt" ? esc(v == null ? "–" : v) : fmt(v, r.dp, r.u, r.sign); };
      h += "<tr><td>" + r.l + "</td><td>" + show(a) + "</td>" + (t ? "<td class='" + cls + "'>" + show(b) + "</td>" : "") + "</tr>";
    });
    // over time
    var key = "<span class='tw-key'><i style='background:#361a54'></i>" + esc(f.n) + "</span>" +
      (t ? "<span class='tw-key'><i style='background:#993bff'></i>" + esc(t.n) + "</span>" : "") +
      "<span class='tw-key'><i class='tw-dash'></i>England</span>";
    h += "<tr class='tw-h'><td colspan='3'>Over time <span class='tw-sub'>" + key + "</span></td></tr>";
    var TS = [
      {a: "va_y", l: "Value added v2.1, all pupils, by year", labs: D.va_years, zero: true, dp: 1, sign: true, prov: D.va_prov,
       note: "Against England (0), Attainment 8 points; unshrunk. Hollow: provisional."},
      {a: "vd_y", l: "Value added v2.1, disadvantaged pupils, by year", labs: D.va_years, zero: true, dp: 1, sign: true, prov: D.va_prov},
      {a: "ab_o", l: "Overall absence, autumn and spring terms", labs: D.ab_years, eng: D.eng_ab.overall, dp: 1, u: "%",
       note: "No data for 2019/20; 2020/21 was a year of lockdowns."},
      {a: "ab_u", l: "Unauthorised absence, autumn and spring terms", labs: D.ab_years, eng: D.eng_ab.unauth, dp: 1, u: "%"}
    ];
    TS.forEach(function (r) {
      h += "<tr class='tw-ts'><td>" + r.l + (r.note ? "<div class='tw-sub'>" + r.note + "</div>" : "") + "</td><td colspan='2'>" +
        spark(f[r.a], t ? t[r.a] : null, r.eng, r.labs, {zero: r.zero, dp: r.dp, u: r.u, sign: r.sign, prov: r.prov,
          na: f.n, nb: t ? t.n : "", title: r.l}) + "</td></tr>";
    });
    cards.innerHTML = "<h3 class='tw-h3'>Fact cards</h3>" + (t ? "<p class='tw-note'>Green, the twin does better; purple, worse.<span class='tw-screen'> Click another twin to compare.</span></p>" : "") + h + "</tbody></table>";
  }

  // ---- printing ------------------------------------------------------------
  // Hide everything on the page but this app (and the site's chrome), and
  // put a header on it that says what was compared, when and where from.
  var GROUP_LOW = function (g) { return g === "The school" ? "the school itself" : g.toLowerCase(); };
  var marked = [];
  function preparePrint() {
    if (!current) return;
    var c = current, f = c.focal, o = c.o;
    var groups = GROUPS.filter(function (g) { return groupsOn[g]; }).map(GROUP_LOW);
    var scopeTxt = {eng: "all of England", notla: "England outside " + f.la, rg: f.rg}[sScope.value];
    printHead.innerHTML = "<div class='tw-ph-site'>School Value Added · Find a twin</div>" +
      "<h2>" + esc(f.n) + " and the schools most like it</h2>" +
      "<p>" + where(f) + ". " + (sMode.value === "anti" ? "Look-alike schools that do better on " : "The closest look-alikes, compared on ") +
      esc(lc(o.l)) + ", from the " + sPool.value + " schools most like it in " + esc(scopeTxt) +
      ". Matched on " + groups.slice(0, -1).join(", ") + (groups.length > 1 ? " and " : "") + groups[groups.length - 1] +
      (sWt.value === "eq" ? ", every characteristic counting equally" : ", weighted by what matters for that measure") + ".</p>" +
      "<p class='tw-ph-meta'>Printed " + new Date().toLocaleDateString("en-GB", {day: "numeric", month: "long", year: "numeric"}) +
      " from " + esc(location.href.split("#")[0]) + " (data built " + esc(D.built) + ").</p>" +
      "<p class='tw-ph-care'><strong>Reading this.</strong> Similarity is measured on published intake and setting only. " +
      "A twin can differ in what the data do not see: its pupils' health, its community, its buildings, its staff. " +
      "A twin that does better is a reason to ask what it does differently, not proof that copying it would work. " +
      "All figures are published school-level data from DfE and Ofsted.</p>";
    marked = [];
    for (var n = root; n && n !== document.body; n = n.parentElement) {
      n.classList.add("tw-print-path"); marked.push(n);
      Array.prototype.forEach.call(n.parentElement ? n.parentElement.children : [], function (sib) {
        if (sib !== n && !sib.classList.contains("tw-print-path")) { sib.classList.add("tw-noprint"); marked.push(sib); }
      });
    }
  }
  function endPrint() {
    marked.forEach(function (m) { m.classList.remove("tw-noprint", "tw-print-path"); }); marked = [];
  }
  printBtn.addEventListener("click", function () { preparePrint(); window.print(); });
  window.addEventListener("beforeprint", preparePrint);
  window.addEventListener("afterprint", endPrint);

  // ---- wiring ------------------------------------------------------------
  sLevel.addEventListener("change", function () { fillAreas(); fillSchools(); run(); });
  sArea.addEventListener("change", function () { fillSchools(); run(); });
  [sSchool, sMode, sOut, sScope, sPool, sN, sWt].forEach(function (n) { n.addEventListener("change", run); });
  fillAreas(q.get("area")); fillSchools(q.get("urn")); run();
})();
