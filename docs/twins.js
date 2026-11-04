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

  var summary = el("div", {"class": "tw-summary"}); root.appendChild(summary);
  var chart = el("div", {"class": "tw-chart"}); root.appendChild(chart);
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
  function distance(a, b, vars) {
    var t = 0, n = 0;
    vars.forEach(function (v) {
      var x = a[v.k], y = b[v.k]; if (x == null || y == null) return;
      t += v.t === "num" ? Math.min(Math.abs(x - y) / SD[v.k] / 3, 1) : (x === y ? 0 : 1); n++;
    });
    return n ? t / n : null;
  }

  var current = null, picked = null;
  function run() {
    var focal = S.find(function (s) { return String(s.URN) === sSchool.value; });
    if (!focal) return;
    var vars = VARS.filter(function (v) { return groupsOn[v.g]; });
    if (!vars.length) { summary.innerHTML = "<p>Choose at least one group to match on.</p>"; chart.innerHTML = table.innerHTML = cards.innerHTML = ""; return; }
    var o = OUT[sOut.value], scope = sScope.value;
    var cand = S.filter(function (s) {
      if (s.URN === focal.URN) return false;
      if (scope === "notla" && s.la === focal.la) return false;
      if (scope === "rg" && s.rg !== focal.rg) return false;
      return true;
    }).map(function (s) { var d = distance(focal, s, vars); return {s: s, sim: d == null ? null : 100 * (1 - d)}; })
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
      on: o.k, scope: scope, pool: sPool.value, n: sN.value,
      match: GROUPS.filter(function (g) { return groupsOn[g]; }).join("|")});
    try { history.replaceState(null, "", "?" + p.toString() + location.hash); } catch (e) {}
    draw();
  }

  // ---- drawing -------------------------------------------------------------
  function draw() {
    var c = current, f = c.focal, o = c.o;
    var fv = score(f, o), better = c.pool.filter(function (x) { return x.gap != null && x.gap > 0; }).length;
    var simRange = c.pool.length ? Math.round(c.pool[c.pool.length - 1].sim) + "% to " + Math.round(c.pool[0].sim) + "%" : "";
    summary.innerHTML = "<p><strong>" + esc(f.n) + "</strong> (" + where(f) + "). Of the " +
      c.pool.length + " schools most like it (similarity " + simRange + "), <strong>" + better + "</strong> do better on " +
      o.l.charAt(0).toLowerCase() + o.l.slice(1) + (o.combo ? "" : " (it has " + fmt(fv, o.dp, o.u, o.sign) + ")") + ". " +
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
      ". Purple, the schools listed below; click any point for its card.</div>" +
      "<svg viewBox='0 0 " + W + " " + H + "' width='100%' role='img' aria-label='Similar schools by similarity and " + esc(o.l) + "'>" + s + "</svg>";
    chart.querySelectorAll("circle").forEach(function (n) { n.addEventListener("click", function () {
      picked = S.find(function (x) { return String(x.URN) === n.getAttribute("data-urn"); }); drawChart(); drawTable(); drawCards(); }); });
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
    c.list.forEach(function (x) {
      var sel = picked && x.s.URN === picked.URN;
      h += "<tr data-urn='" + x.s.URN + "' class='" + (sel ? "tw-sel" : "") + "' style='cursor:pointer'><td>" + esc(x.s.n) + "</td><td>" + esc(x.s.la) +
        "</td><td class='r'>" + x.sim.toFixed(0) + "%</td><td class='r'>" + fmt(x.val, o.dp, o.u, o.sign) + "</td><td class='r'>" +
        (x.gap == null ? "–" : (x.gap > 0 ? "" : "−") + Math.abs(x.gap).toFixed(o.dp) + (o.u === "%" && !o.sign ? " pts" : "")) +
        "</td><td class='r'>" + fmt(x.s.overall, 1, "%") + "</td><td class='r'>" + fmt(x.s.unauth, 1, "%") + "</td><td class='r'>" +
        fmt(x.s.va_all, 1, "", true) + "</td></tr>";
    });
    table.innerHTML = h + "</tbody></table>";
    table.querySelectorAll("tr[data-urn]").forEach(function (r) { r.addEventListener("click", function () {
      picked = S.find(function (x) { return String(x.URN) === r.getAttribute("data-urn"); }); drawChart(); drawTable(); drawCards(); }); });
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
    cards.innerHTML = "<h3 class='tw-h3'>Fact cards</h3>" + (t ? "<p class='tw-note'>Green, the twin does better; purple, worse. Click another twin to compare.</p>" : "") + h + "</tbody></table>";
  }

  // ---- wiring ------------------------------------------------------------
  sLevel.addEventListener("change", function () { fillAreas(); fillSchools(); run(); });
  sArea.addEventListener("change", function () { fillSchools(); run(); });
  [sSchool, sMode, sOut, sScope, sPool, sN].forEach(function (n) { n.addEventListener("change", run); });
  fillAreas(q.get("area")); fillSchools(q.get("urn")); run();
})();
