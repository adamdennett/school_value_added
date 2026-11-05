// simulator.js - Policy simulator v2.0: what the Value Added v2.1 models say
// a school's (or an area's) Attainment 8 would be if absence or intake were
// different. Data: window.SIM, written by simulator.qmd.
(function () {
  "use strict";
  var D = window.SIM, S = D.schools, root = document.getElementById("sim-app");
  if (!root || !S) return;

  var GROUPS = [["all", "All pupils"], ["disadvantaged", "Disadvantaged"], ["non_disadvantaged", "Non-disadvantaged"],
                ["girls", "Girls"], ["boys", "Boys"]];
  // the levers: the model term each moves, and how
  var LEVERS = [
    {k: "abs", term: "log(PERCTOT)", log: true, l: "Overall absence", u: "%", min: -5, max: 5, step: 0.1, dp: 1, kind: "Attendance"},
    {k: "fsm", term: "log(PTFSM6CLA1A)", log: true, l: "Disadvantaged pupils (FSM6)", u: "%", min: -15, max: 15, step: 0.5, dp: 1, kind: "Intake"},
    {k: "eal", term: "log(PNUMEAL)", log: true, l: "English as an additional language", u: "%", min: -15, max: 15, step: 0.5, dp: 1, kind: "Intake"},
    {k: "ks2", term: "ks2_c", l: "KS2 prior attainment (scaled score)", u: "", min: -3, max: 3, step: 0.1, dp: 1, kind: "Intake"},
    {k: "l4", term: "z_l4", l: "Neighbourhood education (SD)", u: "", min: -1, max: 1, step: 0.05, dp: 2, kind: "Intake"},
    {k: "sen", term: "sen_support", l: "SEN support", u: "%", min: -10, max: 10, step: 0.5, dp: 1, kind: "Intake", pct: true},
    {k: "ehcp", term: "sen_ehcp", l: "EHC plans", u: "%", min: -3, max: 3, step: 0.1, dp: 1, kind: "Intake", pct: true},
    {k: "mob", term: "mobile", l: "Joined mid-phase (mobility)", u: "%", min: -5, max: 5, step: 0.5, dp: 1, kind: "Intake", pct: true}
  ];
  var ADM = [["OTHER NON SEL", "Non-selective"], ["SEL", "Selective"], ["NON SEL IN HIGHLY SEL AREA", "Non-selective, in a selective area"]];
  var ADM_TERM = {"OTHER NON SEL": "ADMPOL_PTOTHER NON SEL", "SEL": "ADMPOL_PTSEL"};

  // ---- helpers -------------------------------------------------------------
  function esc(x) { return String(x == null ? "" : x).replace(/[&<>"]/g, function (c) {
    return {"&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;"}[c]; }); }
  function el(tag, attrs, html) { var e = document.createElement(tag);
    for (var a in attrs || {}) e.setAttribute(a, attrs[a]); if (html != null) e.innerHTML = html; return e; }
  function fmt(v, dp, u) { if (v == null || isNaN(v)) return "–"; return (+v).toFixed(dp) + (u || ""); }
  function sgn(v, dp, u) { if (v == null || isNaN(v)) return "–"; var s = (+v).toFixed(dp);
    if (+s > 0) s = "+" + s; if (+s === 0) s = (0).toFixed(dp); return s.replace("-", "−") + (u || ""); }
  function where(s) { return s.lad === s.la ? esc(s.la) : esc(s.lad) + ", " + esc(s.la); }
  // pupil-weighted mean over schools, or over simulated items ({s: school, r: ...})
  function wmean(list, f) { var t = 0, w = 0; list.forEach(function (x) { var v = f(x); if (v == null || isNaN(v)) return;
    var p = x.s ? x.s.pupils : x.pupils, ws = p > 0 ? p : 1; t += ws * v; w += ws; }); return w ? t / w : null; }
  var ENG_ABS = wmean(S, function (s) { return s.abs; });

  // ---- controls ------------------------------------------------------------
  var q = new URLSearchParams(location.search);
  var bar = el("div", {"class": "sm-bar"});
  function ctl(label, node) { var w = el("label", {"class": "sm-ctl"}); w.appendChild(el("span", {}, label)); w.appendChild(node); bar.appendChild(w); return node; }
  function select(opts, val) { var s = el("select"); opts.forEach(function (o) {
    var e = el("option", {value: o[0]}, esc(o[1])); if (o[0] === val) e.selected = true; s.appendChild(e); }); return s; }
  var sMode = ctl("Simulate", select([["school", "One school"], ["area", "Every school in an area"]], q.get("mode") || "school"));
  var sLevel = ctl("Area type", select([["district", "District"], ["la", "Local authority"], ["region", "Region"], ["eng", "England"]], q.get("level") || "district"));
  var sArea = ctl("Area", el("select"));
  var sSchool = ctl("School", el("select"));
  root.appendChild(bar);

  var grid = el("div", {"class": "sm-grid"}); root.appendChild(grid);
  var levBox = el("div", {"class": "sm-levers"}); grid.appendChild(levBox);
  var out = el("div", {"class": "sm-out"}); grid.appendChild(out);

  var areaKey = {region: "rg", la: "la", district: "lad"};
  function fillAreas(pref) {
    sArea.innerHTML = "";
    if (sLevel.value === "eng") { sArea.appendChild(el("option", {value: "England"}, "England")); sArea.disabled = true; return; }
    sArea.disabled = false;
    var list = D.areas[sLevel.value];
    list.forEach(function (a) { sArea.appendChild(el("option", {value: a}, esc(a))); });
    sArea.value = pref && list.indexOf(pref) >= 0 ? pref : (list.indexOf("Brighton and Hove") >= 0 ? "Brighton and Hove" : list[0]);
  }
  function inArea() {
    if (sLevel.value === "eng") return S.slice();
    var k = areaKey[sLevel.value];
    return S.filter(function (s) { return s[k] === sArea.value; });
  }
  function fillSchools(pref) {
    var list = inArea().sort(function (a, b) { return a.n.localeCompare(b.n); });
    sSchool.innerHTML = "";
    list.forEach(function (s) { sSchool.appendChild(el("option", {value: s.URN}, esc(s.n))); });
    if (pref && list.some(function (s) { return String(s.URN) === String(pref); })) sSchool.value = pref;
  }

  // levers
  var val = {}, inputs = {}, labels = {};
  LEVERS.forEach(function (L) { var v = parseFloat(q.get(L.k)); val[L.k] = isNaN(v) ? 0 : v; });
  var absMode = q.get("absm") || "shift";
  var admVal = q.get("adm") || "", sixVal = q.get("six") || "";
  var sAbsMode, sAdm, sSix;

  function buildLevers() {
    levBox.innerHTML = "";
    var area = sMode.value === "area";
    var kinds = ["Attendance", "Intake"];
    kinds.forEach(function (kind) {
      levBox.appendChild(el("h4", {}, kind === "Attendance" ? "Attendance" : "Intake: what if the pupils were different"));
      if (kind === "Attendance" && area) {
        var w = el("div", {"class": "sm-lever"});
        w.appendChild(el("label", {}, "<span>Absence scenario</span>"));
        sAbsMode = select([["shift", "Change every school's absence by"], ["pct", "Cut every school's absence by a share"],
                           ["toEng", "Bring schools above England's average down to it"]], absMode);
        sAbsMode.addEventListener("change", function () { absMode = sAbsMode.value; buildLevers(); run(); });
        w.appendChild(sAbsMode); levBox.appendChild(w);
      }
      LEVERS.filter(function (L) { return L.kind === kind; }).forEach(function (L) {
        if (L.k === "abs" && area && absMode === "toEng") {
          levBox.appendChild(el("p", {"class": "sm-note"}, "England's average absence is " + fmt(ENG_ABS, 1, "%") +
            " (pupil-weighted). Schools already below it stay as they are.")); return; }
        var pctMode = L.k === "abs" && area && absMode === "pct";
        var w = el("div", {"class": "sm-lever"});
        var lab = el("label", {}, "<span>" + esc(L.l) + (pctMode ? " (cut by)" : "") + "</span>");
        var vs = el("span", {"class": "sm-val"}); lab.appendChild(vs); w.appendChild(lab);
        var r = el("input", {type: "range", min: pctMode ? 0 : L.min, max: pctMode ? 50 : L.max, step: pctMode ? 1 : L.step});
        if (pctMode && (val.abs < 0 || val.abs > 50)) val.abs = 0;
        r.value = val[L.k];
        r.addEventListener("input", function () { val[L.k] = parseFloat(r.value); run(); });
        w.appendChild(r);
        var now = el("div", {"class": "sm-now"}); w.appendChild(now);
        levBox.appendChild(w); inputs[L.k] = r; labels[L.k] = {v: vs, now: now, pct: pctMode};
      });
    });
    if (!area) {
      levBox.appendChild(el("h4", {}, "The school"));
      var w1 = el("div", {"class": "sm-lever"}); w1.appendChild(el("label", {}, "<span>Admissions</span>"));
      sAdm = select([["", "As now"]].concat(ADM), admVal); sAdm.addEventListener("change", function () { admVal = sAdm.value; run(); });
      w1.appendChild(sAdm); levBox.appendChild(w1);
      var w2 = el("div", {"class": "sm-lever"}); w2.appendChild(el("label", {}, "<span>Sixth form</span>"));
      sSix = select([["", "As now"], ["1", "Stops at 16"], ["0", "Has a sixth form"]], sixVal); sSix.addEventListener("change", function () { sixVal = sSix.value; run(); });
      w2.appendChild(sSix); levBox.appendChild(w2);
    }
    var rb = el("button", {type: "button", "class": "btn btn-sm btn-outline-secondary sm-reset"}, "Reset every lever");
    rb.addEventListener("click", function () { LEVERS.forEach(function (L) { val[L.k] = 0; }); admVal = ""; sixVal = ""; buildLevers(); run(); });
    levBox.appendChild(rb);
  }

  // ---- the model ------------------------------------------------------------
  function newValues(s) {
    var nv = {}, area = sMode.value === "area";
    LEVERS.forEach(function (L) {
      var x = s[L.k]; if (x == null) { nv[L.k] = null; return; }
      var y;
      if (L.k === "abs" && area && absMode === "pct") y = x * (1 - val.abs / 100);
      else if (L.k === "abs" && area && absMode === "toEng") y = Math.min(x, ENG_ABS);
      else y = x + val[L.k];
      if (L.log) y = Math.max(y, 0.1);
      if (L.pct || L.u === "%") y = Math.min(Math.max(y, 0), 100);
      nv[L.k] = y;
    });
    return nv;
  }
  // each lever's change in log Attainment 8 for one school and group
  function parts(s, g, nv) {
    var b = D.coefs[g] || {}, p = {};
    LEVERS.forEach(function (L) {
      var x = s[L.k], y = nv[L.k], beta = b[L.term];
      if (x == null || y == null || beta == null) { p[L.k] = 0; return; }
      p[L.k] = L.log ? beta * (Math.log(Math.max(y, 0.1)) - Math.log(Math.max(x, 0.1))) : beta * (y - x);
    });
    if (sMode.value === "school") {
      var admB = function (a) { var t = ADM_TERM[a]; return t ? (b[t] || 0) : 0; };
      p.adm = admVal ? admB(admVal) - admB(s.adm) : 0;
      p.six = sixVal !== "" ? (b["age16TRUE"] || 0) * (+sixVal - (s.age16 || 0)) : 0;
    }
    return p;
  }
  function simulate(s) {
    var nv = newValues(s), r = {nv: nv, g: {}};
    GROUPS.forEach(function (G) {
      var a = s["a_" + G[0]]; if (a == null) return;
      var p = parts(s, G[0], nv), tot = 0, alone = {};
      for (var k in p) { tot += p[k]; alone[k] = a * (Math.exp(p[k]) - 1); }
      r.g[G[0]] = {now: a, new: a * Math.exp(tot), alone: alone};
    });
    return r;
  }

  // ---- drawing -------------------------------------------------------------
  function dumbbell(rows) {
    var W = 640, rowH = 34, L = 150, R = 60, T = 22, H = T + rows.length * rowH + 10;
    var vals = []; rows.forEach(function (r) { if (r.now != null) vals.push(r.now, r.new); });
    if (!vals.length) return "";
    var x0 = Math.min.apply(null, vals), x1 = Math.max.apply(null, vals), pad = Math.max((x1 - x0) * 0.15, 1);
    x0 = Math.floor(x0 - pad); x1 = Math.ceil(x1 + pad);
    var X = function (v) { return L + (v - x0) / (x1 - x0) * (W - L - R); };
    var s = "";
    for (var t = Math.ceil(x0 / 5) * 5; t <= x1; t += 5)
      s += "<line x1='" + X(t) + "' x2='" + X(t) + "' y1='" + (T - 6) + "' y2='" + (H - 6) + "' stroke='#ece8f3'/>" +
           "<text x='" + X(t) + "' y='" + (T - 9) + "' text-anchor='middle' font-size='10' fill='#6b6680'>" + t + "</text>";
    rows.forEach(function (r, i) {
      var y = T + i * rowH + rowH / 2;
      s += "<text x='" + (L - 10) + "' y='" + (y + 4) + "' text-anchor='end' font-size='12.5' fill='#1a1a2e'>" + esc(r.l) + "</text>";
      if (r.now == null) { s += "<text x='" + L + "' y='" + (y + 4) + "' font-size='11' fill='#6b6680'>No result</text>"; return; }
      var d = r.new - r.now, col = d > 0.005 ? "#1b7837" : d < -0.005 ? "#762a83" : "#6b6680";
      s += "<line x1='" + X(r.now) + "' x2='" + X(r.new) + "' y1='" + y + "' y2='" + y + "' stroke='" + col + "' stroke-width='3' opacity='.55'/>" +
           "<circle cx='" + X(r.now) + "' cy='" + y + "' r='6' fill='#d3cde0' stroke='#8a8aa3'><title>Now: " + fmt(r.now, 1) + "</title></circle>" +
           "<circle cx='" + X(r.new) + "' cy='" + y + "' r='6' fill='#361a54'><title>Scenario: " + fmt(r.new, 1) + "</title></circle>" +
           "<text x='" + (Math.max(X(r.now), X(r.new)) + 10) + "' y='" + (y + 4) + "' font-size='12' font-weight='600' fill='" + col + "'>" +
           sgn(d, 1) + "</text>";
    });
    return "<svg viewBox='0 0 " + W + " " + H + "' width='100%' role='img' aria-label='Attainment 8 now and in the scenario, by group'>" + s + "</svg>";
  }
  function contribChart(items) {
    items = items.filter(function (it) { return Math.abs(it.v) >= 0.005; });
    if (!items.length) return "<p class='sm-note'>Move a lever to see what each one contributes.</p>";
    var W = 640, rowH = 26, L = 230, R = 60, T = 6, H = T + items.length * rowH + 6;
    var m = Math.max.apply(null, items.map(function (it) { return Math.abs(it.v); })) || 1;
    var X0 = L + (W - L - R) / 2, sc = (W - L - R) / 2 / m, s = "";
    s += "<line x1='" + X0 + "' x2='" + X0 + "' y1='0' y2='" + H + "' stroke='#8a8aa3'/>";
    items.forEach(function (it, i) {
      var y = T + i * rowH, w = it.v * sc, col = it.v > 0 ? "#1b7837" : "#762a83";
      s += "<text x='" + (L - 10) + "' y='" + (y + rowH / 2 + 4) + "' text-anchor='end' font-size='12' fill='#1a1a2e'>" + esc(it.l) + "</text>" +
        "<rect x='" + (w >= 0 ? X0 : X0 + w) + "' y='" + (y + 5) + "' width='" + Math.abs(w) + "' height='" + (rowH - 10) + "' rx='3' fill='" + col + "'/>" +
        "<text x='" + (w >= 0 ? X0 + w + 6 : X0 + w - 6) + "' y='" + (y + rowH / 2 + 4) + "' text-anchor='" + (w >= 0 ? "start" : "end") +
        "' font-size='11.5' fill='" + col + "'>" + sgn(it.v, 2) + "</text>";
    });
    return "<svg viewBox='0 0 " + W + " " + H + "' width='100%' role='img' aria-label='What each lever contributes'>" + s + "</svg>";
  }
  var LEVER_LAB = {adm: "Admissions", six: "Sixth form"};
  LEVERS.forEach(function (L) { LEVER_LAB[L.k] = L.l; });

  // ---- run -------------------------------------------------------------------
  function run() {
    var area = sMode.value === "area";
    sSchool.parentNode.style.display = area ? "none" : "";
    var list = inArea(), focal = area ? null : S.find(function (s) { return String(s.URN) === sSchool.value; });
    // lever read-outs
    LEVERS.forEach(function (L) {
      var lb = labels[L.k]; if (!lb) return;
      var v = val[L.k];
      lb.v.innerHTML = lb.pct ? (v ? "−" + v + "%" : "no change") : (v ? sgn(v, L.dp, L.u === "%" ? " pts" : "") : "no change");
      if (area) { var m = wmean(list, function (s) { return s[L.k]; });
        lb.now.innerHTML = "Area average now: " + fmt(m, L.dp, L.u) + "; England: " + fmt(wmean(S, function (s) { return s[L.k]; }), L.dp, L.u); }
      else if (focal) { var nv = newValues(focal)[L.k];
        lb.now.innerHTML = "Now " + fmt(focal[L.k], L.dp, L.u) + (v ? " → " + fmt(nv, L.dp, L.u) : "") +
          "; England " + fmt(wmean(S, function (s) { return s[L.k]; }), L.dp, L.u); }
    });
    // keep the scenario in the address bar
    var p = {mode: sMode.value, level: sLevel.value, area: sArea.value};
    if (!area && focal) p.urn = focal.URN;
    if (area) p.absm = absMode;
    LEVERS.forEach(function (L) { if (val[L.k]) p[L.k] = val[L.k]; });
    if (!area && admVal) p.adm = admVal; if (!area && sixVal !== "") p.six = sixVal;
    try { history.replaceState(null, "", "?" + new URLSearchParams(p).toString() + location.hash); } catch (e) {}

    if (area) drawArea(list); else if (focal) drawSchool(focal);
  }

  function gapLine(now, nw) {
    if (now == null) return "";
    return " The gap between non-disadvantaged and disadvantaged pupils goes from <strong>" + fmt(now, 1) + "</strong> to <strong>" +
      fmt(nw, 1) + "</strong> points (" + sgn(nw - now, 1) + ").";
  }
  function drawSchool(s) {
    var r = simulate(s), a = r.g.all;
    var gNow = (r.g.non_disadvantaged && r.g.disadvantaged) ? r.g.non_disadvantaged.now - r.g.disadvantaged.now : null;
    var gNew = gNow == null ? null : r.g.non_disadvantaged.new - r.g.disadvantaged.new;
    var h = "<div class='sm-summary'><p><strong>" + esc(s.n) + "</strong> (" + where(s) + "), " + esc(s.year) + ". " +
      "Attainment 8 for all pupils: <strong>" + fmt(a.now, 1) + "</strong> now, <strong>" + fmt(a.new, 1) + "</strong> in this scenario (" +
      sgn(a.new - a.now, 1) + " points, " + sgn(100 * (a.new / a.now - 1), 1, "%") + ")." + gapLine(gNow, gNew) + "</p>" +
      (s.va != null ? "<p class='sm-note'>Its value added (v2.1, all pupils) is " + sgn(s.va, 1) + ": that part of its result stays as it is.</p>" : "") + "</div>";
    h += "<div class='sm-h'>Attainment 8 by group, now and in the scenario</div>" +
      dumbbell(GROUPS.map(function (G) { var x = r.g[G[0]]; return {l: G[1], now: x ? x.now : null, new: x ? x.new : null}; }));
    var items = Object.keys(a.alone).map(function (k) { return {l: LEVER_LAB[k], v: a.alone[k]}; });
    h += "<div class='sm-h'>What each lever contributes, all pupils (Attainment 8 points, each on its own)</div>" + contribChart(items);
    h += "<p class='sm-note'>Each school starts from its latest published Attainment 8. A change goes with the same percentage change " +
      "in its result as between schools in the model, everything else held as it is.</p>";
    out.innerHTML = h;
  }
  function drawArea(list) {
    var sims = list.map(function (s) { return {s: s, r: simulate(s)}; });
    var grp = GROUPS.map(function (G) {
      var have = sims.filter(function (x) { return x.r.g[G[0]]; });
      var now = wmean(have, function (x) { return x.r.g[G[0]].now; }), nw = wmean(have, function (x) { return x.r.g[G[0]].new; });
      return {k: G[0], l: G[1], now: now, new: nw};
    });
    var A = grp[0], gNow = grp[2].now - grp[1].now, gNew = grp[2].new - grp[1].new;
    var pupils = list.reduce(function (t, s) { return t + (s.pupils || 0); }, 0);
    var moved = absMode === "toEng" ? list.filter(function (s) { return s.abs > ENG_ABS; }).length : null;
    var name = sLevel.value === "eng" ? "England" : sArea.value;
    var h = "<div class='sm-summary'><p><strong>" + esc(name) + "</strong>: " + list.length + " schools, about " +
      (Math.round(pupils / 100) * 100).toLocaleString("en-GB") + " pupils. " +
      (moved != null ? moved + " of them have absence above England's average and come down to it. " : "") +
      "Pupil-weighted Attainment 8 for all pupils: <strong>" + fmt(A.now, 1) + "</strong> now, <strong>" + fmt(A.new, 1) +
      "</strong> in this scenario (" + sgn(A.new - A.now, 2) + " points)." + gapLine(gNow, gNew) + "</p></div>";
    h += "<div class='sm-h'>Attainment 8 by group, now and in the scenario (pupil-weighted)</div>" + dumbbell(grp);
    var keys = {}; sims.forEach(function (x) { if (x.r.g.all) Object.keys(x.r.g.all.alone).forEach(function (k) { keys[k] = 1; }); });
    var items = Object.keys(keys).map(function (k) {
      return {l: LEVER_LAB[k], v: wmean(sims.filter(function (x) { return x.r.g.all; }), function (x) { return x.r.g.all.alone[k]; })}; });
    h += "<div class='sm-h'>What each lever contributes, all pupils (Attainment 8 points, each on its own)</div>" + contribChart(items);
    // school by school
    var rows = sims.filter(function (x) { return x.r.g.all; })
      .sort(function (a, b) { return (b.r.g.all.new - b.r.g.all.now) - (a.r.g.all.new - a.r.g.all.now); });
    var t = "<table class='table table-sm table-hover sm-t'><thead><tr><th>School</th><th>Authority</th><th class='r'>Absence now</th>" +
      "<th class='r'>Scenario</th><th class='r'>Attainment 8 now</th><th class='r'>Scenario</th><th class='r'>Change</th></tr></thead><tbody>";
    rows.slice(0, 300).forEach(function (x) {
      var g = x.r.g.all, d = g.new - g.now;
      t += "<tr><td>" + esc(x.s.n) + "</td><td>" + esc(x.s.la) + "</td><td class='r'>" + fmt(x.s.abs, 1, "%") + "</td><td class='r'>" +
        fmt(x.r.nv.abs, 1, "%") + "</td><td class='r'>" + fmt(g.now, 1) + "</td><td class='r'>" + fmt(g.new, 1) + "</td><td class='r " +
        (d > 0.005 ? "up" : d < -0.005 ? "dn" : "") + "'>" + sgn(d, 2) + "</td></tr>";
    });
    h += "<div class='sm-h'>School by school, all pupils" + (rows.length > 300 ? " (the 300 that change most)" : "") + "</div>" +
      "<div class='sm-table'>" + t + "</tbody></table></div>";
    out.innerHTML = h;
  }
  // ---- wiring ----------------------------------------------------------------
  sMode.addEventListener("change", function () { buildLevers(); run(); });
  sLevel.addEventListener("change", function () { fillAreas(); fillSchools(); run(); });
  sArea.addEventListener("change", function () { fillSchools(); run(); });
  sSchool.addEventListener("change", run);
  fillAreas(q.get("area")); fillSchools(q.get("urn")); buildLevers(); run();
})();
