MAP_JS <- '
function(el, x, data) {
  var map = this;
  // htmlwidgets writes the "json" strings into the page as objects
  var schools = data.schools, vars = data.vars, cats = data.cats, none = data.none;
  var heads = data.heads || {};
  var defaultVar = "va_shrunk_all";

  // Every ramp runs bad -> good. Diverging (value added, absence against
  // intake): purple -> pale grey at zero -> green, ColorBrewer PRGn, which
  // reads for those who cannot tell red from green. Sequential with a good
  // direction (absence, attainment): the same hues without the pale middle,
  // so every value reads. Measures with no good or bad direction (FSM,
  // EAL, ...) take a neutral grey ramp, carrying no judgement. The reader
  // can swap to red, amber and green (RdYlGn) or red, yellow and blue
  // (RdYlBu) from a menu, remembered across pages.
  var PAL = {
    prgn: {name: "Purple, green (colour-blind safe)",
          div: [[118,42,131],[153,112,171],[194,165,207],[231,212,232],[247,247,247],[217,240,211],[166,219,160],[90,174,97],[27,120,55]],
          seq: [[118,42,131],[153,112,171],[194,165,207],[231,212,232],[166,219,160],[90,174,97],[27,120,55]],
          words: ["purple", "green"]},
    rag: {name: "Red, amber, green",
          div: [[215,48,39],[244,109,67],[253,174,97],[254,224,139],[255,255,191],[217,239,139],[166,217,106],[102,189,99],[26,152,80]],
          seq: [[215,48,39],[244,109,67],[253,174,97],[254,224,139],[217,239,139],[145,207,96],[26,152,80]],
          words: ["red", "green"]},
    rdylbu: {name: "Red, yellow, blue (colour-blind safe)",
          div: [[215,48,39],[244,109,67],[253,174,97],[254,224,144],[255,255,191],[224,243,248],[171,217,233],[116,173,209],[69,117,180]],
          seq: [[215,48,39],[244,109,67],[253,174,97],[254,224,144],[171,217,233],[116,173,209],[69,117,180]],
          words: ["red", "blue"]}
  };
  var NEUTRAL = [[241,241,245],[158,158,176],[58,58,78]];
  var scheme = "prgn";
  try { var s0 = localStorage.getItem("va-pal"); if (PAL[s0]) scheme = s0; } catch (e) {}
  function pal() { return PAL[scheme]; }
  // Ofsted grades take the chosen scheme (good to bad); other categories keep their own colours
  function catsOf(n) { var o = (data.cats_s || {})[scheme]; return (o && o[n]) || cats[n] || {}; }
  function ramp(C, t) {
    t = Math.max(0, Math.min(1, t));
    var n = C.length - 1, i = Math.min(Math.floor(t * n), n - 1), f = t * n - i;
    return "rgb(" + [0,1,2].map(function(k) {
      return Math.round(C[i][k] + f * (C[i+1][k] - C[i][k])); }).join(",") + ")";
  }
  function css(C) { return "linear-gradient(to right," + C.map(function(c) {
    return "rgb(" + c.join(",") + ")"; }).join(",") + ")"; }

  var markers = L.layerGroup().addTo(map);
  var legDiv;
  var leg = L.control({position: "bottomright"});
  leg.onAdd = function() {
    legDiv = L.DomUtil.create("div");
    legDiv.style.cssText = "background:white;padding:8px 12px;border-radius:5px;" +
      "box-shadow:0 1px 5px rgba(0,0,0,.25);font-size:12px;line-height:1.5;max-width:200px;";
    return legDiv;
  };
  leg.addTo(map);

  function fmt(v, m) { return v == null ? "no data" :
    (m.scale === "div" && v > 0 ? "+" : "") + (+v).toFixed(m.dp) + m.unit; }

  function draw(name) {
    markers.clearLayers();
    var m = vars.find(function(v) { return v["var"] === name; });
    var vals = schools.map(function(d) { return d[name]; })
                      .filter(function(v) { return v != null && (m.scale === "cat" || !isNaN(v)); });
    var lo = 0, hi = 1, colour;
    if (m.scale === "cat") {
      colour = function(v) { return catsOf(name)[v] || "#c9c9d6"; };
    } else if (m.scale === "div") {
      // a fixed national cap (R: colour_caps()) so colours mean the same in every area
      hi = m.cap || Math.max.apply(null, vals.map(Math.abs)) || 1; lo = -hi;
      colour = function(v) { var t = (v - lo) / (hi - lo);
        return ramp(pal().div, m.good === "high" ? t : 1 - t); };
    } else {
      lo = Math.min.apply(null, vals); hi = Math.max.apply(null, vals);
      colour = function(v) { var t = (v - lo) / ((hi - lo) || 1);
        if (!m.good) return ramp(NEUTRAL, t);
        return ramp(pal().seq, m.good === "high" ? t : 1 - t); };
    }
    var missing = 0, counts = {};
    schools.forEach(function(d) {
      var v = d[name], none = v == null;
      if (none) missing++; else if (m.scale === "cat") counts[v] = (counts[v] || 0) + 1;
      L.circleMarker([d.lat, d.lon], {
        radius: Math.sqrt(d.pupils) / 4,
        fillColor: none ? "#ffffff" : colour(v), fillOpacity: none ? 0.6 : 0.92,
        // a thin dark rim, so the pale middle of the ramp still shows on the basemap
        color: none ? "#8a8aa3" : "rgba(40,40,50,0.55)", weight: none ? 1.5 : 0.8
      }).bindTooltip(d.tip + "<br><b>" + m.label + ": " +
          (m.scale === "cat" ? (v || none[name] || "not known") : fmt(v, m)) + "</b>")
        .addTo(markers);
    });

    var h = "<strong>" + m.label + "</strong><br>";
    if (m.scale === "cat") {
      var curHead = null;
      Object.keys(catsOf(name)).forEach(function(k) {
        if (!counts[k]) return;
        var hd = heads[name] ? heads[name][k] : null;
        if (hd && hd !== curHead) {
          h += "<div style=\\"font-size:11px;color:#666;margin:4px 0 1px\\">" + hd + "</div>";
          curHead = hd;
        }
        h += "<div><span style=\\"display:inline-block;width:11px;height:11px;border-radius:50%;" +
          "margin-right:6px;vertical-align:-1px;background:" + catsOf(name)[k] + "\\"></span>" +
          k + " (" + counts[k] + ")</div>";
      });
    } else {
      var base = m.scale === "div" ? pal().div : (m.good ? pal().seq : NEUTRAL);
      var C = (m.good === "low") ? base.slice().reverse() : base;
      var wd = pal().words;
      h += "<div style=\\"height:12px;border-radius:2px;margin:4px 0;background:" + css(C) + "\\"></div>" +
        "<span>" + (m.cap ? "&le; " : "") + fmt(lo, m) + "</span>" +
        (m.scale === "div" ? "<span style=\\"position:absolute;left:50%;transform:translateX(-50%)\\">0</span>" : "") +
        "<span style=\\"float:right\\">" + (m.cap ? "&ge; " : "") + fmt(hi, m) + "</span>";
      if (m.scale === "div") h += "<div style=\\"font-size:11px;color:#666\\">" +
        (m.good === "high" ? wd[0] + " below expectation, " + wd[1] + " above"
                           : wd[0] + " above what the intake predicts, " + wd[1] + " below") + "</div>";
      else if (m.good) h += "<div style=\\"font-size:11px;color:#666\\">" + wd[1] + " is better, " + wd[0] + " worse</div>";
    }
    if (missing) h += "<div style=\\"font-size:11px;color:#666;margin-top:2px\\">" +
      "<span style=\\"display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:5px;" +
      "border:1.5px solid #8a8aa3\\"></span>" + (none[name] || "no data") + " (" + missing + ")</div>";
    h += "<div style=\\"font-size:11px;color:#666\\">Circle size: pupils on roll</div>";
    legDiv.style.position = "relative";
    legDiv.innerHTML = h;
    drawDist(name, m, vals, colour, lo, hi, counts);
  }

  // How the values on the map are spread, under it, in the same colours.
  // Few schools get a dot each, so a district map of a dozen schools is not
  // a histogram of mostly empty bins; many get bins. Both take their fill
  // from the colour() the map is using, so the two cannot disagree.
  var distBox = document.createElement("div");
  distBox.style.cssText = "margin:10px 0 0;";
  el.parentNode.insertBefore(distBox, el.nextSibling);
  var DOT_MAX = 120;

  function esc2(s) { return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;"); }
  function at(x, y, anchor, size, fill, txt) {
    return "<text x=\\"" + x + "\\" y=\\"" + y + "\\"" +
      (anchor ? " text-anchor=\\"" + anchor + "\\"" : "") +
      " font-size=\\"" + size + "\\" fill=\\"" + fill + "\\">" + txt + "</text>";
  }

  function drawDist(name, m, vals, colour, lo, hi, counts) {
    var W = 720, L = 10, R = 10, T = 12, B = 20, iw = W - L - R;
    var s = "", H, ih, cap;

    if (m.scale === "cat") {
      var keys = Object.keys(catsOf(name)).filter(function(k) { return counts[k]; });
      if (!keys.length) { distBox.innerHTML = ""; return; }
      var bh = 15, gap = 4, lw = 190;
      ih = keys.length * (bh + gap) - gap; H = T + ih + B;
      var mxc = 0;
      keys.forEach(function(k) { if (counts[k] > mxc) mxc = counts[k]; });
      keys.forEach(function(k, j) {
        var y = T + j * (bh + gap), w = (counts[k] / mxc) * (iw - lw - 34);
        s += "<rect x=\\"" + (L + lw) + "\\" y=\\"" + y + "\\" width=\\"" + w.toFixed(1) +
             "\\" height=\\"" + bh + "\\" rx=\\"2\\" fill=\\"" + catsOf(name)[k] + "\\"></rect>";
        s += at(L + lw - 7, y + bh - 3.5, "end", 11, "#333", esc2(k));
        s += at(L + lw + w + 5, y + bh - 3.5, null, 11, "#666", counts[k]);
      });
      cap = keys.length + " grades, " + vals.length + " schools";
    } else {
      if (!vals.length) { distBox.innerHTML = ""; return; }
      H = 104; ih = H - T - B;
      var span = (hi - lo) || 1;
      var xs = function(v) { return L + ((v - lo) / span) * iw; };
      s += "<line x1=\\"" + L + "\\" y1=\\"" + (T + ih) + "\\" x2=\\"" + (L + iw) +
           "\\" y2=\\"" + (T + ih) + "\\" stroke=\\"#d8d8e0\\"></line>";
      if (vals.length <= DOT_MAX) {
        var sorted = vals.slice().sort(function(a, b) { return a - b; });
        var rows = [];
        sorted.forEach(function(v) {
          var cx = xs(v), r = 0;
          while (rows[r] != null && cx - rows[r] < 7.6) r++;
          rows[r] = cx;
          var cy = T + ih - 5.5 - r * 7.6;
          if (cy < T + 4) cy = T + 4;
          s += "<circle cx=\\"" + cx.toFixed(1) + "\\" cy=\\"" + cy.toFixed(1) +
               "\\" r=\\"3.4\\" fill=\\"" + colour(v) + "\\" stroke=\\"#fff\\" stroke-width=\\"0.7\\"></circle>";
        });
        cap = "One dot a school, " + vals.length + " of them, coloured as on the map";
      } else {
        var NB = 28, bins = [], k2;
        for (k2 = 0; k2 < NB; k2++) bins[k2] = 0;
        vals.forEach(function(v) {
          var b = Math.floor(((v - lo) / span) * NB);
          if (b < 0) b = 0; if (b > NB - 1) b = NB - 1;
          bins[b]++;
        });
        var mxb = 0;
        bins.forEach(function(c) { if (c > mxb) mxb = c; });
        var bw = iw / NB;
        bins.forEach(function(c, b) {
          if (!c) return;
          var hgt = (c / mxb) * (ih - 4);
          s += "<rect x=\\"" + (L + b * bw + 0.6).toFixed(1) + "\\" y=\\"" + (T + ih - hgt).toFixed(1) +
               "\\" width=\\"" + (bw - 1.2).toFixed(1) + "\\" height=\\"" + hgt.toFixed(1) +
               "\\" fill=\\"" + colour(lo + (b + 0.5) * span / NB) + "\\"></rect>";
        });
        cap = vals.length + " schools in " + NB + " bins, coloured as on the map";
      }
      if (m.scale === "div" && lo < 0 && hi > 0) {
        s += "<line x1=\\"" + xs(0).toFixed(1) + "\\" y1=\\"" + T + "\\" x2=\\"" + xs(0).toFixed(1) +
             "\\" y2=\\"" + (T + ih + 4) + "\\" stroke=\\"#999\\" stroke-dasharray=\\"3 2\\"></line>";
        s += at(xs(0).toFixed(1), T + ih + 15, "middle", 10, "#666", "0");
      }
      s += at(L, T + ih + 15, null, 10, "#666", fmt(lo, m));
      s += at(L + iw, T + ih + 15, "end", 10, "#666", fmt(hi, m));
    }

    distBox.innerHTML =
      "<div style=\\"font-size:12px;color:#444;margin-bottom:2px\\"><b>" + esc2(m.label) +
      "</b> &mdash; how the schools on the map are spread</div>" +
      "<svg viewBox=\\"0 0 " + W + " " + H + "\\" width=\\"100%\\" height=\\"" + H +
      "\\" preserveAspectRatio=\\"xMidYMin meet\\" role=\\"img\\" aria-label=\\"Distribution of " +
      esc2(m.label) + "\\">" + s + "</svg>" +
      "<div style=\\"font-size:11px;color:#666\\">" + cap + "</div>";
  }

  // Dropdown above the map
  var wrap = document.createElement("div");
  wrap.style.cssText = "margin:0 0 8px;font-size:14px;display:flex;flex-wrap:wrap;gap:6px;align-items:center;";
  var lbl = document.createElement("label");
  lbl.textContent = "Colour schools by:";
  lbl.style.fontWeight = "bold";
  var sel = document.createElement("select");
  sel.setAttribute("aria-label", "Variable to colour the schools by");
  sel.style.cssText = "padding:4px 8px;font-size:13px;max-width:100%;border:1px solid #ccc;border-radius:4px;";
  var og = null;
  vars.forEach(function(v) {
    if (!og || og.label !== v.group) {
      og = document.createElement("optgroup"); og.label = v.group; sel.appendChild(og);
    }
    var o = document.createElement("option");
    o.value = v["var"]; o.textContent = v.label;
    if (v["var"] === defaultVar) o.selected = true;
    og.appendChild(o);
  });
  wrap.appendChild(lbl); wrap.appendChild(sel);
  // red and green are hard to tell apart for about one man in twelve
  var pl = document.createElement("label");
  pl.textContent = "Colours:";
  pl.style.cssText = "font-weight:bold;margin-left:10px;";
  var ps = document.createElement("select");
  ps.setAttribute("aria-label", "Colour scheme");
  ps.style.cssText = sel.style.cssText;
  Object.keys(PAL).forEach(function(k) {
    var o = document.createElement("option"); o.value = k; o.textContent = PAL[k].name;
    if (k === scheme) o.selected = true; ps.appendChild(o);
  });
  wrap.appendChild(pl); wrap.appendChild(ps);
  ps.addEventListener("change", function() {
    scheme = ps.value;
    try { localStorage.setItem("va-pal", scheme); } catch (e) {}
    draw(sel.value);
  });
  el.parentNode.insertBefore(wrap, el);
  sel.addEventListener("change", function() { draw(sel.value); });
  draw(defaultVar);
}
'
