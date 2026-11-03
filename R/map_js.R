MAP_JS <- '
function(el, x, data) {
  var map = this;
  // htmlwidgets writes the "json" strings into the page as objects
  var schools = data.schools, vars = data.vars, cats = data.cats, none = data.none;
  var heads = data.heads || {};
  var defaultVar = "va_shrunk_all";

  // Sequential: pale purple -> mid purple -> dark purple. Diverging: blue -> grey -> dark purple.
  var SEQ = [[238,222,255],[186,130,255],[54,26,84]];
  var DIV = [[31,127,160],[232,230,238],[54,26,84]];
  function ramp(C, t) {
    t = Math.max(0, Math.min(1, t));
    var i = Math.min(Math.floor(t * 2), 1), f = t * 2 - i;
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
      colour = function(v) { return cats[name][v] || "#c9c9d6"; };
    } else if (m.scale === "div") {
      // a fixed national cap (R: colour_caps()) so colours mean the same in every area
      hi = m.cap || Math.max.apply(null, vals.map(Math.abs)) || 1; lo = -hi;
      colour = function(v) { var t = (v - lo) / (hi - lo);
        return ramp(DIV, m.good === "high" ? t : 1 - t); };
    } else {
      lo = Math.min.apply(null, vals); hi = Math.max.apply(null, vals);
      colour = function(v) { return ramp(SEQ, (v - lo) / ((hi - lo) || 1)); };
    }
    var missing = 0, counts = {};
    schools.forEach(function(d) {
      var v = d[name], none = v == null;
      if (none) missing++; else if (m.scale === "cat") counts[v] = (counts[v] || 0) + 1;
      L.circleMarker([d.lat, d.lon], {
        radius: Math.sqrt(d.pupils) / 4,
        fillColor: none ? "#ffffff" : colour(v), fillOpacity: none ? 0.6 : 0.9,
        color: none ? "#8a8aa3" : "white", weight: none ? 1.5 : 1
      }).bindTooltip(d.tip + "<br><b>" + m.label + ": " +
          (m.scale === "cat" ? (v || none[name] || "not known") : fmt(v, m)) + "</b>")
        .addTo(markers);
    });

    var h = "<strong>" + m.label + "</strong><br>";
    if (m.scale === "cat") {
      var curHead = null;
      Object.keys(cats[name]).forEach(function(k) {
        if (!counts[k]) return;
        var hd = heads[name] ? heads[name][k] : null;
        if (hd && hd !== curHead) {
          h += "<div style=\\"font-size:11px;color:#666;margin:4px 0 1px\\">" + hd + "</div>";
          curHead = hd;
        }
        h += "<div><span style=\\"display:inline-block;width:11px;height:11px;border-radius:50%;" +
          "margin-right:6px;vertical-align:-1px;background:" + cats[name][k] + "\\"></span>" +
          k + " (" + counts[k] + ")</div>";
      });
    } else {
      var C = m.scale === "div" ? (m.good === "high" ? DIV : DIV.slice().reverse()) : SEQ;
      h += "<div style=\\"height:12px;border-radius:2px;margin:4px 0;background:" + css(C) + "\\"></div>" +
        "<span>" + (m.cap ? "&le; " : "") + fmt(lo, m) + "</span>" +
        (m.scale === "div" ? "<span style=\\"position:absolute;left:50%;transform:translateX(-50%)\\">0</span>" : "") +
        "<span style=\\"float:right\\">" + (m.cap ? "&ge; " : "") + fmt(hi, m) + "</span>";
      if (m.scale === "div") h += "<div style=\\"font-size:11px;color:#666\\">" +
        (m.good === "high" ? "blue below expectation, purple above" : "purple below expectation, blue above") + "</div>";
    }
    if (missing) h += "<div style=\\"font-size:11px;color:#666;margin-top:2px\\">" +
      "<span style=\\"display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:5px;" +
      "border:1.5px solid #8a8aa3\\"></span>" + (none[name] || "no data") + " (" + missing + ")</div>";
    h += "<div style=\\"font-size:11px;color:#666\\">Circle size: pupils on roll</div>";
    legDiv.style.position = "relative";
    legDiv.innerHTML = h;
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
  el.parentNode.insertBefore(wrap, el);
  sel.addEventListener("change", function() { draw(sel.value); });
  draw(defaultVar);
}
'
