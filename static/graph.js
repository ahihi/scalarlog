function init_canvas(parent, data, options) {
  var canvas = $("<canvas>");
  
  var canvas_elem = canvas[0];
  if(!canvas_elem.getContext) {
    return;
  }

  parent.empty();
  
  var style = window.getComputedStyle(parent[0], null);    
  var style_value = function(x) {
    var str_value = style.getPropertyValue(x);
    var value = parseFloat(str_value);
    return value;
  };
  
  canvas.attr("width", parent.innerWidth());
  canvas.attr("height", parent.innerHeight());
  
  parent.append(canvas);

  var ctx = canvas_elem.getContext("2d");
  var chart = new Chart(ctx).Scatter(data, options);
}

function init_graph(json_url) {
  $.getJSON(json_url, success = function(tag) {
    var data = [{
      label: tag.tag + " (" + tag.unit + ")",
      pointColor: "rgb(204, 0, 153)",
      data: $.map(tag.data, function(e) {
        return {
          x: Date.parse(e.time),
          y: e.value
        };
      })
    }];
    var options = {
      scaleType: "date",
      scaleDateFormat: "yyyy-mm-dd",
      scaleTimeFormat: "HH:MM",
      scaleDateTimeFormat: "yyyy-mm-dd HH:MM",
      scaleLabel: function(x) {
        return x.value + " " + tag.unit;
      },
      datasetStroke: true,
      datasetStrokeWidth: 3,
      datasetStrokeColor: "rgba(204, 0, 153, 0.1)",
      responsive: true,
      maintainAspectRatio: false,
      animation: false
    };
  
    var parent = $("#content");
    var init_canvas_closed = function() { init_canvas(parent, data, options); };
  
    var resize_timer = null;
    $(window).resize(function() {
      if(resize_timer !== null) {
        clearTimeout(resize_timer);
      }
      resize_timer = setTimeout(init_canvas_closed, 50);
    })
  
    init_canvas_closed();
  });
}
