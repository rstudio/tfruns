
var MetricsCharts = (function () {

  var MetricsCharts = function(container, metrics, update_url, auto_resize) {

    // save container and update_url
    _container = container;
    _update_url = update_url;

    // determine metrics we will be plotting (filter out val_ prefixed ones)
    var keys = Object.keys(metrics);
    for (var k = 0; k<keys.length; k++) {
      if (keys[k].lastIndexOf('val_', 0) !== 0)
        _metric_names.push(keys[k]);
    }

    // create a C3 chart for each metric
    var total_epochs = get_total_epochs(metrics);
    for (var i = 0; i<_metric_names.length; i++) {

      // get the metric
      var metric = _metric_names[i];

      // default y_axis options
      var y_axis = {
        tick: {
          format: y_tick_format
        }
      };
      // special y-axis treatment for accuracy
      if (is_accuracy(metric)) {
        y_axis.padding = {
          top: 0
        };
      }

      // create a chart wrapper div
      var c3_div = document.createElement("div");
      _container.appendChild(c3_div);

      // create c3 chart bound to div
      var tick_values = null;
      if (total_epochs <= 30) {
        tick_values = [];
        for (var n = 1; n <= total_epochs; n++)
          tick_values.push(n);
      }
      var chart = c3.generate({
        bindto: c3_div,
        axis: {
          x: {
            min: 1,
            tick: {
              values: tick_values
            }
          },
          y: y_axis
        },
        data: {
          columns: chart_columns(metric, metrics)
        },
        transition: {
          duration: 20
        }
      });

      // adjust y axis
      adjust_y_axis(chart, metric, metrics);

      // track chart
      _c3_charts.push(chart);

    }

    // auto update if appropriate
    if (_update_url !== null && !run_completed(metrics) && (window.location.protocol !== "file"))
      auto_update();

    // auto resize if requested
    if (auto_resize) {
      resize();
      window.addEventListener("resize", resize);
    }
  };

  // periodically update metrics from a url
  function auto_update() {
    var updateInterval = setInterval(function() {
      load_metrics(function(data) {

        // refresh each metric
        for (var i = 0; i<_metric_names.length; i++) {
          var metric = _metric_names[i];
          var chart = _c3_charts[i];
          chart.load({
            columns: chart_columns(metric, data)
          });
          adjust_y_axis(chart, metric, data);
          // ensure repaint
          chart.flush();
        }

        // stop refreshing metrics when the run is completed
        if (run_completed(data))
          clearInterval(updateInterval);
      },
      // error handler
      function() {
        clearInterval(updateInterval);
      });
    }, 1000);
  }

  // helper to determine whether we've seen all the data
  function run_completed(data) {
    return get_current_epochs(data) >= get_total_epochs(data);
  }

  // see how many total epochs there are
  function get_total_epochs(data) {
    var first_metric = data[_metric_names[0]];
    return first_metric.length;
  }

  // see how many epochs are in the data
  function get_current_epochs(data) {
    var first_metric = data[_metric_names[0]];
    for (var r = 0; r<first_metric.length; r++) {
      if (first_metric[r] === null) {
        break;
      }
    }
    return r;
  }

  // determine whether a metric is 'accuracy'
  function is_accuracy(metric) {
    return metric === 'acc' || metric === 'accuracy';
  }

  // helper to format y tick marks. use the default
  // d3 formatter but strip long sequences of zeros
  // followed by a single digit at the end (result
  // of JavaScript floating point rouning issues
  // during axis interpolation)
  var _default_format = d3.format("");
  function y_tick_format(d) {
    var fmt = _default_format(d);
    return fmt.replace(/0+\d$/, '');
  }

  // tweak chart y-axis
  function adjust_y_axis(chart, metric, data) {
    var current_epochs = get_current_epochs(data);
    if (is_accuracy(metric) && (current_epochs > 0))
      chart.axis.max({
        y: 1
      });
  }

  // yield chart column data
  function chart_columns(metric, data) {

    var columns = [
      [metric,null].concat(data[metric])
    ];

    var val_metric = "val_" + metric;
    if (data.hasOwnProperty(val_metric)) {
      columns.push(
        [val_metric,null].concat(data[val_metric])
      );
    }

    return columns;
  }

  // load metrics from the update url
  function load_metrics(callback, on_error) {
    var request = new XMLHttpRequest();
    request.onreadystatechange = function() {
    if (request.readyState === 4) {
      if (request.status === 200 || request.status === 0) {
        try {
          var data = JSON.parse(request.responseText);
          if (callback)
            callback(data);
        } catch(err) {
          if (on_error)
            on_error();
        }
      } else {
        if (on_error)
          on_error();
      }
    }
    };
    request.open('GET', _update_url);
    request.setRequestHeader('Cache-Control', 'no-cache');
    request.send();
  }

  // get the array of charts
  function charts() {
    return _c3_charts;
  }

  // resize charts to fit within their container
  function resize() {
    var chart_height = _container.offsetHeight / _c3_charts.length;
    for (var i = 0; i<_c3_charts.length; i++) {
      var chart = _c3_charts[i];
      var element = chart.element;
      element.style.maxHeight = "none";
      element.style.height = chart_height + "px";
      var elementRect = element.getBoundingClientRect();
      chart.resize({
        height: elementRect.height,
        width: elementRect.width
      });
    }
  }

  var _container = null;
  var _metric_names = [];
  var _c3_charts = [];
  var _update_url = null;

  MetricsCharts.prototype = {
    constructor: MetricsCharts,
    charts: charts,
    resize: resize
  };

  return MetricsCharts;

})();







