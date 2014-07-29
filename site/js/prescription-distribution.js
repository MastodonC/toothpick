(function(exports) {

  function value(d) {
    return parseFloat(d.scrips_per_head)
  }

  function normalise_value(d) {
    return Math.log(parseFloat(d.scrips_per_head))
  }

  var _svg = null
    , _chart = null

  function getsvg() {
    _svg = _svg || d3.select("#adhd-distribution-chart").append('svg')
                .attr("width", 600)
                .attr("height", 360)
    return _svg
  }
  
  function chartMeUp(csv, normalise) {
    var svg = getsvg()
      , valuefn = normalise ? normalise_value : value

    var minx = d3.min(csv, valuefn)
      , maxx = d3.max(csv, valuefn)

    var bucketcount = Math.floor(Math.pow(2 * csv.length, 1 / 3)) + 1;
    var step = (maxx - minx) / bucketcount;

    var buckets = []
    for(var i = 0 ; i <= bucketcount ; i++)
      buckets[i] = { Frequency: 0, "Scrips Per Patient": (i * step) + minx, entries: [] }

    for(i =0 ; i < csv.length; i++) {
      var item = csv[i]
        , index = parseInt((valuefn(item) - minx) / step, 10)
      buckets[index].Frequency++
    }

    if(_chart) {
      _chart.data = buckets
      _chart.draw(1000)
    } else {
      _chart = new dimple.chart(svg, buckets);
      _chart.setBounds(60, 30, 510, 305)
      var x = _chart.addCategoryAxis("x", ["Scrips Per Patient"]);
      x.addOrderRule("Scrips Per Patient")
      var y = _chart.addMeasureAxis("y", "Frequency");
      _chart.addSeries(null, dimple.plot.bar);
      _chart.draw();
    }

    
  }

  exports.prescriptionDistribution =  function(normalise) {
    d3.csv("data/adhd-gp-scrips-drift.csv", function(csv) {
      chartMeUp(csv, normalise);
    })
  }

})(window)
