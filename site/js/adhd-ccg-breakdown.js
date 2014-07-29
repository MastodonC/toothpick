(function(exports) {

  var _chart = null

  function generateChartForCcg(svg, ccg, data) {
    // A bit dodgy I know, but last minute stuff
    data = data.filter(function(i) { return i.id.split("-")[0] === ccg })

    if(_chart) {
      _chart.data = data
      _chart.draw(2000)
    } else {
      var myChart = _chart = new dimple.chart(svg, data)
      myChart.setBounds(50, 30, 350, 250)
      var x = myChart.addCategoryAxis("x", "name")
      var y = myChart.addMeasureAxis("y", "drift")
      y.overrideMin = -2  
      y.overrideMax = 10   
      myChart.addSeries(null, dimple.plot.bar)
      myChart.draw()
    }
  }

  var _data = null
    , _svg =  null

  exports.changeCcgInBreakdown = function(ccg) {
    if(!_data) return setTimeout(function() { changeCcgInBreakdown(ccg) }, 500)
    generateChartForCcg(_svg, ccg, _data)
  }

  exports.ccgBreakdown = function(div) {
    _svg = d3.select('#' + div)
                .append('svg')
                .attr('width', 400)
                .attr('height', 280)

    d3.csv('data/adhd-gp-scrips-drift.csv', function(items) {
      _data = items
    })
  }
})(this)
