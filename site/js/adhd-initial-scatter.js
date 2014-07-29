(function(exports) {

  var _svg = null
  function getsvg() {
    if(_svg) return _svg
    _svg = d3.select('#adhd-initial-scatter')
                  .append('svg')
                  .attr('width', 640)
                  .attr('height', 350);

      _svg.append("line")
         .attr("x1", 18)
         .attr("x2", 18)
         .attr("y1", 35)
         .attr("y2", 300)
         .style("stroke", "rgb(0, 0,0)")
      
      _svg.append("text") 
         .attr("x", 0)
         .attr("y", 30)
         .text("Less")

      _svg.append("text") 
         .attr("x", 0)
         .attr("y", 310)
         .text("More")

    return _svg
  }
    
  //id,name,population,scrips_per_head,spend_per_head,drift
  exports.initialScatter = function(removeOutliers) {
    var csvfile = removeOutliers ?  "data/adhd-gp-scrips-drift.csv"
                                 :  "data/adhd-gp-scrips-drift-unclean.csv"

    d3.csv(csvfile, function(items) {
      var svg = getsvg()

      function scrips_per_head(d) {
        return parseFloat(d.scrips_per_head)
      }

      var max_scrips = d3.max(items, scrips_per_head)
      var min_scrips = d3.min(items, scrips_per_head)

      var vertical = d3.scale.linear()
                         .range([20, 300])
                         .domain([min_scrips, max_scrips])

      var horizontal = d3.scale.linear()
                         .range([20, 620])
                         .domain([0, items.length])

      var points = svg.selectAll('circle')
          .data(items)

       points.exit().remove("circle");
       points.transition()
             .duration(1000)
             .attr("cx", function(d, i) {
                 return horizontal(i) })
             .attr("cy", function(d) { 
                 return 20 + vertical(scrips_per_head(d))})

       points.enter()
          .append("circle")
            .attr("class", "datapoint")
            .attr("cx", function(d, i) {
                return horizontal(i) })
            .attr("cy", function(d) { 
                return 20 + vertical(scrips_per_head(d))})
            .attr("r", 2)
            


    })
  }

})(this)
