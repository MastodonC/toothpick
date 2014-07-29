var spend_histogram = function spend_histogram(chart_div) {

    d3.csv("./data/diabetes_per_head_per_ccg_per_month.csv", function (data) {

        var sampsize = 0;
        var maxValue = 0, minValue = 10000;
        var arr = new Array();

        sampsize = data.length;

        for (var i = 0; i < sampsize; i++) {
            maxValue = Math.max(maxValue, parseFloat(data[i].per_capita_spend));
            minValue = Math.min(minValue, parseFloat(data[i].per_capita_spend));

            arr.push(parseFloat(data[i].per_capita_spend));
        }

        maxValue = maxValue + 1;
        minValue = minValue - 1;

        var no_of_bins = Math.floor(Math.pow(2 * sampsize, 1 / 3)) + 1; // Calculated using The Rice Rule
        var stepsize = (maxValue - minValue) / no_of_bins;

        var p = 0,
            w = 550,
            h = 450;

        var vis = d3.select(chart_div).append("svg:svg")
            .attr("width", w)
            .attr("height", h)
            .append("svg:g")
            .attr("transform", "translate(.5)");

        // Add data series
        var histogram = d3.layout.histogram()
            .range([minValue, maxValue])
            .bins(2 * no_of_bins)
            .frequency("frequency")
            (arr);

        var x = d3.scale.ordinal()
            .domain(histogram.map(function (d) {
                return d.x;
            }))
            .rangeRoundBands([0, w - p]);

        var y = d3.scale.linear()
            .domain([0, d3.max(histogram, function (d) {
                return d.y;
            })])
            .range([0, h - 100]);

        vis.selectAll("rect")
            .data(histogram)
            .enter().append("svg:rect")
            .attr("transform", function (d, i) {
                return "translate(" + x(d.x) + "," + ( 70 + (h - 100) - y(histogram[i].y)) + ")";
            })
            .attr("width", x.rangeBand())
            .attr("y", (h - 100) + 35)
            .attr("height", 0)
            .style("stroke", "white")
            .attr("y", 0)
            .attr("height", function (d, i) {
                return y(histogram[i].y);
            });

        var bars = vis.selectAll("g.rule")
            .data(histogram)
            .enter().append("svg:g")
            .attr("class", "rule");

        bars.append("svg:g")
            .attr("transform", function (d, i) {
                return "translate(" + x(d.x) + "," + ( 70 + (h - 100) - y(histogram[i].y)) + ")";
            })
            .attr("width", x.rangeBand())
            .attr("y", (h - 100) + 35)
            .attr("height", 0)
            .style("stroke", "white")
            .attr("y", 0)
            .attr("height", function (d, i) {
                return y(histogram[i].y);
            });

        // X-axis
        vis.append("line")
            .attr("id", "axis")
            .attr("x1", 0)
            .attr("x2", w)
            .attr("y1", h - 25)
            .attr("y2", h - 25);

        // Ticks
        bars.append("line")
            .attr("id", "tick")
            .attr("x1", function (d, i) {
                return x(i * no_of_bins);
            })
            .attr("x2", function (d, i) {
                return x(i * no_of_bins);
            })
            .attr("y1", h - 23)
            .attr("y2", h - 28);

        // Labels
        bars.append("text")
            .style("font-size", "10")
            .attr("x", function (d, i) {
                return x(i * no_of_bins) + 6;
            })
            .attr("y", h - 10)
            .attr("text-anchor", function (d) {
                return "end";
            })
            .text(function (d, i) {
                if (i % 2 == 1 && i > 0) {
                    return Math.round(i * stepsize / 2 + minValue, 1);
                } else {
                    return '';
                }
            });

    });

}