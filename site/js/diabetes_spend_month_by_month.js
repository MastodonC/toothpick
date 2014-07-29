var spend_month_by_month = function spend_month_by_month(chart_div, ccg_code) {

    var chart_svg = dimple.newSvg(chart_div, 600, 300);

    d3.csv("/data/spend_month_by_month2012.csv", function (data) {

        data = dimple.filterData(data, "ccg_code", [ccg_code])
        var myChart = new dimple.chart(chart_svg, data);
        myChart.setBounds(60, 30, 350, 250)
        var x = myChart.addCategoryAxis("x", "month");
        x.addOrderRule(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]);
        x.lineMarkers = true;
        var y = myChart.addMeasureAxis("y", "per_capita_spend");
        y.overrideMax = 28;
        y.overrideMin = 0;
        var s = myChart.addSeries(["ccg_code", "per_capita_spend"], dimple.plot.bar);

        // Handle the hover event - overriding the default behaviour
        s.addEventHandler("mouseover", onHover);
        // Handle the leave event - overriding the default behaviour
        s.addEventHandler("mouseleave", onLeave);

        myChart.draw();
        // Override x-axis
        x.titleShape.text ("Month");

        // Override y-axis title
        y.titleShape.text ("Spend per capita (£)");
        // Override x-axis tick labels

        // Event to handle mouse enter
        function onHover(e) {

            // Get the properties of the selected shape
            var rx = parseFloat(e.selectedShape.attr("x")),
                ry = parseFloat(e.selectedShape.attr("y")),
                h = parseFloat(e.selectedShape.attr("height")),
                w = parseFloat(e.selectedShape.attr("width"));
                fill = e.selectedShape.attr("fill"),
                opacity = e.selectedShape.attr("opacity");

            // Set the size and position of the popup
            var width = 100,
                height = 40,
                x = (rx + width + 10 < e.selectedShape.attr("width") ?
                    rx + 10 :
                    rx - width - 20);
                y = (ry - height / 2 < 0 ?
                    15 :
                    ry - height / 2);

            /* Drop line that goes from the circle to x- and y-axis. Shown on hover. */
            var dropDest = myChart.series[0]._dropLineOrigin(),
                animDuration = 750;

            if (myChart._tooltipGroup !== null && myChart._tooltipGroup !== undefined) {
                myChart._tooltipGroup.remove();
            }
            myChart._tooltipGroup = myChart.svg.append("g");

            // Add a drop line to the y axis
            if (dropDest.y !== null) {
                myChart._tooltipGroup.append("line")
                    .attr("id", "dropRect")
                    .attr("x1", (rx < dropDest.x ? rx : rx))
                    .attr("y1", (ry < myChart.series[0].y._origin ? ry : ry + h - 1))
                    .attr("x2", (rx < dropDest.x ? rx : rx))
                    .attr("y2", (ry < myChart.series[0].y._origin ? ry : ry + h - 1))
                    .style("opacity", opacity)
                    .transition()
                    .delay(animDuration / 2)
                    .duration(animDuration / 2)
                    .ease("linear")
                    .attr("x2", dropDest.x);
            }

             // Highlight the data point
             myChart._tooltipGroup.append("rect")
             .attr("id", "frame")
             .attr("x", rx)
             .attr("y", ry)
             .attr("width", e.selectedShape.attr("width"))
             .attr("height", e.selectedShape.attr("height"))
             .attr("opacity", 0)
             .transition()
             .duration(animDuration / 2)
             .ease("linear")
             .attr("opacity", 1)
             .style("stroke-width", 2);

            // Create a group for the popup objects
            popup = chart_svg.append("g");

            var textLength = measureText(e.seriesValue[1], 10, "font-family: sans-serif");

            // Add a rectangle surrounding the text
            popup
                .append("rect")
                .attr("id", "tooltip")
                .attr("x", rx+18)
                .attr("y", ry-10)
                .attr("width", textLength.width + 10)
                .attr("height", height)
                .attr("rx", 5)
                .attr("ry", 5);

            // Add multiple lines of text
            popup
                .append('text')
                .append('tspan')
                .attr('x', rx + 23)
                .attr('y', ry + 8)
                .text('CCG Code: ' + e.seriesValue[0])
                .style("font-family", "sans-serif")
                .style("font-size", 10)
                .append('tspan')
                .attr('x', rx + 23)
                .attr('y', ry + 20)
                .text('Spend: £' + numeral(e.seriesValue[1]).format('0,0.00'))
                .style("font-family", "sans-serif")
                .style("font-size", 10)
        }

        // Event to handle mouse exit
        function onLeave(e) {
            // Remove the popup
            if (popup !== null) {
                popup.remove();
            }
            // Remove the drop line and ring around circle
            if (myChart._tooltipGroup !== null && myChart._tooltipGroup !== undefined) {
                myChart._tooltipGroup.remove();
            }
        };
    });

    return chart_svg;
}
