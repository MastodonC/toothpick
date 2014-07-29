/* Drop line that goes from the bar to x- and y-axis. Shown on hover. */
function addDropLineRect(myChart, e, rx, ry, h) {

    var dropLine = myChart._tooltipGroup = myChart.svg.append("g");
    var dropDest = myChart.series[0]._dropLineOrigin(),
        animDuration = 750;

    // Add a drop line to the y axis
    if (dropDest.y !== null) {
        dropLine.append("line")
            .attr("id", "dropRect")
            .attr("x1", (rx < dropDest.x ? rx : rx))
            .attr("y1", (ry < myChart.series[0].y._origin ? ry : ry + h - 1))
            .attr("x2", (rx < dropDest.x ? rx : rx))
            .attr("y2", (ry < myChart.series[0].y._origin ? ry : ry + h - 1))
            .transition()
            .delay(animDuration / 2)
            .duration(animDuration / 2)
            .ease("linear")
            .attr("x2", dropDest.x);
    }

    // Highlight the data point
    dropLine.append("rect")
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

    return dropLine;
}

/* Drop line that goes from the circle to x- and y-axis. Shown on hover. */
function addDropLineCircle(myChart, cx, cy, r) {

    var dropLine = myChart._tooltipGroup = myChart.svg.append("g");

    var dropDest = myChart.series[0]._dropLineOrigin(),
        animDuration = 750;

    // Add a ring around the data point
    dropLine.append("circle")
        .attr("id", "ring")
        .attr("cx", cx)
        .attr("cy", cy)
        .attr("r", r)
        .attr("opacity", 0)
        .transition()
        .duration(animDuration / 2)
        .ease("linear")
        .attr("opacity", 1)
        .attr("r", r + 4)
        .style("stroke-width", 2);

    // Add a drop line to the x axis
    if (dropDest.x !== null) {
        dropLine.append("line")
            .attr("id", "drop")
            .attr("x1", (cx < dropDest.x ? cx + r + 4 : cx - r - 4))
            .attr("y1", cy)
            .attr("x2", (cx < dropDest.x ? cx + r + 4 : cx - r - 4))
            .attr("y2", cy)
            .transition()
            .delay(animDuration / 2)
            .duration(animDuration / 2)
            .ease("linear")
            .attr("x2", dropDest.x);
    }

    // Add a drop line to the y axis
    if (dropDest.y !== null) {
        dropLine.append("line")
            .attr("id", "drop")
            .attr("x1", cx)
            .attr("y1", (cy < dropDest.y ? cy + r + 4 : cy - r - 4))
            .attr("x2", cx)
            .attr("y2", (cy < dropDest.y ? cy + r + 4 : cy - r - 4))
            .transition()
            .delay(animDuration / 2)
            .duration(animDuration / 2)
            .ease("linear")
            .attr("y2", dropDest.y);
    }
    return dropLine;
}

function addPopup(chart_svg, width, height, x, y, labels, values, popupWidth) {

    var popup = chart_svg.append("g");

    // Add a rectangle surrounding the text
    popup
        .append("rect")
        .attr("id", "tooltip")
        .attr("x", x + 50)
        .attr("y", y - 10)
        .attr("width", popupWidth)
        .attr("height", height)
        .attr("rx", 5)
        .attr("ry", 5);

    var offset = 9; // Each line of text needs to be moved down
    for (var i = 0; i < labels.length; i++) {
        popup
            .append('text')
            .append('tspan')
            .attr('x', x + 55)
            .attr('y', y + offset)
            .text('' + labels[i] + ': ' + values[i])
            .style("font-family", "sans-serif")
            .style("font-size", 10);
        offset += 12;
    }
    return popup;
}