var total_spend = function total_spend(chart_div) {

    var myChart, popup, popupLabels, popupValues, dropLine, popupWidth;
    var chart_svg = dimple.newSvg(chart_div, 600, 300);

    d3.csv("/data/diabetes_total_spend2013.csv", function (data) {

        myChart = new dimple.chart(chart_svg, data);
        myChart.setBounds(60, 30, 350, 250);
        var x = myChart.addCategoryAxis("x", "month");
        x.addOrderRule(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]);
        x.lineMarkers = true;
        var y = myChart.addMeasureAxis("y", "total_spend");
        y.overrideMax = 6.1380000609999835E7;
        y.overrideMin = 0;
        var s = myChart.addSeries(["month", "total_spend"], dimple.plot.bar);

        popupLabels = new Array("Month", "Total spend");

        // Handle the hover event - overriding the default behaviour
        s.addEventHandler("mouseover", onHover);
        // Handle the leave event - overriding the default behaviour
        s.addEventHandler("mouseleave", onLeave);

        myChart.draw();
        // Override x-axis
        x.titleShape.text ("Month");

        // Override y-axis title
        y.titleShape.text ("Total spend (£)");
        // Override x-axis tick labels

        // Event to handle mouse enter
        function onHover(e) {
            // Get the properties of the selected shape
            var x = parseFloat(e.selectedShape.attr("x")),
                y = parseFloat(e.selectedShape.attr("y"));
            // Set the size and position of the popup
            var width = 100,
                height = 40;

            dropLine = addDropLineRect(myChart, e, x, y);
            popupValues = new Array(e.seriesValue[0], numeral(e.seriesValue[1]).format('0,0.00'));
            var textLength = measureText(e.seriesValue[1], 10, "font-family: sans-serif");
            popupWidth = textLength.width + 30;
            popup = addPopup(chart_svg, width, height, x, y, popupLabels, popupValues, popupWidth);
        }

        // Event to handle mouse exit
        function onLeave(e) {
            // Remove the popup
            if (popup !== null) {
                popup.remove();
            }
            // Remove the drop line and ring around circle
            if (dropLine !== null && dropLine !== undefined) {
                dropLine.remove();
            }
        };
    });

    return chart_svg;
}
