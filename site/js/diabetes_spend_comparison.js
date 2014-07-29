var spend_comparison = function spend_comparison(chart_div, ccg_code) {

    var myChart, popup, popupLabels, popupValues, dropLine, popupWidth;
    var chart_svg = dimple.newSvg(chart_div, 600, 500);

    d3.csv("/data/spend_difference.csv", function (data) {

        data = dimple.filterData(data, "ccg_code", [ccg_code])

        myChart = new dimple.chart(chart_svg, data);
        myChart.setBounds(60, 30, 350, 250);

        var x = myChart.addCategoryAxis("x", "month");
        x.addOrderRule(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]);

        var y = myChart.addMeasureAxis("y", "difference");
        y.overrideMax = 6;
        y.overrideMin = -6;

        var s = myChart.addSeries(["ccg_code", "difference"], dimple.plot.bar);
        popupLabels = new Array("CCG Code", "Difference");

        // Handle the hover event - overriding the default behaviour
        s.addEventHandler("mouseover", onHover);
        // Handle the leave event - overriding the default behaviour
        s.addEventHandler("mouseleave", onLeave);
        //myChart.addLegend(60, 10, 510, 20, "right");
        myChart.draw();
        x.titleShape.text ("Month");
        x.titleShape.attr("y", myChart.height + 60);
        y.titleShape.text("Difference in spend per capita (£)")

        // Event to handle mouse enter
        function onHover(e) {

            // Get the properties of the selected shape
            var x = parseFloat(e.selectedShape.attr("x")),
                y = parseFloat(e.selectedShape.attr("y")),
                h = parseFloat(e.selectedShape.attr("height"));
            // Set the size and position of the popup
            var width = 100,
                height = 40;

            dropLine = addDropLineRect(myChart, e, x, y, h+1);
            popupValues = new Array(e.seriesValue[0], ('£'+numeral(e.seriesValue[1]).format('0,0.00')));
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
