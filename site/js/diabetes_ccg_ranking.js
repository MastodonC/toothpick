var diabetes_ccg_ranking_top_10 = function diabetes_ccg_ranking_top_10(div) {

    /*  Dimple bubble chart */
    var svg = dimple.newSvg(div, 960, 400);
    var dropLine, popup, popupLabels, popupValues, popupWidth;

    d3.csv("./data/ranking_ccg_bottom10.csv", function (data) {

        var myChart = new dimple.chart(svg, data);

        myChart.setBounds(60, 30, 600, 300);
        var x = myChart.addCategoryAxis("x", "CCG Code");
        x.addOrderRule("Prevalence", false); // (ordering measure, descending?)
        myChart.addMeasureAxis("y", "Prevalence");
        myChart.addMeasureAxis("z", "Registered");
        var s = myChart.addSeries(["CCG Code", "CCG Name", "Registered", "Prevalence"], dimple.plot.bubble);
        popupLabels = new Array("CCG Code", "CCG Name", "Registered patients", "Prevalence");

        // Handle the hover event - overriding the default behaviour
        s.addEventHandler("mouseover", onHover);
        // Handle the leave event - overriding the default behaviour
        s.addEventHandler("mouseleave", onLeave);

        myChart.draw();

        // Event to handle mouse enter
        function onHover(e) {

            // Get the properties of the selected shape
            var cx = parseFloat(e.selectedShape.attr("cx")),
                cy = parseFloat(e.selectedShape.attr("cy")),
                r = parseFloat(e.selectedShape.attr("r"));
            // Set the size and position of the popup
            var width = 150,
                height = 70,
                x = (cx + r + width + 10 < svg.attr("width") ?
                    cx + r + 10 :
                    cx - r - width - 20),
                y = (cy - height / 2 < 0 ?
                15 :
                cy - height / 2);

            dropLine = addDropLineCircle(myChart, cx, cy, r);
            popupValues = new Array(e.seriesValue[0], e.seriesValue[1], numeral(e.seriesValue[2]).format('0,0'), (numeral(e.seriesValue[3]).format('0,0.0')+'%'));
            var textLength = measureText(e.seriesValue[1], 10, "font-family: sans-serif");
            popupWidth = textLength.width + 75;
            popup = addPopup(svg, width, height, x-30, y, popupLabels, popupValues, popupWidth);
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
}

var diabetes_ccg_ranking_bottom_10 = function diabetes_ccg_ranking_bottom_10(div) {

    /*  Dimple bubble chart */
    var svg = dimple.newSvg(div, 960, 500);
    var dropLine, popup, popupLabels, popupValues, popupWidth;

    d3.csv("./data/ranking_ccg_top10.csv", function (data) {

        var myChart = new dimple.chart(svg, data);
        myChart.setBounds(60, 30, 600, 300);

        var x = myChart.addCategoryAxis("x", "CCG Code");
        x.addOrderRule("Prevalence", true);
        myChart.addMeasureAxis("y", "Prevalence");
        myChart.addMeasureAxis("z", "Registered");
        var s = myChart.addSeries(["CCG Code", "CCG Name", "Registered", "Prevalence"], dimple.plot.bubble);
        // Color of bubbles is set by changing 'circle' value in css
        popupLabels = new Array("CCG Code", "CCG Name", "Registered patients", "Prevalence");

        // Handle the hover event - overriding the default behaviour
        s.addEventHandler("mouseover", onHover);
        // Handle the leave event - overriding the default behaviour
        s.addEventHandler("mouseleave", onLeave);

        myChart.draw();

        // Event to handle mouse enter
        function onHover(e) {

            // Get the properties of the selected shape
            var cx = parseFloat(e.selectedShape.attr("cx")),
                cy = parseFloat(e.selectedShape.attr("cy")),
                r = parseFloat(e.selectedShape.attr("r"));
            // Set the size and position of the popup
            var width = 150,
                height = 70,
                x = (cx + r + width + 10 < svg.attr("width") ?
                    cx + r + 10 :
                    cx - r - width - 20),
                y = (cy - height / 2 < 0 ?
                    15 :
                    cy - height / 2);

            dropLine = addDropLineCircle(myChart, cx, cy, r);
            popupValues = new Array(e.seriesValue[0], e.seriesValue[1], numeral(e.seriesValue[2]).format('0,0'), (numeral(e.seriesValue[3]).format('0,0.0')+'%'));
            var textLength = measureText(e.seriesValue[1], 10, "font-family: sans-serif");
            popupWidth = textLength.width + 75;
            popup = addPopup(svg, width, height, x-30, y, popupLabels, popupValues, popupWidth);
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

}
