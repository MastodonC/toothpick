var diabetes_per_head_per_ccg_per_month = function diabetes_per_head_per_ccg_per_month(div, chart_div, chart_div2) {

    var map = L.map(div).setView([53.0, -1.5], 6);
    var color = function getColor(d) {
        return  d == 'NA' ? '#333' :
            d == 'undefined' ? '#333' :
            d > 26 ? '#0C2C84' :
            d > 24 ? '#225EA8' :
            d > 22 ? '#1D91C0' :
            d > 20 ? '#41B6C4' :
            d > 18 ? '#7FCDBB' :
            d > 16 ? '#C7E9B4' :
            '#FFFFCC';
    };
    var style = function style(feature) {
        return {
            fillColor: color(feature.properties.per_capita_spend),
            weight: 2,
            opacity: 0.5,
            color: 'white',
            dashArray: '3',
            fillOpacity: 0.8
        }
    };
    var defaultStyle = function defaultstyle(feature) {
        return {
            outlineColor: "#000000",
            outlineWidth: 0.5,
            weight: 1,
            opacity: 1,
            fillOpacity: 0
        };
    };
    var pointToLayer = function pointToLayer(feature, latlng) {
        return L.circleMarker(latlng, {
            radius: 8,
            fillColor: "#ff7800",
            color: "#000",
            weight: 1,
            opacity: 1,
            fillOpacity: 0.8
        });
    };
    var onEachFeature = function onEachFeature(feature, layer) {
        layer.on({
            mouseover: highlightFeature,
            mouseout: resetHighlight,
            click: zoomToFeature,
            pointToLayer: pointToLayer
        });
    };

    L.tileLayer('http://{s}.tile.cloudmade.com/{key}/22677/256/{z}/{x}/{y}.png',
        {
            attribution: 'Map data &copy; 2011 OpenStreetMap contributors, Imagery &copy; 2012 CloudMade',
            key: 'BC9A493B41014CAABB98F0471D759707'
        }).addTo(map);

    mergedFeatureLayer(map, "data/diabetes_per_head_per_ccg_per_month.csv", "data/ccg-boundaries.json", "ccg_code", style, onEachFeature, pointToLayer, "ccg_boundaries");

    addLegend([0, 16, 18, 20, 22, 24, 26, 28], map, color);

    addInfo(map, function (props) {
        var infoBox = '<h3>Diabetes Spend per CCG</h3><br/>' 
            + 'CCG name: ' + props.ccg_name + '<br/>'
            + 'CCG code: ' + props.ccg_code + '<br/>'
            + 'Registered Patients: ' + numeral(props.registered_patients).format('0,0') + '<br/>'
            + 'Diabetes Patients: ' + numeral(props.diabetes_patients).format('0,0') + '<br/>'
            + 'Total Spend: £' + numeral(props.total_spend).format('0,0.00') + '<br/>'
            + 'Spend per diabetes patient: £' + numeral(props.per_capita_spend).format('0,0.00');
        return infoBox;
    });

    // Default charts
    chart_month_by_month = spend_month_by_month(chart_div, "10Y");
    chart_spend_comparison = spend_comparison(chart_div2, "10Y");


    function highlightFeature(e) {
        var layer = e.target;

        layer.setStyle({
            weight: 5,
            color: '#666',
            dashArray: '',
            fillOpacity: 0.6
        });

        if (!L.Browser.ie && !L.Browser.opera) {
            layer.bringToFront();
        }
        map.info.update(layer.feature.properties);
    }

    function resetHighlight(e) {
        var layer = e.target;
        layer.setStyle(style(e.target.feature));
        map.info.update();
    }

    function zoomToFeature(e) {
        map.fitBounds(e.target.getBounds());

        if (chart_month_by_month) { chart_month_by_month.remove() };

        var ccg_code = e.target.feature.properties.ccg_code;
        // Create a time series chart of spend month by month
        chart_month_by_month = spend_month_by_month(chart_div, ccg_code);

        if (chart_spend_comparison) { chart_spend_comparison.remove() };
        chart_spend_comparison = spend_comparison(chart_div2, ccg_code);
    }
}
