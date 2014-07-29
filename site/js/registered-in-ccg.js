var registered_in_ccg = function registered_in_ccg(div) {

    var map = L.map(div).setView([53.0, -1.5], 6);
    var color = function getColor(d) {
        return  d == 'NA' ? '#333' :
            d == 'undefined' ? '#333' :
            d > 850000 ? '#0C2C84' :
            d > 750000 ? '#225EA8' :
            d > 500000 ? '#1D91C0' :
            d > 250000 ? '#41B6C4' :
            d > 100000 ? '#7FCDBB' :
            d > 50000 ? '#C7E9B4' :
            '#FFFFCC';
    };
    var style = function style(feature) {
        return {
            fillColor: color(feature.properties.ccg_population),
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

    mergedFeatureLayer(map, "data/registered_in_ccg.csv", "data/ccg-boundaries.json", "ccg_code", style, onEachFeature, pointToLayer, "ccg_boundaries");

    addLegend([0, 50000, 100000, 250000, 500000, 750000, 850000, 1000000], map, color);

    addInfo(map, function (props) {
        var infoBox = '<h3> CCG Population </h3><br/>' +
            'CCG code: ' + props.ccg_code + '<br/>'
            + 'CCG population: ' + props.ccg_population;
        return infoBox;
    });

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
    }
}
