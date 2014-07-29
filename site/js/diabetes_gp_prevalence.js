var diabetes_prevalence_map = function diabetes_prevalence_map(div) {

    var map = L.map(div).setView([53.0, -1.5], 6);
    var color = function getColor(d) {
        return  d > 25 ? '#D73027' :
                    d > 20 ? '#FC8D59' :
                        d > 15 ? '#FEE08B' :
                            d > 10 ? '#D9EF8B' :
                                d > 5 ? '#91CF60' :
                                    d > 0 ? '#1A9850' :
                                        '#BABABA';
    };
    var style = function style(feature) {
        return {
            fillColor: color(feature.properties.gp_prevalence),
            weight: 1,
            opacity: 0,
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

    featureLayer(map, "data/pct.json", defaultStyle, "pctboundaries");

    mergedFeatureLayer(map, "data/gp_ccg_prevalence.csv", "data/gp_topo.json", "practice_code", style, onEachFeature, pointToLayer, "gp_geojson");

    addLegend([0, 5, 10, 15, 20, 25], map, color);

    addInfo(map, function (props) {
        var infoBox = '<h3> GP Practice Scores </h3><br/>' +
            'Practice name: ' + props.practice_name + '<br />' +
            'Practice code: ' + props.practice_code + '<br />' +
            'Prevalence: ' + props.gp_prevalence + '%<br />' +
            'Patients registered: ' + numeral(props.gp_registered_patients).format('0,0.0') + '<br />' +
            'Diabetes patients: ' + numeral(props.gp_diabetes_patients).format('0,0.0') +'<br />' +
            'CCG Code: ' + props.ccg_code + '<br />' +
            'CCG Prevalence: '+ Math.round(props.ccg_prevalence * 10) / 10  + '%<br />' +
            'Diabetes patients in this practice constitute ' + Math.round(props.gp_ccg_ratio * 10) / 10 +'% of diabetes patients in ' + props.ccg_code + ' CCG.';
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
        e.target._map.info.update(layer.feature.properties);
    }

    function resetHighlight(e) {
        var layer = e.target;
        layer.setStyle(style(e.target.feature));
        e.target._map.info.update();
    }

    function zoomToFeature(e) {
        e.target._map.fitBounds(e.target.getBounds());
    }
}
