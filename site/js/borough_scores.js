var borough_scores_map = function borough_scores_map(div) {

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
            fillColor: color(feature.properties.overall_rank),
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
            fillColor: "#f00",
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

    L.tileLayer('http://a.tile.openstreetmap.org/{z}/{x}/{y}.png',
        {
            attribution: 'Map data &copy; 2011 OpenStreetMap contributors, Imagery &copy; 2012 CloudMade'
        }).addTo(map);

    featureLayer(map, "data/borough_geojson.json", defaultStyle, "pctboundaries");

    // mergedFeatureLayer(map, "data/gp_ccg_prevalence.csv", "data/gp_topo.json", "practice_code", style, onEachFeature, pointToLayer, "gp_geojson");

    addLegend([0, 5, 10, 15, 20, 25], map, color);

    addInfo(map, function (props) {
        var infoBox = '<h3> Borough Statistics </h3><br/>' +
            'Borough Name: ' + props.la_name + '<br />' +
            'Borough Code: ' + props.la_code + '<br />' +
            'Cycling Weekly: ' + props.cycling_weekly + '<br />' +
            'Cycling Rank: ' + props.cycling_rank + '<br />' +
            'Walking Thriceweekly: ' + props.walking_thriceweekly + '<br />' +
            'Walking Rank: ' + props.walking_rank + '<br />' +
            'Weekly Greenspace Vists: ' + props.weekly_greenspace_visits + '<br />' +
            'Greenspace Rank: ' + props.greenspace_rank + '<br />' +
            'Hospital Experience Score: ' + props.hospital_experience_score + '<br />' +
            'Hospital Rank: ' + props.hospital_rank + '<br />' +
            'Percentage Can See GP: ' + props.pct_canseegp + '<br />' +
            'Dentists per 1000: ' + props.dentists_per_thousand + '<br />' +
            'Dentists Rank: ' + props.dentists_rank + '<br />' +
            'Overall Rank: ' + props.overall_rank + '<br />';
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
