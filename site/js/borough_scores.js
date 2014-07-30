var borough_scores_map = function borough_scores_map(div) {

    var map = L.map(div).setView([53.0, -1.5], 6);
    var color = function getColor(d) {
        return d > 115  ? '#1A9850':
            d > 65   ? '#91CF60' :
            d > 15   ? '#D9EF8B' :
            d > -35  ? '#FEE08B' :
            d > -85  ? '#D73027' :
            d > -135 ? '#C62016' :
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
        };
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
            fillColor: "#ff0000",
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
            attribution: 'Map data &copy; 2011 OpenStreetMap contributors, Imagery.'
        }).addTo(map);

    featureLayer(map, "data/borough_boundaries_topo.json", defaultStyle, "boundaries.geo");

    mergedFeatureLayer(map, "data/borough_scores.csv", "data/borough_boundaries_topo.json", "LA_code", style, onEachFeature, pointToLayer, "boundaries.geo");

    addLegend([-134, -84, -34, 16, 66, 116, 166], map, color);

    addInfo(map, function (props) {
        var infoBox = '<h3>' + props.LA_name + ' Statistics</h3><br/>' +
            'Borough Code: ' + props.LA_code + '<br />' +
            'Cycling Weekly: ' + numeral(props.cycling_weekly).format('0,0.00') + '<br />' +
            'Cycling Rank: ' + props.cycling_rank + '<br />' +
            'Walking Thriceweekly: ' + props.walking_thriceweekly + '<br />' +
            'Walking Rank: ' + props.walking_rank + '<br />' +
            'Weekly Greenspace Vists: ' + numeral(props.weekly_greenspace_visits).format('0,0.00') + '<br />' +
            'Greenspace Rank: ' + props.greenspace_rank + '<br />' +
            'Hospital Experience Score: ' + props.hospital_experience_score + '<br />' +
            'Hospital Rank: ' + props.hospital_rank + '<br />' +
            'Can See GP: ' + numeral(100*props.pct_canseegp).format('0,') + '%<br />' +
            'Dentists per 1000: ' + numeral(props.dentists_per_thousand).format('0,0.00') + '<br />' +
            'Dentists Rank: ' + props.dentists_rank + '<br />' +
            'Overall Rank: ' + numeral(props.overall_rank).format('0,0.00') + '<br />';
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
};
