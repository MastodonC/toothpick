var diabetes_gp_prevalence_map = function diabetes_gp_prevalence_map(div) {

    var color = function getColor(d) {
        return d > 9 ? '#0C2C84' :
            d > 7 ? '#225EA8' :
                d > 5 ? '#1D91C0' :
                    d > 3 ? '#41B6C4' :
                        d > 1 ? '#7FCDBB' :
                            d > 0 ? '#C7E9B4' :
                                '#FFFFCC';
    };
    var style = function style(feature) {
        return {
            fillColor: color(feature.properties.ccg_prevalence),
            weight: 2,
            opacity: 1,
            color: 'white',
            dashArray: '3',
            fillOpacity: 0.7
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
    var cloudmadeUrl = 'http://{s}.tile.cloudmade.com/{key}/22677/256/{z}/{x}/{y}.png',
        cloudmadeAttribution = 'Map data &copy; 2011 OpenStreetMap contributors, Imagery &copy; 2012 CloudMade',
        cloudmade = L.tileLayer(cloudmadeUrl, {maxZoom: 17, attribution: cloudmadeAttribution, key: 'BC9A493B41014CAABB98F0471D759707'}),
        latlng = L.latLng(53.0, -1.5);

    var map = L.map(div, {center: latlng, zoom: 6, layers: [cloudmade]});

    mergedFeatureLayer(map, "data/gp_ccg_prevalence.csv", "data/ccg-boundaries.json", "ccg_code", style, onEachFeature, pointToLayer, "ccg_boundaries");

    mergedClusteredMarkers(map, "data/gp_ccg_prevalence.csv", "data/gp_topo.json", "practice_code", style, onEachFeature, pointToLayer, "gp_geojson", "gp_prevalence", addPopup, getCustomIcon);

    addLegend([0, 1, 3, 5, 7, 9], map, color);

    addInfo(map, function (props) {
        var infoBox = '<h3> CCG Diabetes Prevalence </h3><br/>' +
            'CCG Name: ' + props.ccg_name + '<br />' +
            'CCG Code: ' + props.ccg_code + '<br />' +
            'Registered Patients: ' + numeral(props.ccg_registered_patients).format('0,0') + '<br />' +
            'Prevalence: ' + Math.round(props.ccg_prevalence * 10) / 10 + '%<br />';
        return infoBox;
    });

    function getCustomIcon(prevalence) {
        var iconA = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'iconA'
        });
        var iconB = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'iconB'
        });
        var iconC = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'iconC'
        });
        var iconD = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'iconD'
        });
        var iconE = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'iconE'
        });
        var iconF = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'iconF'
        });
        var defaultIcon = L.divIcon({
            iconSize: new L.Point(10, 10),
            className: 'myDefaultIcon'
        })

        return  prevalence > 25 ? iconF :
            prevalence > 20 ? iconE :
                prevalence > 15 ? iconD :
                    prevalence > 10 ? iconC :
                        prevalence > 5 ? iconB :
                            prevalence > 0 ? iconA :
                                defaultIcon;
    };

    function highlightFeature(e) {
        var layer = e.target;

        layer.setStyle({
            weight: 5,
            color: '#666',
            dashArray: '',
            fillOpacity: 0.7
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
        console.log(e.target._map._zoom);
    }

    function addPopup(props) {
        var popup = '<b>Practice name: </b>' + props.practice_name + '<br />' +
            '<b>Practice code: </b>' + props.practice_code + '<br />' +
            '<b>Prevalence: </b>' + props.gp_prevalence + '%<br />' +
            '<b>Patients registered: </b>' + numeral(props.gp_registered_patients).format('0,0') + '<br />' +
            '<b>Diabetes patients: </b>' + numeral(props.gp_diabetes_patients).format('0,0') + '<br /><br />' +
            'Diabetes patients in this practice constitute ' + Math.round(props.gp_ccg_ratio * 10) / 10 + '% of diabetes patients in ' + props.ccg_code + ' CCG.';
        return popup;
    };


}
