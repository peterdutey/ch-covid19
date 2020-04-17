
Highcharts.getJSON('https://ons-inspire.esriuk.com/arcgis/rest/services/Eurostat_Boundaries/NUTS_Level_1_January_2018_Boundaries/MapServer/3/query?where=1%3D1&outFields=*&outSR=3857&f=geojson', function (geojson) {
    chart = Highcharts.mapChart('covid-map', {
        chart: {
            map: geojson
        },
        title: {
            text: 'GeoJSON in Highmaps'
        },

        mapNavigation: {
            enabled: true,
            buttonOptions: {
                verticalAlign: 'bottom'
            }
        },
        colorAxis: {
            tickPixelInterval: 100
        },
        data: {
            csvURL:  window.location.origin + '/assets/data/testdata.csv',
            enablePolling: true,
            seriesMapping: [{
                nuts118cd: 0,
                value: 1
            }]
        },
        series: [{
            name: 'Random data',
            joinBy: 'nuts118cd',
            borderColor: '#606060',
            nullColor: '#bfbfbf',
            dataLabels: {
                enabled: true,
                format: '{point.properties.nuts118nm}'
            }
        }]
    });
});
