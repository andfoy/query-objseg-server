$('#flight_table').DataTable({
        dom: 'Bfrtip',
        stateSave: true,
        buttons: [
            'colvis', {
                text: 'Buscar por nombre de cliente',
                action: function(e, dt, node, config) {
                    $(this).attr("test_attr", "a");
                    state_o = "1";
                    // console.log(state);
                    $("#paramF").text('Buscando por: Nombre de cliente');
                    // alert($(this).attr("test_attr")) 
                },
                className: 'info'
            }, {
                text: 'Buscar por Tipo de cuenta',
                action: function(e, dt, node, config) {
                    $(this).attr("test_attr", "a");
                    state_o = "2";
                    // console.log(state);
                    $("#paramF").text('Buscando por: Tipo de cuenta');
                    // alert($(this).attr("test_attr")) 
                },
                className: 'info'
            }
        ],
        "processing": true,
        "serverSide": true,
        "ajax": {
            "url": "/api/flights",
            "type": "POST",
            "contentType": "application/json",
            "dataType": 'json',
            "data": function(d) {
                d.from = 'New';
                d.to = '';
                d.day = '';
                return JSON.stringify(d)
                // d.creacionStart = $("#fromD").val(),
                // d.creacionStop = $("#toD").val(),
                // d.uMovStart = $("#fromMD").val(),
                // d.uMovStop = $("#toMD").val(),
                // d.saldoFrom = $("#sumFrom").val(),
                // d.saldoTo = $("#sumTo").val()
                // d.custom = $('#myInput').val();
                // etc
            }
        },
        "columns": [{"data":"airline"},
            {"data":"carrier"},
            {"data":"flight_number"},
            {"data":"departure_city"},
            {"data":"departure_airport_name"},
            {"data":"departure_airport_code"},
            {"data":"arrival_city"},
            {"data":"arrival_airport_name"},
            {"data":"arrival_airport_code"},
            {"data":"departure_day"},
            {"data":"departure_time"},
            {"data":"arrival_time"},
            {"data":"total_price"}]
});