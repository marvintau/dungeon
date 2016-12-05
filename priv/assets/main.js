
var rand_choice = function(array){
    return array[Math.floor(Math.random() * array.length)];
}

var perm_slice = function(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;

    while (0 !== currentIndex) {

        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array.slice(0, 5);
}

var pad_with_null = function(array) {
    var pad = Array.apply(null, Array(5 - array.length)).map(String.prototype.valueOf,"none");
    return array.concat(pad);
}

$.setData = function () {
    $('#id1').val("Maxim");
    $('#hp1').val(3400);
    $('#prim_type1').val("physical");
    $('#prim_max_1').val("235");
    $('#prim_min_1').val("190");

    $('#secd_type1').val("shield");
    $('#secd_max_1').val("0");
    $('#secd_min_1').val("0");

    $('#armor1').val("5400");
    $('#hit1').val("15");
    $('#critical1').val("20");
    $('#dodge1').val("20");
    $('#resist1').val("35");
    $('#block1').val("35");
    $('#agi1').val("50");
    $('#talented_skill1').val("blade_dance");

    ms1.clear();
    ms1.setValue(perm_slice(ms1.getData()));

    $('#id2').val("Scarlett");
    $('#hp2').val("2700");
    $('#prim_type2').val("physical");
    $('#prim_max_2').val("205");
    $('#prim_min_2').val("190");

    $('#secd_type2').val("physical");
    $('#secd_max_2').val("190");
    $('#secd_min_2').val("175");

    $('#armor2').val("4500");
    $('#hit2').val("35");
    $('#critical2').val("30");
    $('#dodge2').val("30");
    $('#resist2').val("35");
    $('#block2').val("0");
    $('#agi2').val("75");
    $('#talented_skill2').val("blade_dance");

    ms2.clear();
    ms2.setValue(perm_slice(ms2.getData()));
}


$.getData = function () {
    return {
        player1 : {
            id : $('#id1').val(),
            hp : parseInt($('#hp1').val()),
            prim_type : $('#prim_type1').val(),
            prim_max : parseInt($('#prim_max_1').val()),
            prim_min : parseInt($('#prim_min_1').val()),

            secd_type : $('#secd_type1').val(),
            secd_max : parseInt($('#secd_max_1').val()),
            secd_min : parseInt($('#secd_min_1').val()),

            armor : parseInt($('#armor1').val()),
            hit : parseInt($('#hit1').val()),
            critic : parseInt($('#critical1').val()),
            dodge : parseInt($('#dodge1').val()),
            resist : parseInt($('#resist1').val()),
            block : parseInt($('#block1').val()),
            agility : parseInt($('#agi1').val()),

            talented : $('#talented_skill1').val(),

            cast_list : pad_with_null(ms1.getValue())

        },

        player2 : {
            id : $('#id2').val(),
            hp : parseInt($('#hp2').val()),
            prim_type : $('#prim_type2').val(),
            prim_max : parseInt($('#prim_max_2').val()),
            prim_min : parseInt($('#prim_min_2').val()),

            secd_type : $('#secd_type2').val(),
            secd_max : parseInt($('#secd_max_2').val()),
            secd_min : parseInt($('#secd_min_2').val()),

            armor : parseInt($('#armor2').val()),
            hit : parseInt($('#hit2').val()),
            critic : parseInt($('#critical2').val()),
            dodge : parseInt($('#dodge2').val()),
            resist : parseInt($('#resist2').val()),
            block : parseInt($('#block2').val()),
            agility : parseInt($('#agi2').val()),

            talented_skill2 : $('#talented_skill2').val(),

            cast_list : pad_with_null(ms2.getValue())
        }
    }    
}

$.makeTable = function (mydata) {
    var table = $('<table id="table" class="table table-striped">');
    var tblHeader = "<thead><tr>";
    for (var k in mydata[0]) tblHeader += "<th>" + k + "</th>";
    tblHeader += "</tr></thead>";
    $(tblHeader).appendTo(table);

    var tblBody = $('<tbody></tbody>').appendTo(table);

    $.each(mydata, function (index, value) {
        var TableRow = "<tr>";
        $.each(value, function (key, val) {
            TableRow += "<td>" + val + "</td>";
        });
        TableRow += "</tr>";
        $(tblBody).append(TableRow);
    });
    return ($(table));
};

$.postJSON = function(url, data, callback) {
    return jQuery.ajax({
        headers: { 
            'Accept': 'application/json',
            'Content-Type': 'application/json' 
        },
        'type': 'POST',
        'url': url,
        'data': JSON.stringify(data),
        'dataType': 'json',
        'success': callback
    });
};



$.make_graph = function(full_log){

    var seq = full_log.map(function(record){return record["seq"]}),
        a = full_log.map(function(record){return record["a"]}),
        b = full_log.map(function(record){return record["b"]});

    var a_trace = {x:seq, y:a, type:"scatter", mode:"line", nticks:30, line:{width:1, color:"#862F39"}, fill:"tozeroy"},
        b_trace = {x:seq, y:b, type:"scatter", mode:"line", nticks:30, line:{width:1, color:"#E3D4BF"}, fill:"tozeroy"};

        var layout = {
          xaxis: {
            autotick: false,
            ticks: 'outside',
            tick0: 0,
            dtick: 1,
            ticklen: 8,
            tickcolor: '#000',
            range: [1, 20]
          },
          yaxis: {
            autotick: false,
            ticks: 'outside',
            tick0: 0,
            dtick: 1000,
            ticklen: 8,

            tickcolor: '#000',
            range: [0, 5000]
          }
        };

    Plotly.newPlot('chart-section', [a_trace, b_trace], layout);
}

$("#save-1").on('click', function(){
    var OutgoingData = $.getData().player1;

    $.postJSON("/save_player", OutgoingData, function(data){
        console.log("save 1 done");
    })
});


$("#submit-20").on('click', function(){

    var OutgoingData = $.getData()

    result = {};

    for(var i = 0; i < 1000; i++){
        $.postJSON("/get_result", OutgoingData, function(data){

            // var IncomingData = data.full_log;

            $('#table-section').empty();

            result[data.res] = (result[data.res]||0) + 1;
            
            $("<span>"+JSON.stringify(result)+"</span>").appendTo("#table-section");

        }, "json").fail(function() {
            console.log( "error" );
        }); 

    }
});


$("#submit").on('click', function(){

    OutgoingData = $.getData()

    $.postJSON("/get_result", OutgoingData, function(data){

        $('#table-section').empty();

        var table = $.makeTable(data.proc, OutgoingData.player1.id, OutgoingData.player2.id);
        $(table).appendTo("#table-section");

        $.make_graph(data.full_log);  

        var res ="<div class=\"cap\"><br>Win:" + data.res + "</b>";
        $(res).appendTo("#table");


        data.player1 = OutgoingData.player1;
        data.player2 = OutgoingData.player2;
        delete data.full_log;
        console.log(JSON.stringify(data));


    }, "json").fail(function() {
        console.log( "error" );
    }); 
});

$("#reset").on('click', function(){
    $.setData();
});

$("#class1").ready(function(){
    $.postJSON("/get_list", {id: $('#id1').val(), class:$('#class1').val()}, function(data){
        console.log(data);
        ms1 = $('#cast-list-1').magicSuggest({data:data, maxSelection:50, maxSuggestion:5, allowFreeEntries:false});
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class1").change(function(){

    $.postJSON("/get_list", {id: $('#id1').val(), class:$('#class1').val()}, function(data){
        ms1.clear(); 
        ms1.setData(data);
        console.log(JSON.stringify(ms1.getData()));
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class2").ready(function(){
    $.postJSON("/get_list", {id: $('#id2').val(), class:$('#class2').val()}, function(data){
        ms2 = $('#cast-list-2').magicSuggest({data:data, maxSuggestion:5, allowFreeEntries:false});
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class2").change(function(){
    $.postJSON("/get_list", {id: $('#id2').val(), class:$('#class2').val()}, function(data){
        ms2.clear();
        ms2.setData(data);

    }, "json").fail(function(){
        console.log("error");
    });
});


