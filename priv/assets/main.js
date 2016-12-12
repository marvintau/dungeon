
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
    $('#id1').val("Scarlett");
    $('#hp1').val("2700");
    $('#prim_type1').val("physical");
    $('#prim_max_1').val("205");
    $('#prim_min_1').val("190");

    $('#secd_type1').val("physical");
    $('#secd_max_1').val("190");
    $('#secd_min_1').val("175");

    $('#armor1').val("4500");
    $('#hit1').val("35");
    $('#critical1').val("30");
    $('#dodge1').val("30");
    $('#resist1').val("35");
    $('#block1').val("0");
    $('#agi1').val("75");
    $('#talented_skill1').val("blade_dance");

    ms1.clear();
    ms1.setValue(perm_slice(ms1.getData()));
}


$.makeJSON = function (mydata) {
    var show = $('<pre></pre>');
    var opener = "<code class=\"json\">";

    opener += JSON.stringify(mydata, null, '\t')

    opener += "</code>";
    $(opener).appendTo(show);

    return ($(show));
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

$("#submit-20").on('click', function(){

    var OutgoingData = $.getData()

    result = {};

    for(var i = 0; i < 1000; i++){
        $.postJSON("/battle", OutgoingData, function(data){

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

    $.postJSON("/battle", OutgoingData, function(data){

        $('#table-section').empty();

        $.make_graph(data.full_log);

        var res ="<div class=\"cap\"><br>Win:" + data.res + "</b>";
        $(res).appendTo("#table");


        data.player1 = OutgoingData.player1;
        data.player2 = OutgoingData.player2;
        delete data.full_log;
        var table = $.makeJSON(data);
        $(table).appendTo("#table-section");

        hljs.highlightBlock(table.get(0));

    }, "json").fail(function() {
        console.log( "error" );
    });
});

$("#reset").on('click', function(){
    $.setData();
});

$("#class1").ready(function(){
    $.postJSON("/get_cast_names", {id: $('#id1').val(), class:$('#class1').val()}, function(data){
        console.log(data);
        ms1 = $('#cast-list-1').magicSuggest({data:data, maxSelection:50, maxSuggestion:5, allowFreeEntries:false});
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class1").change(function(){

    $.postJSON("/get_cast_names", {id: $('#id1').val(), class:$('#class1').val()}, function(data){
        ms1.clear();
        ms1.setData(data);
        console.log(JSON.stringify(ms1.getData()));
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#save-1").on('click', function(){
    var OutgoingData = $.getData().player1;

    $.postJSON("/add_profile", OutgoingData, function(data){
        console.log(data);
    })
});

$("#save-2").on('click', function(){
    var OutgoingData = $.getData().player2;

    $.postJSON("/add_profile", OutgoingData, function(data){
        console.log(data);
    })
});
