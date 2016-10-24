
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
    $('#id1').val("Maxim the Warrior");
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

    ms1.clear();
    ms1.setValue(perm_slice(ms1.getData()));

    $('#id2').val("Scarlett the Rogue");
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

$("#submit").on('click', function(){

    OutgoingData = $.getData();

    $.postJSON("/get_result", $.getData(), function(data){

        IncomingData = data;
        IncomingData.player1 = OutgoingData.player1;
        IncomingData.player2 = OutgoingData.player2;

        console.log(JSON.stringify(IncomingData));

        $('#table-section').empty();

        var table = $.makeTable(data.proc);
        $(table).appendTo("#table-section");

        var res ="<div class=\"cap\"><br>Win:" + data.win + "</b>";
        $(res).appendTo("#table");

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
        ms1 = $('#cast-list-1').magicSuggest({data:data, maxSuggestion:5, allowFreeEntries:false});
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class1").change(function(){

    console.log('changed');

    $.postJSON("/get_list", {id: $('#id1').val(), class:$('#class1').val()}, function(data){
        ms1.clear(); 
        ms1.setData(data);

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
