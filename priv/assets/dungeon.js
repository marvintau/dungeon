var listedData;

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

$.setEditData = function(playerData) {
    $('#id').val(playerData.id);
    $('#hp').val(playerData.hp);
    $('#class').val(playerData.class);
    $('#range_type').val(playerData.range_type);

    $('#prim_type').val(playerData.prim_type);
    $('#prim_max').val(playerData.prim_max);
    $('#prim_min').val(playerData.prim_min);

    $('#secd_type').val(playerData.secd_type);
    $('#secd_max').val(playerData.secd_max);
    $('#secd_min').val(playerData.secd_min);

    $('#armor').val(playerData.armor);
    $('#hit').val(playerData.hit);
    $('#critical').val(playerData.critical);
    $('#dodge').val(playerData.dodge);
    $('#resist').val(playerData.resist);
    $('#block').val(playerData.block);
    $('#agi').val(playerData.agi);
    $('#talented_skill').val(playerData.talented_skill);

    ms.clear();
    ms.setValue(playerData.cast_list);
}

$.randomEditData = function () {
    $('#id').val("Scarlett");
    $('#hp').val("2700");
    $('#class').val("rogue");
    $('#prim_type').val("physical");
    $('#prim_max').val("205");
    $('#prim_min').val("190");

    $('#secd_type').val("physical");
    $('#secd_max').val("190");
    $('#secd_min').val("175");

    $('#armor').val("4500");
    $('#hit').val("35");
    $('#critical').val("30");
    $('#dodge').val("30");
    $('#resist').val("35");
    $('#block').val("0");
    $('#agi').val("75");
    $('#talented_skill').val("blade_dance");

    ms.clear();
    ms.setValue(perm_slice(ms.getData()));
}

$.getEditData = function () {
    return {
        id: $('#id').val(),
        hp: parseInt($('#hp').val()),
        class: $('#class').val(),
        range_type : $('#range_type').val(),
        prim_type: $('#prim_type').val(),
        prim_max: parseInt($('#prim_max').val()),
        prim_min: parseInt($('#prim_min').val()),

        secd_type: $('#secd_type').val(),
        secd_max: parseInt($('#secd_max').val()),
        secd_min: parseInt($('#secd_min').val()),

        armor: parseInt($('#armor').val()),
        hit: parseInt($('#hit').val()),
        critical: parseInt($('#critical').val()),
        dodge: parseInt($('#dodge').val()),
        resist: parseInt($('#resist').val()),
        block: parseInt($('#block').val()),
        agi: parseInt($('#agi').val()),
        talented_skill: $('#talented_skill').val(),

        cast_list : ms.getValue()
    }
}


$.setData = function(playerData, i){

    $('#id'+i).text(playerData.id);
    $('#hp'+i).text(playerData.hp);
    $('#class'+i).text(playerData.class);
    $('#range_type'+i).text(playerData.range_type);
    $('#prim_type'+i).text(playerData.prim_type);
    $('#prim_max'+i).text(playerData.prim_max);
    $('#prim_min'+i).text(playerData.prim_min);

    $('#secd_type'+i).text(playerData.secd_type);
    $('#secd_max'+i).text(playerData.secd_max);
    $('#secd_min'+i).text(playerData.secd_min);

    $('#armor'+i).text(playerData.armor);
    $('#hit'+i).text(playerData.hit);
    $('#critical'+i).text(playerData.critical);
    $('#dodge'+i).text(playerData.dodge);
    $('#resist'+i).text(playerData.resist);
    $('#block'+i).text(playerData.block);
    $('#agi'+i).text(playerData.agi);
    $('#talented_skill'+i).text(playerData.talented_skill);

    console.log(playerData.cast_list.toString());
    $('#cast-list'+i).text(playerData.cast_list.toString());
}

$.getData = function(i){
    return {
        id: $('#id'+i).text(),
        hp: $('#hp'+i).text(),
        class: $('#class'+i).text(),
        range_type: $('#range_type'+i).text(),
        prim_type: $('#prim_type'+i).text(),
        prim_max: $('#prim_max'+i).text(),
        prim_min: $('#prim_min'+i).text(),

        secd_type: $('#secd_type'+i).text(),
        secd_max: $('#secd_max'+i).text(),
        secd_min: $('#secd_min'+i).text(),

        armor: $('#armor'+i).text(),
        hit: $('#hit'+i).text(),
        critical: $('#critical'+i).text(),
        dodge: $('#dodge'+i).text(),
        resist: $('#resist'+i).text(),
        block: $('#block'+i).text(),
        agi: $('#agi'+i).text(),
        talented_skill: $('#talented_skill'+i).text(),

        cast_list: $('#cast-list'+i).text().split(',')
    }
}

$.groupby = function(List){

}

$.textify = function(mydata){
    console.log(mydata);
}

$.makeJSON = function (mydata) {

    $.textify(mydata);

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

    var OutgoingData = $.getData()

    OutgoingData = {player1: $("#player-list-1").val(), player2: $("#player-list-2").val()},

    console.log(OutgoingData);

    $.postJSON("/battle", OutgoingData, function(data){

        $('#table-section').empty();

        $.make_graph(data.full_log);

        var res ="<div class=\"cap\"><br>Win:" + data.res + "</b>";
        $(res).appendTo("#table");


        data.player1 = $.getData("1");
        data.player2 = $.getData("2");
        delete data.full_log;
        var table = $.makeJSON(data);
        $(table).appendTo("#table-section");

        hljs.highlightBlock(table.get(0));

    }, "json").fail(function() {
        console.log( "error" );
    });
});

$("#reset").on('click', function(){
    $.randomEditData();
});

var get_player_list = function(Dest){
    $.postJSON("/get_player_list", {}, function(data){
        console.log(data);

        listedData = data.map(function(elem){return {key:elem.id, val:elem.name}});

        $('#player-list select').empty();

        listedData.forEach(function(e){
            $('<option/>', {
                value: e.key,
                html: e.val
            }).appendTo(Dest);
        })

    }, "json").fail(function(){
        console.log("error");
    });
};

$("#player-list").ready(function(){
    get_player_list("#player-list");
    $.postJSON('/get_profile', {id: "85f6d769-713b-48ad-9163-7ba43b7459c7"}, function(data){
        $.setEditData(data);
    })
});

$("#player-list").change(function(){
    $.postJSON('/get_profile', {id: $("#player-list").val()}, function(data){
        $.setEditData(data);
    })
});

$("#player-list-1").ready(function(){
    get_player_list("#player-list-1");
    $.postJSON('/get_profile', {id: "b119a5cb-2311-432c-b6e4-e20be932c714"}, function(data){
        $.setData(data, "1");
    })
})

$("#player-list-1").change(function(){
    $.postJSON('/get_profile', {id: $("#player-list-1").val()}, function(data){
        $.setData(data, "1");
    })
});

$("#player-list-2").ready(function(){
    get_player_list("#player-list-2");
    $.postJSON('/get_profile', {id: "85f6d769-713b-48ad-9163-7ba43b7459c7"}, function(data){
        $.setData(data, "2");
    })
})

$("#player-list-2").change(function(){
    $.postJSON('/get_profile', {id: $("#player-list-2").val()}, function(data){
        $.setData(data, "2");
    })
});

$("#class").ready(function(){
    $.postJSON("/get_cast_names", {id: $('#id').val(), class:$('#class').val()}, function(data){
        console.log(data);
        ms.clear();
        ms.setData(data);
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class").change(function(){

    console.log($("#class").val());

    $.postJSON("/get_cast_names", {id: $('#id1').val(), class:$('#class').val()}, function(data){
        console.log(data);
        ms.clear();
        ms.setData(data);
        console.log(JSON.stringify(ms.getData()));
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#new").on('click', function(){
    var OutgoingData = $.getEditData();

    console.log(JSON.stringify(OutgoingData));

    $.postJSON("/add_profile", JSON.stringify(OutgoingData), function(data){
        console.log(data);
    })
});

$("#update").on('click', function(){

    var temp_id = $("#player-list").val();
    var OutgoingData = {id: temp_id, content:$.getEditData()};

    $.postJSON("/update_profile", OutgoingData, function(data){
        get_player_list("#player-list");
        get_player_list("#player-list-1");
        get_player_list("#player-list-2");

        $.postJSON('/get_profile', {id: temp_id}, function(data){
            $.setEditData(data);
        })
    })
});

$("#refresh-chest1").on('click', function(){
    $.postJSON('/check_chest', {id: $("#player-list-1").val()}, function(data){
        console.log(JSON.stringify(data));
        $("#next-chest1").text(data.next_name);
        $("#remaining1").text(data.remaining);
    })

})

$("#open-chest1").on('click', function(){
    $.postJSON('/open_chest', {id: $("#player-list-1").val()}, function(data){

        console.log(JSON.stringify(data));

        $("#chest-res1").empty();
        if(typeof data === 'string') {
            $("#chest-res1").text(data);
        } else {
            list = $("#chest-res1");
            data.forEach(function(e){
                list.append("<li>"+e.name +" "+e.num+"</li>");
            })
        }
    })

    $.postJSON('/check_chest', {id: $("#player-list-1").val()}, function(data){
        $("#next-chest1").text(data.next_name);

        console.log(data);

        $("#remaining1").text(data.remaining);
    })
})

$("#reset-database").on('click', function(){
    $.postJSON('/reset_database', {}, function(data){
        window.location.reload(true);
    })
})

$(document).ready(function(){
    ms = $('#cast-list').magicSuggest({maxSelection:50, maxSuggestion:15, allowFreeEntries:false});
})
