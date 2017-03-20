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

$.setEditData = function(cardData) {
    $('#card_name').val(cardData.card_profile.card_name);
    $('#hp').val(cardData.card_profile.hp);
    $('#class').val(cardData.card_profile.class);
    $('#range_type').val(cardData.card_profile.range_type);
    $('#image_name').val(cardData.card_profile.image_name);

    $('#prim_type').val(cardData.card_profile.prim_type);
    $('#prim_max').val(cardData.card_profile.prim_max);
    $('#prim_min').val(cardData.card_profile.prim_min);

    $('#secd_type').val(cardData.card_profile.secd_type);
    $('#secd_max').val(cardData.card_profile.secd_max);
    $('#secd_min').val(cardData.card_profile.secd_min);

    $('#armor').val(cardData.card_profile.armor);
    $('#hit').val(cardData.card_profile.hit);
    $('#critical').val(cardData.card_profile.critical);
    $('#dodge').val(cardData.card_profile.dodge);
    $('#resist').val(cardData.card_profile.resist);
    $('#block').val(cardData.card_profile.block);
    $('#agi').val(cardData.card_profile.agi);
    $('#talented_skill').val(cardData.card_profile.talented_skill);

    ms.clear();
    ms.setValue(cardData.card_profile.cast_list);
}


$.getEditData = function () {
    return {
        card_name: $('#card_name').val(),
        hp: parseInt($('#hp').val()),
        class: $('#class').val(),
        range_type : $('#range_type').val(),
        image_name : $('#image_name').val(),

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


$.setData = function(cardData, i){

    console.log(JSON.stringify(cardData.card_profile));

    $('#card_name'+i).text(cardData.card_profile.card_name);
    $('#hp'+i).text(cardData.card_profile.hp);
    $('#class'+i).text(cardData.card_profile.class);
    $('#range_type'+i).text(cardData.card_profile.range_type);
    $('#image_name'+i).text(cardData.card_profile.image_name);

    $('#prim_type'+i).text(cardData.card_profile.prim_type);
    $('#prim_max'+i).text(cardData.card_profile.prim_max);
    $('#prim_min'+i).text(cardData.card_profile.prim_min);

    $('#secd_type'+i).text(cardData.card_profile.secd_type);
    $('#secd_max'+i).text(cardData.card_profile.secd_max);
    $('#secd_min'+i).text(cardData.card_profile.secd_min);

    $('#armor'+i).text(cardData.card_profile.armor);
    $('#hit'+i).text(cardData.card_profile.hit);
    $('#critical'+i).text(cardData.card_profile.critical);
    $('#dodge'+i).text(cardData.card_profile.dodge);
    $('#resist'+i).text(cardData.card_profile.resist);
    $('#block'+i).text(cardData.card_profile.block);
    $('#agi'+i).text(cardData.card_profile.agi);
    $('#talented_skill'+i).text(cardData.card_profile.talented_skill);

    console.log(cardData.card_profile.cast_list.toString());
    $('#cast-list'+i).text(cardData.card_profile.cast_list.toString());
}

$.getData = function(i){
    return {
        card_name: $('#card_name'+i).text(),
        hp: $('#hp'+i).text(),
        class: $('#class'+i).text(),
        range_type: $('#range_type'+i).text(),
        image_name:$('#image_name'+i).text(),

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
        $.postJSON("/api/battle", OutgoingData, function(data){

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

    $.postJSON("/api/battle", OutgoingData, function(data){

        $('#table-section').empty();

        $.make_graph(data.full_log);

        data = JSON.parse(JSON.stringify(data).replace(OutgoingData.player1, "舒克").replace(OutgoingData.player2, "贝塔"));

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
    $.postJSON("/api/get_card_list", {}, function(data){

        listedData = data.map(function(elem){return {key:elem.id, val:elem.card_name}});

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
    $.postJSON('/api/get_card_profile', {id: ["946ae77c-183b-4538-b439-ac9036024676"]}, function(data){
        $.setEditData(data[0]);
    })
});

$("#player-list").change(function(){
    $.postJSON('/api/get_card_profile', {id: [$("#player-list").val()]}, function(data){
        $.setEditData(data[0]);
    })
});

$("#player-list-1").ready(function(){
    get_player_list("#player-list-1");
    $.postJSON('/api/get_card_profile', {id: ["946ae77c-183b-4538-b439-ac9036024676"]}, function(data){
        $.setData(data[0], "1");
    })
})

$("#player-list-1").change(function(){
    $.postJSON('/api/get_card_profile', {id: [$("#player-list-1").val()]}, function(data){
        $.setData(data[0], "1");
    })
});

$("#player-list-2").ready(function(){
    get_player_list("#player-list-2");
    $.postJSON('/api/get_card_profile', {id: ["946ae77c-183b-4538-b439-ac9036024676"]}, function(data){
        $.setData(data[0], "2");
    })
})

$("#player-list-2").change(function(){
    $.postJSON('/api/get_card_profile', {id: [$("#player-list-2").val()]}, function(data){
        $.setData(data[0], "2");
    })
});

$("#class").ready(function(){
    $.postJSON("/api/get_cast_names", {id: $('#id').val(), class:$('#class').val()}, function(data){
        console.log(data);
        ms.clear();
        ms.setData(data);
    }, "json").fail(function(){
        console.log("error");
    });
});

$("#class").change(function(){

    console.log($("#class").val());

    $.postJSON("/api/get_cast_names", {id: $('#id1').val(), class:$('#class').val()}, function(data){
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

    $.postJSON("/api/add_card_profile", JSON.stringify(OutgoingData), function(data){
        console.log(data);
    })
});

$("#update").on('click', function(){

    var temp_id = $("#player-list").val();
    var OutgoingData = {id: temp_id, content:$.getEditData()};

    $.postJSON("/api/update_card_profile", OutgoingData, function(data){
        get_player_list("#player-list");
        get_player_list("#player-list-1");
        get_player_list("#player-list-2");

        $.postJSON('/api/get_card_profile', {id: [temp_id]}, function(data){
            $.setEditData(data[0]);
        })
    })
});

$("#refresh-chest1").on('click', function(){
    $.postJSON('/api/check_chest', {id: "f2740862-674b-479e-b02c-500e8a0285a0"}, function(data){
        console.log(JSON.stringify(data));
        $("#next-chest1").text(data.next_name);
        $("#remaining1").text(data.remaining);
    })

})

$("#open-chest1").on('click', function(){
    $.postJSON('/api/open_chest', {id: "f2740862-674b-479e-b02c-500e8a0285a0"}, function(data){

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

    $.postJSON('/api/check_chest', {id: "f2740862-674b-479e-b02c-500e8a0285a0"}, function(data){
        $("#next-chest1").text(data.next_name);

        console.log(data);

        $("#remaining1").text(data.remaining);
    })
})

$("#reset-database").on('click', function(){
    $.postJSON('/api/reset_database', {}, function(data){
        window.location.reload(true);
    })
})

$(document).ready(function(){
    ms = $('#cast-list').magicSuggest({maxSelection:50, maxSuggestion:15, allowFreeEntries:false});
})
