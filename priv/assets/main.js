$.makeTable = function (mydata) {
    var table = $('<table id="table" class="table-fill">');
    var tblHeader = "<tr>";
    for (var k in mydata[0]) tblHeader += "<th>" + k + "</th>";
    tblHeader += "</tr>";
    $(tblHeader).appendTo(table);
    $.each(mydata, function (index, value) {
        var TableRow = "<tr>";
        $.each(value, function (key, val) {
            TableRow += "<td>" + val + "</td>";
        });
        TableRow += "</tr>";
        $(table).append(TableRow);
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

$.postJSON("/get_result", {"foo": "ba"}, function(data){

    var table = $.makeTable(data.proc);
    $(table).appendTo("#main");

    var res ="<div class=\"cap\"><br>Win:" + data.res.win + "</b>";
    $(res).appendTo("#table");

}, "json").fail(function() {
    console.log( "error" );
}); 

