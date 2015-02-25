$(function() {
    $.ajax(
        { url: "/api"
        , type: "POST"
        , data: JSON.stringify({method: "list"})
        })
    .done(function(data) {
        JSON.parse(data).forEach(function(it, i, arr) {
            $("ul").append("<li id="+i+">" + it + "</li>");
            $("#"+i).click(function() {
                $.ajax(
                    { url: "/api"
                    , type: "POST"
                    , data: JSON.stringify({method: "samples", params: [it]})
                    })
                .done(function(sam){
                    $("#"+i).html(it + "<br>");
                    var samples = JSON.parse(sam)[0];
                    for (s in samples) {
                        $("#"+i).append("<h3>"+s+"</h3><p>"+samples[s].reduce(function(p, c, k){return p + "<hr>" + c;},"")+"</p>");
                    }
                });
            });
        });
    });
});
