addedanims = []
animcounter = 0

rgbsplit = function(rgb) {
    return rgb.replace(/[^\d,]/g, '').split(',')
}
function rgb2hex(rgb) {
    //generates the hex-digits for a colour.
    function hex(x) {
        hexDigits = new Array("0", "1", "2", "3", "4", "5", "6", "7", "8",
"9", "A", "B", "C", "D", "E", "F");
        return isNaN(x) ? "00" : hexDigits[(x - x % 16) / 16] + hexDigits[x
% 16];
    }

    return "#" + hex(rgb[0]) + hex(rgb[1]) + hex(rgb[2]);
}


morecolors = function() {
    cols = $('.colors');
    for(i = 0; i < cols.length; i++) {
        rgbs = rgbsplit(cols[i].style.background);
        color = rgb2hex([rgbs[0],rgbs[1],rgbs[2]]);
        $(cols[i]).colpick( { layout   : 'hex'
                          , submit   : 0
                          , color    : color
                          , onChange : function(hsb,hex,rgb,elem,thing) {
                                          $(elem).css({'background':'#' + hex})
                                       }
                          } ).colpickSetColor(color,false);
    }
}
removeAnimBtns = function() {
    $(".del-anim").unbind("click");
    $(".del-anim").click(function() {
        var myparent = $(this).parents()[4]
        var index = addedanims.indexOf($(myparent).attr("id"));
        addedanims.splice(index,1);
        myparent.remove();
    });
}
animUp = function() {
    $(".anim-up").unbind("click");
    $(".anim-up").click(function() {
        var prev = $($(this).parents()[4]).prev();
        if($(prev).attr("id").substring(0,8) == "newanims") {
            $($(this).parents()[4]).insertBefore(prev);
        }
    });
}
animDown = function() {
    $(".anim-down").unbind("click");
    $(".anim-down").click(function() {
        var next = $($(this).parents()[4]).next();
        if($(next).attr("id").substring(0,8) == "newanims") {
            $($(this).parents()[4]).insertAfter(next);
        }
    });
}
refresh = function() {
    $.get( "/getanims", function( data ) {
        console.log("Received: " + data);
        newanims = $.parseJSON(data);
        loadanims(newanims);
    });
}
loadpreset = function(preset) {
    $.get( "/getpreset/" + preset, function(data) {
        console.log("Loading preset " + preset + ": " + data);
        newanims = $.parseJSON(data);
        loadanims(newanims);
    });
}
loadanims = function(newanims) {
    //This function is awful. Javascript is a god forsaken language, and the
    //fact that most of the web runs on this piece of crap language keeps me
    //up at night.
    for(i = 0; i < addedanims.length; i++) {
        $("#" + addedanims[i]).remove();
    }
    addedanims = []
    for(i = 0; i < newanims.length; i++) {
        var newanim = $("div[animtemplate=\"" + newanims[i]["name"] + "\"]")
            .clone()
            .attr("class","")
            .attr("id","newanims-" + animcounter)
            .attr("animtemplate","")
            .insertBefore($('#btnrow'));
        animcounter++;
        addedanims.push(newanim.attr("id"))
        
        $(newanim.find(".anim-bl")).val(newanims[i]["blendingmode"]);
        optcounts = { "double" : 0
                    , "int"    : 0
                    , "color"  : 0
                    }
        for(j = 0; j < newanims[i]["params"].length; j++) {
            if (newanims[i]["params"][j]["AnimDouble"] != null) {
                $(newanim.find(".doubleopt")[optcounts["double"]++]).val(newanims[i]["params"][j]["AnimDouble"]);
            }
            if (newanims[i]["params"][j]["AnimInt"] != null) {
                $(newanim.find(".intopt")[optcounts["int"]++]).val(newanims[i]["params"][j]["AnimInt"]);
            }
            if (newanims[i]["params"][j]["AnimLED"] != null) {
                color = "rgb(" + newanims[i]["params"][j]["AnimLED"]["red"] + "," + newanims[i]["params"][j]["AnimLED"]["green"] + "," + newanims[i]["params"][j]["AnimLED"]["blue"] + ")";
                $(newanim.find(".coloropt")[optcounts["color"]++]).css({'background':color})
            }
        }
    }
    morecolors();
    removeAnimBtns();
    animUp();
    animDown();
}

getconfig = function() {
    addedanims.sort(function(a,b) { return $("#" + a).index() - $("#" + b).index(); } );
    anims = [];
    for(i = 0; i < addedanims.length; i++) {
        a = $("#" + addedanims[i]);
        params = [];
        ps = a.find("input.doubleopt, input.intopt, button.coloropt");
        for(j = 0; j < ps.length; j++) {
            p = $(ps[j]);
            if(p.is("input.doubleopt")) {
                params.push( { "AnimDouble"  : parseFloat(p.val()) }
                );
            }
            if(p.is("input.intopt")) {
                params.push( { "AnimInt"  : parseInt(p.val()) }
                );
            }
            if(p.is("button.coloropt")) {
                colobj = rgbsplit(p[0].style.background)
                params.push( { "AnimLED"  : { "red"   : parseInt(colobj[0])
                                            , "green" : parseInt(colobj[1])
                                            , "blue"  : parseInt(colobj[2])
                                            }
                             }
                );
            }
        }
        anims.push( { "name" : a.find("h3.anim-name")[0].innerHTML
                    , "params" : params
                    , "blendingmode" : $(a.find(".anim-bl")[0]).val()
                    }
        )
    }
    return anims;
}
$( document ).ready(function() {
    $('.colors').css({'background':'#ffffff'});

    $(".anim-selected").click(function() {
        var tokens = $(this).attr("id").split('-');
        var num = tokens[tokens.length - 1];
        var anim = $("#avail-anim-" + num);
        var newanim = anim.clone()
                          .attr("class","")
                          .attr("id","newanims-" + animcounter)
                          .attr("animtemplate","")
                          .insertBefore($('#btnrow'));
        animcounter++;
        morecolors();
        removeAnimBtns();
        animUp();
        animDown();
        addedanims.push(newanim.attr("id"))
    });
    $("#rfbtn").click(function() {
        refresh();
    });
    $("#okbtn").click(function() {
        anims = getconfig();
        console.log("Sending: " + JSON.stringify(anims));
        $.post("/newanims", { "newanims" : JSON.stringify(anims) } );
    });
    $("#svbtn").click(function() {
        anims = getconfig();
        console.log("Saving preset: " + JSON.stringify(anims));
        $.post("/savepreset/" + $("#svfield").val(), { "animations"
                                                     : JSON.stringify(anims)
                                                     } );
    });
    refresh();
})
