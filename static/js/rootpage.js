addedanims = []
animcounter = 0

morecolors = function() {
    $('.colors').colpick( { layout   : 'hex'
                          , submit   : 0
                          , color    : 'ffffff'
                          , onChange : function(hsb,hex,rgb,elem,thing) {
                                          $(elem).css({'background':'#' + hex})
                                       }
                          } );
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
        console.log(data);
        for(i = 0; i < addedanims.length; i++) {
            $("#" + addedanims[i]).remove();
        }
        addedanims = []
        newanims = $.parseJSON(data);
        for(i = 0; i < newanims.length; i++) {
            var newanim = $("div[animtemplate=\"" + newanims[i]["name"] + "\"]")
                .clone()
                .attr("class","")
                .attr("id","newanims-" + animcounter)
                .attr("animtemplate","")
                .insertBefore($('#btnrow'));
            animcounter++;
            addedanims.push(newanim.attr("id"))
        }
        morecolors();
        removeAnimBtns();
        animUp();
        animDown();
    });
}
$( document ).ready(function() {
    morecolors();
    removeAnimBtns();
    animUp();
    animDown();
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
                    colobj = p[0].style
                                 .background
                                 .replace(/[^\d,]/g, '')
                                 .split(',')
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
        console.log(JSON.stringify(anims));
        $.post("/newanims", { "newanims" : JSON.stringify(anims) } );
    });
})
