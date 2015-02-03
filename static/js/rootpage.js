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
        var newanim = anim.clone().attr("class","").attr("id","newanims-" + animcounter).insertBefore($('#btnrow'));
        animcounter++;
        morecolors();
        removeAnimBtns();
        animUp();
        animDown();
        addedanims.push(newanim.attr("id"))
    });
    var ProtoBuf = dcodeIO.ProtoBuf;
    ProtoBuf.loadProtoFile("/static/rootpage.proto", function(err, builder) {
        var Animations = builder.build("Animations")
        $("#okbtn").click(function() {
            addedanims.sort(function(a,b) { return $("#" + a).index() - $("#" + b).index(); } );
            anims = [];
            for(i = 0; i < addedanims.length; i++) {
                animobj = $("#" + addedanims[i]);
                params = [];
                paramobjs = animobj.find("input.doubleopt, input.intopt, button.coloropt");
                for(j = 0; j < paramobjs.length; j++) {
                    paramobj = $(paramobjs[j]);
                    if(paramobj.is("input.doubleopt")) {
                        params.push( { "type"      : "Double"
                                     , "doubleopt" : parseFloat(paramobj.val())
                                     }
                        );
                    }
                    if(paramobj.is("input.intopt")) {
                        params.push( { "type"   : "Int"
                                     , "intopt" : parseInt(paramobj.val())
                                     }
                        );
                    }
                    if(paramobj.is("button.coloropt")) {
                        params.push( { "type"     : "Color"
                                     , "coloropt" : { "red"   : 0
                                                    , "green" : 0
                                                    , "blue"  : 0
                                                    }
                                     }
                        );
                    }
                }
                anims.push( { "name" : animobj.find("h3.anim-name")[0].innerHTML
                            , "params" : params
                            , "blendingmode" : $(animobj.find("select.anim-bl")[0]).val()
                            }
                )
            }
            console.log(JSON.stringify(anims));
            var protoanims = new Animations(anims);
            console.log(protoanims.encode());
        });
    });
})
