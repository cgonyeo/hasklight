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
$( document ).ready(function() {
    morecolors();
    removeAnimBtns();
    $('.colors').css({'background':'#ffffff'});

    $(".anim-selected").click(function() {
        var tokens = $(this).attr("id").split('-');
        var num = tokens[tokens.length - 1];
        var anim = $("#avail-anim-" + num);
        var newanim = anim.clone().attr("class","").attr("id","newanims-" + animcounter).insertBefore($('#btnrow'));
        animcounter++;
        morecolors();
        removeAnimBtns();
        addedanims.push(newanim.attr("id"))
    });

})
