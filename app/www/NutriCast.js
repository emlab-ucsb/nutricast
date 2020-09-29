
// overview scroll functions

$(document).ready(function() {
    $("#al_marine_seafood_production_scenarios").click(function() {
       $(".sidebar-menu a[href=\'#overview\']").tab("show");
setTimeout(function(){
        var top = $("#marine_seafood_production_scenarios").position().top;
        $(window).scrollTop( top );
        }, 100);
    });
});

$(document).ready(function() {
    $("#al_references_contact").click(function() {
       $(".sidebar-menu a[href=\'#overview\']").tab("show");
setTimeout(function(){
        var top = $("#references_contact").position().top;
        $(window).scrollTop( top );
        }, 100);
    });
});

// global-national-outlook scroll functions

