$(document).ready(function(){$(".course-item-list-header.contracted").next().hide(),$(".course-item-list-header").click(function(e){e.preventDefault();var t=$(this),a=$(this).next();t.hasClass("expanded")?(t.removeClass("expanded"),t.addClass("contracted"),a.slideUp(),t.find("span.icon-chevron-down").removeClass("icon-chevron-down").addClass("icon-chevron-right"),t.find("span.hidden").html("(collapsed, click to expand)")):t.hasClass("contracted")&&(t.removeClass("contracted"),t.addClass("expanded"),a.slideDown(),t.find("span.icon-chevron-right").removeClass("icon-chevron-right").addClass("icon-chevron-down"),t.find("span.hidden").html("(expanded, click to collapse)"))})});