$(document).ready(function() {
  $('.solution').each( function() {
    var $t = $(this).parent();
    var text = { "PRE": "fa-code",
                 "IMG" : "fa-image"}[$(this).prop("tagName")]
    var $b = $('<a href="#"><i class="fa '+text+'"></i> Lösung anzeigen</a>')
              .bind("click", function() {
                $t.toggle();
                $(this).hide();
              });
    $t.before($("<blockquote/>").append($b));
    $t.hide();
  });
});
