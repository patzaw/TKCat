// Hide and show tabs
Shiny.addCustomMessageHandler('hideNavs', function(nav_label) {
 $('a[data-value=\"' + nav_label + '\"]').parent().css('display', 'none');
});

Shiny.addCustomMessageHandler('showNavs', function(nav_label) {
 $('a[data-value=\"' + nav_label + '\"]').parent().css('display', '');
});

Shiny.addCustomMessageHandler('setTabLabel', function(message){
 $('a[data-value=\"'+message.name+'\"]').html(message.label);
});

// Validate changes with keyboard
$(document).keyup(function(event) {
  if($("#connect")[0] && (event.key == "Enter")) {
    $("#connect").click();
  }
});

// Manage skin colors
Shiny.addCustomMessageHandler('change_skin', function(skin) {
  document.body.className = skin;
});
