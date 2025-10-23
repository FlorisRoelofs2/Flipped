# global.R
library(shiny)
library(shinydashboard)
library(DT)
library(later)

# --- JS handlers for buttons and coin animation ---
js_disable_enable <- "
Shiny.addCustomMessageHandler('disableButtons', function(msg){
  (msg.ids || []).forEach(function(id){
    var el = document.getElementById(id);
    if(el){ el.setAttribute('disabled', 'disabled'); }
  });
});
Shiny.addCustomMessageHandler('enableButtons', function(msg){
  (msg.ids || []).forEach(function(id){
    var el = document.getElementById(id);
    if(el){ el.removeAttribute('disabled'); }
  });
});
Shiny.addCustomMessageHandler('coinSpin', function(msg){
  var el = document.getElementById('coin');
  if(!el) return;
  if(msg.spin){ el.classList.add('spin'); } else { el.classList.remove('spin'); }
});
"
