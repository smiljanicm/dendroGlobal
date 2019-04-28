text_renderer <- "
  function(instance, th) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    th.style.background = '#E0FFE0';
  }
"
dropdown_renderer <- "
  function(instance, td) {
    Handsontable.renderers.DropdownRenderer.apply(this, arguments);
    td.style.background = '#E0FFE0';
  }
"
