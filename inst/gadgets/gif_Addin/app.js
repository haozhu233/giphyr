$('#preview').on('click', '.gifpreview', function(){
  Shiny.onInputChange('clickedgif', this.id);
  $('.gifpreview').removeClass('gifselected');
  $(this).addClass('gifselected');
});
