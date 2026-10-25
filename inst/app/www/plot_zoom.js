// Full screen button of the time series charts: shows a chart over the whole window, and back
function plotZoom(el) {
  el = $(el);
  var parent = el.parent().parent();
  if (el.attr('data-full_screen') === 'false') {
    parent.addClass('full-screen').trigger('resize').fadeOut().fadeIn();
    el.attr('data-full_screen', 'true');
  } else {
    parent.removeClass('full-screen').trigger('resize').fadeOut().fadeIn();
    el.attr('data-full_screen', 'false');
  }
}

$(function() {
  $('.plotly-full-screen .plotly.html-widget').append(`
    <div style='position: relative;'>
      <button onclick=plotZoom(this) class='plot-zoom' data-full_screen='false' title='Full Screen'>
        <i class='fa fa-expand-arrows-alt'></i>
      </button>
    </div>`);
});
