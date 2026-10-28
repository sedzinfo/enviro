// Reports the tabs of the app to Google Analytics as pages.
// gtag() only exists once the visitor accepts on the cookie banner (consent.js), so nothing is sent before.
$(document).on('shown.bs.tab', 'a[data-toggle="tab"], a[data-bs-toggle="tab"]', function(e) {
  if (typeof gtag !== 'function') return;
  const tab = $(e.target).attr('data-value');
  gtag('event', 'page_view', {
    page_title: tab,
    page_location: location.origin + location.pathname + '#' + encodeURIComponent(tab)
  });
});
