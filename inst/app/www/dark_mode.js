// Dark mode: a moon/sun button at the right of the navbar switches the dark-mode class of <html>,
// which dark_mode.css styles. The choice is remembered; without one, the setting of the system is followed.
(function() {
  const key = 'dark-mode';
  const saved = (function() { try { return localStorage.getItem(key); } catch (e) { return null; } })();
  const dark = saved === null ? window.matchMedia('(prefers-color-scheme: dark)').matches : saved === 'true';
  // set before the page is drawn, so that it does not flash white
  document.documentElement.classList.toggle('dark-mode', dark);

  $(function() {
    const button = $('<a href="#" id="dark-mode-toggle"></a>');
    const label = function() {
      const on = document.documentElement.classList.contains('dark-mode');
      button.html(on ? '&#9728;' : '&#9790;').attr('title', on ? 'Light mode' : 'Dark mode');
    };
    label();
    button.on('click', function(e) {
      e.preventDefault();
      const on = document.documentElement.classList.toggle('dark-mode');
      try { localStorage.setItem(key, on); } catch (err) {}
      label();
    });
    // inside the collapsing part of the navbar, so that it sits on the row of the tabs
    let navbar = $('.navbar .navbar-collapse').first();
    if (!navbar.length) navbar = $('.navbar .container-fluid').first();
    navbar.append($('<ul class="nav navbar-nav navbar-right"></ul>').append($('<li></li>').append(button)));
  });
})();
