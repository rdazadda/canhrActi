// Shared by the desktop windows: theme, preview mode and small helpers.
// Loaded in <head> so the theme is set before the first paint.
(function () {
  'use strict';

  const query = new URLSearchParams(window.location.search);
  const bridge = typeof window.canhr === 'undefined' ? null : window.canhr;

  function applyTheme(theme) {
    document.documentElement.dataset.theme = theme === 'dark' ? 'dark' : 'light';
  }

  // A theme in the query string paints at once; the bridge then confirms it.
  const asked = query.get('theme');
  if (asked) {
    applyTheme(asked);
  } else if (!bridge) {
    applyTheme(window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
  }

  if (bridge) {
    const fallback = setTimeout(() => { if (!document.documentElement.dataset.theme) applyTheme('light'); }, 400);
    Promise.resolve()
      .then(() => bridge.theme())
      .then((t) => applyTheme(t))
      .catch(() => { if (!document.documentElement.dataset.theme) applyTheme('light'); })
      .finally(() => clearTimeout(fallback));
    // The main process sets nativeTheme, so an open window follows a later theme change.
    window.matchMedia('(prefers-color-scheme: dark)')
      .addEventListener('change', (e) => applyTheme(e.matches ? 'dark' : 'light'));
  }

  // Calls the bridge without letting a missing or failing call break the page.
  function call(name, ...args) {
    if (!bridge || typeof bridge[name] !== 'function') return Promise.resolve(undefined);
    try {
      return Promise.resolve(bridge[name](...args));
    } catch (err) {
      return Promise.reject(err);
    }
  }

  window.ui = {
    query,
    bridge,
    // preview=1 forces the sample values even when the bridge is there
    preview: !bridge || query.get('preview') === '1',
    applyTheme,
    call,
    $: (id) => document.getElementById(id),
  };
})();
