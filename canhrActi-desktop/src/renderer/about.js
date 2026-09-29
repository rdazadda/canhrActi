// About window: name, version and credits. Close and Esc close it.
(function () {
  'use strict';

  const { $, call, preview, query } = window.ui;

  function show(version) {
    $('version').textContent = version ? `Version ${version}` : 'Version unknown';
  }

  $('close').addEventListener('click', () => { call('close'); });
  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      call('close');
    }
  });

  if (preview) {
    show(query.get('version') || '0.4.1');
  } else {
    call('info')
      .then((info) => (info && info.version) || call('version'))
      .catch(() => call('version'))
      .then(show, () => show(''));
  }
})();
