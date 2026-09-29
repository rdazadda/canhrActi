// Startup window: the progress while the app starts, or the error.
(function () {
  'use strict';

  const { $, call, preview, query } = window.ui;
  const ORDER = ['r', 'packages', 'dashboard', 'window'];

  const card = $('card');
  const progress = $('progress');
  const buttons = ['quit', 'retry'].map($);
  const TITLES = { start: 'CANHRActi could not start', stopped: 'CANHRActi stopped' };

  function showStage(stage) {
    if (!ORDER.includes(stage)) return;
    card.dataset.state = 'loading';
    card.dataset.stage = stage;
    progress.setAttribute('aria-valuenow', String(ORDER.indexOf(stage) + 1));
  }

  // kind is 'stopped' when R ended after the window opened
  function showError(kind) {
    $('err-title').textContent = kind === 'stopped' ? TITLES.stopped : TITLES.start;
    card.dataset.state = 'error';
    buttons.forEach((b) => { b.disabled = false; });
    $('retry').focus();
  }

  function onState(msg) {
    if (!msg) return;
    if (msg.error) showError(msg.error);
    else if (msg.stage) showStage(msg.stage);
  }

  $('quit').addEventListener('click', () => {
    buttons.forEach((b) => { b.disabled = true; });
    call('splashAction', 'quit');
  });
  $('retry').addEventListener('click', () => {
    buttons.forEach((b) => { b.disabled = true; });
    showStage('r');
    call('splashAction', 'retry');
  });

  showStage('r');

  if (preview) {
    const state = query.get('state');
    if (state === 'error' || state === 'stopped') showError(state);
    else showStage(query.get('stage') || 'r');
  } else {
    window.canhr.onSplash(onState);
  }
})();
