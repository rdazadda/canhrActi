// Quit confirmation. Cancel has focus, Esc cancels, Enter presses the focused button.
(function () {
  'use strict';

  const { $, call } = window.ui;
  const cancel = $('cancel');
  const quit = $('quit');
  let answered = false;

  function answer(ok) {
    if (answered) return;
    answered = true;
    cancel.disabled = true;
    quit.disabled = true;
    call('dialogResult', ok);
  }

  cancel.addEventListener('click', () => answer(false));
  quit.addEventListener('click', () => answer(true));

  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      answer(false);
    } else if (e.key === 'Enter' && document.activeElement !== cancel && document.activeElement !== quit) {
      // Focus left the buttons (a click on the card): Enter still means the safe choice.
      e.preventDefault();
      answer(false);
    }
  });

  cancel.focus();
})();
