/* Theme.
 *
 * The class goes on <html> while this script parses, before the body exists and
 * before first paint, so switching themes or reloading never shows a flash of
 * the other one. The switch is bound by delegation because Shiny re-renders the
 * landing page, which would drop a directly bound listener.
 */
(function () {
  'use strict';
  var KEY = 'canhrActi.theme';

  function isDark() {
    return document.documentElement.classList.contains('theme-dark');
  }

  function apply(dark) {
    document.documentElement.classList.toggle('theme-dark', dark);
    try { localStorage.setItem(KEY, dark ? 'dark' : 'light'); } catch (e) { /* private mode */ }
    report();
    reflect();
  }

  // The server draws the charts, so it has to know which theme they are being
  // drawn onto. Without this the R plots were painted white on a dark panel.
  function report() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('app_theme', isDark() ? 'dark' : 'light');
    }
    if (window.canhr && window.canhr.setTheme) {
      window.canhr.setTheme(isDark() ? 'dark' : 'light');
    }
  }

  function reflect() {
    var want = isDark() ? 'true' : 'false';
    document.querySelectorAll('.ov-theme-toggle').forEach(function (t) {
      // Only write when it differs: setAttribute always emits a mutation
      // record, and the observer below would call this again forever.
      if (t.getAttribute('aria-checked') !== want) t.setAttribute('aria-checked', want);
    });
  }

  var stored = null;
  try { stored = localStorage.getItem(KEY); } catch (e) { /* private mode */ }
  if (stored === 'dark') document.documentElement.classList.add('theme-dark');

  document.addEventListener('click', function (e) {
    var t = e.target.closest && e.target.closest('.ov-theme-toggle');
    if (!t) return;
    e.preventDefault();
    apply(!isDark());
  });

  // Shiny re-renders the switch, so re-state it whenever the page changes.
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', reflect);
  } else {
    reflect();
  }
  // shiny:value fires before the new markup is in the document, so the switch
  // is re-stated on the next tick and again once the session goes idle.
  document.addEventListener('shiny:value', function () { setTimeout(reflect, 0); });
  document.addEventListener('shiny:idle', reflect);
  // The stored theme is applied before Shiny exists, so the first report has to
  // wait for the connection rather than ride on the toggle. Shiny fires
  // shiny:connected through jQuery, and jQuery.trigger() reaches jQuery
  // handlers only, so a native listener here would never hear it.
  if (window.jQuery) {
    jQuery(document).on('shiny:connected', report);
  } else {
    var waited = 0;
    var poll = setInterval(function () {
      waited += 200;
      if (window.Shiny && Shiny.setInputValue) { report(); clearInterval(poll); }
      else if (waited > 20000) clearInterval(poll);
    }, 200);
  }

  // Shiny re-renders the landing page at unpredictable times, so watch the
  // document rather than guessing when the switch reappears.
  if (window.MutationObserver) {
    var start = function () {
      new MutationObserver(reflect).observe(document.body, { childList: true, subtree: true });
      reflect();
    };
    if (document.body) start(); else document.addEventListener('DOMContentLoaded', start);
  }
})();

/* The omnibox.
 *
 * It drives the sidebar's own rows rather than a list of its own, so it can
 * never fall out of step with them and needs no server round trip: every result
 * is an element already on the page, and running one is a click on that element.
 * Ctrl+P opens it showing everything. There is one mode: the reference has a
 * separate command palette behind >, we have a single list of pages and actions,
 * so a prefix would only pretend to switch between them.
 */
(function () {
  'use strict';

  function init() {
    var box = document.getElementById('omnibox');
    var list = document.querySelector('.omnibox-results');
    if (!box || !list) return;

    var items = [];
    var active = -1;
    var closeTimer = null;
    var showAll = false;

    // Blur shuts the list on a delay so a click on a result still lands. That
    // timer has to be cancellable: without this, focusing the box again inside
    // the delay (Ctrl+P right after clicking away) let the stale close fire a
    // moment later and shut a list that had just opened, leaving the cursor in
    // the box with nothing showing.
    function cancelClose() {
      if (closeTimer === null) return;
      clearTimeout(closeTimer);
      closeTimer = null;
    }

    function collect() {
      var out = [];
      document.querySelectorAll('.main-sidebar .sh-act .sh-a').forEach(function (el) {
        out.push({ label: el.textContent.trim(), kind: 'Action', el: el });
      });
      document.querySelectorAll('.main-sidebar .sidebar-menu > li > a[data-value]').forEach(function (el) {
        var label = el.textContent.trim() || el.getAttribute('data-value');
        out.push({ label: label, kind: 'Page', el: el });
      });
      return out;
    }

    function close() {
      cancelClose();
      list.classList.remove('is-open');
      list.textContent = '';
      active = -1;
      showAll = false;
    }

    function render(q) {
      cancelClose();
      var needle = String(q || '').trim().toLowerCase();
      if (!needle && !showAll) {
        close();
        return;
      }
      items = collect().filter(function (c) {
        return !needle || c.label.toLowerCase().indexOf(needle) >= 0;
      });

      if (active >= items.length) active = items.length - 1;
      if (active < 0 && items.length) active = 0;

      list.textContent = '';
      if (!items.length) {
        var none = document.createElement('div');
        none.className = 'omnibox-empty';
        none.textContent = 'Nothing matches';
        list.appendChild(none);
      } else {
        items.forEach(function (c, i) {
          var row = document.createElement('div');
          row.className = 'omnibox-item' + (i === active ? ' is-active' : '');
          row.setAttribute('role', 'option');
          var label = document.createElement('span');
          label.className = 'omnibox-item-label';
          label.textContent = c.label;
          var kind = document.createElement('span');
          kind.className = 'omnibox-item-kind';
          kind.textContent = c.kind;
          row.appendChild(label);
          row.appendChild(kind);
          // mousedown, not click: blur would close the list first.
          row.addEventListener('mousedown', function (e) {
            e.preventDefault();
            run(i);
          });
          list.appendChild(row);
        });
      }
      list.classList.add('is-open');
    }

    // A label can only open a file input that is actually showing, so a result
    // inside a collapsed section opens that section first. Clicking the header
    // reuses its own handler, so the change is remembered like any other.
    function reveal(el) {
      var li = el.closest && el.closest('li');
      if (!li || !li.classList.contains('sh-hidden')) return;
      var head = li.previousElementSibling;
      while (head && !head.classList.contains('sh-head')) head = head.previousElementSibling;
      if (head) head.click();
    }

    function run(i) {
      var c = items[i];
      if (!c) return;
      close();
      box.value = '';
      box.blur();
      reveal(c.el);
      // A click on the real element: an action link, a file-input label, or a
      // tab link. Still inside the user gesture, so a file dialog may open.
      c.el.click();
    }

    box.addEventListener('input', function () {
      active = -1;
      showAll = false;
      render(box.value);
    });
    box.addEventListener('focus', function () {
      cancelClose();
      if (box.value.trim()) render(box.value);
    });
    box.addEventListener('blur', function () {
      cancelClose();
      closeTimer = setTimeout(function () {
        closeTimer = null;
        close();
      }, 120);
    });
    box.addEventListener('keydown', function (e) {
      if (e.key === 'ArrowDown') {
        e.preventDefault();
        active = Math.min(active + 1, items.length - 1);
        render(box.value);
      } else if (e.key === 'ArrowUp') {
        e.preventDefault();
        active = Math.max(active - 1, 0);
        render(box.value);
      } else if (e.key === 'Enter') {
        e.preventDefault();
        run(active < 0 ? 0 : active);
      } else if (e.key === 'Escape') {
        e.preventDefault();
        box.value = '';
        close();
        box.blur();
      }
    });

    // On window, in the capture phase, so nothing downstream can swallow it
    // first. Matched on e.code as well as e.key: on a non-US layout or with an
    // IME active, e.key for the P key is not necessarily "p".
    window.addEventListener('keydown', function (e) {
      if (!(e.ctrlKey || e.metaKey) || e.altKey || e.shiftKey) return;
      var isP = e.code === 'KeyP' || e.key === 'p' || e.key === 'P';
      if (!isP) return;
      e.preventDefault();
      e.stopPropagation();
      showAll = true;
      box.focus();
      box.select();
      render(box.value);
    }, true);

    // The brand block is the way back to the Overview, which has no row of its
    // own. The toggle sits inside it, so ignore clicks that land there.
    var brand = document.querySelector('.main-header .header-brand');
    if (brand) {
      brand.addEventListener('click', function (e) {
        if (e.target.closest('.sidebar-toggle')) return;
        var tab = document.querySelector('.main-sidebar .sidebar-menu > li > a[data-value="overview"]');
        if (tab) tab.click();
      });
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();

/* Collapsible sidebar sections.
 *
 * The chevron on each section header is a promise, so it has to keep it. A
 * section is its header plus every row up to the next header, which is how the
 * markup already reads; nothing needed a wrapper element. Open or shut is
 * remembered per section, and the chevron is one glyph rotated rather than two,
 * so the two states can never disagree.
 */
(function () {
  'use strict';
  var KEY = 'canhrActi.sections';

  // Rows only. shinydashboard appends a <div class="sidebarMenuSelectedTabItem">
  // as the last child of the menu, which is the input reporting the active tab;
  // it must not be swept up and hidden with the Support section.
  function membersOf(head) {
    var out = [];
    var el = head.nextElementSibling;
    while (el && !(el.tagName === 'LI' && el.classList.contains('sh-head'))) {
      if (el.tagName === 'LI') out.push(el);
      el = el.nextElementSibling;
    }
    return out;
  }

  function nameOf(head) {
    var label = head.querySelector('span');
    return label ? label.textContent.trim().toLowerCase().replace(/[^a-z]+/g, '-') : '';
  }

  function readState() {
    try { return JSON.parse(localStorage.getItem(KEY) || '{}'); } catch (e) { return {}; }
  }

  function writeState(s) {
    try { localStorage.setItem(KEY, JSON.stringify(s)); } catch (e) { /* private mode */ }
  }

  function apply(head, collapsed) {
    head.classList.toggle('is-collapsed', collapsed);
    head.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
    membersOf(head).forEach(function (li) {
      li.classList.toggle('sh-hidden', collapsed);
    });
  }

  function init() {
    var heads = document.querySelectorAll('.main-sidebar .sidebar-menu > li.sh-head');
    if (!heads.length) return;
    var state = readState();

    heads.forEach(function (head) {
      var key = nameOf(head);
      head.setAttribute('role', 'button');
      head.setAttribute('tabindex', '0');
      apply(head, state[key] === true);

      head.addEventListener('click', function () {
        var collapsed = !head.classList.contains('is-collapsed');
        apply(head, collapsed);
        var s = readState();
        s[key] = collapsed;
        writeState(s);
      });

      head.addEventListener('keydown', function (e) {
        if (e.key !== 'Enter' && e.key !== ' ') return;
        e.preventDefault();
        head.click();
      });
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();

/* ---------------------------------------------------------------------------
   Disclosures that remember.

   A <details data-persist="key"> keeps its open state per user, the way the
   sidebar sections do. Shiny rebuilds the element whenever the selected
   recording changes, so the state is applied on every insertion rather than
   once at load; the observer is how a re-rendered output gets its state back.
   --------------------------------------------------------------------------- */
(function () {
  var KEY = 'canhrActi.disclosures';

  function readState() {
    try { return JSON.parse(localStorage.getItem(KEY) || '{}'); } catch (e) { return {}; }
  }

  function writeState(s) {
    try { localStorage.setItem(KEY, JSON.stringify(s)); } catch (e) { /* private mode */ }
  }

  function apply(el) {
    var key = el.getAttribute('data-persist');
    if (!key) return;
    // Until the reader has opened or closed this one, the markup decides:
    // applying a default of closed would also SAVE it, and the page would
    // never again show what its author meant it to show.
    var state = readState();
    if (!Object.prototype.hasOwnProperty.call(state, key)) return;
    var want = state[key] === true;
    if (el.open !== want) el.open = want;
  }

  function applyAll(root) {
    if (!root || !root.querySelectorAll) return;
    if (root.matches && root.matches('details[data-persist]')) apply(root);
    root.querySelectorAll('details[data-persist]').forEach(apply);
  }

  document.addEventListener('toggle', function (e) {
    var el = e.target;
    if (!el || !el.matches || !el.matches('details[data-persist]')) return;
    var s = readState();
    s[el.getAttribute('data-persist')] = el.open;
    writeState(s);
  }, true);

  function init() {
    applyAll(document);
    new MutationObserver(function (records) {
      records.forEach(function (r) {
        Array.prototype.forEach.call(r.addedNodes, function (n) {
          if (n.nodeType === 1) applyAll(n);
        });
      });
    }).observe(document.body, { childList: true, subtree: true });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();

/* Cut text reads in full.
 *
 * Any text a page cuts with an ellipsis, and every rule bar value, gets its
 * whole text as a title while the pointer is over it, so no page has to add
 * titles by hand. A title the page wrote itself is left alone.
 */
(function () {
  'use strict';
  var MARK = 'data-autotitle';
  var RULE_VALUE = /(^|\s)[a-z]+-rule-v(\s|$)/;

  function visit(el) {
    if (el.hasAttribute('title') && !el.hasAttribute(MARK)) return;
    var text = (el.textContent || '').replace(/\s+/g, ' ').trim();
    var cut = el.scrollWidth > el.clientWidth + 1 &&
      getComputedStyle(el).textOverflow === 'ellipsis';
    var want = text && (cut || RULE_VALUE.test(el.getAttribute('class') || '')) ? text : '';
    if (want) {
      if (el.getAttribute('title') !== want) el.setAttribute('title', want);
      el.setAttribute(MARK, '');
    } else if (el.hasAttribute(MARK)) {
      el.removeAttribute('title');
      el.removeAttribute(MARK);
    }
  }

  // The pointer may land on a child of the cut element, so look a few levels up
  document.addEventListener('mouseover', function (e) {
    var el = e.target;
    for (var i = 0; i < 4 && el && el.nodeType === 1; i++, el = el.parentElement) {
      if (!el.closest('.content-wrapper')) return;
      visit(el);
    }
  }, true);
})();

/* Rule bar values wrap only between their " · " parts, never inside one.
 * Each part is put in a span that does not wrap; the separators stay plain
 * text, so a narrow window breaks the line at a separator.
 */
(function () {
  'use strict';
  var VALUE = /(^|\s)[a-z]+-rule-v(\s|$)/;
  var DOT = '\u00b7';

  function part() { var s = document.createElement('span'); s.className = 'rv-seg'; return s; }

  function split(el) {
    if (el.hasAttribute('data-segs')) return;
    el.setAttribute('data-segs', '');
    var nodes = Array.prototype.slice.call(el.childNodes);
    var hasDot = nodes.some(function (n) { return n.nodeType === 3 && n.nodeValue.indexOf(DOT) >= 0; });
    if (!hasDot) return;
    var out = document.createDocumentFragment();
    var seg = part();
    function close() { if (seg.childNodes.length) out.appendChild(seg); seg = part(); }
    nodes.forEach(function (n) {
      if (n.nodeType !== 3) { seg.appendChild(n); return; }
      var bits = n.nodeValue.split(DOT);
      bits.forEach(function (b, i) {
        if (i > 0) { close(); out.appendChild(document.createTextNode(' ' + DOT + ' ')); b = b.replace(/^\s+/, ''); }
        if (i < bits.length - 1) b = b.replace(/\s+$/, '');
        if (b) seg.appendChild(document.createTextNode(b));
      });
    });
    close();
    el.textContent = '';
    el.appendChild(out);
  }

  function scan(root) {
    if (root.nodeType !== 1) return;
    if (VALUE.test(root.getAttribute('class') || '')) split(root);
    root.querySelectorAll('[class*="-rule-v"]').forEach(function (el) {
      if (VALUE.test(el.getAttribute('class') || '')) split(el);
    });
  }

  function start() {
    var host = document.querySelector('.content-wrapper') || document.body;
    scan(host);
    new MutationObserver(function (list) {
      list.forEach(function (m) { m.addedNodes.forEach(scan); });
    }).observe(host, { childList: true, subtree: true });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
