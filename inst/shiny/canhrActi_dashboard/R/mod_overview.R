# Overview module
#
# The landing page: import recordings, see what is loaded, inspect a file and
# run the analyses. Everything shown comes from `shared` plus a little local
# state (unreadable files, running conversions, the active tab, a notice).

# The two file inputs live at app level so the sidebar's Add file(s) and Add
# folder work on every tab: a label can only open an input that is in the DOM,
# and a tab that is not showing is display:none.

# Which pipeline each extension goes to: .agd is counts; .cwa, .bin and .gt3x
# are signals and go to the raw pipeline (Convert .gt3x to .agd derives counts).
OV_COUNTS_EXT <- "agd"
OV_RAW_EXT <- c("gt3x", "cwa", "bin")
OV_ACCEPT <- paste0(".", c(OV_COUNTS_EXT, OV_RAW_EXT))

mod_overview_inputs <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "ov-inputs",
    # One input for every format the app reads; the extension picks the pipeline
    fileInput(ns("files"), NULL, multiple = TRUE, accept = OV_ACCEPT,
              buttonLabel = "", placeholder = ""),
    tags$input(type = "file", id = ns("dir_files"), class = "ov-hidden-input",
               webkitdirectory = NA, multiple = NA, accept = paste(OV_ACCEPT, collapse = ","),
               tabindex = "-1", `aria-hidden` = "true")
  )
}

mod_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "ov-page",

      uiOutput(ns("top")),
      uiOutput(ns("body")),

      # Drawn around the content area while files are dragged over the window
      tags$div(class = "ov-dragring", `aria-hidden` = "true")
    ),
    tags$script(HTML(ov_page_script(ns("")))),
    tags$script(HTML(cv_dialog_script(ns("")))),
    tags$script(HTML(od_dialog_script(ns(""))))
  )
}

# Page script: row selection and removal, keyboard navigation, sorting by
# header, the name filter and drag-and-drop over the window.
ov_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  var filterQuery = '';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }
  function visibleRows(body) {
    return Array.prototype.filter.call(body.querySelectorAll('.ov-row[data-fid]'), function (r) { return !r.hidden; });
  }
  // One row at a time sits in the tab order: the selected one, else the first.
  function roving(body, row) {
    body.querySelectorAll('.ov-row[data-fid]').forEach(function (r) { r.tabIndex = (r === row) ? 0 : -1; });
  }
  function selectRow(row) {
    var body = row.closest('.ov-list-body');
    if (body) {
      body.querySelectorAll('.ov-row.is-selected').forEach(function (r) { r.classList.remove('is-selected'); });
      roving(body, row);
    }
    row.classList.add('is-selected');
    setVal('select_row', row.dataset.fid);
  }
  function applyFilter() {
    var body = document.querySelector('.ov-page .ov-list-body');
    var input = document.querySelector('.ov-page .ov-filter-input');
    if (!body) return;
    if (input && input.value !== filterQuery) input.value = filterQuery;
    var q = filterQuery.trim().toLowerCase();
    var rows = body.querySelectorAll('.ov-row[data-fid]');
    var shown = 0;
    rows.forEach(function (r) {
      var hit = !q || (r.dataset.search || '').indexOf(q) >= 0;
      r.hidden = !hit;
      if (hit) shown++;
    });
    var count = document.querySelector('.ov-page .ov-list-count');
    if (count) {
      count.textContent = shown + ' of ' + rows.length + ' files';
      count.hidden = !q;
    }
  }

  // ---- the trace, as a way of moving around in the recording ----------------
  // It is the only thing on the page that shows the whole file at once, so a
  // click on an hour sends the epoch list to that hour, and a band shows which
  // stretch the list is on. The x is converted to a fraction here and to an
  // epoch on the server, because the browser only ever saw a picture of the
  // timestamps.
  var MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  var hoverWrap = null;

  function traceWrapOf(el) { return el && el.closest ? el.closest('.ov-page .ovl-trace-wrap') : null; }

  function traceAt(wrap, clientX) {
    var svg = wrap.querySelector('.ovl-trace');
    var r = svg.getBoundingClientRect();
    var frac = r.width > 0 ? Math.max(0, Math.min(1, (clientX - r.left) / r.width)) : 0;
    var t0 = Number(wrap.dataset.start), t1 = Number(wrap.dataset.end);
    return { frac: frac, x: (r.left - wrap.getBoundingClientRect().left) + frac * r.width, at: t0 + frac * (t1 - t0) };
  }

  // The file stores device local time as UTC, so it is read back as UTC: the
  // readout has to match the table, not the reader's own clock.
  function stamp(ms) {
    var d = new Date(ms), p = function (n) { return (n < 10 ? '0' : '') + n; };
    return d.getUTCDate() + ' ' + MONTHS[d.getUTCMonth()] + ' ' + p(d.getUTCHours()) + ':' + p(d.getUTCMinutes());
  }

  function hideTraceCursor(wrap) {
    var c = wrap.querySelector('.ovl-trace-cursor'), t = wrap.querySelector('.ovl-trace-tip');
    if (c) c.hidden = true;
    if (t) t.hidden = true;
  }

  // percent, not pixels, so the band survives a resize without recomputing
  function showTraceBand() {
    var wrap = document.querySelector('.ov-page .ovl-trace-wrap');
    var page = document.querySelector('.ov-page .ovl-epochs');
    if (!wrap || !page) return;
    var band = wrap.querySelector('.ovl-trace-band');
    var t0 = Number(wrap.dataset.start), t1 = Number(wrap.dataset.end);
    var a = Number(page.dataset.from), b = Number(page.dataset.to);
    if (!band || !(t1 > t0) || !a || !b) { if (band) band.hidden = true; return; }
    var l = Math.max(0, Math.min(1, (a - t0) / (t1 - t0)));
    var r = Math.max(0, Math.min(1, (b - t0) / (t1 - t0)));
    band.style.left = (l * 100) + '%';
    band.style.width = Math.max(0.25, (r - l) * 100) + '%';
    band.hidden = false;
  }

  // Coverage axis labels sit at the time they name. The lane width is only
  // known here, so day labels that would touch a neighbour or an end are hidden.
  function fitAxis(row) {
    var from = row.querySelector('.is-from'), to = row.querySelector('.is-to');
    var days = Array.prototype.filter.call(row.children, function (s) { return s !== from && s !== to; });
    days.forEach(function (s) { s.hidden = false; });
    var box = row.getBoundingClientRect();
    if (!days.length || !box.width) return;
    var lo = from ? from.getBoundingClientRect().right + 8 : box.left;
    var hi = to ? to.getBoundingClientRect().left - 8 : box.right;
    var rects = days.map(function (s) { return s.getBoundingClientRect(); });
    var widest = Math.max.apply(null, rects.map(function (r) { return r.width; }));
    var pitch = rects.length > 1 ? rects[1].left - rects[0].left : 0;
    var step = pitch > 0 ? Math.max(1, Math.ceil((widest + 8) / pitch)) : 1;
    days.forEach(function (s, i) {
      s.hidden = i % step !== 0 || rects[i].left < lo || rects[i].right > hi;
    });
  }
  // Refit on any size change, which includes the tab coming into view
  var axisWatch = window.ResizeObserver ? new ResizeObserver(function (entries) {
    entries.forEach(function (en) { fitAxis(en.target); });
  }) : null;
  function watchAxes() {
    if (axisWatch) axisWatch.disconnect();
    document.querySelectorAll('.ov-page .ovl-axis-row').forEach(function (row) {
      if (axisWatch) axisWatch.observe(row); else fitAxis(row);
    });
  }

  document.addEventListener('mousemove', function (e) {
    var wrap = traceWrapOf(e.target);
    if (!wrap) {
      if (hoverWrap) { hideTraceCursor(hoverWrap); hoverWrap = null; }
      return;
    }
    hoverWrap = wrap;
    var hit = traceAt(wrap, e.clientX);
    var cursor = wrap.querySelector('.ovl-trace-cursor');
    var tip = wrap.querySelector('.ovl-trace-tip');
    cursor.style.left = hit.x + 'px'; cursor.hidden = false;
    tip.textContent = stamp(hit.at);
    tip.style.left = hit.x + 'px'; tip.hidden = false;
  });

  document.addEventListener('click', function (e) {
    if (!e.target.closest('.ov-page')) return;
    // The raw interface's own three controls: its tab strip, the six figure
    // chips, and the Show me link that jumps from a check to the figure that
    // explains it. Checked before the counts handlers because the raw tabs
    // carry data-rawtab and would otherwise fall through to nothing.
    var rawTab = e.target.closest('.ov-tab[data-rawtab]');
    if (rawTab) { e.preventDefault(); setVal('raw_tab_set', rawTab.dataset.rawtab); return; }
    var chip = e.target.closest('[data-chip]');
    if (chip) { e.preventDefault(); setVal('raw_chip_set', chip.dataset.chip); return; }
    var rawX = e.target.closest('.ovr-x-btn[data-fid]');
    if (rawX) { e.preventDefault(); e.stopPropagation(); setVal('raw_remove', rawX.dataset.fid); return; }
    var rawName = e.target.closest('.ovr-name a[data-fid]');
    if (rawName) { e.preventDefault(); setVal('raw_open', rawName.dataset.fid); return; }
    var rawHead = e.target.closest('.ov-col-head [data-rawsort]');
    if (rawHead) { setVal('raw_sort', rawHead.dataset.rawsort); return; }
    var traceWrap = traceWrapOf(e.target);
    if (traceWrap) { setVal('trace_click', traceAt(traceWrap, e.clientX).frac); return; }
    var remove = e.target.closest('.ov-x');
    var row = e.target.closest('.ov-row[data-fid]');
    if (remove && row) { e.preventDefault(); e.stopPropagation(); setVal('remove_row', row.dataset.fid); return; }
    if (row) { selectRow(row); return; }
    var head = e.target.closest('.ov-col-head [data-sort]');
    if (head) { setVal('sort', head.dataset.sort); return; }
    var tab = e.target.closest('.ov-tab[data-tab]');
    if (tab) {
      e.preventDefault();
      tab.parentNode.querySelectorAll('.ov-tab').forEach(function (t) { t.classList.remove('is-active'); });
      tab.classList.add('is-active');
      setVal('tab', tab.dataset.tab);
    }
  });

  document.addEventListener('dblclick', function (e) {
    var row = e.target.closest('.ov-page .ov-row[data-fid]');
    if (row && !row.classList.contains('is-converting')) setVal('open_details', row.dataset.fid);
  });

  document.addEventListener('keydown', function (e) {
    if (!e.target.closest || !e.target.closest('.ov-page')) return;
    // Escape on the Details tab returns to the list.
    if (e.key === 'Escape' && document.querySelector('.ov-page .ov-detail, .ov-page .ovl-det')) {
      var filesTab = document.querySelector('.ov-page .ov-tab[data-tab=files]');
      if (filesTab) { e.preventDefault(); filesTab.click(); }
      return;
    }
    var head = e.target.closest('.ov-col-head [data-sort]');
    if (head && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); setVal('sort', head.dataset.sort); return; }
    var rawHead = e.target.closest('.ov-col-head [data-rawsort]');
    if (rawHead && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); setVal('raw_sort', rawHead.dataset.rawsort); return; }
    var row = e.target.closest('.ov-row[data-fid]');
    if (!row) return;
    var body = row.closest('.ov-list-body');
    var rows = visibleRows(body);
    var i = rows.indexOf(row);
    var next = null;
    if (e.key === 'ArrowDown') next = rows[Math.min(i + 1, rows.length - 1)];
    else if (e.key === 'ArrowUp') next = rows[Math.max(i - 1, 0)];
    else if (e.key === 'Home') next = rows[0];
    else if (e.key === 'End') next = rows[rows.length - 1];
    else if (e.key === 'Enter') {
      e.preventDefault();
      if (!row.classList.contains('is-converting')) setVal('open_details', row.dataset.fid);
      return;
    }
    else if (e.key === ' ') { e.preventDefault(); selectRow(row); return; }
    else if (e.key === 'Delete' || e.key === 'Backspace') {
      if (row.classList.contains('is-running')) return;
      e.preventDefault(); setVal('remove_row', row.dataset.fid); return;
    }
    if (next) { e.preventDefault(); next.focus(); if (next !== row) selectRow(next); }
  });

  document.addEventListener('input', function (e) {
    if (!e.target.classList || !e.target.classList.contains('ov-filter-input')) return;
    filterQuery = e.target.value;
    applyFilter();
  });
  // The checks list or another recording finishing rebuilds the body, which
  // would send a scrolled raw figure back to the start. Its place is kept when
  // the chip and recording are the same and put back once the image loads.
  var figKeep = null;
  function figBox() { return document.getElementById(NS + 'raw_figure'); }
  function figKey(box) {
    var panel = box && box.closest('.ovr-figpanel');
    return panel ? panel.dataset.fig : null;
  }
  function saveFigScroll() {
    var box = figBox();
    figKeep = box && (box.scrollTop || box.scrollLeft) ?
      { key: figKey(box), top: box.scrollTop, left: box.scrollLeft } : null;
  }
  document.addEventListener('load', function (e) {
    if (!figKeep || e.target.tagName !== 'IMG') return;
    var box = figBox();
    if (!box || !box.contains(e.target)) return;
    if (figKey(box) === figKeep.key) { box.scrollTop = figKeep.top; box.scrollLeft = figKeep.left; }
    figKeep = null;
  }, true);

  $(document).on('shiny:value', function (ev) {
    if (ev.name === NS + 'body') saveFigScroll();
    if (ev.name === NS + 'file_list') setTimeout(applyFilter, 0);
    if (ev.name === NS + 'file_list' || ev.name === NS + 'body') setTimeout(watchAxes, 0);
    if (ev.name === NS + 'epochs' || ev.name === NS + 'details') setTimeout(showTraceBand, 0);
  });

  // Gaps only scrolls sideways, so a plain wheel moves it that way, and
  // Overview too while it has nothing to scroll down; at either end, or with
  // Shift held, the wheel is left to the browser
  document.addEventListener('wheel', function (e) {
    if (e.shiftKey || e.ctrlKey || !e.target.closest) return;
    var box = e.target.closest('.ov-page .ovr-figpanel.is-scroll-x .shiny-plot-output, ' +
                               '.ov-page .ovr-figpanel.is-scroll-xy .shiny-plot-output');
    if (!box || Math.abs(e.deltaY) <= Math.abs(e.deltaX)) return;
    if (box.closest('.is-scroll-xy') && box.scrollHeight > box.clientHeight + 1) return;
    var max = box.scrollWidth - box.clientWidth;
    var dy = e.deltaY * (e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? box.clientWidth : 1);
    if (max <= 0 || (dy < 0 && box.scrollLeft <= 0) || (dy > 0 && box.scrollLeft >= max - 1)) return;
    e.preventDefault();
    box.scrollLeft = Math.max(0, Math.min(max, box.scrollLeft + dy));
  }, { passive: false });

  // The scrollbar's thickness, so a raw figure that scrolls one way is drawn
  // to fit the box exactly the other way
  function sendScrollbar() {
    if (!document.body || !window.Shiny || !Shiny.setInputValue) return;
    var probe = document.createElement('div');
    probe.style.cssText = 'position:absolute;top:-200px;width:100px;height:100px;overflow:scroll;visibility:hidden';
    document.body.appendChild(probe);
    var sb = probe.offsetWidth - probe.clientWidth;
    document.body.removeChild(probe);
    Shiny.setInputValue(NS + 'fig_sb', sb);
  }
  $(document).on('shiny:connected', sendScrollbar);
  window.addEventListener('resize', sendScrollbar);

  // Drag and drop: a file dragged anywhere over the window draws the ring, and
  // the hidden file input stretches over the window to receive the drop.
  var depth = 0;
  function hasFiles(e) {
    var t = e.dataTransfer && e.dataTransfer.types;
    return !!t && Array.prototype.indexOf.call(t, 'Files') >= 0;
  }
  function endDrag() { depth = 0; document.body.classList.remove('ov-dragging'); }
  document.addEventListener('dragenter', function (e) {
    if (!hasFiles(e) || !document.querySelector('.ov-page')) return;
    depth++;
    document.body.classList.add('ov-dragging');
  });
  document.addEventListener('dragleave', function (e) {
    if (!hasFiles(e)) return;
    depth = Math.max(0, depth - 1);
    if (depth === 0) document.body.classList.remove('ov-dragging');
  });
  document.addEventListener('drop', endDrag);
  document.addEventListener('dragend', endDrag);
  window.addEventListener('blur', endDrag);

  // Add file(s), Add folder and a drop send one file at a time, each as its
  // own upload through the input it came from. The next one starts when the
  // server says it has handed the last to its pipeline, so a recording can be
  // opened while the rest upload. Files the app does not read are not sent.
  var READS = /\\.(__EXT__)$/i;
  var SEND = {};
  function freshRun() { return { queue: [], job: null, i: 0, n: 0, sent: 0, skipped: 0, failed: [], folders: true }; }
  var run = freshRun();
  function report(done) {
    var v = done ?
      { done: true, sent: run.sent, skipped: run.skipped, failed: run.failed, folders: run.folders } :
      { i: run.i, n: run.n, name: run.job.file.name };
    Shiny.setInputValue(NS + 'upload', v, { priority: 'event' });
  }
  // A pick made while a run goes on joins its queue
  function take(input, list, folder) {
    var files = Array.prototype.slice.call(list || []);
    if (!files.length) return;
    var n0 = run.n;
    files.forEach(function (f) {
      if (READS.test(f.name)) { run.queue.push({ input: input, file: f }); run.n++; }
      else run.skipped++;
    });
    if (!folder) run.folders = false;
    if (!run.job) next();
    else if (run.n > n0) report(false);
  }
  function next() {
    var job = run.queue.shift() || null;
    run.job = job;
    if (!job) { report(true); run = freshRun(); return; }
    // Small files ride together, as many as the link carries in about two
    // seconds, so a folder of many .agd files is not redrawn once per file
    job.files = [job.file];
    var bytes = job.file.size;
    while (run.rate && run.queue.length && run.queue[0].input === job.input &&
           bytes + run.queue[0].file.size <= 2 * run.rate) {
      var more = run.queue.shift().file;
      job.files.push(more); bytes += more.size;
    }
    job.bytes = bytes; job.t0 = Date.now();
    run.i += job.files.length;
    report(false);
    var $in = $(job.input), before = $in.data('currentUploader');
    var dt = new DataTransfer();
    job.files.forEach(function (f) { dt.items.add(f); });
    job.input.files = dt.files;
    $in.trigger('change', [SEND]);
    // An upload that fails or is cut off gets no answer from the server
    var u = $in.data('currentUploader');
    if (!u || u === before) { fail(job); return; }
    var onError = u.onError, onAbort = u.onAbort;
    u.onError = function (m) { onError.call(u, m); fail(job); };
    u.onAbort = function () { onAbort.call(u); fail(job); };
  }
  function fail(job) {
    if (run.job !== job) return;
    job.files.forEach(function (f) { run.failed.push(f.name); });
    next();
  }
  Shiny.addCustomMessageHandler('ov-upload-ack', function (msg) {
    var job = run.job;
    if (!job) return;
    run.sent += job.files.length;
    // bytes per second this upload managed, from its start to the server's answer
    run.rate = job.bytes / Math.max(0.05, (Date.now() - job.t0) / 1000);
    next();
  });
  // Bound before Shiny binds the inputs, so a pick or a drop stops here
  [['files', false], ['dir_files', true]].forEach(function (x) {
    var el = document.getElementById(NS + x[0]);
    if (!el) return;
    $(el).on('change.ovUpload', function (e, tag) {
      if (tag === SEND) return;
      e.stopImmediatePropagation();
      take(el, el.files, x[1]);
    });
  });
  $(document).on('shiny:disconnected', function () { run = freshRun(); });
})();
"
  js <- sub("__EXT__", paste(c(OV_COUNTS_EXT, OV_RAW_EXT), collapse = "|"), js, fixed = TRUE)
  sub("__NS__", ns_prefix, js, fixed = TRUE)
}

# The Convert dialog's icons: Material Symbols Outlined, the source and
# viewBox msym() uses
cv_icon_paths <- list(
  close = "m256-200-56-56 224-224-224-224 56-56 224 224 224-224 56 56-224 224 224 224-56 56-224-224-224 224Z",
  upload = "M440-320v-326L336-542l-56-58 200-200 200 200-56 58-104-104v326h-80ZM240-160q-33 0-56.5-23.5T160-240v-120h80v120h480v-120h80v120q0 33-23.5 56.5T720-160H240Z",
  draft = "M240-80q-33 0-56.5-23.5T160-160v-640q0-33 23.5-56.5T240-880h320l240 240v480q0 33-23.5 56.5T720-80H240Zm280-520v-200H240v640h480v-440H520ZM240-800v200-200 640-640Z",
  folder = "M160-160q-33 0-56.5-23.5T80-240v-480q0-33 23.5-56.5T160-800h240l80 80h320q33 0 56.5 23.5T880-640v400q0 33-23.5 56.5T800-160H160Zm0-80h640v-400H447l-80-80H160v480Zm0 0v-480 480Z",
  folder_open = "M160-160q-33 0-56.5-23.5T80-240v-480q0-33 23.5-56.5T160-800h240l80 80h320q33 0 56.5 23.5T880-640H447l-80-80H160v480l96-320h684L837-217q-8 26-29.5 41.5T760-160H160Zm84-80h516l72-240H316l-72 240Zm0 0 72-240-72 240Zm-84-400v-80 80Z"
)
cv_icon <- function(name, class = "cv-ico") {
  tags$span(class = class, `aria-hidden` = "true",
    HTML(paste0('<svg viewBox="0 -960 960 960" fill="currentColor" aria-hidden="true"><path d="', cv_icon_paths[[name]], '"></path></svg>')))
}

cv_size <- function(bytes) {
  bytes <- sum(bytes, na.rm = TRUE)
  if (bytes >= 1024^3) return(sprintf("%.1f GB", bytes / 1024^3))
  if (bytes >= 1024^2) return(sprintf("%.1f MB", bytes / 1024^2))
  if (bytes < 1024) return(paste(bytes, if (bytes == 1) "byte" else "bytes"))
  paste0(round(bytes / 1024), " KB")
}

# Convert dialog script: drops onto the dialog, the Choose and Remove
# buttons, and the state of the two actions
cv_dialog_script <- function(ns_prefix) {
  js <- "
(function () {
  if (window.cvDialogReady) return;
  window.cvDialogReady = true;
  var NS = '__NS__';
  var pendingFocus = null, lastPick = null;
  function dlg() { return document.querySelector('.cv-dlg'); }
  // The dim page around the dialog counts as the dialog, so a drop that misses
  // it is still listed rather than opened by the browser
  function inDialog(e) {
    var d = dlg(), m = document.getElementById('shiny-modal');
    return d && m && e.target && m.contains(e.target) ? d : null;
  }
  function hasFiles(e) {
    var t = e.dataTransfer && e.dataTransfer.types;
    return !!t && Array.prototype.indexOf.call(t, 'Files') >= 0;
  }

  // Whether either input is uploading, and how far it has got
  function uploads(d) {
    var busy = false, pct = 0;
    d.querySelectorAll('.cv-inputs .shiny-file-input-progress').forEach(function (p) {
      var bar = p.querySelector('.progress-bar');
      if (!bar || p.style.visibility !== 'visible') return;
      if (bar.classList.contains('progress-bar-danger') || bar.textContent === 'Upload complete') return;
      busy = true;
      pct = Math.max(pct, parseFloat(bar.style.width) || 0);
    });
    return { busy: busy, pct: pct };
  }
  function setOff(b, off) {
    if (b.classList.contains('is-off') !== off) b.classList.toggle('is-off', off);
    var aria = off ? 'true' : 'false';
    if (b.getAttribute('aria-disabled') !== aria) b.setAttribute('aria-disabled', aria);
  }

  // The actions wait for a .gt3x file and for any upload to finish. The picks
  // wait for the upload too, since a new pick would cancel it, and files
  // dropped meanwhile are sent when it ends. An upload draws its progress on
  // the file area. The actions stay in the tab order while they wait:
  // Bootstrap fixes the modal's last tab stop when it opens. Shiny clears a
  // download link's aria-disabled when it binds, so this runs on every change;
  // each write compares first, or the observer calling it would loop.
  function sync() {
    var d = dlg();
    if (!d) return;
    var up = uploads(d);
    if (!up.busy && d.cvQueue && d.cvQueue.length) {
      var q = d.cvQueue;
      d.cvQueue = [];
      send(q);
      up = uploads(d);
    }
    var on = !!d.querySelector('.cv-list') && !up.busy;
    d.querySelectorAll('.cv-act').forEach(function (b) { setOff(b, !on); });
    d.querySelectorAll('[data-pick]').forEach(function (b) { setOff(b, up.busy); });
    // A Convert the server refused leaves no list, and the dialog takes clicks again
    if (!on && d.classList.contains('is-starting')) d.classList.remove('is-starting');
    // A name cut by its ellipsis gets the whole name as a title
    d.querySelectorAll('.cv-name').forEach(function (n) {
      var want = n.scrollWidth > n.clientWidth + 1 ? n.textContent : null;
      if (want && n.getAttribute('title') !== want) n.setAttribute('title', want);
      if (!want && n.hasAttribute('title')) n.removeAttribute('title');
    });
    var files = d.querySelector('.cv-files');
    if (files) {
      if (files.classList.contains('is-busy') !== up.busy) files.classList.toggle('is-busy', up.busy);
      var w = up.pct + '%';
      if (files.style.getPropertyValue('--cv-p') !== w) files.style.setProperty('--cv-p', w);
    }
    // After a removal the focus stays in the list, or goes back to Choose files
    if (pendingFocus && !d.contains(pendingFocus.el)) {
      var rms = d.querySelectorAll('.cv-rm');
      var t = rms.length ? rms[Math.min(pendingFocus.i, rms.length - 1)] : d.querySelector('.cv-drop [data-pick]');
      pendingFocus = null;
      if (t) t.focus();
    }
    // A pick redraws or hides the button it came from, and focus dropped to
    // the page body leaves Esc dead; it goes to the same pick, or the dialog
    var m = document.getElementById('shiny-modal');
    var a = document.activeElement;
    if (m && m.classList.contains('in') && (!a || a === document.body || (d.contains(a) && a.offsetParent === null))) {
      var twin = lastPick && Array.prototype.filter.call(d.querySelectorAll('[data-pick]'), function (b) {
        return b.getAttribute('data-pick') === lastPick && b.offsetParent !== null;
      })[0];
      (twin || m).focus();
    }
    if (m) {
      if (m.getAttribute('role') !== 'dialog') m.setAttribute('role', 'dialog');
      if (m.getAttribute('aria-modal') !== 'true') m.setAttribute('aria-modal', 'true');
      if (m.getAttribute('aria-labelledby') !== NS + 'convert_title') m.setAttribute('aria-labelledby', NS + 'convert_title');
    }
  }
  function watch() {
    var w = document.getElementById('shiny-modal-wrapper');
    if (!w || w.cvWatched) return;
    w.cvWatched = true;
    new MutationObserver(sync).observe(w, { subtree: true, childList: true, characterData: true,
      attributes: true, attributeFilter: ['class', 'style', 'tabindex', 'aria-disabled', 'disabled'] });
    sync();
  }
  new MutationObserver(watch).observe(document.body, { childList: true });
  watch();

  document.addEventListener('click', function (e) {
    var el = e.target;
    if (!el || !el.closest || !el.closest('.cv-dlg')) return;
    if (el.closest('.cv-dlg.is-starting') || el.closest('.cv-act.is-off, [data-pick].is-off')) { e.preventDefault(); e.stopPropagation(); return; }
    // Starting the conversions takes about a second; the dialog shows it and
    // takes no second click until the server closes it
    var go = document.getElementById(NS + 'convert_load');
    if (go && go.contains(el)) { el.closest('.cv-dlg').classList.add('is-starting'); return; }
    // The session converts before the download starts; one at a time
    var dl = document.getElementById(NS + 'download_agd');
    if (dl && dl.contains(el)) {
      if (dl.cvGoing) { e.preventDefault(); e.stopPropagation(); return; }
      dl.cvGoing = true;
      dl.setAttribute('aria-busy', 'true');
      return;
    }
    var pick = el.closest('[data-pick]');
    if (pick) {
      var input = document.getElementById(pick.getAttribute('data-pick'));
      if (input) input.click();
      return;
    }
    var rm = el.closest('.cv-rm');
    if (rm) {
      var all = Array.prototype.slice.call(document.querySelectorAll('.cv-dlg .cv-rm'));
      pendingFocus = { el: rm, i: all.indexOf(rm) };
      Shiny.setInputValue(NS + 'convert_remove', rm.getAttribute('data-key'), { priority: 'event' });
    }
  }, true);
  document.addEventListener('focusin', function (e) {
    var t = e.target && e.target.closest && e.target.closest('.cv-dlg [data-pick]');
    if (t) lastPick = t.getAttribute('data-pick');
    else if (e.target && e.target.closest && e.target.closest('.cv-dlg')) lastPick = null;
  });
  // Shiny takes a handler of exactly one argument
  Shiny.addCustomMessageHandler('cv-download-done', function (msg) {
    var dl = document.getElementById(NS + 'download_agd');
    if (dl) { dl.cvGoing = false; dl.removeAttribute('aria-busy'); }
  });
  // An upload still running when the dialog closes would land in the next one
  $(document).on('hide.bs.modal', function (e) {
    var d = e.target.id === 'shiny-modal' && e.target.querySelector('.cv-dlg');
    if (!d) return;
    d.cvQueue = [];
    ['convert_gt3x', 'convert_dir'].forEach(function (id) {
      var el = document.getElementById(NS + id), u = el && $(el).data('currentUploader');
      if (u) u.abort();
    });
  });

  // A drop goes to the files input, as Shiny's own drop does. A dropped
  // folder is walked for its files.
  function collect(dt) {
    var items = dt.items ? Array.prototype.slice.call(dt.items) : [];
    var entries = items.map(function (it) { return it.webkitGetAsEntry ? it.webkitGetAsEntry() : null; });
    var files = Array.prototype.slice.call(dt.files || []);
    if (!entries.some(function (en) { return en && en.isDirectory; })) return Promise.resolve(files);
    var out = [];
    function walk(en) {
      if (!en) return Promise.resolve();
      if (en.isFile) return new Promise(function (res) { en.file(function (f) { out.push(f); res(); }, function () { res(); }); });
      var reader = en.createReader();
      return new Promise(function (res) {
        (function more() {
          reader.readEntries(function (list) {
            if (!list.length) return res();
            Promise.all(list.map(walk)).then(more);
          }, function () { res(); });
        })();
      });
    }
    return Promise.all(entries.map(walk)).then(function () { return out; });
  }
  function send(files) {
    var d = dlg(), input = document.getElementById(NS + 'convert_gt3x');
    if (!d || !input || !files.length) return;
    if (uploads(d).busy) { d.cvQueue = (d.cvQueue || []).concat(files); return; }
    var dt = new DataTransfer();
    files.forEach(function (f) { dt.items.add(f); });
    input.files = dt.files;
    $(input).trigger('change');
  }
  var depth = 0;
  function over(d, on) {
    var f = d && d.querySelector('.cv-files');
    if (f && f.classList.contains('is-over') !== on) f.classList.toggle('is-over', on);
  }
  document.addEventListener('dragenter', function (e) {
    var d = inDialog(e);
    if (!d || !hasFiles(e)) return;
    depth++;
    over(d, true);
  });
  document.addEventListener('dragleave', function (e) {
    var d = inDialog(e);
    if (!d || !hasFiles(e)) return;
    depth = Math.max(0, depth - 1);
    if (depth === 0) over(d, false);
  });
  document.addEventListener('dragover', function (e) {
    if (!inDialog(e) || !hasFiles(e)) return;
    e.preventDefault();
    e.dataTransfer.dropEffect = 'copy';
  });
  function endDrag() { depth = 0; over(dlg(), false); }
  document.addEventListener('drop', function (e) {
    var d = inDialog(e), inside = d && hasFiles(e);
    endDrag();
    if (!inside) return;
    e.preventDefault();
    if (d.classList.contains('is-starting')) return;
    collect(e.dataTransfer).then(send);
  });
  document.addEventListener('dragend', endDrag);
  window.addEventListener('blur', endDrag);
})();
"
  gsub("__NS__", ns_prefix, js, fixed = TRUE)
}

# Open from disk dialog script: the picks, the path box, removal and the
# state of Open. The server holds the list; this only sends it paths.
od_dialog_script <- function(ns_prefix) {
  js <- "
(function () {
  if (window.odDialogReady) return;
  window.odDialogReady = true;
  var NS = '__NS__';
  var pendingFocus = null, lastPick = null, sent = null;
  function dlg() { return document.querySelector('.od-dlg'); }
  function setOff(b, off) {
    if (b.classList.contains('is-off') !== off) b.classList.toggle('is-off', off);
    var aria = off ? 'true' : 'false';
    if (b.getAttribute('aria-disabled') !== aria) b.setAttribute('aria-disabled', aria);
  }

  // Open waits for the list and Add for a path. A redraw that hides or
  // removes the focused button moves the focus to its twin, the path box or
  // the dialog, so Esc and Tab keep working.
  function sync() {
    var d = dlg();
    if (!d) return;
    var listed = !!d.querySelector('.cv-list');
    var go = document.getElementById(NS + 'inplace_go');
    if (go) setOff(go, !listed);
    var box = d.querySelector('.od-path-in'), add = d.querySelector('.od-path-add');
    if (box && add) setOff(add, !box.value.trim());
    if (!listed && d.classList.contains('is-starting')) d.classList.remove('is-starting');
    if (pendingFocus && !d.contains(pendingFocus.el)) {
      var rms = d.querySelectorAll('.cv-rm');
      var t = rms.length ? rms[Math.min(pendingFocus.i, rms.length - 1)] : (d.querySelector('.od-empty [data-od-pick]') || box);
      pendingFocus = null;
      if (t) t.focus();
    }
    var m = document.getElementById('shiny-modal'), a = document.activeElement;
    if (m && m.classList.contains('in') && (!a || a === document.body || (d.contains(a) && a.offsetParent === null))) {
      var twin = lastPick && Array.prototype.filter.call(d.querySelectorAll('[data-od-pick]'), function (b) {
        return b.getAttribute('data-od-pick') === lastPick && b.offsetParent !== null;
      })[0];
      (twin || m).focus();
    }
    if (m) {
      if (m.getAttribute('role') !== 'dialog') m.setAttribute('role', 'dialog');
      if (m.getAttribute('aria-modal') !== 'true') m.setAttribute('aria-modal', 'true');
      if (m.getAttribute('aria-labelledby') !== NS + 'inplace_title') m.setAttribute('aria-labelledby', NS + 'inplace_title');
    }
  }
  function watch() {
    var w = document.getElementById('shiny-modal-wrapper');
    if (!w || w.odWatched) return;
    w.odWatched = true;
    new MutationObserver(sync).observe(w, { subtree: true, childList: true, characterData: true,
      attributes: true, attributeFilter: ['class', 'style', 'aria-disabled'] });
    sync();
  }
  new MutationObserver(watch).observe(document.body, { childList: true });
  watch();

  function submit(d) {
    var box = d && d.querySelector('.od-path-in');
    var v = box ? box.value.trim() : '';
    if (!v || d.classList.contains('is-starting') || d.classList.contains('is-picking')) return;
    sent = v;
    Shiny.setInputValue(NS + 'inplace_add', v, { priority: 'event' });
  }

  // While a native picker is open the session waits for it, so the dialog
  // takes no other pick; the close x still works
  document.addEventListener('click', function (e) {
    var el = e.target, d = el && el.closest && el.closest('.od-dlg');
    if (!d) return;
    var busy = d.classList.contains('is-starting') || (d.classList.contains('is-picking') && !el.closest('.cv-head, [data-dismiss]'));
    if (busy || el.closest('.is-off')) { e.preventDefault(); e.stopPropagation(); return; }
    var go = document.getElementById(NS + 'inplace_go');
    if (go && go.contains(el)) { d.classList.add('is-starting'); return; }
    var pick = el.closest('[data-od-pick]');
    if (pick) {
      d.classList.add('is-picking');
      Shiny.setInputValue(NS + 'inplace_pick', pick.getAttribute('data-od-pick'), { priority: 'event' });
      return;
    }
    if (el.closest('.od-path-add')) { submit(d); return; }
    var rm = el.closest('.cv-rm');
    if (rm) {
      var all = Array.prototype.slice.call(d.querySelectorAll('.cv-rm'));
      pendingFocus = { el: rm, i: all.indexOf(rm) };
      Shiny.setInputValue(NS + 'inplace_remove', rm.getAttribute('data-key'), { priority: 'event' });
    }
  }, true);
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Enter' || !e.target.matches || !e.target.matches('.od-dlg .od-path-in')) return;
    e.preventDefault();
    submit(dlg());
  });
  document.addEventListener('input', function (e) {
    if (!(e.target.matches && e.target.matches('.od-dlg .od-path-in'))) return;
    var d = dlg();
    if (d && d.querySelector('.od-msg')) Shiny.setInputValue(NS + 'inplace_edit', true, { priority: 'event' });
    sync();
  });
  document.addEventListener('focusin', function (e) {
    var t = e.target && e.target.closest && e.target.closest('.od-dlg [data-od-pick]');
    if (t) lastPick = t.getAttribute('data-od-pick');
    else if (e.target && e.target.closest && e.target.closest('.od-dlg')) lastPick = null;
  });

  // The path was added: the box empties unless something new was typed since
  Shiny.addCustomMessageHandler('od-path-done', function (msg) {
    var box = document.querySelector('.od-dlg .od-path-in');
    if (box && box.value.trim() === sent) box.value = '';
    if (box && document.activeElement && document.activeElement.matches('.od-path-add')) box.focus();
    sync();
  });
  Shiny.addCustomMessageHandler('od-idle', function (msg) {
    var d = dlg();
    if (d) d.classList.remove('is-picking');
  });

  // A drop gives no path, so the dialog takes none, and the browser does not
  // open the file over the app
  function onDialog(e) {
    var m = document.getElementById('shiny-modal');
    return !!(dlg() && m && e.target && m.contains(e.target));
  }
  document.addEventListener('dragover', function (e) {
    if (!onDialog(e)) return;
    e.preventDefault();
    if (e.dataTransfer) e.dataTransfer.dropEffect = 'none';
  });
  document.addEventListener('drop', function (e) { if (onDialog(e)) e.preventDefault(); });
})();
"
  gsub("__NS__", ns_prefix, js, fixed = TRUE)
}

# Helpers at file scope, shared with overview_loaded.R

# First non-empty setting among the names given, or NA.
get_setting <- function(settings, ...) {
  if (is.null(settings) || !is.data.frame(settings)) return(NA_character_)
  for (name in c(...)) {
    value <- settings$settingValue[tolower(settings$settingName) == tolower(name)]
    if (length(value) > 0) {
      value <- as.character(value[1])
      if (!is.na(value) && nzchar(value) && value != "0") return(value)
    }
  }
  NA_character_
}

agd_time <- function(ticks) {
  n <- suppressWarnings(as.numeric(ticks))
  if (length(n) == 0 || is.na(n[1])) return(NULL)
  as.POSIXct(n[1] / 1e7 - 62135596800, origin = "1970-01-01", tz = "UTC")
}

present <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- x[1]
  if (is.na(x) || !nzchar(trimws(as.character(x)))) return(NULL)
  x
}

format_sex <- function(sex) {
  sex <- present(sex)
  if (is.null(sex)) return(NULL)
  key <- tolower(as.character(sex))
  if (key %in% c("male", "m", "1")) return("Male")
  if (key %in% c("female", "f", "2")) return("Female")
  if (key %in% c("undefined", "unknown", "n/a", "na")) return(NULL)
  as.character(sex)
}

fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_dec <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
plural  <- function(n, word) paste0(n, " ", word, if (n == 1) "" else "s")

# English day, month and AM/PM names whatever the locale
fmt_date <- function(x, fmt) {
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  format(x, format = fmt)
}

month_day <- function(t) paste(as.integer(format(t, "%d")), fmt_date(t, "%b"))
date_span <- function(t0, t1) {
  y0 <- format(t0, "%Y"); y1 <- format(t1, "%Y")
  m0 <- format(t0, "%m"); m1 <- format(t1, "%m")
  if (y0 == y1 && m0 == m1) {
    paste(as.integer(format(t0, "%d")), "to", as.integer(format(t1, "%d")), fmt_date(t1, "%b %Y"))
  } else if (y0 == y1) {
    paste(month_day(t0), "to", month_day(t1), y1)
  } else {
    paste(month_day(t0), y0, "to", month_day(t1), y1)
  }
}

# The sort caret of both tables' headers
caret_glyph <- function(up = TRUE) {
  d <- if (isTRUE(up)) "M5 2.5 8.5 7h-7z" else "M5 7.5 1.5 3h7z"
  HTML(paste0('<svg class="ov-caret" viewBox="0 0 10 10" fill="currentColor" aria-hidden="true"><path d="', d, '"></path></svg>'))
}

mod_overview_server <- function(id, shared, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local <- reactiveValues(
      next_id = 1,
      panel_tab = "files",     # Files or Details, the two tabs of the workbench panel
      failed = list(),          # files that could not be read: id, name, error
      running = character(0),   # names of .gt3x files converting right now
      selected_failed = NULL,   # id of the selected unreadable row, if any
      notice = NULL,            # transient status line: text, undo flag, time stamp
      upload = NULL,            # the file uploading now: i of n, and its name
      sort_key = "name",        # Files tab order: name, subject, worn or in-bed
      sort_dir = 1,             # 1 ascending, -1 descending
      epoch_page = 1,           # which hundred epochs the Details tab is showing

      # The raw interface's own tab, selection and view state; view only
      # matters when both kinds of file are loaded
      view = "counts",
      raw_tab = "files",        # Recordings, Data quality or Details
      raw_chip = "overview",    # which of the six figures the quality tab shows
      raw_sel = NULL,           # id of the raw recording in focus
      raw_checks_open = FALSE,  # the checks strip along the bottom of the panel
      raw_epoch_page = 1,
      raw_started = NULL,       # when the running read began, for the elapsed clock
      raw_size_mb = NA_real_,   # its size, which is what the time estimate is built from
      raw_prog = NULL,          # file the background worker appends its stage lines to
      raw_series = "imputed",   # the epoch preview: imputed (what parts 3-5 read) or measured
      raw_sort = "name",        # Recordings order: one of OVR_SORT_KEYS
      raw_sort_dir = 1,
      raw_running = character(0),
      raw_failed = list()
    )
    undo_store <- NULL          # what the last removal took away, for Undo


    # both: the raw interface shows it too
    set_notice <- function(text, undo = FALSE, both = FALSE) {
      local$notice <- list(text = text, undo = undo, both = both, stamp = Sys.time())
    }

    observe({
      n <- local$notice
      if (is.null(n)) return()
      remaining <- 8 - as.numeric(difftime(Sys.time(), n$stamp, units = "secs"))
      if (remaining <= 0) local$notice <- NULL else invalidateLater(ceiling(remaining * 1000) + 50)
    })

    # Reading a file
    load_single_file <- function(file_path, file_name) {
      ext <- tolower(tools::file_ext(file_name))

      file_size_mb <- file.info(file_path)$size / (1024 * 1024)
      if (!is.na(file_size_mb) && file_size_mb > 100) {
        warning("Large file detected (", round(file_size_mb, 1), " MB): ",
                file_name, ". Processing may take longer and use significant memory.")
      }

      result <- tryCatch({
        if (ext == "agd") {
          canhrActi::read.agd(file_path, verbose = FALSE)
        } else {
          stop("Unsupported file type: ", ext, ". Only .agd files are read directly.")
        }
      }, error = function(e) list(error = conditionMessage(e)))

      if (!is.null(result$error)) {
        return(list(success = FALSE, error = result$error))
      }

      sleep_data <- NULL; awakenings_data <- NULL; wear_time_data <- NULL; capsense_data <- NULL

      if (is.list(result) && "data" %in% names(result)) {
        raw_data <- result$data
        settings <- result$settings
        if (!is.null(result$sleep)) sleep_data <- result$sleep
        if (!is.null(result$awakenings)) awakenings_data <- result$awakenings
        if (!is.null(result$wear_time)) wear_time_data <- result$wear_time
        if (!is.null(result$capsense)) capsense_data <- result$capsense
      } else {
        raw_data <- result
        settings <- NULL
      }

      if ("dataTimestamp" %in% names(raw_data)) {
        a1 <- if ("axis1" %in% names(raw_data)) raw_data$axis1 else NA
        a2 <- if ("axis2" %in% names(raw_data)) raw_data$axis2 else NA
        a3 <- if ("axis3" %in% names(raw_data)) raw_data$axis3 else NA

        vm <- if (!all(is.na(a1)) && !all(is.na(a2)) && !all(is.na(a3))) {
          round(sqrt(a1^2 + a2^2 + a3^2), 1)
        } else {
          NA
        }

        data <- data.frame(
          timestamp = as.POSIXct((raw_data$dataTimestamp / 10000000 - 62135596800),
                                 origin = "1970-01-01", tz = "UTC"),
          axis1 = a1,
          axis2 = a2,
          axis3 = a3,
          vector_magnitude = vm,
          steps = if ("steps" %in% names(raw_data)) raw_data$steps else NA,
          lux = if ("lux" %in% names(raw_data)) raw_data$lux else NA,
          inclineOff = if ("inclineOff" %in% names(raw_data)) raw_data$inclineOff else NA,
          inclineStanding = if ("inclineStanding" %in% names(raw_data)) raw_data$inclineStanding else NA,
          inclineSitting = if ("inclineSitting" %in% names(raw_data)) raw_data$inclineSitting else NA,
          inclineLying = if ("inclineLying" %in% names(raw_data)) raw_data$inclineLying else NA,
          stringsAsFactors = FALSE
        )
        data <- data[, colSums(!is.na(data)) > 0]

      } else if ("timestamp" %in% names(raw_data)) {
        if (all(c("axis1", "axis2", "axis3") %in% names(raw_data)) &&
            !"vector_magnitude" %in% names(raw_data)) {
          raw_data$vector_magnitude <- round(sqrt(raw_data$axis1^2 + raw_data$axis2^2 + raw_data$axis3^2), 1)
        }
        data <- raw_data
      } else {
        data <- raw_data
      }

      epoch_len <- as.numeric(get_setting(settings, "epochlength"))
      if (is.na(epoch_len) && "timestamp" %in% names(data) && nrow(data) > 1) {
        epoch_len <- round(as.numeric(difftime(data$timestamp[2], data$timestamp[1], units = "secs")))
      }
      if (is.na(epoch_len)) epoch_len <- 60

      duration_hrs <- NA
      if ("timestamp" %in% names(data) && nrow(data) > 0) {
        duration_hrs <- as.numeric(difftime(max(data$timestamp), min(data$timestamp), units = "hours"))
      }

      # ActiLife writes "devicename", "deviceversion" and "original sample rate";
      # the older names are fallbacks for other exporters
      device_info <- list(
        device_type = get_setting(settings, "devicename", "devicetype"),
        serial_number = get_setting(settings, "deviceserial"),
        firmware = get_setting(settings, "deviceversion", "firmwareversion"),
        battery = get_setting(settings, "batteryvoltage"),
        filter = get_setting(settings, "filter"),
        software = get_setting(settings, "softwarename"),
        software_version = get_setting(settings, "softwareversion"),
        epoch_length = epoch_len,
        start_datetime = get_setting(settings, "startdatetime"),
        stop_datetime = get_setting(settings, "stopdatetime"),
        download_datetime = get_setting(settings, "downloaddatetime"),
        sample_rate = get_setting(settings, "original sample rate", "samplerate"),
        acceleration_scale = get_setting(settings, "accelerationscale"),
        acceleration_min = get_setting(settings, "accelerationmin"),
        acceleration_max = get_setting(settings, "accelerationmax"),
        modes = get_setting(settings, "modesstring")
      )

      mass_val <- get_setting(settings, "mass")
      mass_kg <- suppressWarnings(as.numeric(mass_val))
      weight_lbs_val <- if (!is.na(mass_kg) && mass_kg > 0) round(mass_kg * 2.20462) else 0

      subject_info <- list(
        id = get_setting(settings, "subjectname"),
        sex = get_setting(settings, "sex"),
        age = get_setting(settings, "age"),
        date_of_birth = get_setting(settings, "dateofbirth"),
        height = get_setting(settings, "height"),
        mass = mass_val,
        weight_lbs = weight_lbs_val,
        limb = get_setting(settings, "limb"),
        side = get_setting(settings, "side"),
        dominance = get_setting(settings, "dominance"),
        race = get_setting(settings, "race")
      )

      # read.gt3x returns the label itself for an empty "Subject Name:" line
      if (is.na(subject_info$id) || subject_info$id %in% c("", "Subject Name")) {
        subject_info$id <- tools::file_path_sans_ext(file_name)
      }

      list(
        success = TRUE,
        data = data,
        settings = settings,
        device_info = device_info,
        subject_info = subject_info,
        epoch_length = epoch_len,
        duration_hrs = duration_hrs,
        n_epochs = nrow(data),
        actilife_sleep = sleep_data,
        actilife_awakenings = awakenings_data,
        actilife_wear_time = wear_time_data,
        capsense = capsense_data
      )
    }

    # A file that read cleanly joins shared$files; one that did not is kept locally
    add_file_record <- function(result, display_name, original_path, is_sample = FALSE) {
      if (!isTRUE(result$success)) {
        fid <- paste0("failed_", local$next_id)
        local$next_id <- local$next_id + 1
        local$failed[[fid]] <- list(id = fid, name = display_name, error = result$error %||% "unknown error")
        return(invisible(FALSE))
      }
      file_id <- paste0("file_", local$next_id)
      shared$files[[file_id]] <- list(
        id = file_id, name = display_name, original_path = original_path, is_sample = is_sample,
        data = result$data, settings = result$settings,
        device_info = result$device_info, subject_info = result$subject_info,
        epoch_length = result$epoch_length, duration_hrs = result$duration_hrs,
        n_epochs = result$n_epochs, actilife_sleep = result$actilife_sleep,
        actilife_awakenings = result$actilife_awakenings,
        actilife_wear_time = result$actilife_wear_time, capsense = result$capsense
      )
      local$next_id <- local$next_id + 1
      shared$file_count <- length(shared$files)
      shared$data_loaded <- TRUE
      if (is.null(shared$selected_file)) shared$selected_file <- file_id
      invisible(TRUE)
    }

    # Converting .gt3x: a content-hash cache and a background pool
    gt3x_cache_dir <- file.path(tempdir(), "canhrActi_gt3x_cache")
    dir.create(gt3x_cache_dir, showWarnings = FALSE, recursive = TRUE)

    # A file over 50 MB converts alone; a conversion holds the whole signal in memory
    POOL_N   <- 2
    LARGE_MB <- 50
    conv_queue <- reactiveVal(list())
    pool <- lapply(seq_len(POOL_N), function(i) {
      ExtendedTask$new(function(datapath, out_agd, epoch, lfe, libpaths, name, orig) {
        promises::catch(
          promises::future_promise({
            .libPaths(libpaths)
            # Files with the same content share one cache file and can finish
            # together, so each worker writes its own copy and moves it in
            part <- paste0(out_agd, ".", Sys.getpid(), ".part")
            tryCatch({
              canhrActi::gt3x.to.agd(datapath, agd_path = part, epoch = epoch, lfe = lfe)
              # A rename fails while the session reads a copy that landed first
              if (!file.rename(part, out_agd) && !file.exists(out_agd)) {
                stop("the converted file could not be saved")
              }
            }, finally = unlink(part))
            list(ok = TRUE, agd = out_agd, name = name, orig = orig, epoch = epoch)
          },
          # Named, because globals found by search keep the frame they came
          # from, and with it this session's environment; sending that to the
          # worker held the session about 5 s per file
          globals = list(datapath = datapath, out_agd = out_agd, epoch = epoch, lfe = lfe,
                         libpaths = libpaths, name = name, orig = orig),
          seed = TRUE),
          function(e) list(ok = FALSE, name = name, error = conditionMessage(e))
        )
      })
    })
    pool_large <- reactiveVal(rep(FALSE, POOL_N))
    # The file in each slot, for a task that fails before it can return its name
    pool_name <- rep(NA_character_, POOL_N)

    pump_pool <- function() isolate({
      repeat {
        q <- conv_queue()
        if (length(q) == 0) break
        busy <- vapply(pool, function(t) t$status() == "running", logical(1))
        if (any(busy & pool_large())) break
        job  <- q[[1]]
        if (job$large && any(busy)) break
        free <- which(!busy)[1]
        if (is.na(free)) break
        conv_queue(q[-1])
        pl <- pool_large(); pl[free] <- job$large; pool_large(pl)
        local$running <- c(local$running, job$name)
        pool_name[free] <<- job$name
        pool[[free]]$invoke(job$datapath, job$cached, job$epoch, job$lfe, .libPaths(), job$name, job$datapath)
        if (job$large) break
      }
    })

    lapply(seq_len(POOL_N), function(i) {
      observeEvent(pool[[i]]$status(), {
        st <- pool[[i]]$status()
        if (!st %in% c("success", "error")) return()
        # result() re-throws what stopped the task, such as future_promise() without future
        r <- if (identical(st, "error")) {
          list(ok = FALSE, name = pool_name[i],
               error = tryCatch({ pool[[i]]$result(); "the conversion stopped without saying why" },
                                error = function(e) conditionMessage(e)))
        } else {
          pool[[i]]$result()
        }
        isolate({
          pl <- pool_large(); pl[i] <- FALSE; pool_large(pl)
          local$running <- setdiff(local$running, r$name)
        })
        if (isTRUE(r$ok)) {
          add_file_record(load_single_file(r$agd, agd_name(r$name, r$epoch)), r$name, r$orig)
        } else {
          fid <- paste0("failed_", local$next_id)
          local$next_id <- local$next_id + 1
          local$failed[[fid]] <- list(id = fid, name = r$name, error = r$error %||% "conversion failed")
        }
        pump_pool()
      })
    })

    hash_memo <- new.env(parent = emptyenv())
    cache_path <- function(datapath, epoch, lfe) {
      h <- hash_memo[[datapath]]
      if (is.null(h)) {
        h <- digest::digest(file = datapath, algo = "xxhash64")
        hash_memo[[datapath]] <- h
      }
      file.path(gt3x_cache_dir, paste0(h, "_e", epoch, "_", if (lfe) "l" else "n", ".agd"))
    }

    ensure_agd <- function(datapath, epoch, lfe) {
      cached <- cache_path(datapath, epoch, lfe)
      if (!file.exists(cached)) {
        canhrActi::gt3x.to.agd(datapath, agd_path = cached, epoch = epoch, lfe = lfe)
      }
      cached
    }

    # The converted file's name; with an empty Subject Name the ID falls back
    # to it, as for an uploaded .agd
    agd_name <- function(nm, ep) sub("\\.gt3x$", sprintf("_%ssec.agd", ep), nm, ignore.case = TRUE)

    convert_and_load <- function(datapath, name, epoch = 60, lfe = FALSE) {
      cached <- cache_path(datapath, epoch, lfe)
      if (file.exists(cached)) {
        add_file_record(load_single_file(cached, agd_name(name, epoch)), name, datapath)
        return(invisible())
      }
      large <- isTRUE((file.info(datapath)$size / 1048576) > LARGE_MB)
      isolate({
        q <- conv_queue()
        q[[length(q) + 1]] <- list(datapath = datapath, cached = cached, name = name,
                                   epoch = epoch, lfe = lfe, large = large)
        conv_queue(q)
      })
      pump_pool()
    }

    # The extension picks the pipeline
    process_uploaded_file <- function(datapath, name) {
      ext <- tolower(tools::file_ext(name))
      if (ext %in% OV_RAW_EXT) {
        load_raw_file(datapath, name)
      } else {
        add_file_record(load_single_file(datapath, name), name, datapath)
      }
    }

    # The raw pipeline: one slot, since read.raw.accelerometer holds the whole
    # signal while it calibrates. The cache is keyed by file content and name
    # and kept under R_user_dir, so a read outlives the session and is shared
    # across copies of the same recording under the same name.
    raw_cache_dir <- tryCatch(
      tools::R_user_dir("canhrActi", "cache"),
      error = function(e) file.path(tempdir(), "canhrActi_raw_cache"))
    raw_cache_dir <- file.path(raw_cache_dir, "raw")
    if (!dir.create(raw_cache_dir, showWarnings = FALSE, recursive = TRUE) &&
        !dir.exists(raw_cache_dir)) {
      raw_cache_dir <- file.path(tempdir(), "canhrActi_raw_cache")
      dir.create(raw_cache_dir, showWarnings = FALSE, recursive = TRUE)
    }

    raw_queue <- reactiveVal(list())
    # Each read runs in a fresh R process that exits when it is done
    # (overview_reader.R), so progress goes through a file that the session tails
    raw_child <- new.env(parent = emptyenv())
    raw_task <- ExtendedTask$new(function(datapath, cached, libpaths, name, tz, prog, metrics) {
      raw_child_start(raw_child, list(datapath = datapath, cached = cached, name = name, tz = tz,
                                      prog = prog, metrics = metrics, libpaths = libpaths))
    })
    session$onSessionEnded(function() raw_child_stop(raw_child))

    # What the worker last said it was doing.
    raw_progress_line <- function() {
      p <- local$raw_prog
      if (is.null(p) || !file.exists(p)) return(NULL)
      ln <- tryCatch(utils::tail(readLines(p, warn = FALSE), 1), error = function(e) character(0))
      if (length(ln) == 0 || !nzchar(ln)) return(NULL)
      sub("^[^\t]*\t", "", ln)
    }

    raw_cache_path <- function(datapath, name) {
      h <- hash_memo[[datapath]]
      if (is.null(h)) {
        h <- digest::digest(file = datapath, algo = "xxhash64")
        hash_memo[[datapath]] <- h
      }
      # The metric set is part of the key: a recording read before ENMOa and
      # MAD were asked for does not carry them. So is the file name, which the
      # read carries as GGIR's ID and file name
      nk <- digest::digest(basename(name), algo = "xxhash64", serialize = FALSE)
      file.path(raw_cache_dir, paste0(h, "_", nk, "_raw_", RAW_READ_TAG, ".rds"))
    }

    add_raw_record <- function(res, display_name, original_path) {
      rid <- paste0("raw_", local$next_id)
      local$next_id <- local$next_id + 1
      # The pipeline records the path it was handed, a temp file for an upload
      if (is.list(res$file)) res$file$filename <- display_name else res$file <- display_name
      # Gap runs, found once here; the detector costs about 0.3 s per 118,800 epochs
      res$imputed_runs <- ovr_fill_runs(res)
      shared$raw[[rid]] <- res
      if (is.null(local$raw_sel)) local$raw_sel <- rid
      # Land on the interface that just gained a recording.
      local$view <- "raw"
      invisible(TRUE)
    }

    pump_raw <- function() isolate({
      if (raw_task$status() == "running") return()
      q <- raw_queue()
      if (length(q) == 0) return()
      # At most RAW_CHILD_MAX reads at once across sessions; the job waits in the queue
      if (!raw_child_slot(raw_child, function() withReactiveDomain(session, pump_raw()))) return()
      job <- q[[1]]
      raw_queue(q[-1])
      local$raw_running <- c(local$raw_running, job$name)
      local$raw_started <- Sys.time()
      local$raw_size_mb <- job$size_mb
      local$raw_prog <- paste0(job$cached, ".progress")
      unlink(local$raw_prog)
      raw_task$invoke(job$datapath, job$cached, .libPaths(), job$name, job$tz, local$raw_prog,
                      RAW_READ_METRICS)
    })

    # An upload arrives as a temp file, so the reader's messages name paths
    # like Rtmp.../0.gt3x; put the chosen name back
    raw_clean_msg <- function(msg, display) {
      if (is.null(msg) || !nzchar(msg)) return(msg)
      msg <- gsub("(?:[A-Za-z]:)?[\\\\/][^ ,;]*[\\\\/]", "", msg, perl = TRUE)
      msg <- gsub("\\b[0-9]+\\.(gt3x|cwa|bin|csv)\\b", display, msg, ignore.case = TRUE)
      gsub("[ ]{2,}", " ", trimws(msg))
    }

    # Clears the running list whatever happened to the job
    raw_finish <- function(name, err = NULL) {
      isolate({
        nm <- if (is.null(name) || !nzchar(name)) local$raw_running[1] else name
        local$raw_running <- setdiff(local$raw_running, nm)
        if (length(local$raw_running) == 0 && !is.null(local$raw_prog)) {
          unlink(local$raw_prog); local$raw_prog <- NULL
        }
        if (!is.null(err)) local$raw_failed[[nm]] <- list(name = nm, error = raw_clean_msg(err, nm))
      })
    }

    observeEvent(raw_task$status(), {
      st <- raw_task$status()
      if (!st %in% c("success", "error")) return()

      if (identical(st, "error")) {
        # result() re-throws why the reader process failed: it did not start or it died
        err <- tryCatch({ raw_task$result(); NULL }, error = function(e) e)
        msg <- if (is.null(err)) "the reader stopped without saying why" else conditionMessage(err)
        mb <- isolate(local$raw_size_mb)
        # Only a process that died mid-read can have run out of memory
        if (inherits(err, "raw_child_ended") && !is.null(mb) && is.finite(mb) && mb > 200) {
          msg <- paste0(msg, " This is a ", round(mb), " MB recording; the most likely cause ",
                        "is that the background worker ran out of memory.")
        }
        raw_finish(NULL, msg)
        pump_raw()
        return()
      }

      r <- raw_task$result()
      if (isTRUE(r$ok)) {
        res <- tryCatch(readRDS(r$cached), error = function(e) conditionMessage(e))
        if (is.character(res)) {
          raw_finish(r$name, paste("the result could not be read back:", res))
        } else {
          # The pipeline reports rather than throws, so check the result first
          why <- ovr_unusable(res)
          if (!is.null(why)) {
            unlink(r$cached)   # do not serve this back from cache next time
            raw_finish(r$name, why)
          } else {
            raw_finish(r$name)
            add_raw_record(res, r$name, r$cached)
          }
        }
      } else {
        raw_finish(r$name, r$error %||% "the pipeline failed")
      }
      pump_raw()
    })


    load_raw_file <- function(datapath, name) {
      cached <- raw_cache_path(datapath, name)
      if (file.exists(cached)) {
        res <- tryCatch(readRDS(cached), error = function(e) NULL)
        if (!is.null(res)) {
          why <- ovr_unusable(res)
          if (is.null(why)) return(invisible(add_raw_record(res, name, datapath)))
          unlink(cached)
          local$raw_failed[[name]] <- list(name = name, error = raw_clean_msg(why, name))
          return(invisible(FALSE))
        }
      }
      tz <- tryCatch(Sys.timezone(), error = function(e) "UTC")
      size_mb <- tryCatch(file.info(datapath)$size / 1048576, error = function(e) NA_real_)
      isolate({
        q <- raw_queue()
        q[[length(q) + 1]] <- list(datapath = datapath, cached = cached, name = name,
                                   tz = tz, size_mb = size_mb)
        raw_queue(q)
      })
      pump_raw()
      invisible(TRUE)
    }

    load_batch <- function(files, message = "Reading files") {
      n <- nrow(files)
      withProgress(message = message, value = 0, {
        for (i in seq_len(n)) {
          setProgress(value = i / n, detail = files$name[i])
          process_uploaded_file(files$datapath[i], files$name[i])
        }
      })
    }

    # Adding files. The page sends one file per upload and waits for the ack,
    # which goes once the file is in its pipeline
    upload_ack <- function() session$sendCustomMessage("ov-upload-ack", TRUE)

    observeEvent(input$files, {
      req(input$files)
      on.exit(upload_ack())
      load_batch(input$files)
    })

    observeEvent(input$dir_files, {
      req(input$dir_files)
      on.exit(upload_ack())
      all_files <- input$dir_files
      keep <- tolower(tools::file_ext(all_files$name)) %in% c(OV_COUNTS_EXT, OV_RAW_EXT)
      skipped <- sum(!keep)
      if (!any(keep)) {
        set_notice(paste0("Nothing readable in that folder. Looking for ",
                          paste(OV_ACCEPT, collapse = ", "), "."))
        return()
      }
      load_batch(all_files[keep, , drop = FALSE], "Reading files from the folder")
      if (skipped > 0) {
        set_notice(paste0(plural(skipped, "file"), " skipped. The app reads ",
                          paste(OV_ACCEPT, collapse = ", "), "."))
      }
    })

    # The page reports each upload as it starts and, when the queue is empty,
    # what it sent, what it skipped and what did not upload
    observeEvent(input$upload, {
      u <- input$upload
      if (!isTRUE(u$done)) {
        i <- suppressWarnings(as.integer(u$i)[1]); n <- suppressWarnings(as.integer(u$n)[1])
        if (!is.na(i) && !is.na(n) && n >= 1) {
          local$upload <- list(i = i, n = n, name = as.character(u$name %||% "")[1])
        }
        return()
      }
      local$upload <- NULL
      sent <- suppressWarnings(as.integer(u$sent %||% 0)[1])
      skipped <- suppressWarnings(as.integer(u$skipped %||% 0)[1])
      failed <- as.character(unlist(u$failed))
      accept <- paste(OV_ACCEPT, collapse = ", ")
      msg <- c(
        if (isTRUE(skipped > 0) && identical(sent, 0L) && length(failed) == 0 && isTRUE(u$folders))
          paste0("Nothing readable in that folder. Looking for ", accept, ".")
        else if (isTRUE(skipped > 0))
          paste0(plural(skipped, "file"), " skipped. The app reads ", accept, "."),
        if (length(failed) == 1) paste0(failed, " did not upload.")
        else if (length(failed) > 1) paste0(plural(length(failed), "file"), " did not upload.")
      )
      if (length(msg) > 0) set_notice(paste(msg, collapse = " "), both = TRUE)
    })

    # The page button and the sidebar row are separate inputs so both can sit in the DOM
    load_samples <- function() {
      data_dir <- "data"
      example_files <- list.files(data_dir, pattern = "\\.agd$", full.names = FALSE)
      # Raw samples go down the raw path
      raw_samples <- list.files(data_dir, pattern = "\\.(gt3x|cwa|bin)$", full.names = FALSE)
      for (nm in raw_samples) load_raw_file(file.path(data_dir, nm), nm)
      if (length(example_files) == 0) {
        if (length(raw_samples) == 0)
          set_notice("The sample files are not installed with this copy of the dashboard.")
        return()
      }
      withProgress(message = "Reading sample files", value = 0, {
        for (i in seq_along(example_files)) {
          setProgress(value = i / length(example_files), detail = example_files[i])
          path <- file.path(data_dir, example_files[i])
          add_file_record(load_single_file(path, example_files[i]), example_files[i], path, is_sample = TRUE)
        }
      })
    }

    observeEvent(input$demo_btn, load_samples(), ignoreInit = TRUE)
    observeEvent(input$side_demo, load_samples(), ignoreInit = TRUE)

    # First-run status line
    output$empty_status <- renderUI(status_line())

    # Convert .gt3x to .agd: the files, the two settings, the actions. Every
    # file handed to the dialog, .gt3x or not, is kept with a key for removal.
    conv_picked <- reactiveVal(NULL)
    conv_key <- 0

    open_convert_dialog <- function() {
      conv_picked(NULL)
      showModal(modalDialog(
        title = NULL, footer = NULL, size = "m", easyClose = TRUE,
        tags$div(
          class = "cv-dlg",
          tags$div(
            class = "cv-head",
            tags$h2(class = "cv-title", id = ns("convert_title"), "Convert .gt3x to .agd"),
            tags$button(type = "button", class = "cv-x", `data-dismiss` = "modal",
                        `aria-label` = "Close", cv_icon("close"))
          ),
          tags$div(
            class = "cv-body",
            tags$div(
              class = "cv-inputs", `aria-hidden` = "true",
              # The buttons open these, so they stay out of the tab order
              tagAppendAttributes(
                fileInput(ns("convert_gt3x"), NULL, accept = ".gt3x", multiple = TRUE,
                          buttonLabel = "", placeholder = ""),
                tabindex = "-1", .cssSelector = "input"),
              tags$input(type = "file", id = ns("convert_dir"), webkitdirectory = NA, multiple = NA,
                         accept = ".gt3x", tabindex = "-1", `aria-hidden` = "true"),
              # Shiny draws an upload's progress into <id>_progress when there is one
              tags$div(id = ns("convert_dir_progress"), class = "progress shiny-file-input-progress",
                       tags$div(class = "progress-bar"))
            ),
            tags$div(
              class = "cv-files",
              tags$div(
                class = "cv-drop",
                cv_icon("upload", "cv-ico cv-drop-ico"),
                tags$div(class = "cv-drop-t", "Drop .gt3x files here"),
                tags$div(
                  class = "cv-picks",
                  tags$button(type = "button", class = "cv-btn cv-btn--secondary",
                              `data-pick` = ns("convert_gt3x"), "Choose files"),
                  tags$button(type = "button", class = "cv-btn cv-btn--secondary",
                              `data-pick` = ns("convert_dir"), "Choose folder")
                )
              ),
              uiOutput(ns("convert_list"))
            ),
            uiOutput(ns("convert_skip")),
            tags$div(
              class = "cv-set",
              radioButtons(ns("convert_epoch"), "Epoch length", inline = TRUE, selected = "60",
                           choices = c("5 s" = "5", "10 s" = "10", "15 s" = "15", "30 s" = "30", "60 s" = "60")),
              radioButtons(ns("convert_filter"), "Filter", inline = TRUE, selected = "normal",
                           choices = c("Normal" = "normal", "Low-frequency extension" = "lfe"))
            )
          ),
          tags$div(
            class = "cv-foot",
            tags$button(type = "button", class = "cv-btn cv-btn--ghost", `data-dismiss` = "modal", "Cancel"),
            # Both wait for a .gt3x file; cv_dialog_script() lifts is-off
            tags$a(id = ns("download_agd"), class = "shiny-download-link cv-btn cv-btn--secondary cv-act is-off",
                   href = "", target = "_blank", download = NA, `aria-disabled` = "true",
                   "Download .agd"),
            tags$button(id = ns("convert_load"), type = "button", `aria-disabled` = "true",
                        class = "action-button cv-btn cv-btn--primary cv-act is-off", "Convert and analyze")
          )
        )
      ))
    }

    observeEvent(input$side_convert, open_convert_dialog(), ignoreInit = TRUE)

    # Open from disk: files and folders picked or pasted go into a list, and
    # Open hands their paths to load_batch, so nothing passes through the
    # browser. The native pickers are base R and Windows only.
    od_native <- function() .Platform$OS.type == "windows"
    od_items <- reactiveVal(list())
    od_skip <- reactiveVal(NULL)
    od_msg <- reactiveVal(NULL)
    od_key <- 0
    od_formats <- sub(", ([^,]*)$", " or \\1", paste(OV_ACCEPT, collapse = ", "))
    od_reads <- function(p) tolower(tools::file_ext(p)) %in% c(OV_COUNTS_EXT, OV_RAW_EXT)
    od_same <- function(p) if (od_native()) tolower(p) else p

    # NA when the picker cannot open, character(0) when it is cancelled
    native_pick <- function(kind = c("files", "dir")) {
      kind <- match.arg(kind)
      if (!od_native()) return(NA_character_)
      tryCatch({
        if (kind == "files") {
          filt <- matrix(c("Accelerometer recordings",
                           paste0("*.", c(OV_COUNTS_EXT, OV_RAW_EXT), collapse = ";"),
                           "All files", "*.*"), ncol = 2, byrow = TRUE)
          utils::choose.files(caption = "Choose recordings", multi = TRUE, filters = filt)
        } else {
          d <- utils::choose.dir(caption = "Choose a folder of recordings")
          if (is.na(d)) character(0) else d
        }
      }, error = function(e) NA_character_)
    }

    inplace_frame <- function(paths) {
      paths <- gsub("\\\\", "/", paths)
      paths <- paths[nzchar(paths) & file.exists(paths) & !dir.exists(paths)]
      data.frame(datapath = paths, name = basename(paths), stringsAsFactors = FALSE)
    }

    # A folder is one row for the recordings directly in it; its other files
    # and any other file given are listed as skipped. TRUE when every path
    # was taken.
    # Every file under a folder, subfolders included, as GGIR reads a datadir;
    # NULL past OD_WALK_MAX entries, so a whole drive picked by mistake stops fast
    OD_WALK_MAX <- 20000
    od_walk <- function(dir) {
      out <- character(0); todo <- dir; seen <- 0
      while (length(todo)) {
        ent <- list.files(todo[1], full.names = TRUE)
        todo <- todo[-1]
        seen <- seen + length(ent)
        if (seen > OD_WALK_MAX) return(NULL)
        isd <- dir.exists(ent)
        todo <- c(todo, ent[isd])
        out <- c(out, ent[!isd])
      }
      out
    }

    od_add <- function(paths) {
      paths <- gsub("\\\\", "/", path.expand(paths[!is.na(paths) & nzchar(paths)]))
      paths <- sub("([^:/])/+$", "\\1", paths)
      # ./x, x/../x and short names all point at one place
      real <- file.exists(paths)
      paths[real] <- normalizePath(paths[real], winslash = "/", mustWork = FALSE)
      paths <- unique(paths)
      items <- isolate(od_items())
      skip <- isolate(od_skip())
      msg <- NULL
      skip_row <- function(p, from) {
        if (!is.null(skip) && od_same(p) %in% od_same(skip$path)) return(skip)
        rbind(skip, data.frame(name = basename(p), path = p, from = from, stringsAsFactors = FALSE))
      }
      for (p in paths) {
        if (od_same(p) %in% od_same(vapply(items, function(it) it$path, ""))) next
        if (dir.exists(p)) {
          inside <- od_walk(p)
          if (is.null(inside)) {
            msg <- "That folder holds too many files to search. Choose a smaller one."
            next
          }
          inside <- inside[!tolower(basename(inside)) %in% c("desktop.ini", "thumbs.db")]
          rec <- inside[od_reads(inside)]
          if (length(rec) == 0) {
            msg <- paste0("No ", od_formats, " files in that folder.")
            next
          }
          od_key <<- od_key + 1
          key <- paste0("d", od_key)
          items[[length(items) + 1]] <- list(key = key, path = p, dir = TRUE,
                                             name = if (nzchar(basename(p))) basename(p) else p,
                                             files = rec, sizes = file.info(rec)$size)
          for (q in inside[!od_reads(inside)]) skip <- skip_row(q, key)
        } else if (file.exists(p)) {
          if (!od_reads(p)) {
            skip <- skip_row(p, "")
            next
          }
          od_key <<- od_key + 1
          items[[length(items) + 1]] <- list(key = paste0("d", od_key), path = p, dir = FALSE,
                                             name = basename(p), files = p, sizes = file.info(p)$size)
        } else {
          msg <- "No file or folder at that path."
        }
      }
      od_items(items)
      od_skip(skip)
      od_msg(msg)
      is.null(msg)
    }

    open_inplace_dialog <- function() {
      od_items(list()); od_skip(NULL); od_msg(NULL)
      picks <- od_native()
      help <- "Nothing is copied and there is no size limit."
      showModal(modalDialog(
        title = NULL, footer = NULL, size = "m", easyClose = TRUE,
        tags$div(
          class = "od-dlg",
          tags$div(
            class = "cv-head",
            tags$h2(class = "cv-title", id = ns("inplace_title"), "Open from disk"),
            tags$button(type = "button", class = "cv-x", `data-dismiss` = "modal",
                        `aria-label` = "Close", cv_icon("close"))
          ),
          tags$div(
            class = "cv-body",
            tags$div(
              class = "od-box",
              if (picks) tags$div(
                class = "cv-drop od-empty",
                cv_icon("folder_open", "cv-ico cv-drop-ico"),
                tags$div(class = "od-help", help),
                tags$div(
                  class = "cv-picks",
                  tags$button(type = "button", class = "cv-btn cv-btn--secondary", `data-od-pick` = "files", "Choose files"),
                  tags$button(type = "button", class = "cv-btn cv-btn--secondary", `data-od-pick` = "dir", "Choose folder")
                )
              ),
              uiOutput(ns("inplace_list"))
            ),
            uiOutput(ns("inplace_skip")),
            tags$div(
              class = "od-path",
              tags$input(type = "text", class = "od-path-in", placeholder = "Paste a file or folder path",
                         `aria-label` = "File or folder path", autocomplete = "off", spellcheck = "false"),
              tags$button(type = "button", class = "cv-btn cv-btn--secondary od-path-add is-off",
                          `aria-disabled` = "true", "Add")
            ),
            uiOutput(ns("inplace_msg")),
            if (!picks) tags$div(class = "od-help", help)
          ),
          tags$div(
            class = "cv-foot",
            tags$button(type = "button", class = "cv-btn cv-btn--ghost", `data-dismiss` = "modal", "Cancel"),
            # Waits for the list; od_dialog_script() lifts is-off
            tags$button(id = ns("inplace_go"), type = "button", `aria-disabled` = "true",
                        class = "action-button cv-btn cv-btn--primary is-off", "Open")
          )
        )
      ))
    }

    output$inplace_list <- renderUI({
      items <- od_items()
      if (length(items) == 0) return(NULL)
      files <- unlist(lapply(items, function(it) it$files))
      sizes <- unlist(lapply(items, function(it) it$sizes))
      once <- !duplicated(od_same(files))
      tags$div(
        class = "cv-list",
        tags$div(class = "cv-rows", lapply(items, function(it) {
          tags$div(
            class = "cv-row",
            cv_icon(if (it$dir) "folder" else "draft", "cv-ico cv-row-ico"),
            tags$span(class = "cv-name", title = it$path, it$name),
            tags$span(class = "cv-size",
                      if (it$dir) paste0(plural(length(it$files), "recording"), ", ", cv_size(it$sizes))
                      else cv_size(it$sizes)),
            tags$button(type = "button", class = "cv-rm", `data-key` = it$key,
                        `aria-label` = paste("Remove", it$name), title = "Remove", cv_icon("close"))
          )
        })),
        tags$div(
          class = "cv-sum",
          tags$span(class = "cv-total", paste0(plural(sum(once), "recording"), ", ", cv_size(sizes[once]))),
          if (od_native()) tagList(
            tags$button(type = "button", class = "cv-add", `data-od-pick` = "files", "Add files"),
            tags$button(type = "button", class = "cv-add", `data-od-pick` = "dir", "Add folder")
          )
        )
      )
    })

    output$inplace_skip <- renderUI({
      s <- od_skip()
      if (is.null(s) || nrow(s) == 0) return(NULL)
      tags$div(
        class = "cv-skip", title = paste(s$name, collapse = "\n"),
        if (nrow(s) == 1) paste0("Skipped ", s$name, ", not ", od_formats, ".")
        else paste0("Skipped ", nrow(s), " files that are not ", od_formats, ".")
      )
    })

    output$inplace_msg <- renderUI({
      m <- od_msg()
      if (is.null(m)) return(NULL)
      tags$div(class = "od-msg", role = "alert", m)
    })

    observeEvent(input$side_inplace, open_inplace_dialog(), ignoreInit = TRUE)
    # The session waits while the picker is open; the dialog waits with it
    observeEvent(input$inplace_pick, {
      on.exit(session$sendCustomMessage("od-idle", TRUE), add = TRUE)
      kind <- if (identical(input$inplace_pick, "dir")) "dir" else "files"
      p <- native_pick(kind)
      if (length(p) == 1 && is.na(p)) {
        od_msg(paste0("The ", if (kind == "dir") "folder" else "file",
                      " dialog could not open. Paste the path instead."))
        return()
      }
      if (length(p) > 0) od_add(p)
    })
    observeEvent(input$inplace_add, {
      p <- trimws(input$inplace_add %||% "")
      # Explorer's Copy as path quotes each path, and several can come in one paste
      q <- regmatches(p, gregexpr("\"[^\"]+\"", p))[[1]]
      p <- if (length(q) > 1) trimws(gsub("\"", "", q)) else trimws(gsub("^[\"']|[\"']$", "", p))
      p <- p[nzchar(p)]
      if (!length(p)) return()
      if (od_add(p)) session$sendCustomMessage("od-path-done", TRUE)
    })
    observeEvent(input$inplace_remove, {
      k <- input$inplace_remove
      od_items(Filter(function(it) it$key != k, od_items()))
      s <- od_skip()
      if (!is.null(s)) od_skip(s[s$from != k, , drop = FALSE])
      od_msg(NULL)
    })
    observeEvent(input$inplace_edit, od_msg(NULL))
    observeEvent(input$inplace_go, {
      files <- inplace_frame(unlist(lapply(od_items(), function(it) it$files)))
      files <- files[!duplicated(od_same(files$datapath)), , drop = FALSE]
      if (nrow(files) == 0) {
        od_items(list()); od_skip(NULL)
        od_msg("Those files are no longer there.")
        return()
      }
      removeModal()
      load_batch(files, "Opening from disk")
    })

    # Each pick adds to the list; the same file picked twice is listed once
    add_picked <- function(df) {
      if (is.null(df) || nrow(df) == 0) return()
      df$key <- paste0("f", conv_key + seq_len(nrow(df)))
      conv_key <<- conv_key + nrow(df)
      all <- rbind(isolate(conv_picked()), df)
      conv_picked(all[!duplicated(paste(all$name, all$size)), , drop = FALSE])
    }
    observeEvent(input$convert_gt3x, add_picked(input$convert_gt3x))
    observeEvent(input$convert_dir, add_picked(input$convert_dir))
    observeEvent(input$convert_remove, {
      all <- conv_picked()
      if (!is.null(all)) conv_picked(all[all$key != input$convert_remove, , drop = FALSE])
    })

    is_gt3x <- function(df) tolower(tools::file_ext(df$name)) == "gt3x"

    selected_gt3x <- reactive({
      all <- conv_picked()
      if (is.null(all) || nrow(all) == 0) return(NULL)
      all[is_gt3x(all), , drop = FALSE]
    })

    output$convert_list <- renderUI({
      sel <- selected_gt3x()
      if (is.null(sel) || nrow(sel) == 0) return(NULL)
      tags$div(
        class = "cv-list",
        tags$div(class = "cv-rows", lapply(seq_len(nrow(sel)), function(i) {
          tags$div(
            class = "cv-row",
            cv_icon("draft", "cv-ico cv-row-ico"),
            tags$span(class = "cv-name", sel$name[i]),
            tags$span(class = "cv-size", cv_size(sel$size[i])),
            tags$button(type = "button", class = "cv-rm", `data-key` = sel$key[i],
                        `aria-label` = paste("Remove", sel$name[i]), title = "Remove", cv_icon("close"))
          )
        })),
        tags$div(
          class = "cv-sum",
          tags$span(class = "cv-total", paste0(plural(nrow(sel), "file"), ", ", cv_size(sel$size))),
          tags$button(type = "button", class = "cv-add", `data-pick` = ns("convert_gt3x"), "Add files"),
          tags$button(type = "button", class = "cv-add", `data-pick` = ns("convert_dir"), "Add folder")
        )
      )
    })

    output$convert_skip <- renderUI({
      all <- conv_picked()
      if (is.null(all)) return(NULL)
      other <- all$name[!is_gt3x(all)]
      if (length(other) == 0) return(NULL)
      tags$div(
        class = "cv-skip", title = paste(other, collapse = "\n"),
        if (length(other) == 1) paste0("Skipped ", other, ", not a .gt3x file.")
        else paste0("Skipped ", length(other), " files that are not .gt3x.")
      )
    })

    observeEvent(input$convert_load, {
      sel <- selected_gt3x()
      if (is.null(sel) || nrow(sel) == 0) {
        showNotification("Choose a .gt3x file or a folder first.", type = "warning")
        return()
      }
      ep <- as.numeric(input$convert_epoch %||% 60); lf <- isTRUE(input$convert_filter == "lfe")
      for (i in seq_len(nrow(sel))) {
        convert_and_load(sel$datapath[i], sel$name[i], epoch = ep, lfe = lf)
      }
      removeModal()
    })

    output$download_agd <- downloadHandler(
      filename = function() {
        sel <- selected_gt3x(); ep <- input$convert_epoch %||% "60"
        if (is.null(sel) || nrow(sel) <= 1) {
          agd_name(if (!is.null(sel) && nrow(sel) >= 1) sel$name[1] else "converted.gt3x", ep)
        } else {
          sprintf("converted_%ssec_agd.zip", ep)
        }
      },
      content = function(file) {
        # The dialog takes another Download click once this returns
        on.exit(session$sendCustomMessage("cv-download-done", TRUE), add = TRUE)
        sel <- selected_gt3x()
        if (is.null(sel) || nrow(sel) == 0) stop("Choose a .gt3x file or folder first.")
        ep <- as.numeric(input$convert_epoch %||% 60); lf <- isTRUE(input$convert_filter == "lfe")
        agds <- character(nrow(sel)); outs <- character(nrow(sel))
        withProgress(message = "Converting to .agd", value = 0, {
          for (i in seq_len(nrow(sel))) {
            setProgress(value = i / nrow(sel), detail = sel$name[i])
            agds[i] <- tryCatch(ensure_agd(sel$datapath[i], ep, lf), error = function(e) {
              showNotification(paste0("Could not convert ", sel$name[i], " (",
                                      raw_clean_msg(conditionMessage(e), sel$name[i]),
                                      "), so nothing was downloaded."), type = "error")
              stop(e)
            })
            outs[i] <- agd_name(sel$name[i], ep)
          }
        })
        if (length(agds) == 1) {
          file.copy(agds[1], file, overwrite = TRUE)
        } else {
          stage <- tempfile("agd_zip_"); dir.create(stage)
          for (j in seq_along(agds)) file.copy(agds[j], file.path(stage, outs[j]), overwrite = TRUE)
          zip::zipr(zipfile = file, root = stage, files = outs)
        }
      }
    )

    # Selection, removal, undo. A recording picked here is the focus the other
    # pages open on (focus_set); only a pick by the user sets it.
    observeEvent(input$select_row, {
      fid <- input$select_row
      if (startsWith(fid, "failed_")) {
        if (!is.null(local$failed[[fid]])) local$selected_failed <- fid
      } else if (!is.null(shared$files[[fid]])) {
        shared$selected_file <- fid
        local$selected_failed <- NULL
        focus_set(shared, fid)
      } else if (fid %in% raw_ids()) {
        local$raw_sel <- fid
        focus_set(shared, fid)
      }
    })

    # A recording picked on another page is the one selected here
    on_tab_shown(shared, "overview", function() {
      fid <- focus_get(shared, among = names(shared$files))
      if (!is.null(fid)) {
        shared$selected_file <- fid
        local$selected_failed <- NULL
      }
      rid <- focus_get(shared, among = raw_ids())
      if (!is.null(rid)) local$raw_sel <- rid
    })

    drop_results_for <- function(fid) {
      taken <- list()
      for (key in names(shared$results)) {
        if (!is.null(shared$results[[key]][[fid]])) {
          taken[[key]] <- shared$results[[key]][[fid]]
          shared$results[[key]][[fid]] <- NULL
        }
      }
      taken
    }

    observeEvent(input$remove_row, {
      fid <- input$remove_row

      if (startsWith(fid, "failed_")) {
        local$failed[[fid]] <- NULL
        if (identical(local$selected_failed, fid)) local$selected_failed <- NULL
        return()
      }

      if (startsWith(fid, "queued|")) {
        name <- sub("^queued\\|", "", fid)
        q <- conv_queue()
        q <- Filter(function(job) !identical(job$name, name), q)
        conv_queue(q)
        return()
      }

      record <- shared$files[[fid]]
      if (is.null(record)) return()
      order_before <- ordered_ids()
      position <- match(fid, order_before)

      undo_store <<- list(id = fid, record = record, results = drop_results_for(fid))
      shared$files[[fid]] <- NULL
      shared$file_count <- length(shared$files)
      shared$data_loaded <- length(shared$files) > 0
      if (identical(shared$selected_file, fid)) {
        # Selection moves to the row that took the removed row's place, or the last row
        remaining <- setdiff(order_before, fid)
        shared$selected_file <- if (length(remaining) > 0) remaining[min(position, length(remaining))] else NULL
      }
      set_notice(paste0("Removed ", record$name, "."), undo = TRUE)
    })

    observeEvent(input$undo_remove, {
      u <- undo_store
      if (is.null(u)) return()
      undo_store <<- NULL
      shared$files[[u$id]] <- u$record
      for (key in names(u$results)) shared$results[[key]][[u$id]] <- u$results[[key]]
      shared$file_count <- length(shared$files)
      shared$data_loaded <- TRUE
      if (is.null(shared$selected_file)) shared$selected_file <- u$id
      local$notice <- NULL
    })

    observeEvent(input$clear_all, {
      n <- length(shared$files) + length(local$failed)
      showModal(modalDialog(
        title = NULL, footer = NULL, size = "s", easyClose = TRUE,
        tags$div(
          class = "ov-dialog",
          tags$div(
            class = "ov-dialog-head",
            tags$h2(class = "ov-dialog-title", paste0("Remove all ", plural(n, "file"), "?")),
            tags$div(class = "ov-dialog-sub", "Your files on disk are not affected.")
          ),
          tags$div(
            class = "ov-dialog-foot",
            tags$span(class = "ov-spacer"),
            tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancel"),
            actionButton(ns("clear_all_confirm"), "Remove all", class = "btn-primary")
          )
        )
      ))
    })

    observeEvent(input$clear_all_confirm, {
      removeModal()
      shared$files <- list()
      shared$file_count <- 0
      shared$selected_file <- NULL
      shared$data_loaded <- FALSE
      for (key in names(shared$results)) shared$results[[key]] <- list()
      local$failed <- list()
      local$selected_failed <- NULL
      local$notice <- NULL
      undo_store <<- NULL
    })

    observeEvent(input$tab, {
      if (input$tab %in% c("files", "details")) local$panel_tab <- input$tab
    })

    # A column header sorts the list; the same header again turns the order.
    observeEvent(input$sort, {
      key <- input$sort
      if (!key %in% c("name", "subject", "worn", "inbed")) return()
      if (identical(local$sort_key, key)) {
        local$sort_dir <- -local$sort_dir
      } else {
        local$sort_key <- key
        local$sort_dir <- 1
      }
    })

    # File ids in the order the Files tab shows them
    ordered_ids <- reactive({
      files <- shared$files
      ids <- names(files)
      if (length(ids) < 2) return(ids)
      names_lower <- vapply(files, function(f) tolower(f$name), character(1))
      value <- switch(local$sort_key,
        subject = vapply(files, function(f) tolower(present(f$subject_info$id) %||% ""), character(1)),
        worn = {
          cs <- covs()
          vapply(ids, function(id) {
            c <- cs[[id]]
            if (is.null(c) || is.na(c$worn)) NA_real_ else c$worn
          }, numeric(1))
        },
        inbed = vapply(files, function(f) {
          if (is.data.frame(f$actilife_sleep)) nrow(f$actilife_sleep) else NA_real_
        }, numeric(1)),
        names_lower
      )
      ids[order(value, names_lower, decreasing = local$sort_dir < 0, na.last = TRUE)]
    })

    # Worn against recorded for every loaded file, computed once per change
    covs <- reactive({
      wear <- shared$results$wear_time %||% list()
      lapply(shared$files, function(f) ovl_coverage(f, wear[[f$id]]))
    })

    # The window the coverage bars are drawn on: one shared timeline while the
    # loaded set spans a stretch comparable to one recording, otherwise each
    # row on its own span
    cov_window <- reactive({
      cs <- Filter(Negate(is.null), covs())
      if (length(cs) == 0) return(NULL)
      from <- do.call(min, lapply(cs, `[[`, "start"))
      to <- do.call(max, lapply(cs, `[[`, "end"))
      longest <- max(vapply(cs, function(c) c$recorded, numeric(1)))
      total <- as.numeric(difftime(to, from, units = "hours"))
      list(from = from, to = to, shared = total <= max(3 * longest, longest + 24))
    })


    queued_names <- reactive(vapply(conv_queue(), function(job) job$name, character(1)))

    # Which interface is showing; the switch is drawn only when both kinds are loaded
    raw_ids <- reactive(names(shared$raw %||% list()))
    has_raw <- reactive(length(raw_ids()) > 0 || length(local$raw_running) > 0 ||
                          length(local$raw_failed) > 0 || length(raw_queue()) > 0)
    has_counts <- reactive(length(shared$files) > 0 || length(local$failed) > 0 ||
                             length(queued_names()) > 0 || length(local$running) > 0)

    view <- reactive({
      if (!has_raw()) return("counts")
      if (!has_counts()) return("raw")
      if (identical(local$view, "raw")) "raw" else "counts"
    })

    # The raw ids in the order the Recordings table shows them
    raw_ordered_ids <- reactive(ovr_order(shared$raw, raw_ids(), local$raw_sort, local$raw_sort_dir))

    raw_selected <- reactive({
      ids <- raw_ids()
      if (length(ids) == 0) return(NULL)
      if (!is.null(local$raw_sel) && local$raw_sel %in% ids) local$raw_sel else ids[1]
    })

    # The window the raw coverage lanes are drawn on
    raw_window <- reactive({
      rs <- shared$raw
      sp <- lapply(rs, ovr_span)
      sp <- sp[!vapply(sp, is.null, logical(1))]
      if (length(sp) == 0) return(NULL)
      list(from = do.call(min, lapply(sp, `[[`, "start")),
           to = do.call(max, lapply(sp, `[[`, "end")))
    })

    page_loaded <- reactive({
      has_counts() || has_raw()
    })

    # Shared markup
    cross_glyph <- function() {
      HTML('<svg viewBox="0 0 12 12" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" aria-hidden="true"><path d="M2.5 2.5l7 7M9.5 2.5l-7 7"></path></svg>')
    }

    search_glyph <- function() {
      HTML('<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" aria-hidden="true"><circle cx="7" cy="7" r="4.5"></circle><path d="M10.5 10.5L14 14"></path></svg>')
    }

    # Top: intro and import zone, or figures and strip

    # Two segments, each with its own count
    view_switch <- function() {
      if (!(has_counts() && has_raw())) return(NULL)
      v <- view()
      seg <- function(key, label, n) {
        # Only .action-button, so Shiny binds it and none of the global
        # a:not(.btn) or .btn-default colour rules apply
        tags$button(id = ns(paste0("view_", key)), type = "button",
                    class = paste("action-button ovr-seg-b",
                                  if (identical(v, key)) "is-on" else ""),
                    label, tags$b(fmt_int(n)))
      }
      tags$div(class = "ovr-seg", role = "tablist", `aria-label` = "Which recordings to show",
               seg("counts", "Counts", length(shared$files)),
               seg("raw", "Raw", length(raw_ids())))
    }
    observeEvent(input$view_counts, local$view <- "counts")
    observeEvent(input$view_raw, local$view <- "raw")

    output$top <- renderUI({
      if (identical(view(), "raw")) {
        rs <- shared$raw
        # An upload's own notice and progress show on this side too
        note <- local$notice
        lead <- c(if (isTRUE(note$both)) note$text, upload_part())
        return(tags$div(
          class = "ovl-top ovr-top",
          view_switch(),
          if (length(rs) > 0) ovr_figures(rs),
          if (length(local$raw_running) > 0 || length(local$raw_failed) > 0 || length(raw_queue()) > 0 ||
              length(lead) > 0)
            tags$div(class = "ovl-status", raw_status_line(lead))))
      }
      if (!page_loaded()) {
        # First run: the mark, one line and the two ways in; the rest is in the sidebar
        return(tags$div(
          class = "ov-first",
          tags$div(
            class = "ov-first-group",
            tags$div(
              class = "ov-mark",
              tags$img(src = "logo.png", alt = "", class = "ov-mark-img"),
              tags$span(class = "ov-mark-name", "CANHRActi")
            ),
            tags$div(class = "ov-lead", paste(
              "Analysis of accelerometer data for physical activity, sleep, sedentary behavior and",
              "circadian rhythm research. It works with ActiGraph count files (.agd) and raw recordings",
              "(ActiGraph .gt3x, Axivity .cwa, GENEActiv .bin), and its raw pipeline gives the same",
              "results as GGIR. Developed by the Center for Alaska Native Health Research.")),

            tags$div(
              class = "ov-card",
              tags$div(class = "ov-card-head", "Quick start"),
              tags$label(
                class = "ov-card-row", `for` = ns("files"),
                msym("note_add", class = "ov-card-ico"), tags$span("Add file(s)")
              ),
              actionLink(
                ns("demo_btn"), class = "ov-card-row",
                label = tagList(msym("dataset", class = "ov-card-ico"), tags$span("Try sample files"))
              )
            ),

            # Getting started and the theme switch
            tags$div(
              class = "ov-links",
              tags$a(href = "https://github.com/rdazadda/canhrActi#readme", target = "_blank",
                     class = "ov-link", "Getting started", msym("open_in_new", class = "ov-link-ico")),
              tags$span(class = "ov-links-sep", `aria-hidden` = "true"),
              tags$button(
                type = "button", class = "ov-theme-toggle", role = "switch",
                `aria-checked` = "false",
                tags$span(class = "ov-switch", `aria-hidden` = "true", tags$span(class = "ov-knob")),
                tags$span(class = "ov-theme-label", "Dark mode")
              )
            ),

            # Folder, sample and conversion notices; status_line() is otherwise
            # only read by the loaded layout
            uiOutput(ns("empty_status"), class = "ov-first-status")
          )
        ))
      }

      files <- shared$files
      cs <- covs()
      n_files <- length(files)
      recorded <- sum(vapply(files, function(f) as.numeric(f$duration_hrs %||% NA), numeric(1)), na.rm = TRUE)

      # Worn is a share of what was scored, not of everything loaded
      worn_each <- vapply(cs, function(c) if (is.null(c)) NA_real_ else c$worn, numeric(1))
      scored_each <- vapply(cs, function(c) if (is.null(c) || is.na(c$worn)) NA_real_ else c$recorded, numeric(1))
      worn_total <- if (all(is.na(worn_each))) NA_real_ else sum(worn_each, na.rm = TRUE)
      scored_total <- if (all(is.na(scored_each))) NA_real_ else sum(scored_each, na.rm = TRUE)

      epoch_lengths <- unique(vapply(files, function(f) as.numeric(f$epoch_length %||% NA), numeric(1)))
      epoch_lengths <- epoch_lengths[!is.na(epoch_lengths)]

      starts <- lapply(files, function(f) if ("timestamp" %in% names(f$data) && nrow(f$data) > 0) min(f$data$timestamp) else NULL)
      ends   <- lapply(files, function(f) if ("timestamp" %in% names(f$data) && nrow(f$data) > 0) max(f$data$timestamp) else NULL)
      starts <- Filter(Negate(is.null), starts); ends <- Filter(Negate(is.null), ends)

      figure <- function(value, unit, label) {
        tags$div(class = "ovl-fig",
          tags$div(tags$span(class = "ovl-fig-n", value),
                   if (!is.null(unit)) tags$span(class = "ovl-fig-u", unit)),
          tags$div(class = "ovl-fig-l", label))
      }
      rule <- function() tags$div(class = "ovl-fig-rule", `aria-hidden` = "true")

      status <- status_line()
      # The raw read reports here too, since the page defaults to the counts side
      raw_status <- if (has_raw()) raw_status_line() else NULL

      tagList(
        view_switch(),
        tags$div(
          class = "ovl-figs",
          figure(fmt_int(n_files), NULL, if (n_files == 1) "Recording" else "Recordings"),
          rule(),
          figure(fmt_int(round(recorded)), "h", "Recorded"),
          rule(),
          if (is.na(worn_total)) {
            figure("–", NULL, "Worn")
          } else {
            figure(fmt_int(round(worn_total)),
                   if (!is.na(scored_total) && scored_total > 0)
                     HTML(paste0("h &middot; ", round(worn_total / scored_total * 100), "%")) else "h",
                   "Worn")
          },
          rule(),
          if (length(epoch_lengths) == 1) figure(fmt_int(epoch_lengths), "s", "Epoch length")
          else if (length(epoch_lengths) > 1) figure("Mixed", NULL, "Epoch length")
          else figure("–", NULL, "Epoch length"),
          rule(),
          tags$div(class = "ovl-fig",
            tags$div(class = "ovl-fig-t",
              if (length(starts) > 0) date_span(do.call(min, starts), do.call(max, ends)) else "–"),
            tags$div(class = "ovl-fig-l", "Recorded span"))
        ),
        # Only while something is happening
        if (!is.null(status)) tags$div(class = "ovl-status", status),
        if (!is.null(raw_status)) tags$div(class = "ovl-status", raw_status)
      )
    })

    # Conversions in progress, files that could not be read, and a short-lived
    # notice with Undo after a removal
    status_line <- function() {
      running <- local$running
      queued <- queued_names()
      n_failed <- length(local$failed)
      parts <- as.list(upload_part())
      if (length(running) == 1) parts <- c(parts, paste0("Converting ", running, "."))
      if (length(running) > 1) parts <- c(parts, paste0("Converting ", plural(length(running), "file"), "."))
      if (length(queued) > 0) parts <- c(parts, paste0(length(queued), " queued."))
      if (n_failed > 0) parts <- c(parts, if (n_failed == 1) "1 file could not be read." else paste0(n_failed, " files could not be read."))
      notice <- local$notice
      if (!is.null(notice)) {
        parts <- c(list(notice$text, if (isTRUE(notice$undo)) actionLink(ns("undo_remove"), "Undo")), parts)
      }
      if (length(parts) == 0) return(NULL)
      out <- list()
      for (i in seq_along(parts)) {
        if (i > 1) out <- c(out, list(" "))
        out <- c(out, list(parts[[i]]))
      }
      tagList(out)
    }

    # The upload in progress; a single file goes by name
    upload_part <- function() {
      u <- local$upload
      if (is.null(u)) return(NULL)
      if (u$n == 1) paste0("Uploading ", u$name, ".") else paste0("Uploading ", u$i, " of ", u$n, ".")
    }

    # The panel: the recordings, and one recording in detail
    tab_button <- function(key, label, active) {
      tags$button(type = "button", class = paste("ov-tab", if (active) "is-active" else ""),
                  `data-tab` = key, role = "tab", `aria-selected` = tolower(as.character(active)), label)
    }

    chevron <- function(direction) {
      d <- if (identical(direction, "left")) "M9 3l-4 4 4 4" else "M5 3l4 4-4 4"
      HTML(paste0('<svg viewBox="0 0 14 14" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="', d, '"></path></svg>'))
    }

    step_selection <- function(by) {
      ids <- ordered_ids()
      i <- match(shared$selected_file, ids)
      if (is.na(i)) return()
      j <- i + by
      if (j >= 1 && j <= length(ids)) {
        shared$selected_file <- ids[j]
        local$selected_failed <- NULL
        focus_set(shared, ids[j])
      }
    }
    observeEvent(input$prev_file, step_selection(-1))
    observeEvent(input$next_file, step_selection(1))

    observeEvent(input$open_details, {
      fid <- input$open_details
      if (startsWith(fid, "failed_")) {
        if (!is.null(local$failed[[fid]])) local$selected_failed <- fid
      } else if (!is.null(shared$files[[fid]])) {
        shared$selected_file <- fid
        local$selected_failed <- NULL
        focus_set(shared, fid)
      } else {
        return()
      }
      local$panel_tab <- "details"
    })

    # Measured with the streaming gt3x reader: 22 MB took 21 s and 427 MB about
    # 3 min in the app. Devices it declines still read at about 3 s per MB.
    RAW_SECS_PER_MB <- 0.4
    raw_eta <- function(mb) {
      if (is.null(mb) || !is.finite(mb)) return("This can take a while.")
      mins <- (15 + mb * RAW_SECS_PER_MB) / 60
      if (mins < 1.5) return("This takes about a minute.")
      paste0(round(mb), " MB, so roughly ", round(mins), " minutes",
             if (mb > 300) ", and several GB of memory" else "", ".")
    }

    # lead: lines that go first, such as an upload's progress
    raw_status_line <- function(lead = NULL) {
      n <- length(local$raw_running)
      parts <- as.list(lead)
      if (n > 0) {
        # tick while something is reading, so a long job visibly progresses
        invalidateLater(3000, session)
        el <- if (!is.null(local$raw_started))
          as.numeric(difftime(Sys.time(), local$raw_started, units = "mins")) else NA_real_
        elapsed <- if (is.finite(el) && el >= 1) paste0(" ", round(el), " min so far.") else ""
        head <- if (n == 1) paste0("Reading ", local$raw_running[1], ".")
                else paste0("Reading ", plural(n, "recording"), ".")
        # the worker names the temp upload, not the file the person picked
        doing <- raw_progress_line()
        if (!is.null(doing)) doing <- raw_clean_msg(doing, local$raw_running[1])
        parts <- c(parts, list(paste0(head, " ", raw_eta(local$raw_size_mb), elapsed,
                                      if (!is.null(doing)) paste0(" ", doing, ".") else "")))
      }
      q <- length(raw_queue())
      if (q > 0) {
        parts <- c(parts, list(paste0(q, " queued.", if (n == 0) " Waiting for a free reader." else "")))
      }
      for (f in local$raw_failed) {
        parts <- c(parts, list(tags$span(class = "ovr-fail",
          tags$b(f$name), ": ", f$error,
          actionLink(ns("raw_clear_failed"), "Dismiss", class = "ovr-fail-x"))))
      }
      if (length(parts) == 0) return(NULL)
      out <- list()
      for (i in seq_along(parts)) {
        if (i > 1) out <- c(out, list(tags$br()))
        out <- c(out, parts[i])
      }
      tagList(out)
    }
    observeEvent(input$raw_clear_failed, local$raw_failed <- list())

    # The raw interface's panel: three tabs, the third for the pipeline's figures
    raw_body <- function() {
      # A running or failed read is reported by the status line above
      if (length(raw_ids()) == 0) return(NULL)
      tab <- local$raw_tab
      if (!tab %in% c("files", "quality", "details")) tab <- "files"
      ids <- raw_ordered_ids()
      sel <- raw_selected()
      r <- if (!is.null(sel)) shared$raw[[sel]] else NULL
      if (tab != "files" && is.null(r)) tab <- "files"

      tools <- switch(tab,
        files = tags$div(
          class = "ov-work-tools",
          tags$span(class = "ovl-key", tags$i(class = "on"), "Worn"),
          tags$span(class = "ovl-key", tags$i(class = "off"), "Non-wear"),
          tags$span(class = "ovl-key", tags$i(class = "imp"), "Imputed"),
          # downloadButton's markup without btn-default, which the dark panel rule boxes
          tags$a(id = ns("raw_export"), class = "btn shiny-download-link disabled wt-btn wt-btn--secondary",
                 href = "", target = "_blank", download = NA, `aria-disabled` = "true", tabindex = "-1",
                 "Export quality report"),
          tags$button(type = "button", id = ns("raw_clear"),
                      class = "action-button wt-btn wt-btn--secondary", "Remove all")),
        tags$div(
          class = "ov-work-tools",
          tags$span(class = "ovr-file", ovr_name(r, sel)),
          tags$span(class = "ov-file-pos", paste(match(sel, ids), "of", length(ids))),
          actionButton(ns("raw_prev"), label = chevron("left"), class = "btn-default btn-icon ov-step-btn",
                       title = "Previous recording", disabled = if (match(sel, ids) == 1) NA else NULL),
          actionButton(ns("raw_next"), label = chevron("right"), class = "btn-default btn-icon ov-step-btn",
                       title = "Next recording", disabled = if (match(sel, ids) == length(ids)) NA else NULL)))

      body <- switch(tab,
        files = {
          w <- raw_window()
          if (is.null(w)) tags$div(class = "ovl-fine", "Nothing to draw yet.")
          else ovr_table(shared$raw, ids, sel, ns, w$from, w$to,
                         sort = list(key = local$raw_sort, dir = local$raw_sort_dir))
        },
        quality = {
          # Per day and Nights scroll down a long recording, Gaps across it and
          # Overview either way; data-fig lets the page script keep the scroll
          # across a rebuild
          scroll <- switch(local$raw_chip, days = , nights = "is-scroll-y", gaps = "is-scroll-x",
                           overview = "is-scroll-xy", "")
          tagList(
            tags$div(
              class = paste("ovr-figpanel", scroll), `data-fig` = paste(local$raw_chip, sel),
              tags$div(class = "ovr-figpanel-h",
                       tags$span(OVR_CHIPS[[local$raw_chip]]),
                       ovr_chips(local$raw_chip, ns)),
              tags$div(class = "ovr-figpanel-b",
                       plotOutput(ns("raw_figure"), height = "100%"))),
            ovr_checks_strip(r, isTRUE(local$raw_checks_open), ns, local$raw_chip))
        },
        details = ovr_details(r, sel, ns, local$raw_epoch_page, local$raw_series))

      tab_btn <- function(key, label) {
        tags$button(type = "button", class = paste("ov-tab", if (identical(tab, key)) "is-active" else ""),
                    `data-rawtab` = key, role = "tab",
                    `aria-selected` = tolower(as.character(identical(tab, key))), label)
      }

      tags$div(
        class = paste("ov-panel ov-work ovl-work ovr-work", paste0("is-", tab)),
        tags$div(
          class = "ov-work-head",
          tags$div(class = "ov-tabs ov-work-tabs", role = "tablist",
                   tab_btn("files", "Recordings"), tab_btn("quality", "Data quality"),
                   tab_btn("details", "Details")),
          tools),
        tags$div(class = "ov-work-body", body))
    }

    output$raw_figure <- renderPlot({
      sel <- raw_selected(); req(sel)
      r <- shared$raw[[sel]]; req(r)
      fn <- switch(local$raw_chip,
                   overview = canhrActi::plot_raw_quality,
                   gaps = canhrActi::plot_raw_gaps,
                   calibration = canhrActi::plot_raw_calibration,
                   days = canhrActi::plot_raw_days,
                   nights = canhrActi::plot_raw_nights,
                   chunks = canhrActi::plot_raw_chunks,
                   canhrActi::plot_raw_quality)
      fn(r)
    }, width = function() raw_fig_dims()$width, height = function() raw_fig_dims()$height, res = 96)

    raw_fig_rows <- reactive({
      sel <- raw_selected()
      r <- if (!is.null(sel)) shared$raw[[sel]] else NULL
      if (is.null(r)) 0 else ovr_fig_rows(r, local$raw_chip)
    })
    # The box's own size for most figures; see ovr_fig_size for the four that scroll
    raw_fig_dims <- reactive({
      w <- session$clientData[[paste0("output_", ns("raw_figure"), "_width")]]
      h <- session$clientData[[paste0("output_", ns("raw_figure"), "_height")]]
      req(w, h)
      if (!local$raw_chip %in% OVR_FIG_SCROLL) return(list(width = w, height = h))
      ovr_fig_size(local$raw_chip, raw_fig_rows(), w, h, input$fig_sb %||% 0,
                   session$clientData$pixelratio %||% 1)
    })

    observeEvent(input$raw_tab_set, {
      if (input$raw_tab_set %in% c("files", "quality", "details")) local$raw_tab <- input$raw_tab_set
    })
    observeEvent(input$raw_chip_set, {
      if (input$raw_chip_set %in% names(OVR_CHIPS)) {
        local$raw_chip <- input$raw_chip_set
        local$raw_tab <- "quality"
      }
    })
    observeEvent(input$raw_open, {
      if (input$raw_open %in% raw_ids()) {
        local$raw_sel <- input$raw_open
        focus_set(shared, input$raw_open)
        local$raw_tab <- "quality"
        # a different recording starts on its first page
        local$raw_epoch_page <- 1
      }
    })
    observeEvent(input$raw_chip_overview, local$raw_chip <- "overview")
    observeEvent(input$raw_checks_toggle, local$raw_checks_open <- !isTRUE(local$raw_checks_open))
    observeEvent(input$raw_show_all_checks, { local$raw_tab <- "details" })
    observeEvent(input$raw_series_toggle, {
      local$raw_series <- if (identical(local$raw_series, "measured")) "imputed" else "measured"
      local$raw_epoch_page <- 1
    })
    observeEvent(input$raw_epoch_prev, local$raw_epoch_page <- max(1, local$raw_epoch_page - 1))
    observeEvent(input$raw_epoch_next, local$raw_epoch_page <- local$raw_epoch_page + 1)
    raw_step <- function(by) {
      ids <- raw_ordered_ids(); i <- match(raw_selected(), ids)
      if (is.na(i)) return()
      j <- i + by
      if (j >= 1 && j <= length(ids)) {
        local$raw_sel <- ids[j]; local$raw_epoch_page <- 1
        focus_set(shared, ids[j])
      }
    }

    # A header sorts the raw list; the same header again turns the order
    observeEvent(input$raw_sort, {
      key <- input$raw_sort
      if (!key %in% OVR_SORT_KEYS) return()
      if (identical(local$raw_sort, key)) {
        local$raw_sort_dir <- -local$raw_sort_dir
      } else {
        local$raw_sort <- key
        local$raw_sort_dir <- 1
      }
    })

    # GGIR's data_quality_report.csv, stacked in GGIR's row order and written
    # as GGIR writes it; canhrActi's added columns go to
    # data_quality_report_canhrActi.csv beside it, with the check table in table
    # order. Zipped together, or GGIR's report alone without zip
    output$raw_export <- downloadHandler(
      filename = function() {
        if (requireNamespace("zip", quietly = TRUE)) "data_quality_report.zip" else "data_quality_report.csv"
      },
      content = function(file) {
        ids <- raw_ordered_ids()
        validate(need(length(ids) > 0, "No raw recordings are loaded."))
        rs <- shared$raw[ids]
        gr <- rs[ovr_ggir_order(rs)]
        report <- ovr_quality_frame(gr)
        g <- ovr_quality_ggir_cols(report)
        ggir <- canhrActi:::.raw.report.addsplitnames(ovr_quality_ggir(gr))
        write_ggir <- function(path) {
          data.table::fwrite(ggir, path, row.names = FALSE, na = "", sep = ",", dec = ".")
        }
        if (!requireNamespace("zip", quietly = TRUE)) {
          write_ggir(file)
          return(invisible(NULL))
        }
        stage <- tempfile("quality_"); dir.create(stage)
        outs <- c("data_quality_report.csv", "data_quality_report_canhrActi.csv",
                  "data_quality_checks.csv")
        write_ggir(file.path(stage, outs[1]))
        utils::write.csv(report[c("filename", setdiff(names(report), g))], file.path(stage, outs[2]),
                         row.names = FALSE, na = "")
        utils::write.csv(ovr_checks_frame(rs), file.path(stage, outs[3]), row.names = FALSE, na = "")
        zip::zipr(zipfile = file, root = stage, files = outs)
      })
    observeEvent(input$raw_prev, raw_step(-1))
    observeEvent(input$raw_next, raw_step(1))
    observeEvent(input$raw_remove, {
      fid <- input$raw_remove
      if (is.null(fid) || !fid %in% raw_ids()) return()
      nm <- ovr_name(shared$raw[[fid]], fid)
      shared$raw[[fid]] <- NULL
      if (identical(local$raw_sel, fid)) local$raw_sel <- NULL
      local$raw_epoch_page <- 1
      if (length(raw_ids()) == 0) local$view <- "counts"
      set_notice(paste0(nm, " removed."))
    })

    observeEvent(input$raw_clear, {
      shared$raw <- list(); local$raw_sel <- NULL; local$raw_failed <- list()
      local$view <- "counts"
      set_notice("Raw recordings removed.")
    })

    output$body <- renderUI({
      req(page_loaded())
      if (identical(view(), "raw")) return(raw_body())
      panel <- if (identical(local$panel_tab, "details")) "details" else "files"
      files <- shared$files
      n_rows <- length(files) + length(local$failed) + length(queued_names()) + length(local$running)

      tools <- if (identical(panel, "details")) {
        ids <- ordered_ids()
        i <- match(shared$selected_file, ids)
        if (!is.na(i) && is.null(local$selected_failed)) {
          tags$div(
            class = "ov-work-tools",
            actionButton(ns("prev_file"), label = chevron("left"), class = "btn-default btn-icon ov-step-btn",
                         title = "Previous file", disabled = i == 1),
            tags$span(class = "ov-file-pos", paste0(i, " of ", length(ids))),
            actionButton(ns("next_file"), label = chevron("right"), class = "btn-default btn-icon ov-step-btn",
                         title = "Next file", disabled = i == length(ids))
          )
        }
      } else {
        tags$div(
          class = "ov-work-tools",
          tags$span(class = "ovl-key", tags$i(class = "on"), "Worn"),
          tags$span(class = "ovl-key", tags$i(class = "off"), "Non-wear"),
          # shown only while a filter narrows the list
          tags$span(class = "ov-list-count", hidden = NA),
          if (n_rows >= 8) tags$label(class = "ov-filter",
            search_glyph(),
            tags$input(type = "text", class = "ov-filter-input", placeholder = "Filter files",
                       `aria-label` = "Filter files by name or subject")),
          tags$button(type = "button", id = ns("clear_all"),
                      class = "action-button wt-btn wt-btn--secondary", "Remove all")
        )
      }

      tags$div(
        class = "ov-panel ov-work ovl-work",
        tags$div(
          class = "ov-work-head",
          tags$div(class = "ov-tabs ov-work-tabs", role = "tablist",
            tab_button("files", "Recordings", identical(panel, "files")),
            tab_button("details", "Details", identical(panel, "details"))),
          tools
        ),
        tags$div(
          class = "ov-work-body",
          if (identical(panel, "details")) uiOutput(ns("details")) else uiOutput(ns("file_list"))
        )
      )
    })

    output$file_list <- renderUI({
      files <- shared$files
      failed <- local$failed
      queued <- queued_names()
      running <- local$running
      cs <- covs()
      win <- cov_window()

      remove_cell <- function(name) {
        tags$td(class = "ovl-rm",
          tags$span(class = "ov-x", role = "button", tabindex = "-1",
                    title = paste("Remove", name), `aria-label` = paste("Remove", name), cross_glyph()))
      }

      # One row at a time is in the tab order: the selected one, or else the first
      has_selection <- (is.null(local$selected_failed) && !is.null(shared$selected_file) && shared$selected_file %in% names(files)) ||
        (!is.null(local$selected_failed) && local$selected_failed %in% names(failed))
      first_row <- TRUE
      focus_index <- function(selected) {
        idx <- if (selected || (!has_selection && first_row)) "0" else "-1"
        first_row <<- FALSE
        idx
      }

      rows <- list()
      # Ticks and day lines only on a shared window, where one axis serves every row
      ax <- if (!is.null(win) && win$shared) ovl_axis(win$from, win$to) else NULL

      for (fid in ordered_ids()) {
        f <- files[[fid]]
        cov <- cs[[fid]]
        selected <- identical(shared$selected_file, fid) && is.null(local$selected_failed)
        subject <- present(f$subject_info$id)
        who <- c(format_sex(f$subject_info$sex), present(f$subject_info$age))
        in_bed <- if (is.data.frame(f$actilife_sleep)) nrow(f$actilife_sleep) else NA_integer_

        rows[[length(rows) + 1]] <- tags$tr(
          class = paste("ov-row", if (selected) "is-selected" else ""),
          `data-fid` = fid, `data-search` = tolower(paste(f$name, subject %||% "")),
          role = "option", tabindex = focus_index(selected), `aria-selected` = tolower(as.character(selected)),
          tags$td(title = f$name,
                  as.character(subject %||% tools::file_path_sans_ext(f$name)),
                  tags$span(class = "sub", paste0(" · ", f$name))),
          tags$td(
            if (length(who) == 0) tags$span(class = "miss", "Not in file") else paste(who, collapse = ", "),
            tags$span(class = "sub", paste0(" · ", fmt_int(f$epoch_length), " s"))),
          tags$td(class = "ovl-cov-td", ax$grid,
                  if (is.null(win)) tags$span(class = "ovl-cov-none", "–")
                  else if (win$shared) ovl_cov_cell(cov, win$from, win$to)
                  else ovl_cov_cell(cov, cov$start, cov$end)),
          tags$td(ovl_gauge(cov)),
          tags$td(class = "r", if (is.na(in_bed)) "–" else fmt_int(in_bed)),
          remove_cell(f$name)
        )
      }

      msg_row <- function(fid, name, cls, message, removable = TRUE, tail = NULL) {
        tags$tr(
          class = paste("ov-row", cls),
          `data-fid` = fid, `data-search` = tolower(name),
          role = "option", tabindex = focus_index(FALSE), `aria-selected` = "false",
          tags$td(title = name, name),
          tags$td(class = "ovl-msg", colspan = "4", message, tail),
          if (removable) remove_cell(name) else tags$td()
        )
      }
      for (name in running) {
        rows[[length(rows) + 1]] <- msg_row(paste0("running|", name), name, "is-converting is-running",
                                            "Converting", tail = tags$span(class = "ov-progress"))
      }
      for (name in queued) {
        rows[[length(rows) + 1]] <- msg_row(paste0("queued|", name), name, "is-converting", "Queued")
      }
      for (fid in names(failed)) {
        f <- failed[[fid]]
        selected <- identical(local$selected_failed, fid)
        rows[[length(rows) + 1]] <- tags$tr(
          class = paste("ov-row is-error", if (selected) "is-selected" else ""),
          `data-fid` = fid, `data-search` = tolower(f$name),
          role = "option", tabindex = focus_index(selected), `aria-selected` = tolower(as.character(selected)),
          tags$td(title = f$name, tags$span(class = "ov-dot-error", `aria-hidden` = "true"), f$name),
          tags$td(class = "ovl-msg", colspan = "4", paste0("Could not read: ", f$error)),
          remove_cell(f$name)
        )
      }

      sort_head <- function(key, label, width = NULL, right = FALSE) {
        active <- identical(local$sort_key, key)
        up <- local$sort_dir > 0
        tags$th(
          style = paste0(if (!is.null(width)) paste0("width: ", width, "px;"), if (right) " text-align: right;"),
          class = if (active) "is-sorted" else NULL,
          `aria-sort` = if (active) (if (up) "ascending" else "descending") else "none",
          tags$span(class = "ovl-sortable", `data-sort` = key, role = "button", tabindex = "0",
                    title = paste("Sort by", tolower(label)), label, if (active) caret_glyph(up))
        )
      }

      # Shared axis or per-row span, as cov_window() decided; the header says which
      cov_title <- if (is.null(win)) "Coverage"
        else if (win$shared) paste0("Coverage · ", date_span(win$from, win$to))
        else "Coverage · each recording on its own span"

      tagList(
        tags$div(
          class = "ovl-scroll",
          tags$table(
            class = "ovl-gt",
            tags$colgroup(tags$col(style = "width: 260px;"), tags$col(style = "width: 150px;"),
                          tags$col(), tags$col(style = "width: 190px;"),
                          tags$col(style = "width: 86px;"), tags$col(style = "width: 34px;")),
            tags$thead(
              tags$tr(
                class = "ov-col-head",
                sort_head("name", "Recording", 260),
                sort_head("subject", "Subject", 150),
                tags$th(cov_title),
                sort_head("worn", "Worn", 190),
                sort_head("inbed", "In-bed", 86, right = TRUE),
                tags$th()
              ),
              if (!is.null(ax)) tags$tr(class = "ovl-axis",
                tags$th(), tags$th(),
                tags$th(tags$span(class = "ovl-axis-row", ax$ticks)),
                tags$th(), tags$th(), tags$th())
            ),
            tags$tbody(class = "ov-list-body", role = "listbox", `aria-label` = "Loaded recordings", rows)
          )
        ),
        tags$div(class = "ovl-foot", cov_note())
      )
    })

    # Coverage and worn hours are read per file; one may come from the wear
    # time analysis and the next from what ActiLife stored
    cov_note <- function() {
      cs <- Filter(Negate(is.null), covs())
      if (length(cs) == 0) return(NULL)
      sources <- vapply(cs, function(c) c$source, character(1))
      n_none <- sum(sources == "none")
      parts <- character(0)
      if (any(sources == "analysis")) parts <- c(parts, paste0(sum(sources == "analysis"), " from the wear time analysis"))
      if (any(sources == "agd")) parts <- c(parts, paste0(sum(sources == "agd"), " from the wear bouts in the file"))
      out <- if (length(parts) == 0) "No recording has been scored for wear yet."
        else paste0("Coverage and worn hours: ", paste(parts, collapse = ", "), ".")
      if (n_none > 0) {
        out <- paste0(out, " ", plural(n_none, "recording"), if (n_none == 1) " carries" else " carry",
                      " neither, so ", if (n_none == 1) "it is" else "they are", " left unscored.")
      }
      out
    }

    output$details <- renderUI({
      failed_id <- local$selected_failed
      if (!is.null(failed_id) && !is.null(local$failed[[failed_id]])) {
        f <- local$failed[[failed_id]]
        return(tags$div(
          class = "ov-detail",
          tags$div(class = "ov-det-head",
            tags$div(class = "ov-det-name", title = f$name, f$name),
            tags$div(class = "ov-det-sub", "Could not read")),
          tags$div(
            class = "ov-err-box",
            tags$span(class = "ov-err-title", tags$span(class = "ov-dot-error", `aria-hidden` = "true"), "This file could not be read"),
            tags$span(paste0("The parser reported: ", f$error, ". Check that it is an ActiGraph .agd or .gt3x export and add it again.")),
            actionButton(ns(paste0("remove_failed_", failed_id)), "Remove file", class = "btn-default",
                         onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("remove_row"), failed_id))
          )
        ))
      }

      fid <- shared$selected_file
      f <- if (!is.null(fid)) shared$files[[fid]] else NULL
      if (is.null(f)) {
        return(tags$div(
          class = "ov-detail",
          tags$div(class = "ov-det-empty", "Select a recording to see its signal, its settings and its epochs.")
        ))
      }

      cov <- covs()[[fid]]
      trace <- ovl_trace(f, cov, uid = fid)
      fields <- ovl_fields(f)
      hidden <- length(fields$device) + length(fields$recording) + length(fields$subject)

      trace_note <- function() {
        if (is.null(cov) || is.na(cov$worn)) {
          return(paste0("No wear bouts are stored in this file and the wear time analysis has not run for it, ",
                        "so every hour is drawn as though the device was on."))
        }
        source <- if (identical(cov$source, "analysis")) "the wear time analysis" else "the wear bouts in the file"
        if (cov$gaps == 0) {
          return(HTML(paste0("Worn for the whole recording, <strong>", fmt_dec(cov$worn, 1),
                             " h</strong>, according to ", source, ".")))
        }
        longest <- max(as.numeric(difftime(cov$segs$to[!cov$segs$worn], cov$segs$from[!cov$segs$worn], units = "hours")))
        HTML(paste0(
          "The line stops where the device came off. Those hours read zero in the file, but a zero nobody wore ",
          "is not stillness, so the series breaks rather than drawing through it. <strong>",
          fmt_dec(cov$worn, 1), " h worn of ", fmt_dec(cov$recorded, 1), " h recorded</strong>, ",
          plural(cov$gaps, "gap"), ", the longest running ", fmt_dec(longest, 1), " h. From ", source, "."))
      }

      axis_labels <- if (!is.null(cov)) {
        at <- cov$start + seq(0, 1, length.out = 5) * as.numeric(difftime(cov$end, cov$start, units = "secs"))
        lapply(seq_along(at), function(i) tags$span(fmt_date(at[i], if (i == 1 || i == 5) "%d %b %H:%M" else "%d %b")))
      } else NULL

      tags$div(
        class = "ovl-det",

        tags$div(class = "ovl-det-id",
          tags$span(class = "who", as.character(present(f$subject_info$id) %||% tools::file_path_sans_ext(f$name))),
          tags$span(class = "what", title = f$name, f$name)),

        if (!is.null(trace)) tags$div(
          class = "ovl-block",
          tags$div(class = "ovl-block-head",
            tags$span(class = "ovl-block-t", "Mean counts per hour", tags$span(" · axis 1")),
            tags$span(class = "ovl-legend",
              tags$span(class = "ovl-key", tags$i(class = "on"), "Worn"),
              tags$span(class = "ovl-key", tags$i(class = "off"), "Non-wear"),
              tags$span(class = "ovl-aside", paste0("peak ", fmt_int(trace$peak))))),
          # Clicking an hour jumps the epoch list to it. Start and end ride on
          # the element in ms UTC, which is how the file stores device local time
          tags$div(
            class = "ovl-trace-wrap",
            `data-start` = sprintf("%.0f", as.numeric(cov$start) * 1000),
            `data-end` = sprintf("%.0f", as.numeric(cov$end) * 1000),
            trace$svg,
            tags$div(class = "ovl-trace-band", `aria-hidden` = "true", hidden = NA),
            tags$div(class = "ovl-trace-cursor", `aria-hidden` = "true", hidden = NA),
            tags$div(class = "ovl-trace-tip", `aria-hidden` = "true", hidden = NA)
          ),
          tags$div(class = "ovl-trace-axis", axis_labels),
          tags$div(class = "ovl-fine", trace_note(),
                   tags$span(class = "ovl-hint", "Click the chart to move the list below to that hour."))
        ),

        # Closed by default; the summary row carries the values that decide
        # which analyses are valid
        tags$details(
          class = "ovl-disc", `data-persist` = "overview.meta",
          tags$summary(
            tags$span(class = "ovl-chev", `aria-hidden` = "true", ovl_chevron()),
            tags$span(class = "ovl-disc-t", "What the file says about itself"),
            ovl_meta_chips(fields),
            tags$span(class = "ovl-disc-more", paste(hidden, "more fields"))),
          tags$div(class = "ovl-disc-body",
            ovl_meta_table(fields),
            tags$div(class = "ovl-fine",
              "Epoch length, filter and wear site decide which cut-points and sleep algorithms apply. Body mass ",
              "and age are what energy expenditure needs; a file without them can still be scored for everything else."))
        ),

        uiOutput(ns("epochs"), class = "ovl-epochs-out")
      )
    })

    output$epochs <- renderUI({
      fid <- shared$selected_file
      f <- if (!is.null(fid)) shared$files[[fid]] else NULL
      req(f)
      preview_table(f, local$epoch_page)
    })

    EPOCH_PAGE <- 100

    # A different recording starts on its first page
    observeEvent(shared$selected_file, local$epoch_page <- 1)

    step_epochs <- function(by) {
      f <- shared$files[[shared$selected_file]]
      if (is.null(f) || !is.data.frame(f$data)) return()
      last <- max(1, ceiling(nrow(f$data) / EPOCH_PAGE))
      local$epoch_page <- min(last, max(1, local$epoch_page + by))
    }
    # A trace click arrives as a fraction of the recorded span; the epoch is
    # found here from the file's own timestamps
    observeEvent(input$trace_click, {
      fid <- shared$selected_file
      f <- if (!is.null(fid)) shared$files[[fid]] else NULL
      cov <- if (!is.null(fid)) covs()[[fid]] else NULL
      if (is.null(f) || is.null(cov) || !is.data.frame(f$data) || nrow(f$data) == 0) return()
      frac <- suppressWarnings(as.numeric(input$trace_click))
      if (length(frac) != 1 || is.na(frac)) return()
      at <- cov$start + max(0, min(1, frac)) * as.numeric(difftime(cov$end, cov$start, units = "secs"))
      i <- findInterval(as.numeric(at), as.numeric(f$data$timestamp))
      local$epoch_page <- max(1, ceiling(max(1, i) / EPOCH_PAGE))
    })

    observeEvent(input$epoch_prev, step_epochs(-1))
    observeEvent(input$epoch_next, step_epochs(1))

    # The epoch list, a hundred rows at a time with a step either side; a
    # fortnight at 5 s is a quarter of a million rows
    preview_table <- function(f, page = 1) {
      data <- f$data
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(tags$div(class = "ov-det-empty", "No epochs to show."))
      }
      n <- nrow(data)
      last_page <- max(1, ceiling(n / EPOCH_PAGE))
      page <- min(last_page, max(1, page))
      from <- (page - 1) * EPOCH_PAGE + 1
      to <- min(n, page * EPOCH_PAGE)
      shown <- data[seq(from, to), , drop = FALSE]

      labels <- c(timestamp = "Time", axis1 = "Axis 1", axis2 = "Axis 2", axis3 = "Axis 3",
                  vector_magnitude = "VM", steps = "Steps", lux = "Lux", inclineOff = "Off",
                  inclineStanding = "Stand", inclineSitting = "Sit", inclineLying = "Lie")
      cols <- names(shown)
      time_format <- if (f$epoch_length < 60) "%Y-%m-%d %H:%M:%S" else "%Y-%m-%d %H:%M"
      cell_text <- function(col, i) {
        v <- shown[[col]][i]
        if (col == "timestamp") return(format(v, time_format))
        if (is.na(v)) return("")
        if (is.numeric(v)) {
          if (col == "vector_magnitude") return(fmt_dec(v, 1))
          return(fmt_int(v))
        }
        as.character(v)
      }
      numeric_col <- function(col) col != "timestamp" && is.numeric(shown[[col]])
      header <- tags$tr(class = "ov-col-head", lapply(cols, function(col) {
        tags$th(style = if (numeric_col(col)) "text-align: right;" else NULL, labels[col] %||% col)
      }))
      body <- lapply(seq_len(nrow(shown)), function(i) {
        tags$tr(lapply(cols, function(col) {
          tags$td(class = if (numeric_col(col)) "r" else NULL, cell_text(col, i))
        }))
      })
      # The time column keeps a fixed width; the others share the rest.
      widths <- lapply(cols, function(col) {
        if (col == "timestamp") tags$col(style = "width: 160px;") else tags$col()
      })

      # The span this page covers, for the band on the trace; the last epoch
      # runs to its own end, not its start
      has_times <- "timestamp" %in% names(shown) && nrow(shown) > 0
      page_from <- if (has_times) sprintf("%.0f", as.numeric(min(shown$timestamp)) * 1000) else NULL
      page_to <- if (has_times) sprintf("%.0f", (as.numeric(max(shown$timestamp)) + f$epoch_length) * 1000) else NULL

      tags$div(
        class = "ovl-epochs", `data-from` = page_from, `data-to` = page_to,
        tags$div(class = "ovl-block-head",
          tags$span(class = "ovl-block-t", "Epochs"),
          tags$span(
            class = "ovl-pager",
            tags$span(class = "ovl-aside",
                      paste0(fmt_int(from), "–", fmt_int(to), " of ", fmt_int(n), ", device local time")),
            if (last_page > 1) tagList(
              actionButton(ns("epoch_prev"), label = chevron("left"), class = "btn-default btn-icon ov-step-btn",
                           title = "Previous 100 epochs", disabled = page == 1),
              actionButton(ns("epoch_next"), label = chevron("right"), class = "btn-default btn-icon ov-step-btn",
                           title = "Next 100 epochs", disabled = page == last_page)
            ))),
        tags$div(class = "ovl-scroll", tabindex = "0",
                 `aria-label` = paste0("Epochs ", from, " to ", to, " of ", n),
          tags$table(class = "ovl-gt", tags$colgroup(widths), tags$thead(header), tags$tbody(body)))
      )
    }
  })
}
