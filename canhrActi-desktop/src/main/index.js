const { app, BrowserWindow, Menu, ipcMain, shell, session, nativeTheme } = require('electron');
const path = require('node:path');
const fs = require('node:fs');
const { pathToFileURL } = require('node:url');
const log = require('electron-log/main');
const { startShiny, stopShiny, STARTUP_TIMEOUT_MS } = require('./r-process');
const { restoreWindow, persistWindow } = require('./window-state');
const buildMenu = require('./menu');

// CANHRACTI_PREVIEW renders one window off screen to a PNG and exits, without R.
const PREVIEW = process.env.CANHRACTI_PREVIEW || '';
// CANHRACTI_SMOKE=1 (for CI) starts as usual, exits 0 once the dashboard is connected, 1 on any failure.
const SMOKE = !PREVIEW && process.env.CANHRACTI_SMOKE === '1';
const SMOKE_CONNECT_MS = 60_000;

log.initialize({ preload: true });
log.transports.file.level = PREVIEW ? false : 'info';
log.transports.console.level = 'debug';
Object.assign(console, log.functions);

// Preview keeps off the real profile, so it can run next to the app.
// A smoke run takes no lock, so a stale instance on a CI machine cannot end it early with 0.
if (PREVIEW) {
  app.setPath('userData', path.join(app.getPath('temp'), 'canhrActi-preview'));
  app.disableHardwareAcceleration();
} else if (!SMOKE && !app.requestSingleInstanceLock()) {
  app.quit();
  process.exit(0);
}

const LINKS = {
  github: 'https://github.com/rdazadda/canhrActi',
  issues: 'https://github.com/rdazadda/canhrActi/issues',
};
const GROUND = { light: '#f5f7fa', dark: '#202020' };
const RENDERER = path.join(__dirname, '..', 'renderer');
const PRELOAD = path.join(__dirname, '..', 'preload', 'index.js');
const STAGES = ['r', 'packages', 'dashboard', 'window'];

let splashWindow = null;
let mainWindow = null;
let dialogWindow = null;
let aboutWindow = null;
let rState = null;
let rInfo = { r: null, pkg: null };
let splashState = { stage: 'r' };
let shinyOrigin = null;
let theme = 'light';
let starting = false;
let quitConfirmed = SMOKE;
let shuttingDown = false;
let smokeEnded = false;

const logFile = () => log.transports.file.getFile().path;
// Read when needed, so preview (which moves userData) never touches the real file.
const uiFile = () => path.join(app.getPath('userData'), 'ui.json');
const alive = (win) => Boolean(win) && !win.isDestroyed();

function openLogs() {
  return shell.openPath(path.dirname(logFile()));
}

function openLink(key) {
  if (Object.hasOwn(LINKS, key)) shell.openExternal(LINKS[key]);
}

function openExternal(url) {
  try {
    const { protocol } = new URL(url);
    if (['http:', 'https:', 'mailto:'].includes(protocol)) shell.openExternal(url);
  } catch { /* not a URL */ }
}

function isShinyUrl(url) {
  try {
    return Boolean(shinyOrigin) && new URL(url).origin === shinyOrigin;
  } catch {
    return false;
  }
}

function readUi() {
  try {
    return JSON.parse(fs.readFileSync(uiFile(), 'utf8')) || {};
  } catch {
    return {};
  }
}

function savedTheme() {
  const t = readUi().theme;
  return t === 'light' || t === 'dark' ? t : null;
}

function setTheme(t) {
  if (t !== 'light' && t !== 'dark') return;
  nativeTheme.themeSource = t;
  if (alive(mainWindow)) mainWindow.setBackgroundColor(GROUND[t]);
  const ui = readUi();
  theme = t;
  if (ui.theme === t) return;
  try {
    fs.writeFileSync(uiFile(), JSON.stringify({ ...ui, theme: t }, null, 2));
  } catch (err) {
    log.warn('Could not save the theme:', err.message);
  }
}

// Modal to the main window and centred on it; without a parent the window centres on screen.
function childOf(parent, width, height) {
  if (!alive(parent)) return {};
  const b = parent.getBounds();
  return {
    parent,
    modal: true,
    skipTaskbar: true,
    x: Math.round(b.x + (b.width - width) / 2),
    y: Math.round(b.y + (b.height - height) / 2),
  };
}

// The app's own pages: frameless, sandboxed, and unable to leave their file.
function localWindow(page, options) {
  const file = path.join(RENDERER, page);
  const own = pathToFileURL(file).href;
  const win = new BrowserWindow({
    frame: false,
    transparent: true,
    backgroundColor: '#00000000',
    resizable: false,
    minimizable: false,
    maximizable: false,
    fullscreenable: false,
    show: false,
    ...options,
    webPreferences: {
      sandbox: true,
      contextIsolation: true,
      nodeIntegration: false,
      webviewTag: false,
      allowRunningInsecureContent: false,
      offscreen: Boolean(PREVIEW),
      preload: PRELOAD,
    },
  });

  const guard = (event, url) => {
    if (url.split(/[?#]/)[0] !== own) event.preventDefault();
  };
  win.webContents.on('will-navigate', guard);
  win.webContents.on('will-redirect', guard);
  win.webContents.setWindowOpenHandler(() => ({ action: 'deny' }));
  if (!PREVIEW) win.once('ready-to-show', () => win.show());

  const loaded = win.loadFile(file);
  loaded.catch((err) => log.error(`Could not load ${page}:`, err.message));
  return { win, loaded };
}

function closeOnEscape(win, onEscape) {
  win.webContents.on('before-input-event', (event, input) => {
    if (input.type === 'keyDown' && input.key === 'Escape') {
      event.preventDefault();
      onEscape();
    }
  });
}

function createSplash() {
  const opened = localWindow('splash.html', {
    width: 400,
    height: 240,
    title: 'CANHRActi',
    alwaysOnTop: !PREVIEW,
  });
  const { win } = opened;
  splashWindow = win;
  win.on('closed', () => {
    if (splashWindow === win) splashWindow = null;
  });
  return opened;
}

function setSplash(state) {
  splashState = state;
  if (alive(splashWindow)) splashWindow.webContents.send('splash:state', state);
}

function openQuitDialog() {
  if (alive(dialogWindow)) {
    dialogWindow.focus();
    return null;
  }
  const parent = alive(mainWindow) ? mainWindow : null;
  const opened = localWindow('dialog.html', {
    width: 360,
    height: 144,
    title: 'Quit CANHRActi',
    ...childOf(parent, 360, 144),
  });
  const { win } = opened;
  dialogWindow = win;
  closeOnEscape(win, () => win.close());
  win.on('closed', () => {
    if (dialogWindow === win) dialogWindow = null;
    if (!quitConfirmed && alive(mainWindow)) mainWindow.focus();
  });
  return opened;
}

function openAbout() {
  if (alive(aboutWindow)) {
    aboutWindow.focus();
    return null;
  }
  const parent = alive(mainWindow) ? mainWindow : null;
  const opened = localWindow('about.html', {
    width: 400,
    height: 240,
    title: 'About CANHRActi',
    ...childOf(parent, 400, 240),
  });
  const { win } = opened;
  aboutWindow = win;
  closeOnEscape(win, () => win.close());
  win.on('closed', () => {
    if (aboutWindow === win) aboutWindow = null;
  });
  return opened;
}

function confirmQuit() {
  quitConfirmed = true;
  if (alive(dialogWindow)) dialogWindow.destroy();
  if (alive(mainWindow)) mainWindow.destroy();
  app.quit();
}

function applySecurityHeaders() {
  const csp = [
    "default-src 'self' http://127.0.0.1:* ws://127.0.0.1:*",
    "script-src 'self' 'unsafe-inline' 'unsafe-eval' http://127.0.0.1:*",
    "style-src 'self' 'unsafe-inline' http://127.0.0.1:*",
    "img-src 'self' data: blob: http://127.0.0.1:*",
    "font-src 'self' data: http://127.0.0.1:*",
    "connect-src 'self' http://127.0.0.1:* ws://127.0.0.1:*",
  ].join('; ');
  session.defaultSession.webRequest.onHeadersReceived((details, callback) => {
    if (!details.url.startsWith('http://127.0.0.1:')) {
      callback({ responseHeaders: details.responseHeaders });
      return;
    }
    callback({
      responseHeaders: {
        ...details.responseHeaders,
        'Content-Security-Policy': [csp],
      },
    });
  });
}

async function createMainWindow(url) {
  const state = restoreWindow('main', { width: 1400, height: 900 });
  const win = new BrowserWindow({
    x: state.x,
    y: state.y,
    width: state.width,
    height: state.height,
    minWidth: 1024,
    minHeight: 640,
    show: false,
    title: 'CANHRActi',
    // build/ is not packed into the app, src/renderer is.
    icon: path.join(RENDERER, 'icon.png'),
    backgroundColor: GROUND[theme],
    autoHideMenuBar: true,
    webPreferences: {
      sandbox: true,
      contextIsolation: true,
      nodeIntegration: false,
      webviewTag: false,
      allowRunningInsecureContent: false,
      preload: PRELOAD,
    },
  });
  mainWindow = win;
  persistWindow('main', win);

  // maximize() also shows the window, so it waits for the first paint like show() does.
  win.once('ready-to-show', () => {
    if (splashState.error || !alive(win)) return;
    if (state.isMaximized) win.maximize();
    win.show();
    const splash = splashWindow;
    setTimeout(() => {
      if (alive(splash) && !splashState.error) splash.close();
    }, 250);
  });

  win.webContents.setWindowOpenHandler(({ url: target }) => {
    if (isShinyUrl(target)) return { action: 'allow' };
    openExternal(target);
    return { action: 'deny' };
  });
  win.webContents.on('will-navigate', (event, target) => {
    if (isShinyUrl(target)) return;
    event.preventDefault();
    openExternal(target);
  });

  win.webContents.on('context-menu', (_event, params) => {
    const can = params.editFlags;
    Menu.buildFromTemplate([
      { role: 'cut', enabled: can.canCut },
      { role: 'copy', enabled: can.canCopy },
      { role: 'paste', enabled: can.canPaste },
      { type: 'separator' },
      { role: 'selectAll', enabled: can.canSelectAll },
    ]).popup({ window: win });
  });
  win.webContents.on('render-process-gone', (_event, details) => {
    if (details.reason === 'clean-exit' || !alive(win)) return;
    lostDashboard('renderer-gone', `The dashboard page stopped (${details.reason}, exit code ${details.exitCode}).`);
  });

  win.on('close', (event) => {
    if (quitConfirmed) return;
    event.preventDefault();
    openQuitDialog();
  });
  // A Windows shutdown or log off must not wait on the quit dialog.
  win.on('session-end', () => {
    quitConfirmed = true;
  });
  win.on('closed', () => {
    if (mainWindow === win) mainWindow = null;
  });

  setSplash({ stage: 'window' });
  log.info('Loading Shiny URL:', url);
  try {
    await win.loadURL(url);
  } catch (err) {
    err.reason = 'load';
    throw err;
  }
}

// The line CI reads is written bare to stdout and to the log, then R is stopped.
async function endSmoke(ok, reason) {
  if (smokeEnded) return;
  smokeEnded = true;
  shuttingDown = true;
  const line = ok ? 'CANHRACTI_SMOKE_OK' : `CANHRACTI_SMOKE_FAIL ${String(reason).replace(/\s+/g, ' ').trim()}`;
  log.transports.console.level = false;
  if (ok) log.info(line);
  else log.error(line);
  await new Promise((resolve) => {
    setTimeout(resolve, 1000);
    try {
      process.stdout.once('error', () => resolve());
      process.stdout.write(`${line}\n`, () => resolve());
    } catch {
      resolve();
    }
  });
  try {
    await stopShiny(rState);
  } catch (err) {
    log.error('Error stopping R:', err);
  }
  app.exit(ok ? 0 : 1);
}

async function smokeCheck(win) {
  // isConnected() is true as soon as the socket object exists, so wait for an open socket that
  // stays open with no disconnect overlay for 5 s: a server that fails at session start drops it.
  const probe = "(() => { const s = window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket; " +
    "return Boolean(s && s.readyState === 1 && !document.getElementById('shiny-disconnected-overlay')); })()";
  const deadline = Date.now() + SMOKE_CONNECT_MS;
  let steady = 0;
  while (!smokeEnded && Date.now() < deadline) {
    if (!alive(win)) {
      endSmoke(false, 'window-gone: the dashboard window closed');
      return;
    }
    if (isShinyUrl(win.webContents.getURL()) && !win.webContents.isLoading()) {
      const connected = await win.webContents.executeJavaScript(probe).catch(() => false);
      steady = connected === true ? steady + 1 : 0;
      if (steady >= 20) {
        endSmoke(true);
        return;
      }
    } else {
      steady = 0;
    }
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
  endSmoke(false, `not-connected: no Shiny session within ${SMOKE_CONNECT_MS / 1000} s`);
}

// The splash card again, for a start that failed or an R that stopped later.
function showFailure(error) {
  if (!alive(splashWindow)) createSplash();
  setSplash({ error });
  splashWindow.setAlwaysOnTop(false);
  if (splashWindow.isVisible()) splashWindow.focus();
  if (alive(mainWindow)) mainWindow.destroy();
}

function failStartup(err) {
  log.error('Failed to start CANHRActi:', err);
  if (SMOKE) endSmoke(false, `${err.reason || 'error'}: ${err.message}`);
  else showFailure(true);
}

// R or the dashboard page stopped after the window opened.
function lostDashboard(reason, message) {
  if (shuttingDown) return;
  log.error(message);
  if (SMOKE) endSmoke(false, `${reason}: ${message}`);
  else showFailure('stopped');
}

// A quit or a retry stops R on purpose, so only other exits count.
function watchR(state) {
  const onExit = (code, signal) => {
    if (starting || rState !== state) return;
    lostDashboard('r-stopped', `R stopped after startup (code ${code}${signal ? `, signal ${signal}` : ''}).`);
  };
  const { child } = state;
  if (child.exitCode !== null || child.signalCode !== null) onExit(child.exitCode, child.signalCode);
  else child.once('exit', onExit);
}

async function boot() {
  starting = true;
  rInfo = { r: null, pkg: null };
  setSplash({ stage: 'r' });
  let ready = false;
  try {
    rState = await startShiny({
      onSpawn: (state) => { rState = state; },
      onStage: (stage) => setSplash({ stage }),
      onInfo: (info) => {
        rInfo = {
          r: typeof info.r === 'string' ? info.r : null,
          pkg: typeof info.pkg === 'string' ? info.pkg : null,
        };
      },
    });
    shinyOrigin = `http://127.0.0.1:${rState.port}`;
    await createMainWindow(`${shinyOrigin}/`);
    ready = true;
  } catch (err) {
    if (!shuttingDown) failStartup(err);
  } finally {
    starting = false;
  }
  if (!ready || shuttingDown) return;
  watchR(rState);
  if (SMOKE) smokeCheck(mainWindow);
}

async function retry() {
  if (starting || shuttingDown) return;
  starting = true;
  log.info('Trying again');
  try {
    await stopShiny(rState);
  } catch (err) {
    log.error('Error stopping R:', err);
  }
  rState = null;
  if (alive(mainWindow)) mainWindow.destroy();
  if (alive(splashWindow)) splashWindow.setAlwaysOnTop(true);
  boot();
}

function fromWindow(event, ...wins) {
  const win = BrowserWindow.fromWebContents(event.sender);
  return Boolean(win) && wins.filter(Boolean).includes(win);
}

const localWindows = () => [splashWindow, dialogWindow, aboutWindow];
const anyWindow = () => [...localWindows(), mainWindow];

ipcMain.handle('app:theme', (event) => (fromWindow(event, ...anyWindow()) ? theme : null));
ipcMain.handle('app:version', (event) => (fromWindow(event, ...anyWindow()) ? app.getVersion() : null));
ipcMain.handle('app:info', (event) => {
  if (!fromWindow(event, ...anyWindow())) return null;
  return {
    version: app.getVersion(),
    electron: process.versions.electron,
    chrome: process.versions.chrome,
    node: process.versions.node,
    r: rInfo.r,
    pkg: rInfo.pkg,
  };
});

ipcMain.on('app:set-theme', (event, t) => {
  if (!fromWindow(event, mainWindow) || !event.senderFrame || !isShinyUrl(event.senderFrame.url)) return;
  setTheme(t);
});

ipcMain.on('splash:ready', (event) => {
  if (fromWindow(event, splashWindow)) event.sender.send('splash:state', splashState);
});

ipcMain.handle('splash:action', (event, action) => {
  if (!fromWindow(event, splashWindow)) return false;
  if (action === 'quit' && !PREVIEW) {
    app.quit();
  } else if (action === 'retry' && !PREVIEW) {
    retry();
  } else {
    return false;
  }
  return true;
});

ipcMain.on('dialog:result', (event, ok) => {
  if (!fromWindow(event, dialogWindow)) return;
  if (ok === true && !PREVIEW) confirmQuit();
  else dialogWindow.close();
});

ipcMain.on('win:close', (event) => {
  if (!fromWindow(event, ...localWindows())) return;
  BrowserWindow.fromWebContents(event.sender).close();
});


function descriptionVersion() {
  try {
    const text = fs.readFileSync(path.join(__dirname, '..', '..', '..', 'DESCRIPTION'), 'utf8');
    const m = text.match(/^Version:\s*(\S+)/m);
    return m ? m[1] : null;
  } catch {
    return null;
  }
}

async function runPreview() {
  const out = process.env.CANHRACTI_SHOT;
  const forced = process.env.CANHRACTI_THEME;
  theme = forced === 'light' || forced === 'dark'
    ? forced
    : savedTheme() || (nativeTheme.shouldUseDarkColors ? 'dark' : 'light');
  nativeTheme.themeSource = theme;
  rInfo = { r: 'R version 4.6.1 (2026-06-24 ucrt)', pkg: descriptionVersion() };

  const stage = STAGES.includes(process.env.CANHRACTI_STAGE) ? process.env.CANHRACTI_STAGE : 'packages';
  const pages = {
    'splash-loading': () => { splashState = { stage }; return createSplash(); },
    'splash-error': () => { splashState = { error: true }; return createSplash(); },
    'splash-stopped': () => { splashState = { error: 'stopped' }; return createSplash(); },
    quit: openQuitDialog,
    about: openAbout,
  };
  if (!Object.hasOwn(pages, PREVIEW) || !out) {
    log.error(`Preview needs CANHRACTI_PREVIEW (one of ${Object.keys(pages).join(', ')}) and CANHRACTI_SHOT.`);
    app.exit(2);
    return;
  }

  setTimeout(() => {
    log.error('Preview timed out');
    app.exit(1);
  }, 30_000);

  try {
    const { win, loaded } = pages[PREVIEW]();
    await loaded;
    await win.webContents.executeJavaScript('document.fonts ? document.fonts.ready.then(() => true) : true');
    await new Promise((r) => setTimeout(r, Number(process.env.CANHRACTI_WAIT) || 1500));
    const image = await win.webContents.capturePage();
    fs.mkdirSync(path.dirname(out), { recursive: true });
    fs.writeFileSync(out, image.toPNG());
    const size = image.getSize();
    log.info(`Saved ${out} (${size.width} x ${size.height}, ${theme})`);
    app.exit(0);
  } catch (err) {
    log.error('Preview failed:', err.message);
    app.exit(1);
  }
}

async function startApp() {
  const saved = savedTheme();
  if (saved) nativeTheme.themeSource = saved;
  theme = saved || (nativeTheme.shouldUseDarkColors ? 'dark' : 'light');

  applySecurityHeaders();
  await session.defaultSession.clearCache();

  Menu.setApplicationMenu(buildMenu({
    quit: () => (alive(mainWindow) ? mainWindow.close() : app.quit()),
    reload: () => alive(mainWindow) && mainWindow.webContents.reload(),
    openDevtools: () => alive(mainWindow) && mainWindow.webContents.openDevTools({ mode: 'detach' }),
    openLink,
    openLogs,
    about: openAbout,
  }));

  if (SMOKE) {
    log.info(`Smoke run, log file ${logFile()}`);
    setTimeout(() => endSmoke(false, 'timeout: the smoke run took too long'),
      STARTUP_TIMEOUT_MS + 2 * SMOKE_CONNECT_MS);
  }
  createSplash();
  boot();
}

app.whenReady()
  .then(PREVIEW ? runPreview : startApp)
  .catch((err) => log.error('Startup error:', err));

app.on('second-instance', () => {
  if (alive(mainWindow) && mainWindow.isMinimized()) mainWindow.restore();
  const win = [dialogWindow, aboutWindow, mainWindow, splashWindow].find(alive);
  if (win) win.focus();
});

// Every quit waits here for R to stop; a second quit request must not skip past it.
app.on('before-quit', (event) => {
  event.preventDefault();
  if (SMOKE) {
    endSmoke(false, 'quit: the app quit before the dashboard connected');
    return;
  }
  if (shuttingDown) return;
  shuttingDown = true;
  log.info('Shutting down R engine');
  stopShiny(rState)
    .catch((err) => log.error('Error during R shutdown:', err))
    .finally(() => app.exit(0));
});

app.on('window-all-closed', () => {
  app.quit();
});
