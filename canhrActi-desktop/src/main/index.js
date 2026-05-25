const { app, BrowserWindow, Menu, ipcMain, dialog, shell, session } = require('electron');
const path = require('node:path');
const log = require('electron-log/main');
const { startShiny, stopShiny } = require('./r-process');
const { restoreWindow, persistWindow } = require('./window-state');
const buildMenu = require('./menu');

log.initialize({ preload: true });
log.transports.file.level = 'info';
log.transports.console.level = 'debug';
Object.assign(console, log.functions);

if (!app.requestSingleInstanceLock()) {
  app.quit();
  process.exit(0);
}

let splashWindow = null;
let mainWindow = null;
let rState = null;
let quitConfirmed = false;

function createSplash() {
  splashWindow = new BrowserWindow({
    width: 480,
    height: 320,
    frame: false,
    transparent: true,
    resizable: false,
    alwaysOnTop: true,
    show: false,
    skipTaskbar: false,
    webPreferences: {
      sandbox: true,
      contextIsolation: true,
      nodeIntegration: false,
      preload: path.join(__dirname, '..', 'preload', 'index.js'),
    },
  });
  splashWindow.loadFile(path.join(__dirname, '..', 'renderer', 'splash.html'));
  splashWindow.once('ready-to-show', () => splashWindow.show());
}

function applySecurityHeaders() {
  session.defaultSession.webRequest.onHeadersReceived((details, callback) => {
    const csp = [
      "default-src 'self' http://127.0.0.1:* ws://127.0.0.1:*",
      "script-src 'self' 'unsafe-inline' 'unsafe-eval' http://127.0.0.1:*",
      "style-src 'self' 'unsafe-inline' http://127.0.0.1:*",
      "img-src 'self' data: blob: http://127.0.0.1:*",
      "font-src 'self' data: http://127.0.0.1:*",
      "connect-src 'self' http://127.0.0.1:* ws://127.0.0.1:*",
    ].join('; ');
    callback({
      responseHeaders: {
        ...details.responseHeaders,
        'Content-Security-Policy': [csp],
      },
    });
  });
}

async function createMainWindow(shinyUrl) {
  const state = restoreWindow('main', { width: 1400, height: 900 });
  mainWindow = new BrowserWindow({
    x: state.x,
    y: state.y,
    width: state.width,
    height: state.height,
    minWidth: 1024,
    minHeight: 640,
    show: false,
    title: 'CANHRActi',
    icon: path.join(__dirname, '..', '..', 'build', 'icon.png'),
    backgroundColor: '#0f172a',
    webPreferences: {
      sandbox: true,
      contextIsolation: true,
      nodeIntegration: false,
      webviewTag: false,
      allowRunningInsecureContent: false,
      preload: path.join(__dirname, '..', 'preload', 'index.js'),
    },
  });

  if (state.isMaximized) mainWindow.maximize();
  persistWindow('main', mainWindow);

  Menu.setApplicationMenu(buildMenu({
    openDevtools: () => mainWindow.webContents.openDevTools({ mode: 'detach' }),
    reload: () => mainWindow.webContents.reload(),
  }));

  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
    if (splashWindow && !splashWindow.isDestroyed()) {
      setTimeout(() => splashWindow.close(), 250);
    }
  });

  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (url.startsWith('http://127.0.0.1:')) return { action: 'allow' };
    shell.openExternal(url);
    return { action: 'deny' };
  });

  mainWindow.on('close', async (event) => {
    if (quitConfirmed) return;
    event.preventDefault();
    const { response } = await dialog.showMessageBox(mainWindow, {
      type: 'question',
      buttons: ['Yes', 'No'],
      defaultId: 1,
      cancelId: 1,
      title: 'Quit CANHRActi',
      message: 'Are you sure you want to quit CANHRActi?',
    });
    if (response === 0) {
      quitConfirmed = true;
      mainWindow.destroy();
    }
  });

  log.info('Loading Shiny URL:', shinyUrl);
  await mainWindow.loadURL(shinyUrl);
}

app.whenReady().then(async () => {
  applySecurityHeaders();
  await session.defaultSession.clearCache();
  createSplash();

  try {
    rState = await startShiny();
    await createMainWindow(`http://127.0.0.1:${rState.port}/`);
  } catch (err) {
    log.error('Failed to start CANHRActi:', err);
    const logFile = log.transports.file.getFile().path;
    dialog.showErrorBox(
      'CANHRActi failed to start',
      `${err.message}\n\nLog: ${logFile}`
    );
    app.quit();
  }
});

ipcMain.handle('app:version', () => app.getVersion());
ipcMain.handle('app:logs-path', () => log.transports.file.getFile().path);
ipcMain.handle('app:open-logs', async () => {
  await shell.openPath(path.dirname(log.transports.file.getFile().path));
  return true;
});

app.on('second-instance', () => {
  if (mainWindow) {
    if (mainWindow.isMinimized()) mainWindow.restore();
    mainWindow.focus();
  }
});

let shuttingDown = false;
app.on('before-quit', async (event) => {
  if (shuttingDown) return;
  event.preventDefault();
  shuttingDown = true;
  log.info('Shutting down R engine');
  try {
    await stopShiny(rState);
  } catch (err) {
    log.error('Error during R shutdown:', err);
  }
  app.exit(0);
});

app.on('window-all-closed', () => {
  app.quit();
});
