const { spawn, exec } = require('node:child_process');
const path = require('node:path');
const fs = require('node:fs');
const { app } = require('electron');
const log = require('electron-log/main');
const getPortModule = require('get-port');
const getPort = getPortModule.default || getPortModule;

const READY_SENTINEL = '__CANHRACTI_READY__';
const STARTUP_TIMEOUT_MS = 120_000;

function resolveR() {
  const rRoot = app.isPackaged
    ? path.join(process.resourcesPath, 'R')
    : path.join(__dirname, '..', '..', 'resources', 'R');

  const launcher = app.isPackaged
    ? path.join(process.resourcesPath, 'app-r', 'launch.R')
    : path.join(__dirname, '..', '..', 'rcode', 'launch.R');

  let rscript;
  if (process.platform === 'win32') {
    const candidates = [
      path.join(rRoot, 'bin', 'x64', 'Rscript.exe'),
      path.join(rRoot, 'bin', 'Rscript.exe'),
    ];
    rscript = candidates.find((p) => fs.existsSync(p));
  } else if (process.platform === 'darwin') {
    rscript = path.join(rRoot, 'R.framework', 'Resources', 'Rscript');
  } else {
    rscript = path.join(rRoot, 'bin', 'Rscript');
  }

  if (!rscript || !fs.existsSync(rscript)) {
    throw new Error(
      `Rscript not found under ${rRoot}. ` +
      `In development, run "npm run setup:r" first.`
    );
  }
  if (!fs.existsSync(launcher)) {
    throw new Error(`R launcher not found at ${launcher}`);
  }
  return { rRoot, rscript, launcher };
}

async function pickPort() {
  const portList = [];
  for (let p = 13000; p <= 13999; p++) portList.push(p);
  return getPort({ host: '127.0.0.1', port: portList });
}

async function startShiny({ onLog } = {}) {
  const { rRoot, rscript, launcher } = resolveR();
  const port = await pickPort();
  log.info(`Spawning R: ${rscript}`);
  log.info(`Shiny port: 127.0.0.1:${port}`);

  const env = {
    ...process.env,
    R_HOME: rRoot,
    R_LIBS_SITE: path.join(rRoot, 'library'),
    R_LIBS_USER: path.join(app.getPath('userData'), 'R-library'),
    CANHR_SHINY_PORT: String(port),
    CANHR_SHINY_HOST: '127.0.0.1',
    R_DISABLE_HTTPD: '1',
  };

  // Relocated R needs help finding its own shared libraries on *nix platforms.
  if (process.platform === 'linux') {
    env.LD_LIBRARY_PATH = path.join(rRoot, 'lib', 'R', 'lib') + ':' + (process.env.LD_LIBRARY_PATH || '');
  } else if (process.platform === 'darwin') {
    const fwLib = path.join(rRoot, 'R.framework', 'Resources', 'lib');
    if (fs.existsSync(fwLib)) {
      env.DYLD_FALLBACK_LIBRARY_PATH = fwLib + ':' + (process.env.DYLD_FALLBACK_LIBRARY_PATH || '');
    }
  }

  try {
    fs.mkdirSync(env.R_LIBS_USER, { recursive: true });
  } catch { /* ignore */ }

  const child = spawn(rscript, ['--vanilla', launcher], {
    env,
    windowsHide: true,
    stdio: ['ignore', 'pipe', 'pipe'],
  });

  const forward = (stream) => (data) => {
    const lines = data.toString().split(/\r?\n/).filter(Boolean);
    for (const line of lines) {
      if (stream === 'stderr') log.warn(`[R] ${line}`);
      else log.info(`[R] ${line}`);
      if (onLog) onLog(line);
    }
  };
  child.stdout.on('data', forward('stdout'));
  child.stderr.on('data', forward('stderr'));

  await new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      reject(new Error(`R startup timed out after ${STARTUP_TIMEOUT_MS / 1000}s.`));
    }, STARTUP_TIMEOUT_MS);

    const checkLine = (data) => {
      if (data.toString().includes(READY_SENTINEL)) {
        clearTimeout(timer);
        child.stdout.off('data', checkLine);
        resolve();
      }
    };
    child.stdout.on('data', checkLine);

    child.on('error', (err) => {
      clearTimeout(timer);
      reject(err);
    });
    child.on('exit', (code) => {
      clearTimeout(timer);
      if (code !== 0) {
        reject(new Error(`R exited with code ${code} during startup.`));
      }
    });
  });

  log.info('R + Shiny ready');
  return { child, port };
}

async function stopShiny(state) {
  if (!state || !state.child || state.child.killed) return;
  const { child } = state;

  // Rscript ignores SIGTERM on Windows; kill the process tree directly.
  if (process.platform === 'win32') {
    await new Promise((resolve) => {
      exec(`taskkill /pid ${child.pid} /T /F`, () => resolve());
    });
  } else {
    child.kill('SIGTERM');
    await new Promise((resolve) => {
      const fallback = setTimeout(() => {
        try { child.kill('SIGKILL'); } catch { /* ignore */ }
        resolve();
      }, 3000);
      child.on('exit', () => {
        clearTimeout(fallback);
        resolve();
      });
    });
  }
}

module.exports = { startShiny, stopShiny };
