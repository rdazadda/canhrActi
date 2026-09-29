const { spawn, execFile } = require('node:child_process');
const net = require('node:net');
const path = require('node:path');
const fs = require('node:fs');
const { app } = require('electron');
const log = require('electron-log/main');
const getPortModule = require('get-port');
const getPort = getPortModule.default || getPortModule;

const READY_SENTINEL = '__CANHRACTI_READY__';
const STAGE_PREFIX = '__CANHRACTI_STAGE__ ';
const INFO_PREFIX = '__CANHRACTI_INFO__ ';
const R_STAGES = ['packages', 'dashboard'];
const STARTUP_TIMEOUT_MS = 120_000;

// reason tells the main process which plain message to show.
function startupError(reason, message) {
  const err = new Error(message);
  err.reason = reason;
  return err;
}

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
    // Portable R for macOS is a flat tree with no R.framework.
    const candidates = [
      path.join(rRoot, 'bin', 'Rscript'),
      path.join(rRoot, 'R.framework', 'Resources', 'Rscript'),
    ];
    rscript = candidates.find((p) => fs.existsSync(p));
  } else {
    rscript = path.join(rRoot, 'bin', 'Rscript');
  }

  if (!rscript || !fs.existsSync(rscript)) {
    throw startupError(
      'r-missing',
      `Rscript not found under ${rRoot}. ` +
      `In development, run "npm run setup:r" first.`
    );
  }
  if (!fs.existsSync(launcher)) {
    throw startupError('launcher-missing', `R launcher not found at ${launcher}`);
  }
  return { rRoot, rscript, launcher };
}

async function pickPort() {
  const portList = [];
  for (let p = 13000; p <= 13999; p++) portList.push(p);
  return getPort({ host: '127.0.0.1', port: portList });
}

// Splits a stream into whole lines, keeping a partial line until the next chunk.
function lineReader(onLine) {
  let rest = '';
  return {
    push(chunk) {
      const parts = (rest + chunk).split(/\r?\n/);
      rest = parts.pop();
      for (const line of parts) if (line) onLine(line);
    },
    end() {
      if (rest) onLine(rest);
      rest = '';
    },
  };
}

function exitText(code, signal) {
  if (signal) return `R was stopped by ${signal} during startup.`;
  return `R exited with code ${code} during startup.`;
}

// R's temp folders go under a root the app owns, one folder per R named <app pid>-<time>,
// because a killed R never deletes its own.
const tempRoot = () => path.join(app.getPath('temp'), 'CANHRActi-R');

function isRunning(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (err) {
    return err.code === 'EPERM';
  }
}

function removeDir(dir) {
  try {
    fs.rmSync(dir, { recursive: true, force: true, maxRetries: 3, retryDelay: 100 });
  } catch (err) {
    log.warn(`Could not remove ${dir}: ${err.message}`);
  }
}

// The root must be a real folder owned by this user; on a shared /tmp someone else could make it first.
function ownTempRoot() {
  try {
    fs.mkdirSync(tempRoot(), { recursive: true, mode: 0o700 });
    const st = fs.lstatSync(tempRoot());
    return st.isDirectory() && !st.isSymbolicLink() && (!process.getuid || st.uid === process.getuid());
  } catch {
    return false;
  }
}

// Clears the folders of earlier R processes, keeping those of other app runs still open.
function sweepTemp() {
  let names;
  try {
    names = fs.readdirSync(tempRoot());
  } catch {
    return;
  }
  for (const name of names) {
    if (!/^d+-d+$/.test(name)) continue;
    const pid = parseInt(name, 10);
    if (pid !== process.pid && pid > 0 && isRunning(pid)) continue;
    removeDir(path.join(tempRoot(), name));
  }
}

async function startShiny({ onStage, onInfo, onSpawn } = {}) {
  const { rRoot, rscript, launcher } = resolveR();
  // R_HOME is resources/R on Windows and macOS and resources/R/lib/R on Linux.
  const rHome = [path.join(rRoot, 'lib', 'R'), rRoot]
    .find((p) => fs.existsSync(path.join(p, 'library', 'base'))) || rRoot;
  const port = await pickPort();
  log.info(`Spawning R: ${rscript}`);
  log.info(`Shiny port: 127.0.0.1:${port}`);

  const env = {
    ...process.env,
    R_HOME: rHome,
    R_LIBS_SITE: path.join(rHome, 'library'),
    R_LIBS_USER: path.join(app.getPath('userData'), 'R-library'),
    CANHR_SHINY_PORT: String(port),
    CANHR_SHINY_HOST: '127.0.0.1',
    R_DISABLE_HTTPD: '1',
  };

  // Relocated R needs help finding its own shared libraries on Linux.
  if (process.platform === 'linux') {
    env.LD_LIBRARY_PATH = path.join(rHome, 'lib') + ':' + (process.env.LD_LIBRARY_PATH || '');
  }

  // An app opened from Finder or the Dock gets no LANG, and R would then read text as ASCII.
  if (process.platform === 'darwin' && !env.LC_ALL && !env.LC_CTYPE && !env.LANG) {
    env.LANG = 'en_US.UTF-8';
  }

  try {
    fs.mkdirSync(env.R_LIBS_USER, { recursive: true });
  } catch { /* ignore */ }

  // R falls back to the system temp folder if this one cannot be made.
  const ownRoot = ownTempRoot();
  if (ownRoot) sweepTemp();
  const tempDir = path.join(tempRoot(), `${process.pid}-${Date.now()}`);
  try {
    if (!ownRoot) throw new Error('the temp root is not a private folder');
    fs.mkdirSync(tempDir, { recursive: true, mode: 0o700 });
    env.TMPDIR = tempDir;
    env.TMP = tempDir;
    env.TEMP = tempDir;
  } catch (err) {
    log.warn(`Could not create the R temp folder ${tempDir}: ${err.message}`);
  }

  const child = spawn(rscript, ['--vanilla', launcher], {
    env,
    windowsHide: true,
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  child.once('exit', () => removeDir(tempDir));
  const state = { child, port };
  if (onSpawn) onSpawn(state);
  if (onStage) onStage('r');

  let stage = 'r';
  let settled = false;

  await new Promise((resolve, reject) => {
    // A failed start leaves nothing worth keeping, so R is stopped before the error is reported.
    const settle = (err) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      if (!err) {
        resolve();
        return;
      }
      err.stage = stage;
      stopShiny(state).finally(() => reject(err));
    };

    const timer = setTimeout(() => {
      settle(startupError('timeout', `R startup timed out after ${STARTUP_TIMEOUT_MS / 1000}s.`));
    }, STARTUP_TIMEOUT_MS);

    // READY is printed before runApp, so the window waits until Shiny accepts connections.
    const waitForPort = () => {
      if (settled) return;
      const socket = net.connect({ host: '127.0.0.1', port });
      socket.once('connect', () => {
        socket.destroy();
        settle();
      });
      socket.once('error', () => {
        socket.destroy();
        setTimeout(waitForPort, 150);
      });
    };

    const onLine = (stream) => (line) => {
      if (stream === 'stderr') log.warn(`[R] ${line}`);
      else log.info(`[R] ${line}`);
      if (stream !== 'stdout' || settled) return;

      const text = line.trim();
      if (text === READY_SENTINEL) {
        waitForPort();
      } else if (text.startsWith(STAGE_PREFIX)) {
        const name = text.slice(STAGE_PREFIX.length).trim();
        if (R_STAGES.includes(name)) {
          stage = name;
          if (onStage) onStage(name);
        }
      } else if (text.startsWith(INFO_PREFIX)) {
        try {
          if (onInfo) onInfo(JSON.parse(text.slice(INFO_PREFIX.length)));
        } catch (err) {
          log.warn('Could not read R version info:', err.message);
        }
      }
    };

    const out = lineReader(onLine('stdout'));
    const errs = lineReader(onLine('stderr'));
    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');
    child.stdout.on('data', (chunk) => out.push(chunk));
    child.stderr.on('data', (chunk) => errs.push(chunk));

    child.on('error', (err) => {
      settle(startupError('spawn', err.message));
    });

    // 'close' comes after the last output, so R's own error lines are logged before the report.
    child.once('exit', (code, signal) => {
      log.info(`R exited (code ${code}${signal ? `, signal ${signal}` : ''})`);
      const report = () => {
        out.end();
        errs.end();
        settle(startupError('exit', exitText(code, signal)));
      };
      const fallback = setTimeout(report, 500);
      child.once('close', () => {
        clearTimeout(fallback);
        report();
      });
    });
  });

  log.info('R + Shiny ready');
  return state;
}

async function stopShiny(state) {
  if (!state || !state.child || state.child.killed) return;
  const { child } = state;
  if (!child.pid || child.exitCode !== null || child.signalCode !== null) return;

  const exited = new Promise((resolve) => child.once('exit', resolve));

  // Rscript ignores SIGTERM on Windows; kill the process tree directly.
  if (process.platform === 'win32') {
    await new Promise((resolve) => {
      execFile('taskkill.exe', ['/pid', String(child.pid), '/T', '/F'], { windowsHide: true }, () => resolve());
    });
    await Promise.race([exited, new Promise((resolve) => setTimeout(resolve, 2000))]);
  } else {
    child.kill('SIGTERM');
    await new Promise((resolve) => {
      const fallback = setTimeout(() => {
        try { child.kill('SIGKILL'); } catch { /* ignore */ }
        resolve();
      }, 3000);
      exited.then(() => {
        clearTimeout(fallback);
        resolve();
      });
    });
  }
}

module.exports = { startShiny, stopShiny, STARTUP_TIMEOUT_MS };
