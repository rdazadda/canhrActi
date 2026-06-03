// Wrapper that locates the bundled Rscript and runs install-canhrActi.R.
// Needed because npm scripts on Windows treat `resources/...` as a command
// name on the PATH, not a relative path. Also sets R_HOME and the OS-specific
// dynamic-library path so a relocated R can find its own shared libraries.

const { spawnSync } = require('node:child_process');
const path = require('node:path');
const fs = require('node:fs');

const projectRoot = path.join(__dirname, '..');
const rRoot = path.join(projectRoot, 'resources', 'R');

let rscript;
let extraEnv = {};

if (process.platform === 'win32') {
  const candidates = [
    path.join(rRoot, 'bin', 'x64', 'Rscript.exe'),
    path.join(rRoot, 'bin', 'Rscript.exe'),
  ];
  rscript = candidates.find((p) => fs.existsSync(p));
} else if (process.platform === 'darwin') {
  const candidates = [
    path.join(rRoot, 'R.framework', 'Resources', 'Rscript'),
    path.join(rRoot, 'bin', 'Rscript'),
  ];
  rscript = candidates.find((p) => fs.existsSync(p));
  const frameworkDir = path.join(rRoot, 'R.framework', 'Resources', 'lib');
  if (fs.existsSync(frameworkDir)) {
    extraEnv.DYLD_FALLBACK_LIBRARY_PATH = frameworkDir + ':' + (process.env.DYLD_FALLBACK_LIBRARY_PATH || '');
  }
} else {
  rscript = path.join(rRoot, 'bin', 'Rscript');
  // rstudio/r-builds .deb has libR.so under lib/R/lib; without LD_LIBRARY_PATH
  // a relocated R can't find its own libraries.
  extraEnv.LD_LIBRARY_PATH = path.join(rRoot, 'lib', 'R', 'lib') + ':' + (process.env.LD_LIBRARY_PATH || '');
}

if (!rscript || !fs.existsSync(rscript)) {
  console.error(`Rscript not found under ${rRoot}`);
  console.error('Run `npm run setup:r` first.');
  process.exit(1);
}

extraEnv.R_HOME = rRoot;

// Remove any prior canhrActi + leftover lock so the reinstall starts clean.
const libDir = path.join(rRoot, 'library');
for (const d of ['canhrActi', '00LOCK-canhrActi']) {
  const stale = path.join(libDir, d);
  if (fs.existsSync(stale)) {
    try {
      fs.rmSync(stale, { recursive: true, force: true });
      console.log(`Removed prior ${d} from bundled library`);
    } catch (e) {
      console.warn(`Could not remove ${stale}: ${e.message}`);
    }
  }
}

const installScript = path.join(__dirname, 'install-canhrActi.R');
console.log(`Running: ${rscript} ${installScript}`);
if (extraEnv.LD_LIBRARY_PATH) console.log(`LD_LIBRARY_PATH: ${extraEnv.LD_LIBRARY_PATH}`);
if (extraEnv.DYLD_FALLBACK_LIBRARY_PATH) console.log(`DYLD_FALLBACK_LIBRARY_PATH: ${extraEnv.DYLD_FALLBACK_LIBRARY_PATH}`);

const result = spawnSync(rscript, ['--vanilla', installScript], {
  stdio: 'inherit',
  cwd: projectRoot,
  env: { ...process.env, ...extraEnv },
});

process.exit(result.status ?? 1);
