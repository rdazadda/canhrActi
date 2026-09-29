// Wrapper that locates the bundled Rscript and runs install-canhrActi.R.
// Needed because npm scripts on Windows treat `resources/...` as a command
// name on the PATH, not a relative path. On macOS it then rewrites package
// libraries that name /Library/Frameworks/R.framework and re-signs them.

const { spawnSync } = require('node:child_process');
const crypto = require('node:crypto');
const path = require('node:path');
const fs = require('node:fs');

const projectRoot = path.join(__dirname, '..');
const rRoot = path.join(projectRoot, 'resources', 'R');

// R_HOME is resources/R on Windows and macOS and resources/R/lib/R on Linux.
const rHome = [path.join(rRoot, 'lib', 'R'), path.join(rRoot, 'R.framework', 'Resources'), rRoot]
  .find((p) => fs.existsSync(path.join(p, 'library', 'base'))) || rRoot;

let rscript;
const extraEnv = {};

if (process.platform === 'win32') {
  const candidates = [
    path.join(rRoot, 'bin', 'x64', 'Rscript.exe'),
    path.join(rRoot, 'bin', 'Rscript.exe'),
  ];
  rscript = candidates.find((p) => fs.existsSync(p));
} else if (process.platform === 'darwin') {
  const candidates = [
    path.join(rRoot, 'bin', 'Rscript'),
    path.join(rRoot, 'R.framework', 'Resources', 'Rscript'),
  ];
  rscript = candidates.find((p) => fs.existsSync(p));
  // Packages compiled here would otherwise require the build machine's macOS.
  extraEnv.MACOSX_DEPLOYMENT_TARGET = process.env.MACOSX_DEPLOYMENT_TARGET || '12.0';
} else {
  rscript = path.join(rRoot, 'bin', 'Rscript');
  extraEnv.LD_LIBRARY_PATH = path.join(rHome, 'lib') + ':' + (process.env.LD_LIBRARY_PATH || '');
}

if (!rscript || !fs.existsSync(rscript)) {
  console.error(`Rscript not found under ${rRoot}`);
  console.error('Run `npm run setup:r` first.');
  process.exit(1);
}

extraEnv.R_HOME = rHome;

// Remove any prior canhrActi + leftover lock so the reinstall starts clean.
const libDir = path.join(rHome, 'library');
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
for (const [k, v] of Object.entries(extraEnv)) console.log(`${k}: ${v}`);

const result = spawnSync(rscript, ['--vanilla', installScript], {
  stdio: 'inherit',
  cwd: projectRoot,
  env: { ...process.env, ...extraEnv },
});
if (result.status !== 0) process.exit(result.status ?? 1);

function sharedLibraries(dir, found = []) {
  if (!fs.existsSync(dir)) return found;
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, entry.name);
    if (entry.isDirectory()) sharedLibraries(p, found);
    else if (entry.isFile() && /\.(so|dylib)$/.test(entry.name)) found.push(p);
  }
  return found;
}

function hashes(files) {
  const out = new Map();
  for (const f of files) {
    out.set(f, crypto.createHash('sha1').update(fs.readFileSync(f)).digest('hex'));
  }
  return out;
}

function run(cmd, args) {
  const r = spawnSync(cmd, args, { stdio: 'inherit' });
  if (r.status !== 0) {
    console.error(`${cmd} ${args.join(' ')} failed`);
    process.exit(r.status ?? 1);
  }
}

// CRAN and some PPM macOS binaries name /Library/Frameworks/R.framework libraries,
// which on a Mac with CRAN R installed load that R's copies instead of ours.
if (process.platform === 'darwin') {
  const fixDylibs = path.join(rHome, 'bin', 'fix-dylibs');
  if (!fs.existsSync(fixDylibs)) {
    console.warn(`${fixDylibs} not found; package libraries were not rewritten.`);
  } else {
    const dirs = [path.join(rHome, 'library'), path.join(rHome, 'modules')];
    const before = hashes(dirs.flatMap((d) => sharedLibraries(d)));
    run('bash', [fixDylibs]);
    const after = hashes([...before.keys()]);
    const changed = [...before.keys()].filter((f) => before.get(f) !== after.get(f));
    for (const f of changed) run('codesign', ['--force', '--sign', '-', f]);
    console.log(`Rewrote and re-signed ${changed.length} package libraries.`);
  }
}
