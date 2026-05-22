// Wrapper that locates the bundled Rscript and runs install-canhrActi.R.
// Needed because npm scripts on Windows treat `resources/...` as a command
// name on the PATH, not a relative path.

const { spawnSync } = require('node:child_process');
const path = require('node:path');
const fs = require('node:fs');

const projectRoot = path.join(__dirname, '..');
const rRoot = path.join(projectRoot, 'resources', 'R');

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
  console.error(`Rscript not found under ${rRoot}`);
  console.error('Run `npm run setup:r` first.');
  process.exit(1);
}

const installScript = path.join(__dirname, 'install-canhrActi.R');
console.log(`Running: ${rscript} ${installScript}`);

const result = spawnSync(rscript, ['--vanilla', installScript], {
  stdio: 'inherit',
  cwd: projectRoot,
});

process.exit(result.status ?? 1);
