// Removes R's manuals and per-package help, HTML and vignettes from the bundled
// R tree, and on macOS the tcltk package, whose library needs XQuartz and a Tcl
// under /opt/R that no user Mac has. share/, translations and fonts stay.

const fs = require('node:fs');
const path = require('node:path');

const R_DIR = path.join(__dirname, '..', 'resources', 'R');

if (!fs.existsSync(R_DIR)) {
  console.error(`No R bundle found at ${R_DIR}`);
  console.error('Run `npm run setup:r` first.');
  process.exit(1);
}

// R_HOME is resources/R on Windows and macOS and resources/R/lib/R on Linux.
const R_HOME = [path.join(R_DIR, 'lib', 'R'), path.join(R_DIR, 'R.framework', 'Resources'), R_DIR]
  .find((p) => fs.existsSync(path.join(p, 'library', 'base')));
if (!R_HOME) {
  console.error(`No R library found under ${R_DIR}`);
  process.exit(1);
}

function treeSize(p) {
  const stat = fs.lstatSync(p);
  if (!stat.isDirectory()) return stat.size;
  let bytes = 0;
  for (const entry of fs.readdirSync(p)) bytes += treeSize(path.join(p, entry));
  return bytes;
}

function delTree(p) {
  if (!fs.existsSync(p)) return 0;
  const bytes = treeSize(p);
  fs.rmSync(p, { recursive: true, force: true });
  return bytes;
}

const mb = (bytes) => `${(bytes / 1024 / 1024).toFixed(1)} MB`;
const start = treeSize(R_DIR);
console.log(`R home: ${R_HOME}`);

const topLevel = ['doc/manual'];
if (process.platform === 'darwin') topLevel.push('library/tcltk');
for (const rel of topLevel) {
  const full = path.join(R_HOME, rel);
  if (fs.existsSync(full)) console.log(`  removed ${rel} (${mb(delTree(full))})`);
}

const libDir = path.join(R_HOME, 'library');
let perPkg = 0;
for (const pkg of fs.readdirSync(libDir)) {
  for (const sub of ['help', 'html', 'doc']) {
    perPkg += delTree(path.join(libDir, pkg, sub));
  }
}
console.log(`  removed per-package help/html/doc (${mb(perPkg)})`);

// macOS R ships fontconfig's conf.d entries as absolute links into
// /Library/Frameworks, which break once R is copied into the app and stop
// electron-builder. Each broken link is pointed at the bundle's own copy in a
// sibling conf.avail, or removed when there is none.
function fixBrokenLinks(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, entry.name);
    if (entry.isSymbolicLink()) {
      if (fs.existsSync(p)) continue;
      const local = path.join(dir, '..', 'conf.avail', entry.name);
      fs.unlinkSync(p);
      if (fs.existsSync(local)) {
        fs.symlinkSync(path.relative(dir, local), p);
        console.log(`  relinked ${path.relative(R_DIR, p)}`);
      } else {
        console.log(`  removed broken link ${path.relative(R_DIR, p)}`);
      }
    } else if (entry.isDirectory()) {
      fixBrokenLinks(p);
    }
  }
}
fixBrokenLinks(R_DIR);

const end = treeSize(R_DIR);
console.log(`R bundle: ${mb(start)} -> ${mb(end)} (saved ${mb(start - end)})`);
