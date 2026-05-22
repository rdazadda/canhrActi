// Removes PDF manuals, non-English locales, and per-package help/HTML from
// the bundled R tree. All targeted paths are safe to delete for a runtime
// embed (R itself does not require them).

const fs = require('node:fs');
const path = require('node:path');

const R_DIR = path.join(__dirname, '..', 'resources', 'R');

if (!fs.existsSync(R_DIR)) {
  console.error(`No R bundle found at ${R_DIR}`);
  console.error('Run `npm run setup:r` first.');
  process.exit(1);
}

function delTree(p) {
  if (!fs.existsSync(p)) return 0;
  const stat = fs.statSync(p);
  if (stat.isFile()) {
    const size = stat.size;
    fs.unlinkSync(p);
    return size;
  }
  let bytes = 0;
  for (const entry of fs.readdirSync(p)) {
    bytes += delTree(path.join(p, entry));
  }
  fs.rmdirSync(p);
  return bytes;
}

let saved = 0;

const topLevel = ['doc/manual', 'share/locale'];
for (const rel of topLevel) {
  const full = path.join(R_DIR, rel);
  if (fs.existsSync(full)) {
    const bytes = delTree(full);
    saved += bytes;
    console.log(`  removed ${rel} (${(bytes / 1024 / 1024).toFixed(1)} MB)`);
  }
}

const libDir = path.join(R_DIR, 'library');
if (fs.existsSync(libDir)) {
  let perPkg = 0;
  for (const pkg of fs.readdirSync(libDir)) {
    for (const sub of ['help', 'html', 'doc']) {
      const target = path.join(libDir, pkg, sub);
      if (fs.existsSync(target)) {
        perPkg += delTree(target);
      }
    }
  }
  saved += perPkg;
  console.log(`  removed per-package help/html/doc (${(perPkg / 1024 / 1024).toFixed(1)} MB)`);
}

console.log(`Total saved: ${(saved / 1024 / 1024).toFixed(1)} MB`);
