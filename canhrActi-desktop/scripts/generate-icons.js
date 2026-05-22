// Generates platform-specific app icons (.ico, .icns) from build/icon.png.
//   node scripts/generate-icons.js

const fs = require('node:fs');
const path = require('node:path');
const png2icons = require('png2icons');

const buildDir = path.join(__dirname, '..', 'build');
const srcPng = path.join(buildDir, 'icon.png');
const icoOut = path.join(buildDir, 'icon.ico');
const icnsOut = path.join(buildDir, 'icon.icns');

if (!fs.existsSync(srcPng)) {
  console.error(`Source not found: ${srcPng}`);
  process.exit(1);
}

const input = fs.readFileSync(srcPng);

const ico = png2icons.createICO(input, png2icons.BICUBIC, 0, false, true);
if (!ico) {
  console.error('ICO generation failed');
  process.exit(1);
}
fs.writeFileSync(icoOut, ico);
console.log(`Wrote ${icoOut} (${ico.length} bytes)`);

const icns = png2icons.createICNS(input, png2icons.BICUBIC, 0);
if (!icns) {
  console.error('ICNS generation failed');
  process.exit(1);
}
fs.writeFileSync(icnsOut, icns);
console.log(`Wrote ${icnsOut} (${icns.length} bytes)`);
