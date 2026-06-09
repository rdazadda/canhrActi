// Downloads a portable R distribution into ./resources/R/, matching the host
// platform by default. CI workflows can override with --platform.
//
//   node scripts/fetch-r-portable.js
//   node scripts/fetch-r-portable.js --platform win --arch x64
//   node scripts/fetch-r-portable.js --platform mac --arch arm64
//   node scripts/fetch-r-portable.js --platform linux --arch x64

const fs = require('node:fs');
const path = require('node:path');
const https = require('node:https');
const { execSync } = require('node:child_process');
const os = require('node:os');

function arg(name, fallback) {
  const i = process.argv.indexOf(name);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
}

const R_VERSION = arg('--version', '4.5.3');
const PLATFORM = arg('--platform', detectPlatform());
const ARCH = arg('--arch', detectArch());

function detectPlatform() {
  if (process.platform === 'win32') return 'win';
  if (process.platform === 'darwin') return 'mac';
  return 'linux';
}

function detectArch() {
  if (process.arch === 'arm64') return 'arm64';
  return 'x64';
}

const RES_DIR = path.join(__dirname, '..', 'resources');
const R_DIR = path.join(RES_DIR, 'R');

const SOURCES = {
  'win-x64':   `https://github.com/portable-r/portable-r-windows/releases/download/v${R_VERSION}/portable-r-${R_VERSION}-win-x64.zip`,
  'mac-arm64': `https://github.com/portable-r/portable-r-macos/releases/download/v${R_VERSION}/portable-r-${R_VERSION}-macos-arm64.tar.gz`,
  'mac-x64':   `https://github.com/portable-r/portable-r-macos/releases/download/v${R_VERSION}/portable-r-${R_VERSION}-macos-x86_64.tar.gz`,
  'linux-x64': `https://cdn.posit.co/r/manylinux_2_34/R-${R_VERSION}-manylinux_2_34.tar.gz`,
};

const key = `${PLATFORM}-${ARCH}`;
const URL = SOURCES[key];
if (!URL) {
  console.error(`Unsupported platform/arch combination: ${key}`);
  console.error('Supported: win-x64, mac-arm64, mac-x64, linux-x64');
  process.exit(1);
}

const tmpName = path.basename(URL);
const TMP = path.join(os.tmpdir(), tmpName);

function download(url, dest) {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(dest);
    const req = (u) =>
      https.get(u, (res) => {
        if (res.statusCode >= 300 && res.statusCode < 400 && res.headers.location) {
          return req(res.headers.location);
        }
        if (res.statusCode !== 200) {
          return reject(new Error(`HTTP ${res.statusCode} from ${u}`));
        }
        const total = parseInt(res.headers['content-length'] || '0', 10);
        let downloaded = 0;
        res.on('data', (chunk) => {
          downloaded += chunk.length;
          if (total) {
            const pct = ((downloaded / total) * 100).toFixed(1);
            process.stdout.write(`\rDownloading ${key} R ${R_VERSION}: ${pct}%   `);
          }
        });
        res.pipe(file);
        file.on('finish', () => {
          process.stdout.write('\n');
          file.close(() => resolve());
        });
      });
    req(url).on('error', reject);
  });
}

function extractZip() {
  if (process.platform === 'win32') {
    execSync(
      `powershell -NoProfile -Command "Expand-Archive -Path '${TMP}' -DestinationPath '${RES_DIR}' -Force"`,
      { stdio: 'inherit' }
    );
  } else {
    execSync(`unzip -q '${TMP}' -d '${RES_DIR}'`, { stdio: 'inherit' });
  }
}

function extractTarball() {
  fs.mkdirSync(R_DIR, { recursive: true });
  execSync(`tar -xzf '${TMP}' -C '${R_DIR}' --strip-components=1`, { stdio: 'inherit' });
}

(async () => {
  console.log(`Source: ${URL}`);
  fs.mkdirSync(RES_DIR, { recursive: true });

  // Self-healing: if R_DIR exists but doesn't contain a recognizable Rscript,
  // wipe it and re-fetch. Protects against poisoned cache from a previous run.
  if (fs.existsSync(R_DIR)) {
    const probes = [
      path.join(R_DIR, 'bin', 'Rscript'),
      path.join(R_DIR, 'bin', 'Rscript.exe'),
      path.join(R_DIR, 'bin', 'x64', 'Rscript.exe'),
      path.join(R_DIR, 'R.framework', 'Resources', 'Rscript'),
    ];
    if (probes.some((p) => fs.existsSync(p))) {
      console.log(`R already present at ${R_DIR}. Delete that directory to re-download.`);
      return;
    }
    console.log(`R_DIR exists but no Rscript found; wiping and re-downloading.`);
    fs.rmSync(R_DIR, { recursive: true, force: true });
  }

  try {
    await download(URL, TMP);
  } catch (err) {
    console.error(`\nDownload failed: ${err.message}`);
    process.exit(1);
  }
  console.log('Extracting...');
  try {
    if (URL.endsWith('.zip')) {
      extractZip();
      const entries = fs.readdirSync(RES_DIR);
      const extracted = entries.find((e) => e.startsWith('portable-r-'));
      if (extracted) fs.renameSync(path.join(RES_DIR, extracted), R_DIR);
    } else if (URL.endsWith('.tar.gz') || URL.endsWith('.tgz')) {
      extractTarball();
    } else {
      throw new Error(`Unknown archive type for ${URL}`);
    }
  } catch (err) {
    console.error(`Extraction failed: ${err.message}`);
    process.exit(1);
  }
  try { fs.unlinkSync(TMP); } catch { /* ignore */ }
  console.log(`R installed at ${R_DIR}`);
  console.log(`Next: npm run setup:packages`);
})();
