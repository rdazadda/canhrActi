// Renders the NSIS installer pictures from build/installer-art and writes 24-bit BMPs.
//   electron scripts/make-installer-art.js

const { app, BrowserWindow, nativeImage } = require('electron');
const fs = require('node:fs');
const path = require('node:path');

const buildDir = path.join(__dirname, '..', 'build');
const artDir = path.join(buildDir, 'installer-art');

const jobs = [
  { page: 'sidebar.html', width: 164, height: 314, out: ['installerSidebar.bmp', 'uninstallerSidebar.bmp'] },
  { page: 'header.html', width: 150, height: 57, out: ['installerHeader.bmp'] },
];

// One red pixel, used to learn the channel order of toBitmap().
const RED_PNG = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGP4z8DwHwAFAAH/iZk9HQAAAABJRU5ErkJggg==';

// Runs in the page: the font and the icon must be loaded and nothing may spill over the edge.
const READY = `(async () => {
  const faces = await document.fonts.load('500 16px Roboto', 'CANHRActi').catch(() => []);
  await Promise.all([...document.images].map((i) => i.decode().catch(() => {})));
  await new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r)));
  const spill = [...document.body.querySelectorAll('*')].some((el) => {
    const b = el.getBoundingClientRect();
    return b.right > innerWidth + 0.5 || b.bottom > innerHeight + 0.5 || el.scrollWidth > el.clientWidth + 1;
  });
  return {
    font: faces.length > 0,
    images: [...document.images].every((i) => i.naturalWidth > 0),
    spill,
  };
})()`;

// One pixel per CSS pixel, and grey text edges instead of coloured subpixel ones.
app.commandLine.appendSwitch('force-device-scale-factor', '1');
app.commandLine.appendSwitch('disable-lcd-text');
app.disableHardwareAcceleration();

// Closing one page's window must not quit before the next page renders.
app.on('window-all-closed', () => {});

function channelOrder() {
  const px = nativeImage.createFromBuffer(Buffer.from(RED_PNG, 'base64')).toBitmap();
  if (px[2] === 255 && px[0] === 0) return { r: 2, b: 0 };
  if (px[0] === 255 && px[2] === 0) return { r: 0, b: 2 };
  throw new Error('Unknown bitmap channel order');
}

// BITMAPFILEHEADER + BITMAPINFOHEADER, bottom-up rows, BGR, rows padded to 4 bytes.
function toBmp(bitmap, width, height, order) {
  const rowSize = Math.ceil((width * 3) / 4) * 4;
  const imageSize = rowSize * height;
  const buf = Buffer.alloc(54 + imageSize);
  buf.write('BM', 0, 'ascii');
  buf.writeUInt32LE(54 + imageSize, 2);
  buf.writeUInt32LE(54, 10);
  buf.writeUInt32LE(40, 14);
  buf.writeInt32LE(width, 18);
  buf.writeInt32LE(height, 22);
  buf.writeUInt16LE(1, 26);
  buf.writeUInt16LE(24, 28);
  buf.writeUInt32LE(0, 30);
  buf.writeUInt32LE(imageSize, 34);
  buf.writeInt32LE(2835, 38);
  buf.writeInt32LE(2835, 42);

  for (let y = 0; y < height; y++) {
    const src = (height - 1 - y) * width * 4;
    let dst = 54 + y * rowSize;
    for (let x = 0; x < width; x++) {
      const i = src + x * 4;
      if (bitmap[i + 3] !== 255) throw new Error(`Transparent pixel at ${x}, ${height - 1 - y}`);
      buf[dst++] = bitmap[i + order.b];
      buf[dst++] = bitmap[i + 1];
      buf[dst++] = bitmap[i + order.r];
    }
  }
  return buf;
}

async function render(job, order) {
  const win = new BrowserWindow({
    width: job.width,
    height: job.height,
    useContentSize: true,
    show: false,
    frame: false,
    backgroundColor: '#000000',
    webPreferences: { offscreen: true, sandbox: true, contextIsolation: true, nodeIntegration: false },
  });
  try {
    await win.loadFile(path.join(artDir, job.page));
    const state = await win.webContents.executeJavaScript(READY);
    if (!state.font) throw new Error(`${job.page}: Roboto did not load`);
    if (!state.images) throw new Error(`${job.page}: the icon did not load`);
    if (state.spill) throw new Error(`${job.page}: something runs past the edge`);
    await new Promise((r) => setTimeout(r, 150));

    const img = await win.webContents.capturePage();
    const size = img.getSize();
    if (size.width !== job.width || size.height !== job.height) {
      throw new Error(`${job.page}: captured ${size.width} x ${size.height}, expected ${job.width} x ${job.height}`);
    }
    const bitmap = img.toBitmap();
    if (bitmap.length !== job.width * job.height * 4) throw new Error(`${job.page}: unexpected bitmap stride`);

    const bmp = toBmp(bitmap, job.width, job.height, order);
    for (const name of job.out) {
      const file = path.join(buildDir, name);
      fs.writeFileSync(file, bmp);
      console.log(`Wrote ${file} (${job.width} x ${job.height})`);
    }
  } finally {
    win.destroy();
  }
}

app.whenReady()
  .then(async () => {
    const order = channelOrder();
    for (const job of jobs) await render(job, order);
    app.exit(0);
  })
  .catch((err) => {
    console.error(err.message);
    app.exit(1);
  });
