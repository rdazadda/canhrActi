// Persists BrowserWindow position/size/maximized across launches.

const { app, screen } = require('electron');
const path = require('node:path');
const fs = require('node:fs');

const STATE_FILE = () => path.join(app.getPath('userData'), 'window-state.json');

function readState() {
  try {
    return JSON.parse(fs.readFileSync(STATE_FILE(), 'utf8'));
  } catch {
    return {};
  }
}

function writeState(state) {
  try {
    fs.writeFileSync(STATE_FILE(), JSON.stringify(state, null, 2));
  } catch { /* ignore */ }
}

function restoreWindow(name, defaults) {
  const state = readState();
  const saved = state[name];
  if (!saved) return defaults;

  // The saved rectangle must still intersect at least one display
  // (handles "unplugged second monitor" case).
  const visible = screen.getAllDisplays().some((d) => {
    const b = d.bounds;
    return (
      saved.x + saved.width > b.x &&
      saved.x < b.x + b.width &&
      saved.y + saved.height > b.y &&
      saved.y < b.y + b.height
    );
  });
  return visible ? saved : defaults;
}

function persistWindow(name, win) {
  let pending = null;
  const save = () => {
    pending = null;
    if (win.isDestroyed()) return;
    const isMaximized = win.isMaximized();
    const b = isMaximized ? win.getNormalBounds() : win.getBounds();
    const state = readState();
    state[name] = { ...b, isMaximized };
    writeState(state);
  };
  const schedule = () => {
    if (pending) clearTimeout(pending);
    pending = setTimeout(save, 400);
  };
  win.on('resize', schedule);
  win.on('move', schedule);
  win.on('close', save);
}

module.exports = { restoreWindow, persistWindow };
