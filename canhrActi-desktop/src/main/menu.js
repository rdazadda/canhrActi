const { Menu, app } = require('electron');

// actions come from index.js, which owns the windows and the link URLs.
module.exports = function buildMenu(actions = {}) {
  const isDebug = process.env.CANHRACTI_DEBUG === '1' || !app.isPackaged;
  const isMac = process.platform === 'darwin';
  const run = (name, ...args) => () => actions[name] && actions[name](...args);
  // Not role 'quit', which would skip the quit dialog.
  const quit = { label: isMac ? 'Quit CANHRActi' : 'Exit CANHRActi', accelerator: 'CmdOrCtrl+Q', click: run('quit') };
  const about = { label: 'About CANHRActi', click: run('about') };

  // On macOS copy and paste only work through Edit menu items.
  const template = [
    ...(isMac
      ? [{
          role: 'appMenu',
          submenu: [
            about,
            { type: 'separator' },
            { role: 'services' },
            { type: 'separator' },
            { role: 'hide', label: 'Hide CANHRActi' },
            { role: 'hideOthers' },
            { role: 'unhide' },
            { type: 'separator' },
            quit,
          ],
        }]
      : []),
    {
      label: 'File',
      submenu: [isMac ? { role: 'close' } : quit],
    },
    ...(isMac ? [{ role: 'editMenu' }] : []),
    {
      label: 'View',
      submenu: [
        { label: 'Reload', accelerator: 'CmdOrCtrl+R', click: run('reload') },
        { type: 'separator' },
        { role: 'resetZoom' },
        { role: 'zoomIn' },
        { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' },
      ],
    },
    ...(isMac ? [{ role: 'windowMenu' }] : []),
    {
      label: 'Help',
      submenu: [
        { label: 'CANHRActi on GitHub', click: run('openLink', 'github') },
        { label: 'Report an Issue', click: run('openLink', 'issues') },
        { type: 'separator' },
        { label: 'Open Log Folder', click: run('openLogs') },
        ...(isDebug
          ? [
              { type: 'separator' },
              { label: 'Toggle Developer Tools', accelerator: 'F12', click: run('openDevtools') },
            ]
          : []),
        ...(isMac ? [] : [{ type: 'separator' }, about]),
      ],
    },
  ];

  return Menu.buildFromTemplate(template);
};
