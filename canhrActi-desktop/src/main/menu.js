const { Menu, app, shell, dialog } = require('electron');
const path = require('node:path');
const log = require('electron-log/main');

module.exports = function buildMenu({ openDevtools, reload } = {}) {
  const isDebug = process.env.CANHRACTI_DEBUG === '1' || !app.isPackaged;

  const template = [
    {
      label: 'File',
      submenu: [{ role: 'quit', label: 'Exit CANHRActi' }],
    },
    {
      label: 'View',
      submenu: [
        { label: 'Reload', accelerator: 'CmdOrCtrl+R', click: () => reload && reload() },
        { type: 'separator' },
        { role: 'resetZoom' },
        { role: 'zoomIn' },
        { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' },
      ],
    },
    {
      label: 'Help',
      submenu: [
        {
          label: 'CANHRActi on GitHub',
          click: () => shell.openExternal('https://github.com/rdazadda/canhrActi'),
        },
        {
          label: 'Report an Issue',
          click: () => shell.openExternal('https://github.com/rdazadda/canhrActi/issues'),
        },
        { type: 'separator' },
        {
          label: 'Open Log Folder',
          click: () => shell.openPath(path.dirname(log.transports.file.getFile().path)),
        },
        ...(isDebug
          ? [
              { type: 'separator' },
              {
                label: 'Toggle Developer Tools',
                accelerator: 'F12',
                click: () => openDevtools && openDevtools(),
              },
            ]
          : []),
        { type: 'separator' },
        {
          label: 'About CANHRActi',
          click: () =>
            dialog.showMessageBox({
              type: 'info',
              title: 'About CANHRActi',
              message: `CANHRActi ${app.getVersion()}`,
              detail:
                'CANHR Accelerometer Physical Activity and Sleep Analysis\n' +
                'Center for Alaska Native Health Research\n\n' +
                `Electron ${process.versions.electron} · ` +
                `Node ${process.versions.node} · ` +
                `Chromium ${process.versions.chrome}`,
            }),
        },
      ],
    },
  ];

  return Menu.buildFromTemplate(template);
};
