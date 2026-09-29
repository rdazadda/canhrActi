const { contextBridge, ipcRenderer } = require('electron');

// The main process checks which window each call comes from, so every page gets the same bridge.
contextBridge.exposeInMainWorld('canhr', {
  theme: () => ipcRenderer.invoke('app:theme'),
  version: () => ipcRenderer.invoke('app:version'),
  info: () => ipcRenderer.invoke('app:info'),
  onSplash: (cb) => {
    ipcRenderer.removeAllListeners('splash:state');
    ipcRenderer.on('splash:state', (_event, state) => cb(state));
    ipcRenderer.send('splash:ready');
  },
  splashAction: (action) => ipcRenderer.invoke('splash:action', action),
  dialogResult: (ok) => ipcRenderer.send('dialog:result', ok === true),
  close: () => ipcRenderer.send('win:close'),
  setTheme: (theme) => ipcRenderer.send('app:set-theme', theme),
});
