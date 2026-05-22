const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('canhr', {
  appVersion: () => ipcRenderer.invoke('app:version'),
  logsPath: () => ipcRenderer.invoke('app:logs-path'),
  openLogs: () => ipcRenderer.invoke('app:open-logs'),
});
