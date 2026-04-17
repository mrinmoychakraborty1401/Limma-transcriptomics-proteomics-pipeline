const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('progressAPI', {
    onUpdate: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('progress-update', listener);
        return () => ipcRenderer.removeListener('progress-update', listener);
    },
    onLog: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('log-message', listener);
        return () => ipcRenderer.removeListener('log-message', listener);
    }
});

