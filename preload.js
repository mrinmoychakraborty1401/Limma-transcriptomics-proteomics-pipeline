const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
    // Renderer to main (one-way)
    runFullAnalysis: (data) => ipcRenderer.send('run-full-analysis', data),
    runMetaAnalysis: (data) => ipcRenderer.send('run-meta-analysis', data),
    saveSession: (data) => ipcRenderer.send('save-session', data),
    runVennAnalysis: (data) => ipcRenderer.send('run-venn-analysis', data),
    runPostAnalysisScript: (data) => ipcRenderer.send('run-post-analysis-script', data),
    updateAnalysisConfig: (data) => ipcRenderer.send('update-analysis-config', data),
    haltAnalysis: () => ipcRenderer.send('halt-analysis'),
    runGeoPipeline: (data) => ipcRenderer.send('run-geo-pipeline', data),
    runGeoSplitAnalysis: (data) => ipcRenderer.send('run-geo-split-analysis', data),
    runGeoIndependent: (data) => ipcRenderer.send('run-geo-independent', data),
    runGeoManualFetch: (data) => ipcRenderer.send('run-geo-manual-fetch', data), // NEW: Fetch for Manual Configuration
    openPath: (path) => ipcRenderer.send('open-path', path),
    
    // Evo-Devo & KEGG Mapper Bindings
    runCrossSpecies: (data) => ipcRenderer.send('run-cross-species', data),
    runKeggMapper: (data) => ipcRenderer.send('run-kegg-mapper', data),

    // Renderer to main (two-way - AWAITABLE)
    getFileHeaders: (filePath) => ipcRenderer.invoke('get-file-headers', filePath),
    openFile: () => ipcRenderer.invoke('dialog:openFile'),
    openDirectory: () => ipcRenderer.invoke('dialog:openDirectory'),
    getDefaultPath: () => ipcRenderer.invoke('get-default-path'),
    loadSession: (filePath) => ipcRenderer.invoke('load-session', filePath),
    mergeFiles: (data) => ipcRenderer.invoke('merge-files', data),
    readJsonFile: (filePath) => ipcRenderer.invoke('read-json-file', filePath),
    getPostAnalysisScripts: () => ipcRenderer.invoke('get-post-analysis-scripts'),
    checkAnalysisExists: (filePath) => ipcRenderer.invoke('check-analysis-exists', filePath),

    // Listeners
    onAnalysisError: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('analysis-error', listener);
        return () => ipcRenderer.removeListener('analysis-error', listener);
    },
    onAnalysisComplete: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('analysis-complete', listener);
        return () => ipcRenderer.removeListener('analysis-complete', listener);
    },
    onCrossSpeciesComplete: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('cross-species-complete', listener);
        return () => ipcRenderer.removeListener('cross-species-complete', listener);
    },
    onAnalysisProgress: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('analysis-progress', listener);
        return () => ipcRenderer.removeListener('analysis-progress', listener);
    },
    onGeoComplete: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('geo-complete', listener);
        return () => ipcRenderer.removeListener('geo-complete', listener);
    },
    onGeoSplitComplete: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('geo-split-complete', listener);
        return () => ipcRenderer.removeListener('geo-split-complete', listener);
    },
    onGeoIndependentComplete: (callback) => {
        const listener = (_event, value) => callback(value);
        ipcRenderer.on('geo-independent-complete', listener);
        return () => ipcRenderer.removeListener('geo-independent-complete', listener);
    }
});