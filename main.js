// ==============================================================================
// PROTOCOL: STRICT PRESERVATION MODE
// 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
// 2. IF EDITING: Only modify the specific logic requested.
// 3. PRESERVE: All handlers, progress windows, and robustness checks.
// 4. ALWAYS send the full code without truncation.
// ==============================================================================

const { app, BrowserWindow, ipcMain, dialog, shell } = require('electron');
const path = require('path');
const { spawn, execSync } = require('child_process');
const fs = require('fs');
const crypto = require('crypto');

if (require('electron-squirrel-startup')) {
  app.quit();
}

let mainWindow;
let progressWindow;
let currentRProcess = null; // Track the running R process

const getResourcesPath = () => {
  return app.isPackaged
    ? path.join(process.resourcesPath, 'r_scripts')
    : path.join(__dirname, 'r_scripts');
};

const createWindow = () => {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 900,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
    },
  });

  if (app.isPackaged) {
    mainWindow.loadFile(path.join(__dirname, 'dist', 'index.html'));
  } else {
    mainWindow.loadURL('http://localhost:5173');
    mainWindow.webContents.openDevTools();
  }

  mainWindow.on('closed', () => {
    mainWindow = null;
    if (progressWindow) progressWindow.close();
  });
};

const createProgressWindow = () => {
    if (progressWindow) return; 
    progressWindow = new BrowserWindow({
        width: 600,
        height: 400,
        parent: mainWindow,
        modal: false,
        show: false,
        webPreferences: {
            preload: path.join(__dirname, 'progressPreload.js'),
            contextIsolation: true,
            nodeIntegration: false,
        },
    });

    progressWindow.loadFile(path.join(__dirname, 'progress.html'));
    progressWindow.once('ready-to-show', () => {
        progressWindow.show();
    });

    progressWindow.on('closed', () => {
        progressWindow = null;
    });
};

app.on('ready', createWindow);
app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit(); });
app.on('activate', () => { if (BrowserWindow.getAllWindows().length === 0) createWindow(); });

const resultsDir = path.join(app.getPath('userData'), 'analysis_temp_cache');
if (!fs.existsSync(resultsDir)) fs.mkdirSync(resultsDir, { recursive: true });

const getRscriptPath = () => {
    if (process.platform !== 'win32') return 'Rscript';
    try {
        const whereOutput = execSync('where Rscript').toString().trim();
        const rscriptPath = whereOutput.split('\r\n')[0];
        if (fs.existsSync(rscriptPath)) return rscriptPath;
    } catch (error) { /* continue */ }
    
    try {
        const searchPaths = [ process.env['ProgramFiles'], process.env['ProgramFiles(x86)'], 'C:\\', 'E:\\Rrelated' ];
        for (const rBasePath of searchPaths) {
            const rRoot = path.join(rBasePath, 'R');
            if (fs.existsSync(rRoot)) {
                const rVersions = fs.readdirSync(rRoot).filter(dir => dir.startsWith('R-')).sort().reverse();
                if (rVersions.length > 0) {
                    const rscriptPath = path.join(rRoot, rVersions[0], 'bin', 'Rscript.exe');
                    if (fs.existsSync(rscriptPath)) return rscriptPath;
                }
            }
        }
    } catch (error) { /* continue */ }
    return null;
};

const rscriptExecutable = getRscriptPath();
const normalizePathForR = (p) => p.replace(/\\/g, '/');

const runRScript = (event, scriptName, args, errorChannel = 'analysis-error') => {
    return new Promise((resolve, reject) => {
        if (!rscriptExecutable) {
            const msg = `FATAL ERROR: Rscript executable not found.`;
            if (progressWindow) progressWindow.webContents.send('log-message', msg);
            event.sender.send(errorChannel, msg);
            return reject(new Error(msg));
        }

        const resourcesPath = getResourcesPath();
        const scriptPath = path.join(resourcesPath, scriptName);

        if (!fs.existsSync(scriptPath)) {
            const msg = `FATAL ERROR: R script not found at path: ${scriptPath}.`;
            if (progressWindow) progressWindow.webContents.send('log-message', msg);
            event.sender.send(errorChannel, msg);
            return reject(new Error(msg));
        }
        
        let stdout = '', stderr = '';
        
        currentRProcess = spawn(rscriptExecutable, [scriptPath, ...args], { cwd: resourcesPath });
        
        currentRProcess.stderr.on('data', (data) => {
            const msg = data.toString();
            console.error(`[R-STDERR]: ${msg}`);
            if (progressWindow && !progressWindow.isDestroyed()) {
                progressWindow.webContents.send('log-message', `[STDERR]: ${msg}`);
                if (msg.startsWith("PROGRESS:")) progressWindow.webContents.send('progress-update', msg.replace("PROGRESS:", "").trim());
            }
            stderr += msg;
        });
        
        currentRProcess.stdout.on('data', (data) => { 
            const msg = data.toString();
            console.log(`[R-STDOUT]: ${msg}`);
            if (progressWindow && !progressWindow.isDestroyed()) progressWindow.webContents.send('log-message', `[STDOUT]: ${msg}`);
            stdout += msg; 
        });
        
        currentRProcess.on('close', (code, signal) => {
            currentRProcess = null; // Reset on close
            
            if (signal === 'SIGTERM' || signal === 'SIGKILL') {
                const msg = "Process was halted by user.";
                console.log(msg);
                return reject(new Error(msg));
            }

            if (code !== 0) {
                const msg = `Error in ${scriptName}:\n\n${stderr}`;
                event.sender.send(errorChannel, msg);
                return reject(new Error(msg));
            } else {
                return resolve(stdout);
            }
        });
    });
};

const sanitizeHeaderForR = (header) => {
    if (!header) return header;
    let sanitized = header.replace(/[^a-zA-Z0-9_.]/g, '.');
    if (/^[0-9.]/.test(sanitized)) sanitized = 'X' + sanitized;
    return sanitized;
};

// --- IPC HANDLERS ---

ipcMain.on('halt-analysis', (event) => {
    if (currentRProcess) {
        console.log("Killing R process...");
        currentRProcess.kill('SIGKILL');
        currentRProcess = null;
        if (progressWindow && !progressWindow.isDestroyed()) {
            progressWindow.webContents.send('log-message', '[SYSTEM]: Analysis halted by user.');
            progressWindow.webContents.send('progress-update', 'Analysis Halted.');
            setTimeout(() => progressWindow.close(), 1000);
        }
    }
});

ipcMain.handle('load-session', async (event, filePath) => {
    try {
        const sessionPath = filePath + '.session.json';
        if (fs.existsSync(sessionPath)) {
            console.log("[Main] Loading session from .session.json (Highest Priority)");
            return JSON.parse(fs.readFileSync(sessionPath, 'utf8'));
        }

        const fileDir = path.dirname(filePath);
        const projectName = path.parse(filePath).name;
        
        const potentialDirs = [
            fileDir,
            path.join(fileDir, `Analysis_Results_for_${projectName}`),
            path.join(fileDir, 'results'),
            path.join(fileDir, 'output'),
            path.join(fileDir, '..') 
        ];

        let sampleConfigPath = null;
        let analysisConfigPath = null;

        for (const dir of potentialDirs) {
            const scPath = path.join(dir, 'sample_config.json');
            if (fs.existsSync(scPath)) {
                sampleConfigPath = scPath;
                analysisConfigPath = path.join(dir, 'analysis_config.json');
                break;
            }
        }

        if (sampleConfigPath) {
            console.log("[Main] Loading session from sample_config.json (Fallback)");
            const sampleConfigData = JSON.parse(fs.readFileSync(sampleConfigPath, 'utf8'));
            const analysisConfigData = fs.existsSync(analysisConfigPath) 
                ? JSON.parse(fs.readFileSync(analysisConfigPath, 'utf8')) 
                : { contrasts: [], vennContrasts: [], signatureConfig: [] };

            const columnConfig = {};
            const addConfig = (key, entry) => {
                if(!key) return;
                columnConfig[key] = entry;
                const sanitized = sanitizeHeaderForR(key);
                if(sanitized !== key) columnConfig[sanitized] = entry;
            };

            if (sampleConfigData.idColumn) {
                const idEntry = { role: 'ID', finalName: sampleConfigData.idColumn, group: null, batch: null, replicateID: null, runOrder: null };
                addConfig(sampleConfigData.idColumn, idEntry);
            }

            if (Array.isArray(sampleConfigData.sampleConfig)) {
                sampleConfigData.sampleConfig.forEach(s => {
                    const configEntry = { role: 'Sample', finalName: s.finalName, group: s.group, batch: s.batch, replicateID: s.replicateID, runOrder: s.runOrder };
                    addConfig(s.originalName, configEntry);
                    if (s.finalName && s.finalName !== s.originalName) addConfig(s.finalName, configEntry);
                });
            }

            if (Array.isArray(sampleConfigData.metadataColumns)) {
                sampleConfigData.metadataColumns.forEach(m => {
                    const metaEntry = { role: 'Metadata', finalName: m, group: null, batch: null, replicateID: null, runOrder: null };
                    addConfig(m, metaEntry);
                });
            }

            const analysisSets = sampleConfigData.analysisSets || [];
            
            const analysisOptions = sampleConfigData.analysisOptions || { 
                runPpi: true, 
                runVenn: true, 
                runSignature: true, 
                runLoess: false, 
                runCombat: false, 
                runRuv: false, 
                splitBatches: false 
            };

            return {
                idColumn: sampleConfigData.idColumn,
                columnConfig,
                contrasts: analysisConfigData.contrasts || [],
                vennContrasts: analysisConfigData.vennContrasts || [],
                signatureConfig: analysisConfigData.signatureConfig || [],
                annotationKeywords: sampleConfigData.annotationKeywords || [],
                analysisSets: analysisSets, 
                analysisOptions: analysisOptions
            };
        }

        return null;
    } catch (error) {
        console.error('Failed to load session:', error);
        return null;
    }
});

ipcMain.on('run-full-analysis', async (event, { filePath, idColumn, sampleConfig, metadataColumns, contrasts, vennContrasts, signatureConfig, analysisSets, annotationKeywords, analysisOptions, pCutoff, fcCutoff, ppiCluster, ppiHub, ppiScore }) => {
    createProgressWindow();
    const projectName = path.parse(filePath).name;
    const baseOutputDir = path.join(path.dirname(filePath), `Analysis_Results_for_${projectName}`);
    
    if (!fs.existsSync(baseOutputDir)) fs.mkdirSync(baseOutputDir, { recursive: true });

    const configData = { 
        idColumn, 
        sampleConfig, 
        metadataColumns, 
        annotationKeywords: annotationKeywords || [], 
        analysisSets, 
        analysisOptions
    };
    
    const configPath = path.join(baseOutputDir, 'sample_config.json');
    let configChanged = true;
    
    if (fs.existsSync(configPath)) {
        try {
            const oldConfig = fs.readFileSync(configPath, 'utf8');
            if (oldConfig === JSON.stringify(configData, null, 2)) {
                configChanged = false;
            }
        } catch(e) { }
    }
    
    fs.writeFileSync(configPath, JSON.stringify(configData, null, 2));

    const analysisConfig = { contrasts, vennContrasts, signatureConfig };
    const analysisConfigPath = path.join(baseOutputDir, 'analysis_config.json');
    fs.writeFileSync(analysisConfigPath, JSON.stringify(analysisConfig, null, 2));

    try {
        const annotatedFile = path.join(baseOutputDir, 'annotated_quant.csv');
        const annotationArgs = ['--input', normalizePathForR(filePath), '--config', normalizePathForR(configPath), '--output', normalizePathForR(baseOutputDir)];

        if (!configChanged && fs.existsSync(annotatedFile)) {
            if (progressWindow) progressWindow.webContents.send('progress-update', 'Configuration unchanged. Skipping annotation step (using existing cache)...');
            console.log("[Main] Skipping 00_annotate_data.R (Checkpoint Hit)");
        } else {
            if (progressWindow) progressWindow.webContents.send('progress-update', 'Annotating raw data...');
            console.log("[Main] Running 00_annotate_data.R");
            await runRScript(event, '00_annotate_data.R', annotationArgs);
        }
        
        if (!fs.existsSync(annotatedFile)) throw new Error('Annotation output file missing. Checkpoint failed or script error.');

        let executionGroups = [];

        if (analysisSets && analysisSets.length > 0) {
            console.log(`[Main] Analysis Sets Detected: ${analysisSets.length}. Splitting execution.`);
            
            analysisSets.forEach(set => {
                const setGroups = new Set(Array.isArray(set.groups) ? set.groups : [set.groups]);
                console.log(`[Main] Processing Set: ${set.name}, Groups: ${Array.from(setGroups).join(', ')}`);

                const setSpecificConfig = sampleConfig.filter(s => setGroups.has(s.group));
                
                if (setSpecificConfig.length < 2) {
                    console.warn(`[Main] Skipping set "${set.name}": Too few samples.`);
                    return;
                }

                executionGroups.push({
                    name: set.name,
                    subFolder: path.join(baseOutputDir, `Set_${set.name.replace(/[^a-zA-Z0-9]/g, '_')}`),
                    config: setSpecificConfig
                });
            });
        } else {
            console.log(`[Main] No Analysis Sets. Running global analysis.`);
            executionGroups.push({
                name: "Global",
                subFolder: baseOutputDir,
                config: sampleConfig
            });
        }

        for (const group of executionGroups) {
            const { name, subFolder, config } = group;
            
            if (progressWindow) progressWindow.webContents.send('progress-update', `Preparing analysis for: ${name}...`);
            if (!fs.existsSync(subFolder)) fs.mkdirSync(subFolder, { recursive: true });

            const q = (str) => {
                if (str === null || str === undefined) return '""';
                return `"${String(str).replace(/"/g, '""')}"`;
            };

            const metadataCsv = "Sample,OriginalSample,Group,Batch,ReplicateID,RunOrder\n" + 
                config.map(s => `${q(s.finalName)},${q(s.originalName)},${q(s.group)},${q(s.batch || 'Batch1')},${q(s.replicateID || s.finalName)},${q(s.runOrder || 0)}`).join("\n");
            
            const setMetadataPath = path.join(subFolder, 'sample_metadata.csv');
            fs.writeFileSync(setMetadataPath, metadataCsv);

            const groupsInSet = new Set(config.map(s => s.group));
            
            const validContrasts = contrasts.filter(c => {
                if (typeof c === 'string') {
                    const normC = c.trim();
                    let matchCount = 0;
                    for (const g of groupsInSet) {
                        if (normC.includes(g)) matchCount++;
                    }
                    if (matchCount >= 2) {
                        return true;
                    }
                    return false;
                } else {
                    if (c.id && c.id.startsWith('legacy_')) {
                        const origStr = c.id.replace('legacy_', '');
                        let matchCount = 0;
                        const matchedGroups = [];
                        for (const g of groupsInSet) {
                            if (origStr.includes(g)) {
                                matchCount++;
                                matchedGroups.push(g);
                            }
                        }
                        if (matchCount >= 2) {
                            matchedGroups.sort((a, b) => b.length - a.length);
                            c.cond1.value = matchedGroups[0];
                            c.cond2.value = matchedGroups[1];
                            c.name = `${matchedGroups[0]} vs ${matchedGroups[1]}`;
                            return true;
                        }
                    }

                    const setSampleNames = new Set(config.map(s => s.finalName));
                    
                    let cond1Valid = false;
                    if (c.cond1.type === 'group' && groupsInSet.has(c.cond1.value)) cond1Valid = true;
                    else if (c.cond1.type === 'samples' && c.cond1.value.some(s => setSampleNames.has(s))) cond1Valid = true;

                    let cond2Valid = false;
                    if (c.cond2.type === 'group' && groupsInSet.has(c.cond2.value)) cond2Valid = true;
                    else if (c.cond2.type === 'samples' && c.cond2.value.some(s => setSampleNames.has(s))) cond2Valid = true;

                    if (cond1Valid && cond2Valid) {
                        return true;
                    }
                    return false;
                }
            });
            
            if (validContrasts.length === 0 && name !== "Global") {
                console.warn(`[Main] Skipping set "${name}": No valid contrasts apply to these groups.`);
                if(progressWindow) progressWindow.webContents.send('log-message', `[WARNING] Skipping set "${name}": No matching contrasts found for groups: ${Array.from(groupsInSet).join(", ")}`);
                continue;
            }

            const contrastsJsonPath = path.join(subFolder, '.tmp_contrasts.json');
            fs.writeFileSync(contrastsJsonPath, JSON.stringify(validContrasts, null, 2));

            const setAnalysisConfig = { ...analysisConfig, contrasts: validContrasts };
            const setAnalysisConfigPath = path.join(subFolder, 'analysis_config.json');
            fs.writeFileSync(setAnalysisConfigPath, JSON.stringify(setAnalysisConfig, null, 2));

            const analysisOptionsArgs = [
                '--run_ppi', analysisOptions.runPpi ? 'TRUE' : 'FALSE',
                '--run_venn', analysisOptions.runVenn ? 'TRUE' : 'FALSE',
                '--run_signature', analysisOptions.runSignature ? 'TRUE' : 'FALSE',
                '--run_median', analysisOptions.runMedian ? 'TRUE' : 'FALSE',
                '--run_loess', analysisOptions.runLoess ? 'TRUE' : 'FALSE',
                '--run_combat', analysisOptions.runCombat ? 'TRUE' : 'FALSE',
                '--run_ruv', analysisOptions.runRuv ? 'TRUE' : 'FALSE',
                '--split_batches', analysisOptions.splitBatches ? 'TRUE' : 'FALSE',
                '--outlier_method', analysisOptions.outlierMethod || 'none',
                '--p_cutoff', String(pCutoff || 0.05),
                '--fc_cutoff', String(fcCutoff || 1.0),
                '--ppi_cluster', String(ppiCluster || 'louvain'),
                '--ppi_hub', String(ppiHub || 'pagerank'),
                '--ppi_score', String(ppiScore || 900)
            ];

            analysisOptionsArgs.push('--contrasts_json', normalizePathForR(contrastsJsonPath));
            if (analysisOptions.runPairwiseQC) analysisOptionsArgs.push('--run_pairwise_qc', 'TRUE');
            if (analysisOptions.runDegHeatmaps) analysisOptionsArgs.push('--run_deg_heatmaps', 'TRUE');
            
            const analysisArgs = [
                '--input', normalizePathForR(annotatedFile),
                '--metadata', normalizePathForR(setMetadataPath),
                '--output', normalizePathForR(subFolder),
                '--project_name', `${projectName}_${name}`, 
                '--analysis_config', normalizePathForR(setAnalysisConfigPath), 
                '--scripts_dir', normalizePathForR(getResourcesPath()), 
                ...analysisOptionsArgs
            ];
            
            if (progressWindow) progressWindow.webContents.send('progress-update', `Running Statistics for Set: ${name}...`);
            await runRScript(event, 'run_analysis.R', analysisArgs);
        }
        
        if (progressWindow) progressWindow.webContents.send('progress-update', 'All Analyses Complete!');
        setTimeout(() => { if (progressWindow && !progressWindow.isDestroyed()) progressWindow.close(); }, 2000);

        const vennSetsPath = path.join(baseOutputDir, `${projectName}_Global_Summary_Analysis`, "venn_sets.json"); 
        event.sender.send('analysis-complete', { resultsPath: baseOutputDir, vennSetsPath: fs.existsSync(vennSetsPath) ? vennSetsPath : null });

    } catch (error) {
        if (error.message.includes("halted")) return; 
        console.error("Analysis pipeline failed:", error.message);
        if (progressWindow && !progressWindow.isDestroyed()) progressWindow.webContents.send('log-message', error.message);
    }
});

ipcMain.on('run-meta-analysis', (event, { filePath, metaContrasts, vennContrasts, signatureConfig, analysisOptions, countCol, filterOp, filterVal, ppiCluster, ppiHub, ppiScore }) => {
    createProgressWindow();
    const projectName = path.parse(filePath).name;
    const outputDir = path.join(path.dirname(filePath), `Analysis_Results_for_${projectName}`);
    
    if (!fs.existsSync(outputDir)) fs.mkdirSync(outputDir, { recursive: true });

    const analysisConfig = { metaContrasts, vennContrasts, signatureConfig };
    const analysisConfigPath = path.join(outputDir, 'analysis_config.json');
    fs.writeFileSync(analysisConfigPath, JSON.stringify(analysisConfig, null, 2));

    const analysisArgs = [
        '--input', normalizePathForR(filePath), 
        '--output', normalizePathForR(outputDir), 
        '--project_name', projectName,
        '--analysis_config', normalizePathForR(analysisConfigPath),
        '--run_ppi', analysisOptions.runPpi ? 'TRUE' : 'FALSE',
        '--run_venn', analysisOptions.runVenn ? 'TRUE' : 'FALSE',
        '--run_signature', analysisOptions.runSignature ? 'TRUE' : 'FALSE',
        '--scripts_dir', normalizePathForR(getResourcesPath()),
        '--ppi_cluster', String(ppiCluster || 'louvain'),
        '--ppi_hub', String(ppiHub || 'pagerank'),
        '--ppi_score', String(ppiScore || 900)
    ];

    if (countCol) {
        analysisArgs.push('--count_col', countCol);
        if (filterOp) analysisArgs.push('--filter_op', filterOp);
        if (filterVal) analysisArgs.push('--filter_value', filterVal);
    }
    
    (async () => {
        try {
            await runRScript(event, '03_run_meta_analysis.R', analysisArgs);
            if (progressWindow) progressWindow.webContents.send('progress-update', 'Analysis Complete!');
            setTimeout(() => { if (progressWindow) progressWindow.close(); }, 2000);
            event.sender.send('analysis-complete', { resultsPath: outputDir, vennSetsPath: null });
        } catch (error) {
            console.error("Meta-analysis pipeline failed:", error.message);
        }
    })();
});

// --- FULLY RESTORED GEO & CROSS-SPECIES HANDLERS ---

ipcMain.on('run-geo-split-analysis', async (event, data) => {
    createProgressWindow();
    
    let gseIds, targetDir, controlGroup, caseGroup;
    if (typeof data === 'string') {
        gseIds = data;
        targetDir = path.join(app.getPath('userData'), 'geo_downloads');
        controlGroup = 'Normal'; 
        caseGroup = 'Tumor';
    } else {
        gseIds = data.gseIds;
        targetDir = (data.targetDir && data.targetDir.trim() !== '') ? data.targetDir : path.join(app.getPath('userData'), 'geo_downloads');
        controlGroup = data.control || 'Normal';
        caseGroup = data.case || 'Tumor';
    }

    if (!fs.existsSync(targetDir)) fs.mkdirSync(targetDir, { recursive: true });
    
    const args = [
        '--gse_ids', gseIds, 
        '--output_dir', normalizePathForR(targetDir),
        '--control', controlGroup,
        '--case', caseGroup,
        '--scripts_dir', normalizePathForR(getResourcesPath())
    ];

    try {
        if (progressWindow) progressWindow.webContents.send('progress-update', 'Starting GEO Split Analysis (Individual Datasets)...');
        
        await runRScript(event, '05_geo_split_analysis.R', args);
        
        const finalFile = path.join(targetDir, 'Meta_Analysis_Summary', 'Meta_Analysis_Results.csv');
        if (!fs.existsSync(finalFile)) throw new Error("R script completed but meta-analysis output not found.");
        
        if (progressWindow) {
             progressWindow.webContents.send('progress-update', 'Meta-Analysis Complete!');
             setTimeout(() => { if (progressWindow && !progressWindow.isDestroyed()) progressWindow.close(); }, 1000);
        }

        event.sender.send('geo-split-complete', { resultsPath: targetDir });

    } catch (error) {
        if (error.message.includes("halted")) return;
        console.error("GEO Split Analysis failed:", error.message);
        if (progressWindow && !progressWindow.isDestroyed()) progressWindow.webContents.send('log-message', `[ERROR]: ${error.message}`);
    }
});

ipcMain.on('run-geo-pipeline', async (event, data) => {
    createProgressWindow();
    let gseIds = typeof data === 'string' ? data : data.gseIds;
    let targetDir = (typeof data !== 'string' && data.targetDir && data.targetDir.trim() !== '') ? data.targetDir : path.join(app.getPath('userData'), 'geo_downloads');
    if (!fs.existsSync(targetDir)) fs.mkdirSync(targetDir, { recursive: true });
    
    const outputFile = path.join(targetDir, 'merged_geo_data.csv');
    const args = ['--gse_ids', gseIds, '--output_dir', normalizePathForR(targetDir)];

    try {
        if (progressWindow) progressWindow.webContents.send('progress-update', 'Starting Global GEO pipeline (Merging)...');
        await runRScript(event, '04_geo_merge.R', args);
        if (!fs.existsSync(outputFile)) throw new Error("R script completed but output file not found.");
        
        const headerOutput = await runRScript(event, 'read_headers.R', ['--input', normalizePathForR(outputFile)]);
        const headers = JSON.parse(headerOutput);
        
        if (progressWindow) { progressWindow.webContents.send('progress-update', 'Merge complete!'); setTimeout(() => { if (progressWindow) progressWindow.close(); }, 1000); }
        event.sender.send('geo-complete', { filePath: outputFile, headers: headers });
    } catch (error) {
        if (error.message.includes("halted")) return;
        console.error("GEO pipeline failed:", error.message);
        if (progressWindow) progressWindow.webContents.send('log-message', `[ERROR]: ${error.message}`);
    }
});

ipcMain.on('run-geo-independent', async (event, data) => {
    createProgressWindow();
    
    let gseIds = typeof data === 'string' ? data : data.gseIds;
    let targetDir = (typeof data !== 'string' && data.targetDir && data.targetDir.trim() !== '') ? data.targetDir : path.join(app.getPath('userData'), 'geo_downloads');
    
    if (!fs.existsSync(targetDir)) fs.mkdirSync(targetDir, { recursive: true });
    
    const args = [
        '--gse_ids', gseIds, 
        '--output_dir', normalizePathForR(targetDir),
        '--scripts_dir', normalizePathForR(getResourcesPath())
    ];

    try {
        if (progressWindow) progressWindow.webContents.send('progress-update', 'Starting Independent GEO Analysis (Smart Detect)...');
        
        await runRScript(event, '06_geo_independent_processor.R', args);
        
        if (progressWindow) {
             progressWindow.webContents.send('progress-update', 'Independent Analysis Complete!');
             setTimeout(() => { if (progressWindow && !progressWindow.isDestroyed()) progressWindow.close(); }, 1000);
        }

        event.sender.send('geo-independent-complete', { resultsPath: targetDir });

    } catch (error) {
        if (error.message.includes("halted")) return;
        console.error("GEO Independent Analysis failed:", error.message);
        if (progressWindow && !progressWindow.isDestroyed()) progressWindow.webContents.send('log-message', `[ERROR]: ${error.message}`);
    }
});

// NEW: Fetch GEO for Manual Configuration (Smart Detect)
ipcMain.on('run-geo-manual-fetch', async (event, data) => {
    createProgressWindow();
    let gseIds = typeof data === 'string' ? data : data.gseIds;
    let targetDir = (typeof data !== 'string' && data.targetDir && data.targetDir.trim() !== '') ? data.targetDir : path.join(app.getPath('userData'), 'geo_downloads');

    if (!fs.existsSync(targetDir)) fs.mkdirSync(targetDir, { recursive: true });

    // Output file will be named based on the first GSE ID in the list
    const firstGse = gseIds.split(',')[0].trim();
    const outputFile = path.join(targetDir, `${firstGse}_manual_matrix.csv`);
    
    const args = [
        '--gse_ids', gseIds,
        '--output_dir', normalizePathForR(targetDir),
        '--output_file', normalizePathForR(outputFile),
        '--scripts_dir', normalizePathForR(getResourcesPath())
    ];

    try {
        if (progressWindow) progressWindow.webContents.send('progress-update', 'Fetching dataset safely for manual configuration...');

        // Run the new extraction script
        await runRScript(event, '07_geo_manual_fetch.R', args);

        if (!fs.existsSync(outputFile)) throw new Error("R script completed but output file not found. Ensure the dataset has a valid matrix.");

        // Read the headers to populate the Setup grid
        const headerOutput = await runRScript(event, 'read_headers.R', ['--input', normalizePathForR(outputFile)]);
        const headers = JSON.parse(headerOutput);

        if (progressWindow) {
             progressWindow.webContents.send('progress-update', 'Fetch complete! Loading into configuration window...');
             setTimeout(() => { if (progressWindow && !progressWindow.isDestroyed()) progressWindow.close(); }, 1000);
        }

        // Trick the frontend into treating it exactly like a regular File Load or Merge!
        event.sender.send('geo-complete', { filePath: outputFile, headers: headers });

    } catch (error) {
        if (error.message.includes("halted")) return;
        console.error("GEO Manual Fetch failed:", error.message);
        if (progressWindow && !progressWindow.isDestroyed()) progressWindow.webContents.send('log-message', `[ERROR]: ${error.message}`);
    }
});

ipcMain.on('run-cross-species', async (event, config) => {
    if (currentRProcess) {
        dialog.showErrorBox('Busy', 'An analysis is already running.');
        return;
    }
    try {
        const args = [
            '--axolotl_file', normalizePathForR(config.axolotl),
            '--human_file', normalizePathForR(config.human),
            '--ortho_file', normalizePathForR(config.ortho),
            '--output_dir', normalizePathForR(config.targetDir)
        ];
        await runRScript(event, 'run_cross_species.R', args);
        event.sender.send('cross-species-complete', { resultsPath: config.targetDir });
    } catch (error) {
        if (!error.message.includes("halted")) event.sender.send('analysis-error', error.message);
    }
});

ipcMain.on('run-kegg-mapper', async (event, data) => {
    if (currentRProcess) {
        dialog.showErrorBox('Busy', 'An analysis is already running.');
        return;
    }
    const { dgeFile, species, workingDir } = data;
    
    if (!fs.existsSync(workingDir)) fs.mkdirSync(workingDir, { recursive: true });

    const scriptPath = '05_run_kegg_mapper.R'; 
    const args = [
        '--dge_file', normalizePathForR(dgeFile),
        '--species', species,
        '--output_dir', normalizePathForR(workingDir)
    ];
    
    try {
        if (progressWindow) progressWindow.webContents.send('progress-update', 'Generating KEGG Mapper input...');
        await runRScript(event, scriptPath, args);
        dialog.showMessageBox({ type: 'info', title: 'Complete', message: 'KEGG Mapper input generated successfully. Check your output directory.' });
    } catch (error) {
        if (!error.message.includes("halted")) {
            console.error(error);
            event.sender.send('analysis-error', 'Failed to generate KEGG Mapper file. Check R logs.');
        }
    }
});

ipcMain.handle('get-file-headers', async (event, filePath) => {
    try {
        const stdout = await runRScript(event, 'read_headers.R', ['--input', normalizePathForR(filePath)]);
        return JSON.parse(stdout); 
    } catch (e) {
        console.error("Header parsing failed:", e);
        throw e; 
    }
});

ipcMain.handle('dialog:openFile', async () => {
    const { canceled, filePaths } = await dialog.showOpenDialog({ 
        properties: ['openFile'], 
        filters: [{ name: 'Data Files', extensions: ['csv', 'xlsx', 'xls', 'tsv', 'txt'] }] 
    });
    return !canceled ? filePaths[0] : null;
});

ipcMain.handle('dialog:openDirectory', async () => {
    const { canceled, filePaths } = await dialog.showOpenDialog({ 
        properties: ['openDirectory'] 
    });
    return !canceled ? filePaths[0] : null;
});

ipcMain.handle('get-default-path', async () => {
    return path.join(app.getPath('userData'), 'geo_downloads');
});

ipcMain.on('open-path', (event, dirPath) => { if (fs.existsSync(dirPath)) shell.openPath(dirPath); });
ipcMain.handle('check-analysis-exists', async (event, filePath) => { return false; }); 
ipcMain.on('save-session', (event, { filePath, sessionData }) => {
    const sessionPath = filePath + '.session.json';
    fs.writeFileSync(sessionPath, JSON.stringify(sessionData, null, 2));
});
ipcMain.handle('get-post-analysis-scripts', async () => {
    try { return fs.readdirSync(getResourcesPath()).filter(f => f.startsWith('post_') && f.endsWith('.R')); } catch (e) { return []; }
});
ipcMain.on('run-post-analysis-script', (event, { scriptName, basePath }) => {
    const args = ['--results_dir', normalizePathForR(basePath)];
    runRScript(event, scriptName, args); 
});
ipcMain.handle('read-json-file', async (event, filePath) => {
    try { if (fs.existsSync(filePath)) return JSON.parse(fs.readFileSync(filePath, 'utf8')); return null; } catch (e) { return null; }
});
ipcMain.on('update-analysis-config', (event, { resultsDir, annotationKeywords }) => { /* ... */ });
ipcMain.on('run-venn-analysis', (event, { selections, runPpi, basePath }) => { 
    if(!progressWindow) createProgressWindow();
    const summaryDir = path.join(basePath, path.basename(basePath).replace('Analysis_Results_for_', '') + '_Summary_Analysis');
    const vennSubdir = path.join(summaryDir, "Venn_Subset_Analysis");

    selections.forEach(selection => {
        const setData = selection; 
        const excelFile = setData.excel_file;
        const dgeFile = setData.dge_file1;
        const setNameHash = crypto.createHash('md5').update(setData.name).digest('hex').substring(0, 12);
        const finalOutputDir = path.join(vennSubdir, setNameHash);
        if (!fs.existsSync(finalOutputDir)) fs.mkdirSync(finalOutputDir, { recursive: true });
        
        const absoluteExcelPath = path.join(basePath, excelFile);
        const absoluteDgePath = path.join(basePath, dgeFile);
        let analysisType = setData.name.startsWith("Up:") ? "Upregulated" : (setData.name.startsWith("Down:") ? "Downregulated" : "Combined");
        
        const args = [
            '--input', normalizePathForR(absoluteExcelPath),
            '--set_name', setData.name,
            '--dge_file', normalizePathForR(absoluteDgePath),
            '--output_dir', normalizePathForR(finalOutputDir),
            '--analysis_type', analysisType,
            '--run_ppi', runPpi ? 'TRUE' : 'FALSE',
            '--scripts_dir', normalizePathForR(getResourcesPath())
        ];
        
        (async () => {
            try {
                await runRScript(event, '02_run_ppi_on_subset.R', args);
                if (progressWindow) progressWindow.webContents.send('progress-update', `Analysis for "${setData.name}" complete.`);
                dialog.showMessageBox({ type: 'info', title: 'Analysis Complete', message: `Venn subset analysis complete.` });
            } catch (error) {
                 if (error.message.includes("halted")) return;
                 console.error(`Venn subset analysis failed: ${error.message}`);
            }
        })();
    });
});