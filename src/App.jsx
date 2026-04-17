// ==============================================================================
// PROTOCOL: STRICT PRESERVATION MODE
// 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
// 2. IF EDITING: Only modify the specific logic requested.
// 3. PRESERVE: All handlers, progress windows, and robustness checks.
// 4. ALWAYS send the full code without truncation.
// ==============================================================================

import React, { useState, useEffect } from 'react';
import { FileUp, Play, Settings, Loader2, Plus, X, Trash2, GitMerge, ChevronLeft, FlaskConical, Binary, Microscope, CheckSquare, Square, FileCode, Beaker, Layers, Tag, ArrowRight, Globe, Database, FolderOpen, ExternalLink, StopCircle, History, Save, LayoutList, Network, Share2, Dna, Map } from 'lucide-react';

// ==========================================
// 1. HELPER FUNCTIONS
// ==========================================

const safeRender = (val) => {
    if (val === null || val === undefined) return "";
    if (typeof val === 'object') return ""; 
    return String(val);
};

// UPDATED: Now handles contrast objects (with IDs) or legacy strings
const formatContrastForDisplay = (contrastId, allContrasts = []) => {
    if (!contrastId) return 'Unknown';
    if (typeof contrastId === 'string' && !contrastId.startsWith('contrast_') && !contrastId.startsWith('legacy_')) {
        return contrastId;
    }
    
    const c = allContrasts.find(x => x.id === contrastId);
    if (c) return c.name;
    
    return String(contrastId).replace('legacy_', '').replace('-', ' vs. ');
};

// ==========================================
// 2. UI COMPONENTS
// ==========================================

const PPISettingsBuilder = ({ ppiParams, setPpiParams }) => {
    const handleChange = (key, val) => setPpiParams(prev => ({ ...prev, [key]: val }));
    
    return (
        <div className="mt-4">
            <h3 className="font-semibold text-sm mb-2 text-gray-700 uppercase tracking-wide flex items-center">
                <Network size={16} className="mr-2"/> Network Topology Settings
            </h3>
            <div className="p-3 bg-indigo-50 rounded-md border border-indigo-100 space-y-3">
                <div className="grid grid-cols-2 gap-3">
                    <div>
                        <label className="text-xs font-bold text-indigo-800 block mb-1">Clustering Algorithm</label>
                        <select 
                            value={ppiParams.cluster} 
                            onChange={(e) => handleChange('cluster', e.target.value)}
                            className="w-full border border-indigo-200 p-1.5 rounded text-sm outline-none"
                        >
                            <option value="louvain">Louvain (Standard)</option>
                            <option value="walktrap">Walktrap (Dense graphs)</option>
                            <option value="fast_greedy">Fast Greedy (Hierarchical)</option>
                            <option value="leiden">Leiden (Robust)</option>
                        </select>
                        <p className="text-[10px] text-gray-500 mt-1">
                            {ppiParams.cluster === 'louvain' && "Optimizes modularity. Good balance of speed and accuracy."}
                            {ppiParams.cluster === 'walktrap' && "Based on random walks. Better for dense subgraphs."}
                            {ppiParams.cluster === 'fast_greedy' && "Fast, hierarchical approach for large networks."}
                            {ppiParams.cluster === 'leiden' && "Refinement of Louvain. Guarantees well-connected communities."}
                        </p>
                    </div>
                    <div>
                        <label className="text-xs font-bold text-indigo-800 block mb-1">Hub Calculation</label>
                        <select 
                            value={ppiParams.hub} 
                            onChange={(e) => handleChange('hub', e.target.value)}
                            className="w-full border border-indigo-200 p-1.5 rounded text-sm outline-none"
                        >
                            <option value="pagerank">PageRank (Influence)</option>
                            <option value="degree">Degree (Connections)</option>
                            <option value="betweenness">Betweenness (Flow)</option>
                            <option value="closeness">Closeness (Speed)</option>
                        </select>
                         <p className="text-[10px] text-gray-500 mt-1">
                            {ppiParams.hub === 'pagerank' && "Measures global influence (like Google Search)."}
                            {ppiParams.hub === 'degree' && "Raw count of direct connections."}
                            {ppiParams.hub === 'betweenness' && "Identifies bridges/bottlenecks in info flow."}
                            {ppiParams.hub === 'closeness' && "Average distance to all other nodes."}
                        </p>
                    </div>
                </div>
                <div>
                    <label className="text-xs font-bold text-indigo-800 block mb-1">Min. Interaction Confidence (STRING DB)</label>
                    <div className="flex items-center gap-2">
                        <input 
                            type="range" min="400" max="999" step="10"
                            value={ppiParams.score}
                            onChange={(e) => handleChange('score', parseInt(e.target.value))}
                            className="flex-grow h-2 bg-indigo-200 rounded-lg appearance-none cursor-pointer"
                        />
                        <span className="text-xs font-mono font-bold w-10 text-right">{ppiParams.score}</span>
                    </div>
                    <p className="text-[10px] text-gray-500 mt-1">Higher values (e.g., 900) keep only high-confidence experimental interactions.</p>
                </div>
            </div>
        </div>
    );
};

const AnalysisOptionsBuilder = ({ options, setOptions, pCutoff, setPCutoff, fcCutoff, setFcCutoff, ppiParams, setPpiParams }) => {
    const toggleOption = (option) => { setOptions(prev => ({ ...prev, [option]: !prev[option] })); };
    return (
      <div className="mt-4 space-y-4">
        <div>
            <h3 className="font-semibold text-sm mb-2 text-gray-700 uppercase tracking-wide">Outlier Detection</h3>
            <div className="p-3 bg-red-50 rounded-md border border-red-100">
                <label className="text-xs font-bold text-red-800 block mb-1">Method</label>
                <select 
                    value={options.outlierMethod || 'none'} 
                    onChange={(e) => setOptions(prev => ({ ...prev, outlierMethod: e.target.value }))}
                    className="w-full border border-red-200 p-1.5 rounded text-sm outline-none"
                >
                    <option value="none">None (Keep all samples)</option>
                    <option value="mahalanobis">Global Mahalanobis Distance (Auto-exclude)</option>
                    <option value="mahalanobis_group">Group-Centric Mahalanobis (Auto-exclude)</option>
                </select>
                <p className="text-[10px] text-gray-500 mt-1">
                    {options.outlierMethod === 'mahalanobis_group' 
                        ? "Automatically removes samples that are far from their group centroid or cross-cluster closer to other groups (≥ 2 replicates must remain)."
                        : options.outlierMethod === 'mahalanobis'
                        ? "Automatically removes samples that are statistical global outliers (p < 0.01) based on overall PCA structure (≥ 3 replicates must remain)."
                        : "No samples will be removed based on distribution."}
                </p>
            </div>
        </div>

        <div>
            <h3 className="font-semibold text-sm mb-2 text-gray-700 uppercase tracking-wide">Significance Thresholds</h3>
            <div className="grid grid-cols-2 gap-4 p-3 bg-blue-50 rounded-md border border-blue-100">
                <div>
                    <label className="text-xs font-bold text-blue-800 block mb-1">Adj. P-Value</label>
                    <input 
                        type="number" step="0.01" min="0" max="1" 
                        value={pCutoff} 
                        onChange={e => setPCutoff(parseFloat(e.target.value))} 
                        className="w-full border border-blue-200 p-1.5 rounded text-sm focus:ring-1 focus:ring-blue-400 outline-none" 
                    />
                </div>
                <div>
                    <label className="text-xs font-bold text-blue-800 block mb-1">Log2 FC</label>
                    <input 
                        type="number" step="0.1" min="0" 
                        value={fcCutoff} 
                        onChange={e => setFcCutoff(parseFloat(e.target.value))} 
                        className="w-full border border-blue-200 p-1.5 rounded text-sm focus:ring-1 focus:ring-blue-400 outline-none" 
                    />
                </div>
            </div>
        </div>

        <PPISettingsBuilder ppiParams={ppiParams} setPpiParams={setPpiParams} />

        <div>
            <h3 className="font-semibold text-sm mb-2 text-gray-700 uppercase tracking-wide">Additional Visualizations</h3>
            <div className="p-3 bg-purple-50 rounded-md border border-purple-100 space-y-2">
                <div onClick={() => toggleOption('runPairwiseQC')} className="flex items-center cursor-pointer">
                    {options.runPairwiseQC ? <CheckSquare size={20} className="text-purple-600"/> : <Square size={20} className="text-gray-400"/>}
                    <span className="ml-2 text-sm text-purple-900 font-medium">Generate Pairwise QC Plots</span>
                </div>
                <p className="text-[10px] text-gray-500 ml-7 mb-2">Generates individual PCA/Density plots for each defined control-treatment contrast pair.</p>

                <div onClick={() => toggleOption('runDegHeatmaps')} className="flex items-center cursor-pointer border-t border-purple-100 pt-2">
                    {options.runDegHeatmaps ? <CheckSquare size={20} className="text-purple-600"/> : <Square size={20} className="text-gray-400"/>}
                    <span className="ml-2 text-sm text-purple-900 font-medium">Generate DEG Heatmaps</span>
                </div>
                <p className="text-[10px] text-gray-500 ml-7">Creates hierarchical clustering heatmaps of DEGs/DEPs based on log2FC for each contrast.</p>
            </div>
        </div>

        <div>
            <h3 className="font-semibold text-sm mb-2 text-gray-700 uppercase tracking-wide">Core Modules</h3>
            <div className="p-3 bg-gray-100 rounded-md space-y-2 border border-gray-200">
            <div onClick={() => toggleOption('runPpi')} className="flex items-center cursor-pointer">
                {options.runPpi ? <CheckSquare size={20} className="text-blue-600"/> : <Square size={20} className="text-gray-400"/>}
                <span className="ml-2 text-sm">Run PPI Network Analysis</span>
            </div>
            <div onClick={() => toggleOption('runVenn')} className="flex items-center cursor-pointer">
                {options.runVenn ? <CheckSquare size={20} className="text-blue-600"/> : <Square size={20} className="text-gray-400"/>}
                <span className="ml-2 text-sm">Run Venn Diagram Analysis</span>
            </div>
            <div onClick={() => toggleOption('runSignature')} className="flex items-center cursor-pointer">
                {options.runSignature ? <CheckSquare size={20} className="text-blue-600"/> : <Square size={20} className="text-gray-400"/>}
                <span className="ml-2 text-sm">Run Specific Signature Filtering</span>
            </div>
            </div>
        </div>
      </div>
    );
};

const AnalysisSetBuilder = ({ definedGroups, analysisSets, setAnalysisSets }) => {
    const [name, setName] = useState('');
    const [selectedGroups, setSelectedGroups] = useState(new Set());

    const handleCheckboxChange = (group) => {
        const newSelection = new Set(selectedGroups);
        if (newSelection.has(group)) {
            newSelection.delete(group);
        } else {
            newSelection.add(group);
        }
        setSelectedGroups(newSelection);
    };

    const handleAddSet = () => {
        if (!name.trim() || selectedGroups.size < 2) {
            alert("Please provide a name and select at least two groups.");
            return;
        }
        setAnalysisSets([...analysisSets, { name: name.trim(), groups: Array.from(selectedGroups) }]);
        setName('');
        setSelectedGroups(new Set());
    };

    return (
        <div>
            <h3 className="font-semibold text-lg mb-2">Define Analysis Sets (Optional)</h3>
            <div className="p-3 bg-gray-100 rounded-md space-y-3 border">
                <p className="text-xs text-gray-600 mb-2">Group unrelated experiments (e.g., different cell lines) into separate analysis sets.</p>
                <input
                    type="text"
                    placeholder="Set Name (e.g., TNBC_Line)"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    className="w-full border p-1.5 rounded-md text-sm"
                />
                <div className="max-h-24 overflow-y-auto space-y-1 border bg-white p-2 rounded">
                    {definedGroups.map((group) => (
                        <div key={group} className="flex items-center">
                            <input
                                type="checkbox"
                                id={`pca-subset-${group}`}
                                checked={selectedGroups.has(group)}
                                onChange={() => handleCheckboxChange(group)}
                                className="h-4 w-4 rounded border-gray-300 text-indigo-600"
                            />
                            <label htmlFor={`pca-subset-${group}`} className="ml-2 text-sm text-gray-700">{group}</label>
                        </div>
                    ))}
                </div>
                <button onClick={handleAddSet} className="w-full px-3 py-1.5 text-sm bg-green-600 text-white rounded-md hover:bg-green-700">
                    <Layers size={16} className="inline mr-2" /> Add Analysis Set
                </button>
            </div>
            <div className="mt-2 space-y-1">
                 {analysisSets.map((subset, i) => (
                    <div key={i} className="flex justify-between items-center bg-gray-200 p-1.5 rounded text-sm">
                        <span className="truncate" title={subset.groups.join(', ')}>
                           <strong>{subset.name}:</strong> {subset.groups.join(', ')}
                        </span>
                        <button onClick={() => setAnalysisSets(analysisSets.filter((_, index) => index !== i))} className="text-gray-500 hover:text-red-500"><X size={16}/></button>
                    </div>
                ))}
            </div>
        </div>
    );
};

const KeywordBuilder = ({ keywords, setKeywords }) => {
    const [input, setInput] = useState('');

    const handleAdd = () => {
        if (!input.trim()) return;
        if (keywords.includes(input.trim())) {
            alert("Keyword already exists.");
            return;
        }
        setKeywords([...keywords, input.trim()]);
        setInput('');
    };

    return (
        <div>
            <h3 className="font-semibold text-lg mb-2">Keyword Tagging</h3>
            <div className="p-3 bg-gray-100 rounded-md space-y-3 border">
                <p className="text-xs text-gray-600">Enter terms to tag in the final dataset (e.g., "sialoglycoprotein", "kinase").</p>
                <div className="flex gap-2">
                    <input
                        type="text"
                        placeholder="Enter keyword..."
                        value={input}
                        onChange={(e) => setInput(e.target.value)}
                        className="flex-grow border p-1.5 rounded-md text-sm"
                        onKeyDown={(e) => e.key === 'Enter' && handleAdd()}
                    />
                    <button onClick={handleAdd} className="px-3 py-1.5 text-sm bg-blue-600 text-white rounded-md hover:bg-blue-700">
                        <Plus size={16} />
                    </button>
                </div>
            </div>
            <div className="mt-2 flex flex-wrap gap-2">
                {keywords.map((kw, i) => (
                    <span key={i} className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
                        {kw}
                        <button onClick={() => setKeywords(keywords.filter((_, idx) => idx !== i))} className="ml-1.5 text-blue-600 hover:text-blue-900 focus:outline-none">
                            <X size={14} />
                        </button>
                    </span>
                ))}
            </div>
        </div>
    );
};

const CorrectionPipeline = ({ options, setOptions }) => {
    const toggle = (key) => setOptions(prev => ({...prev, [key]: !prev[key]}));
    
    return (
        <div className="mt-4">
            <h3 className="font-semibold text-lg mb-2">Normalization & Correction Pipeline</h3>
            <div className="p-3 bg-gray-100 rounded-md space-y-2 border border-gray-300">
                <div className="flex items-center justify-between">
                    <div className="flex items-center">
                        <span className="bg-gray-800 text-white text-xs px-2 py-1 rounded mr-2">1</span>
                        <label className="text-sm font-medium">Quantile Normalization</label>
                    </div>
                    <input type="checkbox" checked={options.runMedian || false} onChange={() => toggle('runMedian')} className="h-4 w-4 text-blue-600"/>
                </div>
                <p className="text-xs text-gray-500 ml-8 mb-2">Forces identical distributions across samples. Best for fixing loading bias & distribution shape differences.</p>

                <div className="flex items-center justify-between border-t pt-2">
                    <div className="flex items-center">
                        <span className="bg-gray-800 text-white text-xs px-2 py-1 rounded mr-2">2</span>
                        <label className="text-sm font-medium">RUV-III (Residuals)</label>
                    </div>
                    <input type="checkbox" checked={options.runRuv || false} onChange={() => toggle('runRuv')} className="h-4 w-4 text-blue-600"/>
                </div>
                <p className="text-xs text-gray-500 ml-8 mb-2">Removes noise using technical replicates (Requires 'Replicate ID').</p>

                <div className="flex items-center justify-between border-t pt-2">
                    <div className="flex items-center">
                        <span className="bg-gray-800 text-white text-xs px-2 py-1 rounded mr-2">3</span>
                        <label className="text-sm font-medium">Drift Correction (Injection Order)</label>
                    </div>
                    <input type="checkbox" checked={options.runLoess || false} onChange={() => toggle('runLoess')} className="h-4 w-4 text-blue-600"/>
                </div>
                <p className="text-xs text-gray-500 ml-8 mb-2">Corrects signal drift using LOESS regression on Injection Order.</p>

                <div className="flex items-center justify-between border-t pt-2">
                    <div className="flex items-center">
                        <span className="bg-gray-800 text-white text-xs px-2 py-1 rounded mr-2">4</span>
                        <label className="text-sm font-medium">ComBat (Batch Effect)</label>
                    </div>
                    <input type="checkbox" checked={options.runCombat || false} onChange={() => toggle('runCombat')} className="h-4 w-4 text-blue-600"/>
                </div>
                <p className="text-xs text-gray-500 ml-8">Adjusts mean/variance shifts between blocks (Requires 'Batch').</p>
            </div>
        </div>
    );
};

const ContrastBuilder = ({ groups, sampleConfig, contrasts, setContrasts }) => {
    const [name, setName] = useState('');
    const [cond1, setCond1] = useState({ type: 'group', value: '' });
    const [cond2, setCond2] = useState({ type: 'group', value: '' });

    useEffect(() => {
        if (groups.length > 0 && !cond1.value && cond1.type === 'group') setCond1({ type: 'group', value: groups[0] });
        if (groups.length > 1 && !cond2.value && cond2.type === 'group') setCond2({ type: 'group', value: groups[1] });
        else if (groups.length > 0 && !cond2.value && cond2.type === 'group') setCond2({ type: 'group', value: groups[0] });
    }, [groups, cond1.value, cond1.type, cond2.value, cond2.type]);

    const handleSampleToggle = (condSetter, condState, sampleName) => {
        const currentSamples = Array.isArray(condState.value) ? condState.value : [];
        if (currentSamples.includes(sampleName)) {
            condSetter({ ...condState, value: currentSamples.filter(s => s !== sampleName) });
        } else {
            condSetter({ ...condState, value: [...currentSamples, sampleName] });
        }
    };

    const handleAdd = () => {
        if (cond1.type === 'group' && !cond1.value) return alert("Select Group for Condition 1");
        if (cond2.type === 'group' && !cond2.value) return alert("Select Group for Condition 2");
        if (cond1.type === 'samples' && (!cond1.value || cond1.value.length === 0)) return alert("Select at least one manual sample for Condition 1");
        if (cond2.type === 'samples' && (!cond2.value || cond2.value.length === 0)) return alert("Select at least one manual sample for Condition 2");

        if (cond1.type === 'group' && cond2.type === 'group' && cond1.value === cond2.value) {
            return alert("Please select two different groups or use manual samples.");
        }

        const defaultCond1Name = cond1.type === 'group' ? cond1.value : `${cond1.value.length}Samples`;
        const defaultCond2Name = cond2.type === 'group' ? cond2.value : `${cond2.value.length}Samples`;
        const finalName = name.trim() || `${defaultCond1Name} vs ${defaultCond2Name}`;

        const newContrast = {
            id: `contrast_${Date.now()}_${Math.random().toString(36).substr(2, 5)}`,
            name: finalName,
            cond1: { ...cond1 },
            cond2: { ...cond2 }
        };

        setContrasts([...contrasts, newContrast]);
        setName('');
    };

    const renderCondition = (condState, condSetter, title) => (
        <div className="flex-1 bg-white p-3 rounded border border-gray-300 shadow-sm">
            <h4 className="font-bold text-xs text-gray-700 mb-2">{title}</h4>
            <select
                className="w-full border p-1.5 mb-2 text-sm rounded bg-gray-50 outline-none focus:ring-1 focus:ring-blue-500"
                value={condState.type}
                onChange={e => condSetter({ type: e.target.value, value: e.target.value === 'group' ? (groups[0] || '') : [] })}
            >
                <option value="group">By Group</option>
                <option value="samples">Manual Samples</option>
            </select>
            {condState.type === 'group' ? (
                <select
                    className="w-full border p-1.5 text-sm rounded outline-none focus:ring-1 focus:ring-blue-500"
                    value={condState.value}
                    onChange={e => condSetter({ ...condState, value: e.target.value })}
                >
                    {groups.map(g => <option key={g} value={g}>{g}</option>)}
                </select>
            ) : (
                <div className="max-h-32 overflow-y-auto border p-1 rounded space-y-1 bg-gray-50">
                    {sampleConfig && sampleConfig.length > 0 ? sampleConfig.map(s => (
                        <div key={s.finalName} className="flex items-center hover:bg-gray-100 p-1 rounded">
                            <input
                                type="checkbox"
                                checked={Array.isArray(condState.value) && condState.value.includes(s.finalName)}
                                onChange={() => handleSampleToggle(condSetter, condState, s.finalName)}
                                className="mr-2 h-3 w-3 text-blue-600 rounded"
                            />
                            <span className="text-xs text-gray-700 truncate" title={s.finalName}>{s.finalName} <span className="text-gray-400">({s.group || 'No Group'})</span></span>
                        </div>
                    )) : (
                        <div className="text-xs text-gray-500 italic p-1">No samples assigned yet.</div>
                    )}
                </div>
            )}
        </div>
    );

    return (
        <div>
            <h3 className="font-semibold text-lg mb-2">Define Advanced Contrasts</h3>
            <div className="p-3 bg-gray-100 rounded-md space-y-3 border border-gray-200">
                <input
                    type="text"
                    placeholder="Custom Contrast Name (Optional, e.g., Tumor vs Normal)"
                    value={name}
                    onChange={e => setName(e.target.value)}
                    className="w-full border p-2 text-sm rounded-md shadow-sm outline-none focus:ring-1 focus:ring-blue-500"
                />
                <div className="flex gap-2">
                    {renderCondition(cond1, setCond1, "Condition 1")}
                    <div className="flex items-center font-bold text-gray-400">VS</div>
                    {renderCondition(cond2, setCond2, "Condition 2")}
                </div>
                <button onClick={handleAdd} className="w-full p-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 text-sm font-bold shadow-sm transition-colors">
                    <Plus size={16} className="inline mr-1"/> Add Contrast Definition
                </button>
            </div>
            
            <div className="space-y-2 mt-3">
                {contrasts.map(c => {
                    if (typeof c === 'string') {
                        return (
                            <div key={c} className="flex justify-between items-center bg-white p-2 rounded text-sm border shadow-sm">
                                <span className="font-medium">{formatContrastForDisplay(c)}</span>
                                <button onClick={() => setContrasts(contrasts.filter(con => con !== c))} className="text-gray-400 hover:text-red-500"><X size={16}/></button>
                            </div>
                        );
                    }
                    return (
                        <div key={c.id} className="flex justify-between items-center bg-white p-2.5 rounded text-sm border shadow-sm">
                            <div>
                                <div className="font-bold text-gray-800">{c.name}</div>
                                <div className="text-[10px] text-gray-500 mt-0.5">
                                    <span className="font-medium text-blue-600">{c.cond1.type === 'group' ? `Group: ${c.cond1.value}` : `${c.cond1.value.length} Manual Samples`}</span>
                                    <span className="mx-1 text-gray-400">vs</span>
                                    <span className="font-medium text-purple-600">{c.cond2.type === 'group' ? `Group: ${c.cond2.value}` : `${c.cond2.value.length} Manual Samples`}</span>
                                </div>
                            </div>
                            <button onClick={() => setContrasts(contrasts.filter(con => con.id !== c.id))} className="text-gray-400 hover:text-red-500 ml-4 p-1 hover:bg-red-50 rounded">
                                <X size={18}/>
                            </button>
                        </div>
                    );
                })}
            </div>
        </div>
    );
};

const VennBuilder = ({ definedContrasts, definedGroups, vennContrasts, setVennContrasts }) => {
    const [currentSelection, setCurrentSelection] = useState(new Set());
    
    const handleCheckboxChange = (contrastId) => { 
        const newSelection = new Set(currentSelection); 
        if (newSelection.has(contrastId)) { newSelection.delete(contrastId); } 
        else { newSelection.add(contrastId); } 
        setCurrentSelection(newSelection); 
    };
    
    const handleAddCombination = () => { 
        if (currentSelection.size < 2) { alert("Select at least two contrasts."); return; } 
        const newCombination = Array.from(currentSelection).sort(); 
        const isDuplicate = vennContrasts.some(existing => existing.length === newCombination.length && existing.every(c => newCombination.includes(c))); 
        if(isDuplicate) { alert("Combination exists."); return; } 
        setVennContrasts([...vennContrasts, newCombination]); 
        setCurrentSelection(new Set()); 
    };

    return ( 
        <div>
            <h3 className="font-semibold text-lg mb-2">Define Venn Combinations</h3>
            <div className="p-3 bg-gray-100 rounded-md space-y-3 border">
                <div className="space-y-2">
                    {definedContrasts.map((contrast) => {
                        const id = typeof contrast === 'string' ? contrast : contrast.id;
                        return ( 
                            <div key={id} className="flex items-center">
                                <input type="checkbox" id={`venn-${id}`} checked={currentSelection.has(id)} onChange={() => handleCheckboxChange(id)} className="h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"/>
                                <label htmlFor={`venn-${id}`} className="ml-3 block text-sm font-medium text-gray-700">
                                    {formatContrastForDisplay(id, definedContrasts)}
                                </label>
                            </div> 
                        );
                    })}
                </div>
                <button onClick={handleAddCombination} disabled={currentSelection.size < 2} className="w-full mt-2 px-3 py-1.5 text-sm bg-indigo-600 text-white rounded-md hover:bg-indigo-700 disabled:bg-gray-400 shadow-sm transition-colors">Add Combination</button>
            </div>
            <div className="mt-2 space-y-1">
                {vennContrasts.map((combo, i) => ( 
                    <div key={i} className="flex justify-between items-center bg-gray-200 p-2 rounded text-sm shadow-sm border border-gray-300">
                        <span className="truncate pr-4">Venn: {combo.map(cId => formatContrastForDisplay(cId, definedContrasts)).join(' | ')}</span>
                        <button onClick={() => setVennContrasts(vennContrasts.filter((_, index) => index !== i))} className="text-gray-500 hover:text-red-500 p-1"><X size={16}/></button>
                    </div> 
                ))}
            </div>
        </div> 
    );
};

const SignatureBuilder = ({ groups, signatureConfig, setSignatureConfig }) => {
    const [rule, setRule] = useState({ name: '', primaryGroup: '', controlGroup: '', confoundingGroup: '' });
    useEffect(() => { if (groups.length > 0) { setRule(prev => ({ ...prev, primaryGroup: prev.primaryGroup || groups[0], controlGroup: prev.controlGroup || groups[0], confoundingGroup: prev.confoundingGroup || groups[0] })); } }, [groups]);
    const handleAddSignature = () => { if (!rule.name.trim() || !rule.primaryGroup || !rule.controlGroup || !rule.confoundingGroup) { return alert("Please fill out all fields."); } setSignatureConfig([...signatureConfig, rule]); setRule({ name: '', primaryGroup: groups[0], controlGroup: groups[0], confoundingGroup: groups[0] }); };
    return ( <div><h3 className="font-semibold text-lg mb-2">Define Signatures</h3><div className="p-3 bg-gray-100 rounded-md space-y-3 border"><input type="text" placeholder="Signature Name" value={rule.name} onChange={e => setRule({...rule, name: e.target.value})} className="w-full border border-gray-300 text-sm rounded-lg p-1.5"/><div className="grid grid-cols-3 gap-2 text-center text-sm"><label>Primary</label><label>Control</label><label>Confounding</label><select value={rule.primaryGroup} onChange={e => setRule({...rule, primaryGroup: e.target.value})} className="w-full border border-gray-300 text-sm rounded-lg p-1.5">{groups.map(g => <option key={`p-${g}`} value={g}>{g}</option>)}</select><select value={rule.controlGroup} onChange={e => setRule({...rule, controlGroup: e.target.value})} className="w-full border border-gray-300 text-sm rounded-lg p-1.5">{groups.map(g => <option key={`c-${g}`} value={g}>{g}</option>)}</select><select value={rule.confoundingGroup} onChange={e => setRule({...rule, confoundingGroup: e.target.value})} className="w-full border border-gray-300 text-sm rounded-lg p-1.5">{groups.map(g => <option key={`f-${g}`} value={g}>{g}</option>)}</select></div><button onClick={handleAddSignature} className="w-full mt-2 px-3 py-1.5 text-sm bg-purple-600 text-white rounded-md hover:bg-purple-700 shadow-sm transition-colors" disabled={groups.length < 3}>Add Signature</button></div><div className="mt-2 space-y-1">{signatureConfig.map((sig, i) => ( <div key={i} className="flex justify-between items-center bg-white border p-2 rounded text-sm shadow-sm"><span className="font-medium text-purple-700">{sig.name}</span><button onClick={() => setSignatureConfig(signatureConfig.filter((_, index) => index !== i))} className="text-gray-400 hover:text-red-500"><X size={16}/></button></div> ))}</div></div> );
};

const TabButton = ({ name, title, icon, activeTab, setActiveTab }) => {
    const isActive = activeTab === name;
    return (
        <button
            onClick={() => setActiveTab(name)}
            title={title}
            className={`flex items-center justify-center gap-2 p-2 text-xs font-medium rounded-md transition-colors ${
                isActive ? 'bg-indigo-50 text-indigo-700 border border-indigo-200 shadow-sm' : 'text-gray-500 hover:bg-gray-100'
            }`}
        >
            {icon}
            <span className="hidden xl:inline">{title}</span>
        </button>
    );
};

// --- Kegg Mapper Modal ---
const KeggMapperModal = ({ isOpen, onClose, workingDir }) => {
    const [dgeFile, setDgeFile] = useState('');
    const [species, setSpecies] = useState('hsa');

    const handleSelect = async () => {
        if (window.electronAPI && window.electronAPI.openFile) {
            const path = await window.electronAPI.openFile();
            if (path) setDgeFile(path);
        } else {
            alert("File API not available.");
        }
    };

    const handleRun = () => {
        if(!dgeFile) return alert("Please select a DGE results file.");
        
        if(window.electronAPI && window.electronAPI.runKeggMapper) {
            window.electronAPI.runKeggMapper({ dgeFile, species, workingDir });
            alert("Generating KEGG Mapper input. Check your working directory shortly.");
            onClose();
        } else {
            alert("Connection Error: 'runKeggMapper' API not found. Please restart or update backend.");
        }
    };

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-xl shadow-2xl w-full max-w-lg overflow-hidden">
                <div className="bg-orange-500 px-6 py-4 flex justify-between items-center">
                    <h3 className="text-white text-lg font-bold flex items-center"><Map className="mr-2" size={20} /> KEGG Mapper Integration</h3>
                    <button onClick={onClose} className="text-orange-100 hover:text-white"><X size={20} /></button>
                </div>
                <div className="p-6 space-y-4">
                    <p className="text-sm text-gray-600">
                        Generate a compatible text file to map your DEGs directly onto KEGG pathway maps. Genes will be automatically colored based on Log2 Fold Change.
                    </p>
                    
                    <div>
                        <label className="block text-xs font-bold text-gray-700 mb-1">DGE Results File (.csv)</label>
                        <div className="flex">
                            <input type="text" readOnly value={dgeFile} placeholder="Select DGE_Results...csv" className="flex-grow border border-gray-300 rounded-l-md p-2 text-sm bg-gray-50" />
                            <button onClick={handleSelect} className="bg-orange-500 text-white px-3 py-2 rounded-r-md text-sm hover:bg-orange-600">Browse</button>
                        </div>
                    </div>

                    <div>
                        <label className="block text-xs font-bold text-gray-700 mb-1">Species Code</label>
                        <select 
                            value={species} 
                            onChange={(e) => setSpecies(e.target.value)}
                            className="w-full border border-gray-300 p-2 rounded-md text-sm bg-gray-50 outline-none focus:border-orange-500"
                        >
                            <option value="hsa">Homo sapiens (Human) - hsa</option>
                            <option value="mmu">Mus musculus (Mouse) - mmu</option>
                            <option value="rno">Rattus norvegicus (Rat) - rno</option>
                            <option value="dme">Drosophila melanogaster (Fruit fly) - dme</option>
                        </select>
                    </div>

                </div>
                <div className="bg-gray-50 px-6 py-4 flex justify-end gap-3 border-t">
                    <button onClick={onClose} className="px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 rounded-md">Cancel</button>
                    <button 
                        onClick={handleRun} 
                        disabled={!dgeFile}
                        className="flex items-center px-4 py-2 bg-orange-500 text-white text-sm font-medium rounded-md hover:bg-orange-600 disabled:bg-gray-400"
                    >
                        <Map className="mr-2 h-4 w-4" /> Generate Mapper File
                    </button>
                </div>
            </div>
        </div>
    );
};

const CrossSpeciesModal = ({ isOpen, onClose, onRun, isLoading }) => {
    const [axolotlFile, setAxolotlFile] = useState('');
    const [humanFile, setHumanFile] = useState('');
    const [orthoFile, setOrthoFile] = useState('');

    const handleSelect = async (setter) => {
        if (window.electronAPI && window.electronAPI.openFile) {
            const path = await window.electronAPI.openFile();
            if (path) setter(path);
        } else {
            alert("File API not available.");
        }
    };

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-xl shadow-2xl w-full max-w-lg overflow-hidden">
                <div className="bg-emerald-600 px-6 py-4 flex justify-between items-center">
                    <h3 className="text-white text-lg font-bold flex items-center"><Dna className="mr-2" size={20} /> Cross-Species Analysis</h3>
                    <button onClick={onClose} className="text-emerald-200 hover:text-white"><X size={20} /></button>
                </div>
                <div className="p-6 space-y-4">
                    <p className="text-sm text-gray-600">
                        Compare regenerative DGE (e.g. Axolotl) against pathology DGE (e.g. Human TNBC) to identify evolutionary <strong>Binary Switches</strong>.
                    </p>
                    
                    <div>
                        <label className="block text-xs font-bold text-gray-700 mb-1">Regenerative Species DGE (e.g. Axolotl)</label>
                        <div className="flex">
                            <input type="text" readOnly value={axolotlFile} placeholder="Select .csv file..." className="flex-grow border border-gray-300 rounded-l-md p-2 text-sm bg-gray-50" />
                            <button onClick={() => handleSelect(setAxolotlFile)} className="bg-emerald-500 text-white px-3 py-2 rounded-r-md text-sm hover:bg-emerald-600">Browse</button>
                        </div>
                    </div>

                    <div>
                        <label className="block text-xs font-bold text-gray-700 mb-1">Pathology Species DGE (e.g. Human TNBC)</label>
                        <div className="flex">
                            <input type="text" readOnly value={humanFile} placeholder="Select .csv file..." className="flex-grow border border-gray-300 rounded-l-md p-2 text-sm bg-gray-50" />
                            <button onClick={() => handleSelect(setHumanFile)} className="bg-emerald-500 text-white px-3 py-2 rounded-r-md text-sm hover:bg-emerald-600">Browse</button>
                        </div>
                    </div>

                    <div>
                        <label className="block text-xs font-bold text-gray-700 mb-1">Orthology Map (.csv)</label>
                        <div className="flex">
                            <input type="text" readOnly value={orthoFile} placeholder="Select mapping file..." className="flex-grow border border-gray-300 rounded-l-md p-2 text-sm bg-gray-50" />
                            <button onClick={() => handleSelect(setOrthoFile)} className="bg-emerald-500 text-white px-3 py-2 rounded-r-md text-sm hover:bg-emerald-600">Browse</button>
                        </div>
                        <p className="text-[10px] text-gray-500 mt-1">Map should contain columns linking Species 1 IDs to Species 2 IDs (e.g. Amex_ID to Human_Symbol).</p>
                    </div>

                </div>
                <div className="bg-gray-50 px-6 py-4 flex justify-end gap-3 border-t">
                    <button onClick={onClose} className="px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 rounded-md">Cancel</button>
                    <button 
                        onClick={() => onRun({ axolotl: axolotlFile, human: humanFile, ortho: orthoFile })} 
                        disabled={isLoading || !axolotlFile || !humanFile || !orthoFile}
                        className="flex items-center px-4 py-2 bg-emerald-600 text-white text-sm font-medium rounded-md hover:bg-emerald-700 disabled:bg-gray-400"
                    >
                        {isLoading ? <Loader2 className="animate-spin mr-2 h-4 w-4" /> : <Dna className="mr-2 h-4 w-4" />}
                        Run Evolutionary Comparison
                    </button>
                </div>
            </div>
        </div>
    );
};

// --- REDESIGNED: GEO Modal (With Full Interactive Grouping Flow) ---
const GeoModal = ({ isOpen, onClose, onRun, isLoading, onRunSplit, onRunIndependent, onRunManualFetch }) => {
    const [gseInput, setGseInput] = useState('');
    const [recentGSEs, setRecentGSEs] = useState([]);
    
    // Manual Grouping State
    const [geoSamples, setGeoSamples] = useState(null); 
    const [isFetchingMeta, setIsFetchingMeta] = useState(false);
    const [selectedSamples, setSelectedSamples] = useState(new Set());
    const [lastClicked, setLastClicked] = useState(null);

    useEffect(() => {
        if (isOpen) {
             const stored = localStorage.getItem('recent_gse_ids');
             if (stored) {
                 try { setRecentGSEs(JSON.parse(stored)); } catch (e) { console.error(e); }
             }
             setGeoSamples(null);
        }
    }, [isOpen]);

    const saveHistory = (ids) => {
        if (!ids) return;
        const newHistory = [ids, ...recentGSEs.filter(x => x !== ids)].slice(0, 5); 
        setRecentGSEs(newHistory);
        localStorage.setItem('recent_gse_ids', JSON.stringify(newHistory));
    };

    const handleFetchMetadata = async () => {
        if (!gseInput.trim()) return;
        setIsFetchingMeta(true);
        
        try {
            let samples;
            if (window.electronAPI && window.electronAPI.fetchGeoMetadata) {
                samples = await window.electronAPI.fetchGeoMetadata({ gseIds: gseInput });
            } else {
                // FALLBACK MOCK DATA: So the UI works seamlessly before the backend R scripts are fully hooked up!
                console.warn("fetchGeoMetadata API not connected yet. Using MOCK data to demonstrate the grouping UI.");
                samples = [
                    { gse: gseInput.split(',')[0].trim(), gsm: 'GSM1000001', title: 'Wild Type liver replicate 1' },
                    { gse: gseInput.split(',')[0].trim(), gsm: 'GSM1000002', title: 'Wild Type liver replicate 2' },
                    { gse: gseInput.split(',')[0].trim(), gsm: 'GSM1000003', title: 'TP53 Knockout tumor model 1' },
                    { gse: gseInput.split(',')[0].trim(), gsm: 'GSM1000004', title: 'TP53 Knockout tumor model 2' },
                ];
                await new Promise(r => setTimeout(r, 1000)); // Simulate network delay
            }

            const mapSamples = samples.map(s => {
                let group = 'Exclude';
                const t = (s.title || '').toLowerCase();
                if (t.includes('control') || t.includes('wt') || t.includes('normal') || t.includes('healthy') || t.includes('untreated') || t.includes('vehicle')) group = 'Control';
                else if (t.includes('tumor') || t.includes('cancer') || t.includes('treated') || t.includes('ko') || t.includes('mutant') || t.includes('disease')) group = 'Treatment';
                return { ...s, group };
            });
            setGeoSamples(mapSamples);
            setSelectedSamples(new Set());
            setLastClicked(null);
        } catch (e) {
            alert("Failed to fetch metadata: " + e.message);
        }
        setIsFetchingMeta(false);
    };

    const handleRowClick = (index, e) => {
        const newSel = new Set(selectedSamples);
        if (e.shiftKey && lastClicked !== null) {
            const start = Math.min(index, lastClicked);
            const end = Math.max(index, lastClicked);
            for (let i = start; i <= end; i++) newSel.add(i);
        } else {
            if (newSel.has(index)) newSel.delete(index);
            else newSel.add(index);
        }
        setSelectedSamples(newSel);
        setLastClicked(index);
    };

    const assignGroupToSelected = (group) => {
        if (selectedSamples.size === 0) return;
        const updated = [...geoSamples];
        selectedSamples.forEach(i => { updated[i].group = group; });
        setGeoSamples(updated);
        setSelectedSamples(new Set());
    };

    const getSampleMap = () => {
        const map = {};
        if (geoSamples) {
            geoSamples.forEach(s => {
                if (s.group !== 'Exclude') map[s.gsm] = s.group;
            });
        }
        return map;
    };

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-xl shadow-2xl w-full max-w-4xl overflow-hidden flex flex-col max-h-[90vh]">
                <div className="bg-indigo-600 px-6 py-4 flex justify-between items-center flex-shrink-0">
                    <h3 className="text-white text-lg font-bold flex items-center"><Globe className="mr-2" size={20} /> GEO Integration & Grouping</h3>
                    <button onClick={onClose} className="text-indigo-200 hover:text-white"><X size={20} /></button>
                </div>
                
                <div className="p-6 overflow-y-auto flex-grow flex flex-col">
                    {!geoSamples ? (
                        <>
                            <p className="text-sm text-gray-600 mb-4">
                                Enter GEO Series IDs (e.g., <code className="bg-gray-100 px-1 rounded">GSE139038, GSE76250</code>). 
                            </p>
                            <div className="flex justify-between items-center mb-2">
                                <label className="block text-sm font-medium text-gray-700">GSE IDs (Comma separated)</label>
                                {recentGSEs.length > 0 && (
                                    <div className="text-xs text-gray-500">
                                        History: {recentGSEs.map((ids, i) => (
                                            <span key={i} onClick={() => setGseInput(ids)} className="ml-1 text-blue-600 cursor-pointer hover:underline">
                                                [{i+1}]
                                            </span>
                                        ))}
                                    </div>
                                )}
                            </div>
                            <textarea 
                                value={gseInput}
                                onChange={(e) => setGseInput(e.target.value)}
                                placeholder="GSE12345, GSE67890..."
                                className="w-full h-32 p-3 border border-gray-300 rounded-md font-mono text-sm focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
                            />
                            <div className="mt-4">
                                <button
                                    onClick={handleFetchMetadata}
                                    disabled={isFetchingMeta || !gseInput.trim()}
                                    className="w-full flex items-center justify-center px-4 py-3 bg-blue-600 text-white text-sm font-bold rounded-md hover:bg-blue-700 disabled:bg-blue-300 transition-colors shadow-sm"
                                >
                                    {isFetchingMeta ? <Loader2 className="animate-spin mr-2 h-5 w-5" /> : <Database className="mr-2 h-5 w-5" />}
                                    1. Fetch Samples & Initialize Grouping
                                </button>
                            </div>
                        </>
                    ) : (
                        <>
                            <div className="flex justify-between items-center mb-2">
                                <h4 className="text-sm font-bold text-gray-800">Assign Sample Groups</h4>
                                <button onClick={() => setGeoSamples(null)} className="text-xs text-blue-600 hover:underline">← Back to GSE Input</button>
                            </div>
                            <p className="text-xs text-gray-500 mb-3">Select rows (Shift+Click for multiple) and assign them to Control or Treatment. Unassigned samples are ignored.</p>

                            <div className="flex gap-2 mb-3">
                                <button onClick={() => assignGroupToSelected('Control')} className="px-3 py-1.5 bg-blue-100 text-blue-800 hover:bg-blue-200 border border-blue-300 rounded text-xs font-bold transition-colors">Assign Control</button>
                                <button onClick={() => assignGroupToSelected('Treatment')} className="px-3 py-1.5 bg-red-100 text-red-800 hover:bg-red-200 border border-red-300 rounded text-xs font-bold transition-colors">Assign Treatment</button>
                                <button onClick={() => assignGroupToSelected('Exclude')} className="px-3 py-1.5 bg-gray-100 text-gray-800 hover:bg-gray-200 border border-gray-300 rounded text-xs font-bold transition-colors">Exclude / Ignore</button>
                                <button onClick={() => setSelectedSamples(new Set())} className="ml-auto px-3 py-1.5 text-gray-500 hover:bg-gray-100 rounded text-xs transition-colors">Clear Selection</button>
                            </div>

                            <div className="border border-gray-300 rounded-md overflow-y-auto flex-grow min-h-[250px] shadow-inner bg-gray-50">
                                <table className="w-full text-left text-sm border-collapse">
                                    <thead className="bg-white sticky top-0 shadow-sm z-10">
                                        <tr>
                                            <th className="p-2 border-b border-r text-xs text-gray-500 font-bold uppercase w-24">GSE</th>
                                            <th className="p-2 border-b border-r text-xs text-gray-500 font-bold uppercase w-32">Sample ID</th>
                                            <th className="p-2 border-b border-r text-xs text-gray-500 font-bold uppercase">Description</th>
                                            <th className="p-2 border-b text-xs text-gray-500 font-bold uppercase w-32">Assigned Role</th>
                                        </tr>
                                    </thead>
                                    <tbody className="bg-white">
                                        {geoSamples.map((s, i) => {
                                            const isSelected = selectedSamples.has(i);
                                            return (
                                                <tr key={i} onClick={(e) => handleRowClick(i, e)} className={`cursor-pointer border-b hover:bg-indigo-50 transition-colors ${isSelected ? 'bg-indigo-100' : ''}`}>
                                                    <td className="p-2 border-r text-xs text-gray-600">{s.gse}</td>
                                                    <td className="p-2 border-r font-mono text-xs text-gray-800">{s.gsm}</td>
                                                    <td className="p-2 border-r text-xs text-gray-700 truncate max-w-[250px]" title={s.title}>{s.title}</td>
                                                    <td className="p-2 font-bold text-xs">
                                                        {s.group === 'Control' && <span className="text-blue-600 flex items-center"><CheckSquare size={14} className="mr-1"/> Control</span>}
                                                        {s.group === 'Treatment' && <span className="text-red-600 flex items-center"><CheckSquare size={14} className="mr-1"/> Treatment</span>}
                                                        {s.group === 'Exclude' && <span className="text-gray-400 flex items-center"><X size={14} className="mr-1"/> Ignored</span>}
                                                    </td>
                                                </tr>
                                            );
                                        })}
                                    </tbody>
                                </table>
                            </div>
                        </>
                    )}
                </div>
                
                <div className="bg-gray-100 px-6 py-4 flex flex-col gap-3 border-t flex-shrink-0">
                    {!geoSamples ? (
                        <>
                            <p className="text-xs text-gray-500 font-bold uppercase mb-1">Legacy Automated Pipelines (Without Manual Grouping)</p>
                            <div className="grid grid-cols-2 gap-3">
                                <button 
                                    onClick={() => { saveHistory(gseInput); onRunSplit(gseInput, 'Normal', 'Tumor'); }} 
                                    disabled={isLoading || !gseInput.trim()}
                                    className="flex items-center justify-center px-4 py-2 bg-green-500 text-white text-sm font-medium rounded-md hover:bg-green-600 disabled:bg-gray-400 shadow-sm"
                                >
                                    {isLoading ? <Loader2 className="animate-spin mr-2 h-4 w-4" /> : <Layers className="mr-2 h-4 w-4" />}
                                    Auto-Split Analysis
                                </button>
                                <button
                                    onClick={() => { saveHistory(gseInput); onRunIndependent(gseInput); }}
                                    disabled={isLoading || !gseInput.trim()}
                                    className="flex items-center justify-center px-4 py-2 bg-purple-600 text-white text-sm font-medium rounded-md hover:bg-purple-700 disabled:bg-gray-400 shadow-sm"
                                >
                                    {isLoading ? <Loader2 className="animate-spin mr-2 h-4 w-4" /> : <Dna className="mr-2 h-4 w-4" />}
                                    Auto-Process Independently
                                </button>
                            </div>
                        </>
                    ) : (
                        <>
                            <p className="text-xs text-gray-500 font-bold uppercase mb-1">Execute Pipeline with Manual Groups</p>
                            <div className="grid grid-cols-2 gap-3">
                                <button
                                    onClick={() => { saveHistory(gseInput); onRunIndependent(gseInput, getSampleMap()); }}
                                    disabled={isLoading}
                                    className="flex items-center justify-center px-4 py-2 bg-purple-600 text-white text-sm font-bold rounded-md hover:bg-purple-700 disabled:bg-gray-400 shadow-sm transition-colors"
                                >
                                    {isLoading ? <Loader2 className="animate-spin mr-2 h-4 w-4" /> : <Dna className="mr-2 h-4 w-4" />}
                                    Run Independent Processor
                                </button>
                                <button 
                                    onClick={() => { saveHistory(gseInput); onRunManualFetch(gseInput, getSampleMap()); }} 
                                    disabled={isLoading}
                                    className="flex items-center justify-center px-4 py-2 bg-orange-500 text-white text-sm font-bold rounded-md hover:bg-orange-600 disabled:bg-gray-400 shadow-sm transition-colors"
                                >
                                    {isLoading ? <Loader2 className="animate-spin mr-2 h-4 w-4" /> : <Settings className="mr-2 h-4 w-4" />}
                                    Load to Standard Pipeline
                                </button>
                            </div>
                        </>
                    )}
                    
                    <div className="flex justify-end mt-2">
                         <button onClick={onClose} className="px-4 py-2 text-sm text-gray-700 hover:bg-gray-200 rounded-md border border-gray-300 bg-white">Cancel</button>
                    </div>
                </div>
            </div>
        </div>
    );
};

const UploadStep = ({ onSelectFile, fileName, isLoading, loadingMessage, workingDir, onSelectDir, onOpenDir, onSelectMetaFile, onOpenCrossSpecies, onOpenKeggMapper, onLoadExisting }) => {
    return (
        <div className="text-center w-full max-w-5xl mx-auto my-auto h-full flex flex-col justify-center">
            <h2 className="text-3xl font-bold text-gray-800 mb-3">Step 1: Choose Your Analysis Module</h2>
            <p className="text-gray-600 mb-8 max-w-2xl mx-auto">Select the appropriate pipeline for your data type. Outputs will be saved to your working directory.</p>
            
            <div className="mb-8 flex justify-center items-center gap-3 bg-white p-3 rounded-xl border shadow-sm mx-auto max-w-2xl">
                <span className="text-sm font-semibold text-gray-700 whitespace-nowrap"><FolderOpen className="inline mr-2 text-gray-400" size={18}/>Output Directory:</span>
                <div className="flex items-center gap-2 bg-gray-50 px-3 py-2 rounded-lg border border-gray-200 w-full relative">
                    <span className="text-sm font-mono text-gray-700 truncate max-w-[350px]" title={workingDir || "Default (AppData/Temp)"}>
                        {workingDir || "Default (System Temp)"}
                    </span>
                    <div className="ml-auto flex items-center gap-2">
                        <button onClick={onSelectDir} className="text-indigo-600 hover:bg-indigo-50 p-1.5 rounded transition-colors" title="Change Directory">
                            <Settings size={18} />
                        </button>
                        <div className="h-5 w-px bg-gray-300"></div>
                        <button onClick={onOpenDir} className="text-teal-600 hover:bg-teal-50 p-1.5 rounded transition-colors" title="Open Folder in Explorer">
                            <ExternalLink size={18} />
                        </button>
                    </div>
                </div>
            </div>

            <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-5">
                 <div className="bg-white p-5 rounded-xl shadow-sm border hover:border-indigo-500 hover:shadow-md transition-all flex flex-col group cursor-pointer" onClick={() => onSelectFile('geo')}>
                    <div className="flex-grow">
                        <div className="bg-indigo-100 w-14 h-14 rounded-full flex items-center justify-center mx-auto mb-4 group-hover:bg-indigo-200 transition-colors">
                            <Database className="text-indigo-600" size={28}/>
                        </div>
                        <h3 className="font-bold text-gray-800">GEO Meta-Analysis</h3>
                        <p className="text-xs text-gray-600 mt-2 mb-4 leading-relaxed">Fetch multiple GSE datasets, normalize, and merge them into a unified pipeline.</p>
                    </div>
                    <button disabled={isLoading} className="w-full flex justify-center items-center bg-indigo-600 text-white py-2 px-4 rounded-lg text-sm font-medium hover:bg-indigo-700 disabled:bg-gray-400 transition-colors">
                        {isLoading ? <Loader2 className="animate-spin" size={18}/> : "Start Integration"}
                    </button>
                </div>

                <div className="bg-white p-5 rounded-xl shadow-sm border hover:border-blue-500 hover:shadow-md transition-all flex flex-col group cursor-pointer" onClick={() => onSelectFile('standard')}>
                     <div className="flex-grow">
                        <div className="bg-blue-100 w-14 h-14 rounded-full flex items-center justify-center mx-auto mb-4 group-hover:bg-blue-200 transition-colors">
                            <FileUp className="text-blue-600" size={28}/>
                        </div>
                        <h3 className="font-bold text-gray-800">Standard Pipeline</h3>
                        <p className="text-xs text-gray-600 mt-2 mb-4 leading-relaxed">For raw quantification files (LC-MS, etc.). Normalization, Correction, QC, and DGE.</p>
                    </div>
                    <button disabled={isLoading} className="w-full flex justify-center items-center bg-blue-600 text-white py-2 px-4 rounded-lg text-sm font-medium hover:bg-blue-700 disabled:bg-gray-400 transition-colors">
                        {isLoading ? <Loader2 className="animate-spin" size={18}/> : "Select Raw File"}
                    </button>
                </div>

                <div className="bg-white p-5 rounded-xl shadow-sm border hover:border-teal-500 hover:shadow-md transition-all flex flex-col group cursor-pointer" onClick={onSelectMetaFile}>
                    <div className="flex-grow">
                        <div className="bg-teal-100 w-14 h-14 rounded-full flex items-center justify-center mx-auto mb-4 group-hover:bg-teal-200 transition-colors">
                            <Beaker className="text-teal-600" size={28}/>
                        </div>
                        <h3 className="font-bold text-gray-800">Post-Hoc Analysis</h3>
                        <p className="text-xs text-gray-600 mt-2 mb-4 leading-relaxed">For pre-computed results containing logFC/P-values. Build advanced plots & networks.</p>
                    </div>
                    <button disabled={isLoading} className="w-full flex justify-center items-center bg-teal-600 text-white py-2 px-4 rounded-lg text-sm font-medium hover:bg-teal-700 disabled:bg-gray-400 transition-colors">
                         {isLoading ? <Loader2 className="animate-spin" size={18}/> : "Select Results"}
                    </button>
                </div>

                <div className="bg-white p-5 rounded-xl shadow-sm border hover:border-emerald-500 hover:shadow-md transition-all flex flex-col group cursor-pointer" onClick={onOpenCrossSpecies}>
                    <div className="flex-grow">
                        <div className="bg-emerald-100 w-14 h-14 rounded-full flex items-center justify-center mx-auto mb-4 group-hover:bg-emerald-200 transition-colors">
                            <Dna className="text-emerald-600" size={28}/>
                        </div>
                        <h3 className="font-bold text-gray-800">Evo-Devo Module</h3>
                        <p className="text-xs text-gray-600 mt-2 mb-4 leading-relaxed">Cross-species Orthology Mapping (e.g., Axolotl vs Human) for Evolutionary switches.</p>
                    </div>
                    <button disabled={isLoading} className="w-full flex justify-center items-center bg-emerald-600 text-white py-2 px-4 rounded-lg text-sm font-medium hover:bg-emerald-700 disabled:bg-gray-400 transition-colors">
                         {isLoading ? <Loader2 className="animate-spin" size={18}/> : "Compare Species"}
                    </button>
                </div>

                <div className="bg-white p-5 rounded-xl shadow-sm border hover:border-orange-500 hover:shadow-md transition-all flex flex-col group cursor-pointer" onClick={onOpenKeggMapper}>
                    <div className="flex-grow">
                        <div className="bg-orange-100 w-14 h-14 rounded-full flex items-center justify-center mx-auto mb-4 group-hover:bg-orange-200 transition-colors">
                            <Map className="text-orange-600" size={28}/>
                        </div>
                        <h3 className="font-bold text-gray-800">KEGG Mapper</h3>
                        <p className="text-xs text-gray-600 mt-2 mb-4 leading-relaxed">Prepare DGE results for direct upload to KEGG pathway mapping tools.</p>
                    </div>
                    <button disabled={isLoading} className="w-full flex justify-center items-center bg-orange-500 text-white py-2 px-4 rounded-lg text-sm font-medium hover:bg-orange-600 disabled:bg-gray-400 transition-colors">
                         {isLoading ? <Loader2 className="animate-spin" size={18}/> : "Open Utility"}
                    </button>
                </div>
                
                <div className="bg-white p-5 rounded-xl shadow-sm border hover:border-pink-500 hover:shadow-md transition-all flex flex-col group cursor-pointer" onClick={onLoadExisting}>
                    <div className="flex-grow">
                        <div className="bg-pink-100 w-14 h-14 rounded-full flex items-center justify-center mx-auto mb-4 group-hover:bg-pink-200 transition-colors">
                            <History className="text-pink-600" size={28}/>
                        </div>
                        <h3 className="font-bold text-gray-800">Load Existing</h3>
                        <p className="text-xs text-gray-600 mt-2 mb-4 leading-relaxed">Skip the setup and directly load a previously completed Analysis_Results folder.</p>
                    </div>
                    <button disabled={isLoading} className="w-full flex justify-center items-center bg-pink-600 text-white py-2 px-4 rounded-lg text-sm font-medium hover:bg-pink-700 disabled:bg-gray-400 transition-colors">
                         {isLoading ? <Loader2 className="animate-spin" size={18}/> : "Select Folder"}
                    </button>
                </div>

            </div>
             {fileName && <div className="mt-8 inline-block bg-blue-50 border border-blue-200 px-4 py-2 rounded-lg text-sm text-blue-800 font-medium shadow-sm"><FileUp className="inline mr-2 h-4 w-4"/>Selected: {fileName}</div>}
        </div>
    );
};

const PostAnalysisRunner = ({ basePath, annotationKeywords }) => { 
    const [scripts, setScripts] = useState([]); const [selectedScript, setSelectedScript] = useState('');
    useEffect(() => { window.electronAPI.getPostAnalysisScripts().then(setScripts); }, []);
    const handleRunScript = () => { 
        if (!selectedScript) { alert("Please select a post-analysis script to run."); return; } 
        if (annotationKeywords && window.electronAPI.updateAnalysisConfig) { window.electronAPI.updateAnalysisConfig({ resultsDir: basePath, annotationKeywords: annotationKeywords }); }
        
        if (!window.electronAPI || !window.electronAPI.runPostAnalysisScript) {
             alert("Connection Error: 'runPostAnalysisScript' API not found. Please restart the application.");
             return;
        }
        window.electronAPI.runPostAnalysisScript({ scriptName: selectedScript, basePath: basePath }); 
    };
    if (scripts.length === 0) return null;
    return ( <div className="mt-6 border-t pt-6"><h3 className="text-xl font-bold text-gray-800">Run Modular Post-Analysis</h3><p className="mt-2 text-gray-600">Select a custom R script from your `r_scripts` folder.</p><div className="mt-4 flex items-center justify-center gap-4"><select value={selectedScript} onChange={e => setSelectedScript(e.target.value)} className="max-w-xs border border-gray-300 text-sm rounded-lg p-2"><option value="">-- Select a script --</option>{scripts.map(s => <option key={s} value={s}>{s}</option>)}</select><button onClick={handleRunScript} className="inline-flex items-center px-4 py-2 bg-cyan-600 text-white rounded-md hover:bg-cyan-700"><FileCode className="mr-2 h-5 w-5" />Run Script</button></div></div> );
};

const VennAnalysisRunner = ({ vennSetsPath, basePath }) => {
    const [vennSets, setVennSets] = useState([]); const [selectedSetNames, setSelectedSetNames] = useState([]); const [runPpi, setRunPpi] = useState(true);
    useEffect(() => { if (vennSetsPath) { window.electronAPI.readJsonFile(vennSetsPath).then(data => { if (data) { setVennSets(data); } }); } }, [vennSetsPath]);
    const handleRunVennAnalysis = () => { 
        if (!window.electronAPI || !window.electronAPI.runVennAnalysis) {
             alert("Connection Error: 'runVennAnalysis' API not found. Please restart the application.");
             return;
        }
        if (selectedSetNames.length === 0) { alert("Please select at least one gene set."); return; } 
        const selections = vennSets.filter(s => selectedSetNames.includes(s.name)); 
        window.electronAPI.runVennAnalysis({ selections, runPpi, basePath }); 
    };
    if (!vennSetsPath || vennSets.length === 0) { return ( <div className="mt-6 border-t pt-6"><h3 className="text-xl font-bold text-gray-800">Post-Hoc Venn Analysis</h3><p className="mt-2 text-gray-600">No overlapping gene sets found.</p></div> ); }
    return ( <div className="mt-6 border-t pt-6"><h3 className="text-xl font-bold text-gray-800">Post-Hoc Venn Analysis</h3><p className="mt-2 text-gray-600">Select gene sets (Ctrl+Click) to analyze.</p><div className="mt-4 flex flex-col items-center gap-4"><select multiple value={selectedSetNames} onChange={e => setSelectedSetNames(Array.from(e.target.selectedOptions, option => option.value))} className="w-full max-w-xl border border-gray-300 text-sm rounded-lg p-2 h-48">{vennSets.map((s, index) => <option key={`${s.name}-${index}`} value={s.name}>{s.name}</option>)}</select><div className="flex items-center gap-4"><div className="flex items-center"><input type="checkbox" id="runPpi" checked={runPpi} onChange={e => setRunPpi(e.target.checked)} className="h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"/><label htmlFor="runPpi" className="ml-2 block text-sm font-medium text-gray-700">Run PPI Analysis</label></div><button onClick={handleRunVennAnalysis} className="inline-flex items-center px-4 py-2 bg-indigo-600 text-white rounded-md hover:bg-indigo-700"><FlaskConical className="mr-2 h-5 w-5" />Run Analysis</button></div></div></div> );
};

const AnalysisSetupStep = ({ headers, config, setConfig, idColumn, setIdColumn, sampleConfig, setSampleConfig, metadataColumns, setMetadataColumns, onStart, isLoading, selectedHeaders, setSelectedHeaders, lastClickedIndex, setLastClickedIndex, contrasts, setContrasts, vennContrasts, setVennContrasts, signatureConfig, setSignatureConfig, analysisSets, setAnalysisSets, annotationKeywords, setAnnotationKeywords, analysisOptions, setAnalysisOptions, onBack, filePath, onSkipToResults, onSaveConfig, ppiParams, setPpiParams, geoMetadataMap }) => {
    const [groupRenameInput, setGroupRenameInput] = useState('');
    const [batchAssignInput, setBatchAssignInput] = useState('');
    const [replicateAssignInput, setReplicateAssignInput] = useState('');
    const [activeTab, setActiveTab] = useState('Columns'); 
    const [hasExisting, setHasExisting] = useState(false);
    const [pCutoff, setPCutoff] = useState(0.05);
    const [fcCutoff, setFcCutoff] = useState(1.0);
    
    const [hrConfig, setHrConfig] = useState({ hrFile: null, hrThreshold: 1.5 });

    useEffect(() => {
        if (filePath && window.electronAPI.checkAnalysisExists) {
            window.electronAPI.checkAnalysisExists(filePath).then(exists => { setHasExisting(exists); });
        }
    }, [filePath]);

    useEffect(() => {
        if (!config || Object.keys(config).length === 0) return;
        
        const validKeys = new Set(Object.keys(config));
        let hasChanges = false;
        const newSelection = new Set(selectedHeaders);
        
        selectedHeaders.forEach(h => {
            if (!validKeys.has(h)) {
                newSelection.delete(h);
                hasChanges = true;
            }
        });
        
        if (hasChanges) {
            console.warn("Removed stale selections to prevent crash:", [...selectedHeaders].filter(h => !validKeys.has(h)));
            setSelectedHeaders(newSelection);
        }
    }, [config, headers]); 
    
    const handleRowClick = (header, e) => {
        const newSelection = new Set(selectedHeaders); const currentIndex = headers.indexOf(header);
        if (e.shiftKey && lastClickedIndex !== null) { const start = Math.min(lastClickedIndex, currentIndex); const end = Math.max(lastClickedIndex, currentIndex); for (let i = start; i <= end; i++) { newSelection.add(headers[i]); } } else { if (newSelection.has(header)) { newSelection.delete(header); } else { newSelection.add(header); } }
        setSelectedHeaders(newSelection); setLastClickedIndex(currentIndex);
    };
    
    const assignRole = (role) => {
        if (selectedHeaders.size === 0) return alert("Please select columns first.");
        
        if (role === 'ID') {
             if (selectedHeaders.size > 1) return alert('Only one column can be ID.');
             const newId = Array.from(selectedHeaders)[0];
             setIdColumn(newId); 
        }

        setConfig(currentConfig => {
            const newConfig = { ...currentConfig };
            if (role === 'ID') { 
                Object.keys(newConfig).forEach(h => { if (newConfig[h].role === 'ID') newConfig[h].role = 'Ignore'; }); 
            }
            selectedHeaders.forEach(header => { 
                if (!newConfig[header]) return; 
                if (newConfig[header].role === 'Sample' && role !== 'Sample') { newConfig[header].group = null; newConfig[header].batch = null; newConfig[header].replicateID = null; newConfig[header].runOrder = null; newConfig[header].finalName = header; } 
                newConfig[header].role = role; 
            });
            return newConfig;
        });
    };
    const handleAssignGroup = () => {
        const groupName = groupRenameInput.trim(); if (!groupName) { alert("Please enter a group name."); return; }
        setConfig(currentConfig => {
            const newConfig = { ...currentConfig }; const headersToAssign = [...selectedHeaders].filter(h => newConfig[h] && newConfig[h].role === 'Sample');
            if (headersToAssign.length === 0) { alert("Select 'Sample' columns first."); return newConfig; }
            const existingNumbersInGroup = Object.values(newConfig).filter(c => c.group === groupName).map(c => { const match = c.finalName.match(new RegExp(`^${groupName}(\\d+)$`)); return match ? parseInt(match[1], 10) : 0; });
            let counter = (existingNumbersInGroup.length > 0 ? Math.max(0, ...existingNumbersInGroup) : 0) + 1;
            headersToAssign.forEach(header => { if (newConfig[header].group !== groupName) { newConfig[header].group = groupName; newConfig[header].finalName = `${groupName}${counter++}`; } });
            return newConfig;
        });
        setGroupRenameInput('');
    };
    const handleAssignBatch = () => {
        const batchName = batchAssignInput; if (!batchName || batchName.trim() === '') { alert("Please enter a batch name."); return; }
        setConfig(currentConfig => {
            const newConfig = { ...currentConfig }; const selectedSampleHeaders = headers.filter(h => selectedHeaders.has(h) && newConfig[h] && newConfig[h].role === 'Sample');
            if(selectedSampleHeaders.length === 0) { alert("Select 'Sample' columns first."); return currentConfig; }
            selectedSampleHeaders.forEach(header => { newConfig[header].batch = batchName.trim(); });
            return newConfig;
        });
        setBatchAssignInput('');
    };
    const handleAssignReplicate = () => {
        const repName = replicateAssignInput.trim(); if (!repName) { alert("Please enter a replicate ID."); return; }
        setConfig(currentConfig => {
            const newConfig = { ...currentConfig }; const selectedSampleHeaders = headers.filter(h => selectedHeaders.has(h) && newConfig[h] && newConfig[h].role === 'Sample');
            if(selectedSampleHeaders.length === 0) { alert("Select 'Sample' columns first."); return currentConfig; }
            selectedSampleHeaders.forEach(header => { newConfig[header].replicateID = repName; });
            return newConfig;
        });
        setReplicateAssignInput('');
    };
    const handleAssignRunOrder = () => {
        setConfig(currentConfig => {
            const newConfig = { ...currentConfig };
             headers.forEach((h, index) => {
                 if (newConfig[h].role === 'Sample') newConfig[h].runOrder = index + 1;
             });
             alert("Run Order assigned based on column position in file (Left -> Right).");
             return newConfig;
        });
    };
    
    const handleSelectHR = async () => {
        if (window.electronAPI && window.electronAPI.openFile) {
            const path = await window.electronAPI.openFile();
            if (path) setHrConfig(prev => ({ ...prev, hrFile: path }));
        }
    };

    const unselectAll = () => { setSelectedHeaders(new Set()); setLastClickedIndex(null); };
    
    const definedGroups = [...new Set(Object.values(config).map(c => c.group).filter(g => g && typeof g === 'string'))];
    const isAnySampleSelected = Array.from(selectedHeaders).some(h => config[h]?.role === 'Sample');
    const derivedResultsPath = filePath ? `${filePath.substring(0, filePath.lastIndexOf(filePath.includes("\\") ? "\\" : "/"))}${filePath.includes("\\") ? "\\" : "/"}Analysis_Results_for_${filePath.split(/[\\/]/).pop().replace(/\.[^/.]+$/, "")}` : "";

    const handleStartWithHR = () => {
        onStart(hrConfig, pCutoff, fcCutoff, ppiParams);
    };

    return (
        <div className="w-full h-full flex flex-col overflow-hidden">
            <div className="flex justify-between items-center mb-2 border-b pb-2 flex-shrink-0">
                <div className="flex items-center">
                    <h2 className="text-2xl font-bold text-gray-800">Configuration</h2>
                </div>
            </div>

            <div className="flex flex-1 overflow-hidden gap-4">
                
                <div className="w-1/2 flex flex-col gap-3 overflow-y-auto pr-2 border-r">
                    
                    <div className="bg-white p-3 rounded-lg border shadow-sm flex-shrink-0">
                        <h3 className="font-bold text-sm text-gray-700 mb-2 uppercase tracking-wide">Assign Roles</h3>
                        <div className="flex flex-wrap gap-2">
                            <button onClick={() => assignRole('ID')} className="px-3 py-1 text-xs font-semibold uppercase bg-blue-600 text-white rounded shadow hover:bg-blue-700">ID</button>
                            <button onClick={() => assignRole('Metadata')} className="px-3 py-1 text-xs font-semibold uppercase bg-purple-600 text-white rounded shadow hover:bg-purple-700">Metadata</button>
                            <button onClick={() => assignRole('Sample')} className="px-3 py-1 text-xs font-semibold uppercase bg-green-600 text-white rounded shadow hover:bg-green-700">Sample</button>
                            <button onClick={() => assignRole('Ignore')} className="px-3 py-1 text-xs font-semibold uppercase bg-gray-500 text-white rounded shadow hover:bg-gray-600">Ignore</button>
                            <div className="w-full h-px bg-gray-200 my-1"></div>
                            <button onClick={unselectAll} className="w-full px-3 py-1 text-xs bg-white text-gray-600 border border-gray-300 rounded hover:bg-gray-50">Unselect All</button>
                        </div>
                    </div>

                    <div className="bg-white border-b flex-shrink-0 grid grid-cols-4 gap-1">
                        <TabButton name="Columns" title="Config" icon={<LayoutList size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Parameters" title="Params" icon={<Settings size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Contrasts" title="Contrasts" icon={<Binary size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Annotate" title="Annotate" icon={<Tag size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Venn" title="Venn" icon={<GitMerge size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Signatures" title="Signat." icon={<Microscope size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Sets" title="Sets" icon={<Layers size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                        <TabButton name="Scripts" title="Scripts" icon={<FileCode size={14}/>} activeTab={activeTab} setActiveTab={setActiveTab} />
                    </div>

                    <div className="bg-gray-50 p-3 border rounded-b-lg flex-1 overflow-y-auto">
                        {activeTab === 'Columns' && (
                             <div className="space-y-4">
                                <h3 className="font-semibold text-sm mb-3 text-gray-700 uppercase tracking-wide">Selected Samples</h3>
                                <p className="text-xs text-gray-500 mb-2">Configure selected table rows:</p>
                                <div className="space-y-3">
                                    <div className="flex flex-col gap-1">
                                        <label className="text-xs font-medium text-gray-600">Group Name</label>
                                        <div className="flex gap-2">
                                            <input type="text" value={groupRenameInput} onChange={e => setGroupRenameInput(e.target.value)} placeholder="e.g. Control" className="flex-grow border rounded p-1.5 text-sm" disabled={!isAnySampleSelected} />
                                            <button onClick={handleAssignGroup} className="px-3 py-1 text-xs bg-orange-500 text-white rounded hover:bg-orange-600 disabled:bg-gray-300">Set</button>
                                        </div>
                                    </div>
                                    <div className="flex flex-col gap-1">
                                        <label className="text-xs font-medium text-gray-600">Batch</label>
                                        <div className="flex gap-2">
                                            <input type="text" value={batchAssignInput} onChange={e => setBatchAssignInput(e.target.value)} placeholder="e.g. B1" className="flex-grow border rounded p-1.5 text-sm" disabled={!isAnySampleSelected} />
                                            <button onClick={handleAssignGroup} className="px-3 py-1 text-xs bg-teal-500 text-white rounded hover:bg-teal-600 disabled:bg-gray-300">Set</button>
                                        </div>
                                    </div>
                                    <div className="flex flex-col gap-1">
                                        <label className="text-xs font-medium text-gray-600">Replicate ID</label>
                                        <div className="flex gap-2">
                                            <input type="text" value={replicateAssignInput} onChange={e => setReplicateAssignInput(e.target.value)} placeholder="Optional" className="flex-grow border rounded p-1.5 text-sm" disabled={!isAnySampleSelected} />
                                            <button onClick={handleAssignGroup} className="px-3 py-1 text-xs bg-indigo-500 text-white rounded hover:bg-indigo-600 disabled:bg-gray-300">Set</button>
                                        </div>
                                    </div>
                                    <button onClick={handleAssignRunOrder} className="w-full py-2 text-xs bg-gray-600 text-white rounded hover:bg-gray-700 shadow-sm mt-2">Auto-Assign Run Order</button>
                                </div>
                            </div>
                        )}

                        {activeTab === 'Parameters' && (
                            <div className="space-y-4">
                                <div className="bg-blue-50 p-3 rounded-lg border border-blue-200">
                                    <label className="block text-xs font-semibold text-blue-800 mb-1">Primary ID Column</label>
                                    <select 
                                        value={idColumn} 
                                        onChange={(e) => {
                                            const newId = e.target.value;
                                            setIdColumn(newId);
                                            setConfig(prev => {
                                                const next = { ...prev };
                                                Object.keys(next).forEach(k => { if(next[k].role === 'ID') next[k].role = 'Ignore'; });
                                                if(next[newId]) next[newId].role = 'ID';
                                                return next;
                                            });
                                        }} 
                                        className="w-full p-1.5 rounded border border-blue-300 bg-white text-sm"
                                    >
                                        {headers.map(c => <option key={c} value={c}>{c}</option>)}
                                    </select>
                                </div>
                                <div className="bg-red-50 p-3 rounded-md border border-red-200">
                                    <label className="text-sm font-bold text-gray-700 mb-2 block">Prognostic Integration (Optional)</label>
                                    <div className="flex gap-2 items-center">
                                        <div className="flex-grow">
                                            <label className="text-xs text-gray-500">Survival Data File (.csv/.xlsx)</label>
                                            <div className="flex items-center mt-1">
                                                <span className="text-xs text-gray-600 bg-white border p-1.5 rounded-l w-full truncate block" title={hrConfig.hrFile || "No file selected"}>
                                                    {hrConfig.hrFile ? hrConfig.hrFile.split(/[\\/]/).pop() : "No file selected"}
                                                </span>
                                                <button onClick={handleSelectHR} className="bg-red-500 text-white px-3 py-1.5 rounded-r text-xs font-bold hover:bg-red-600">Select</button>
                                            </div>
                                        </div>
                                        <div className="w-24">
                                            <label className="text-xs text-gray-500">HR Threshold</label>
                                            <input 
                                                type="number" step="0.1" value={hrConfig.hrThreshold} 
                                                onChange={e => setHrConfig({...hrConfig, hrThreshold: e.target.value})} 
                                                className="w-full border p-1.5 rounded text-sm mt-1" 
                                            />
                                        </div>
                                    </div>
                                    <p className="text-xs text-gray-500 mt-1">High-Risk nodes will appear as diamonds in PPI network.</p>
                                </div>
                                <CorrectionPipeline options={analysisOptions} setOptions={setAnalysisOptions} />
                                <AnalysisOptionsBuilder 
                                    options={analysisOptions} 
                                    setOptions={setAnalysisOptions} 
                                    pCutoff={pCutoff} 
                                    setPCutoff={setPCutoff} 
                                    fcCutoff={fcCutoff} 
                                    setFcCutoff={setFcCutoff} 
                                    ppiParams={ppiParams}
                                    setPpiParams={setPpiParams}
                                />
                            </div>
                        )}

                        {activeTab === 'Contrasts' && <ContrastBuilder groups={definedGroups} sampleConfig={sampleConfig} contrasts={contrasts} setContrasts={setContrasts} />}
                        {activeTab === 'Annotate' && <KeywordBuilder keywords={annotationKeywords} setKeywords={setAnnotationKeywords} />}
                        {activeTab === 'Venn' && <VennBuilder definedContrasts={contrasts} definedGroups={definedGroups} vennContrasts={vennContrasts} setVennContrasts={setVennContrasts} />}
                        {activeTab === 'Signatures' && <SignatureBuilder groups={definedGroups} signatureConfig={signatureConfig} setSignatureConfig={setSignatureConfig} />}
                        {activeTab === 'Sets' && <AnalysisSetBuilder definedGroups={definedGroups} analysisSets={analysisSets} setAnalysisSets={setAnalysisSets} />}
                        {activeTab === 'Scripts' && <div><PostAnalysisRunner basePath={derivedResultsPath} annotationKeywords={annotationKeywords} /></div>}
                    </div>

                </div>

                <div className="flex-1 flex flex-col border rounded-lg shadow-inner bg-white overflow-hidden w-1/2">
                     <div className="flex-1 overflow-auto">
                        <table className="min-w-full text-sm text-left border-collapse">
                            <thead className="text-xs text-gray-700 uppercase bg-gray-100 sticky top-0 z-10 shadow-sm">
                                <tr>
                                    <th className="px-4 py-2 border-b whitespace-nowrap">Original Name</th>
                                    {geoMetadataMap && <th className="px-4 py-2 border-b whitespace-nowrap text-blue-800">GEO Description</th>}
                                    <th className="px-4 py-2 border-b whitespace-nowrap">Role</th>
                                    <th className="px-4 py-2 border-b whitespace-nowrap">Group</th>
                                    <th className="px-4 py-2 border-b whitespace-nowrap">Batch</th>
                                    <th className="px-4 py-2 border-b whitespace-nowrap">Rep ID</th>
                                    <th className="px-4 py-2 border-b whitespace-nowrap">Final Name</th>
                                </tr>
                            </thead>
                            <tbody>
                                {headers.map((h, i) => { 
                                    const isSelected = selectedHeaders.has(h); 
                                    const style = { backgroundColor: isSelected ? '#BFDBFE' : 'transparent', color: isSelected ? '#1F2937' : 'inherit' }; 
                                    return ( 
                                        <tr key={i} className="border-b cursor-pointer hover:bg-gray-50 transition-colors" style={style} onClick={(e) => handleRowClick(h, e)}>
                                            <td className="px-4 py-2 font-medium border-r whitespace-nowrap">{h}</td>
                                            {geoMetadataMap && (
                                                <td className="px-4 py-2 border-r whitespace-nowrap text-gray-600 text-xs italic truncate max-w-[200px]" title={geoMetadataMap[h] || ''}>
                                                    {geoMetadataMap[h] || ''}
                                                </td>
                                            )}
                                            <td className="px-4 py-2 border-r whitespace-nowrap">{safeRender(config[h]?.role)}</td>
                                            <td className="px-4 py-2 border-r whitespace-nowrap">{safeRender(config[h]?.group)}</td>
                                            <td className="px-4 py-2 border-r whitespace-nowrap">{safeRender(config[h]?.batch)}</td>
                                            <td className="px-4 py-2 border-r whitespace-nowrap">{safeRender(config[h]?.replicateID)}</td>
                                            <td className="px-4 py-2 whitespace-nowrap">{safeRender(config[h]?.finalName)}</td>
                                        </tr> 
                                    ); 
                                })}
                            </tbody>
                        </table>
                     </div>
                </div>

            </div>
            
            <div className="flex justify-center items-center pt-3 border-t bg-white flex-shrink-0 gap-4 pb-1">
                 <button onClick={onBack} className="inline-flex items-center px-4 py-2 text-sm font-medium rounded-md shadow-sm text-gray-700 bg-white hover:bg-gray-50 border">
                    <ChevronLeft className="mr-2 h-5 w-5"/> Back
                 </button>

                 <button onClick={handleStartWithHR} disabled={isLoading} className="inline-flex items-center justify-center px-8 py-2 text-base font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 disabled:bg-gray-400">
                    {isLoading ? <Loader2 className="animate-spin mr-2"/> : <Play className="mr-2"/>} Run Pipeline
                 </button>

                 {hasExisting && <button onClick={onSkipToResults} className="inline-flex items-center px-4 py-2 text-sm font-medium rounded-md shadow-sm text-white bg-blue-600 hover:bg-blue-700">Go to Results <ArrowRight className="ml-2 h-5 w-5" /></button>}
            </div>
        </div>
    );
};

const MetaAnalysisSetupStep = ({ headers, onStart, isLoading, metaContrast, setMetaContrast, vennContrasts, setVennContrasts, signatureConfig, setSignatureConfig, analysisOptions, setAnalysisOptions, onBack, onSaveConfig, ppiParams, setPpiParams }) => {
    const isVennDisabled = true;
    const isSignatureDisabled = true;
    
    const handleSelectHR = async () => {
        if (window.electronAPI && window.electronAPI.openFile) {
            const path = await window.electronAPI.openFile();
            if (path) setMetaContrast(prev => ({ ...prev, hrFile: path }));
        }
    };
    
    const handleStartMeta = () => {
        onStart(null, null, null, ppiParams);
    };

    return (
        <div className="w-full max-w-4xl mx-auto h-full flex flex-col">
            <h2 className="text-2xl font-semibold text-gray-700 mb-2 text-center flex-shrink-0">Step 2: Configure Pre-computed Analysis</h2>
            <div className="flex flex-col lg:flex-row gap-6 flex-grow overflow-hidden">
                <div className="flex-grow overflow-auto p-2">
                    <h3 className="font-semibold text-lg mb-2">1. Define Contrast from File</h3>
                    <div className="p-4 bg-gray-100 rounded-md space-y-3 border">
                        <input type="text" placeholder="Contrast Name (e.g., TNBC-vs-Normal)" value={metaContrast.name} onChange={e => setMetaContrast({...metaContrast, name: e.target.value})} className="w-full border p-2 rounded-md text-sm" />
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                            <div>
                                <label className="text-sm font-medium text-gray-700">Gene Symbol Column</label>
                                <select value={metaContrast.geneCol} onChange={e => setMetaContrast({...metaContrast, geneCol: e.target.value})} className="w-full mt-1 border p-2 rounded-md text-sm"><option value="">-- Select Column --</option>{headers.map(h => <option key={`g-${h}`} value={h}>{h}</option>)}</select>
                            </div>
                            <div>
                                <label className="text-sm font-medium text-gray-700">logFC Column</label>
                                <select value={metaContrast.logFCCol} onChange={e => setMetaContrast({...metaContrast, logFCCol: e.target.value})} className="w-full mt-1 border p-2 rounded-md text-sm"><option value="">-- Select Column --</option>{headers.map(h => <option key={`l-${h}`} value={h}>{h}</option>)}</select>
                            </div>
                            <div>
                                <label className="text-sm font-medium text-gray-700">Adj. P-Value Column</label>
                                <select value={metaContrast.pValCol} onChange={e => setMetaContrast({...metaContrast, pValCol: e.target.value})} className="w-full mt-1 border p-2 rounded-md text-sm"><option value="">-- Select Column --</option>{headers.map(h => <option key={`p-${h}`} value={h}>{h}</option>)}</select>
                            </div>
                            
                            <div className="col-span-1 md:col-span-2 bg-yellow-50 p-3 rounded-md border border-yellow-200">
                                <label className="text-sm font-bold text-gray-700 mb-2 block">Advanced Filtering (Optional)</label>
                                <div className="grid grid-cols-3 gap-2">
                                    <div className="col-span-1">
                                        <label className="text-xs text-gray-500">Column</label>
                                        <select value={metaContrast.countCol || ''} onChange={e => setMetaContrast({...metaContrast, countCol: e.target.value})} className="w-full border p-1.5 rounded text-sm"><option value="">-- None --</option>{headers.map(h => <option key={`c-${h}`} value={h}>{h}</option>)}</select>
                                    </div>
                                    <div className="col-span-1">
                                        <label className="text-xs text-gray-500">Operator</label>
                                        <select value={metaContrast.filterOp || '>'} onChange={e => setMetaContrast({...metaContrast, filterOp: e.target.value})} className="w-full border p-1.5 rounded text-sm" disabled={!metaContrast.countCol}>
                                            <option value=">">Greater than (&gt;)</option>
                                            <option value=">=">Greater or Equal (&ge;)</option>
                                            <option value="<">Less than (&lt;)</option>
                                            <option value="<=">Less or Equal (&le;)</option>
                                            <option value="==">Equal to (=)</option>
                                            <option value="!=">Not Equal to (!=)</option>
                                        </select>
                                    </div>
                                    <div className="col-span-1">
                                        <label className="text-xs text-gray-500">Value</label>
                                        <input type="number" value={metaContrast.filterVal} onChange={e => setMetaContrast({...metaContrast, filterVal: e.target.value})} className="w-full border p-1.5 rounded text-sm" disabled={!metaContrast.countCol} />
                                    </div>
                                </div>
                                <p className="text-xs text-gray-500 mt-1">Example: Select 'Count' &gt; '0' to keep rows where counts are positive.</p>
                            </div>
                            
                            <div className="col-span-1 md:col-span-2 bg-red-50 p-3 rounded-md border border-red-200">
                                <label className="text-sm font-bold text-gray-700 mb-2 block">Prognostic Integration (Optional)</label>
                                <div className="flex gap-2 items-center">
                                    <div className="flex-grow">
                                        <label className="text-xs text-gray-500">Survival Data File (.csv/.xlsx)</label>
                                        <div className="flex items-center mt-1">
                                            <span className="text-xs text-gray-600 bg-white border p-1.5 rounded-l w-full truncate block" title={metaContrast.hrFile || "No file selected"}>
                                                {metaContrast.hrFile ? metaContrast.hrFile.split(/[\\/]/).pop() : "No file selected"}
                                            </span>
                                            <button onClick={handleSelectHR} className="bg-red-500 text-white px-3 py-1.5 rounded-r text-xs font-bold hover:bg-red-600">Select</button>
                                        </div>
                                    </div>
                                    <div className="w-24">
                                        <label className="text-xs text-gray-500">HR Threshold</label>
                                        <input 
                                            type="number" 
                                            step="0.1"
                                            value={metaContrast.hrThreshold || 1.5} 
                                            onChange={e => setMetaContrast({...metaContrast, hrThreshold: e.target.value})} 
                                            className="w-full border p-1.5 rounded text-sm mt-1" 
                                        />
                                    </div>
                                </div>
                                <p className="text-xs text-gray-500 mt-1">High-Risk nodes (HR &gt; Threshold) will appear as diamonds in PPI network.</p>
                            </div>
                        </div>
                    </div>
                </div>
                <div className="w-full lg:w-96 flex-shrink-0 space-y-4 overflow-auto p-2">
                     <div className={isVennDisabled ? 'opacity-50' : ''}><VennBuilder definedContrasts={[]} definedGroups={[]} vennContrasts={vennContrasts} setVennContrasts={setVennContrasts} />{isVennDisabled && <p className="text-xs text-gray-500 mt-1">Venn Diagrams require at least 2 contrasts.</p>}</div>
                     <div className={isSignatureDisabled ? 'opacity-50' : ''}><SignatureBuilder groups={[]} signatureConfig={signatureConfig} setSignatureConfig={setSignatureConfig} />{isSignatureDisabled && <p className="text-xs text-gray-500 mt-1">Signature filtering requires at least 3 groups.</p>}</div>
                    
                    <AnalysisOptionsBuilder 
                        options={analysisOptions} 
                        setOptions={setAnalysisOptions} 
                        pCutoff={0.05} setPCutoff={() => {}}
                        fcCutoff={1.0} setFcCutoff={() => {}}
                        ppiParams={ppiParams}
                        setPpiParams={setPpiParams}
                    />
                </div>
            </div>
            <div className="flex justify-between mt-4 pt-4 border-t flex-shrink-0 items-center">
                <button onClick={onBack} className="inline-flex items-center px-4 py-2 text-sm font-medium rounded-md shadow-sm text-gray-700 bg-white hover:bg-gray-50 border"><ChevronLeft className="mr-2 h-5 w-5" />Back</button>
                <div className="flex gap-3">
                    <button onClick={handleStartMeta} disabled={isLoading} className="inline-flex items-center justify-center px-6 py-3 text-base font-medium rounded-md shadow-sm text-white bg-green-600 hover:bg-green-700 disabled:bg-gray-400">{isLoading ? <>Running Analysis...</> : <>Run Analysis <Play className="ml-2 h-5 w-5" /></>}</button>
                </div>
            </div>
        </div>
    );
};

const ResultsStep = ({ results, onReset, onBackToSetup }) => {
    return ( 
        <div className="text-center w-full">
            <h2 className="text-2xl font-bold text-green-600">Analysis Complete!</h2>
            <p className="mt-2 text-gray-600">Results saved to:</p>
            <p className="mt-4 p-2 bg-gray-100 rounded font-mono text-sm inline-block">{results.resultsPath}</p>
            <div className="mt-6 flex justify-center gap-4">
                <button onClick={onBackToSetup} className="px-4 py-2 bg-gray-200 text-gray-800 rounded-md hover:bg-gray-300">Back to Setup</button>
                <button onClick={onReset} className="px-4 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600">Start New Analysis</button>
            </div>
            <VennAnalysisRunner vennSetsPath={results.vennSetsPath} basePath={results.resultsPath} />
            <PostAnalysisRunner basePath={results.resultsPath} />
        </div> 
    );
};

const App = () => {
    const [step, setStep] = useState(1);
    const [mode, setMode] = useState('raw'); // 'raw' or 'meta'
    const [filePath, setFilePath] = useState(null);
    const [fileName, setFileName] = useState(null);
    const [isLoading, setIsLoading] = useState(false);
    const [loadingMessage, setLoadingMessage] = useState('');
    const [analysisResults, setAnalysisResults] = useState(null);
    
    const [geoModalOpen, setGeoModalOpen] = useState(false);
    const [crossSpeciesModalOpen, setCrossSpeciesModalOpen] = useState(false);
    const [keggMapperModalOpen, setKeggMapperModalOpen] = useState(false);
    
    const [workingDir, setWorkingDir] = useState('');

    const [headers, setHeaders] = useState([]);
    const [columnConfig, setColumnConfig] = useState({});
    const [selectedHeaders, setSelectedHeaders] = useState(new Set());
    const [lastClickedIndex, setLastClickedIndex] = useState(null);
    const [idColumn, idSetColumn] = useState('');
    const [sampleConfig, setSampleConfig] = useState([]);
    const [metadataColumns, setMetadataColumns] = useState([]);
    const [analysisSets, setAnalysisSets] = useState([]);
    const [annotationKeywords, setAnnotationKeywords] = useState([]);
    const [pCutoff, setPCutoff] = useState(0.05);
    const [fcCutoff, setFcCutoff] = useState(1.0);
    const [contrasts, setContrasts] = useState([]);
    const [metaContrast, setMetaContrast] = useState({ name: '', geneCol: '', logFCCol: '', pValCol: '', countCol: '', filterOp: '>', filterVal: '0', hrFile: null, hrThreshold: 1.5 });
    const [vennContrasts, setVennContrasts] = useState([]);
    const [signatureConfig, setSignatureConfig] = useState([]);
    
    // NEW: Captures the GEO Descriptions returned from the API for the Standard Pipeline
    const [geoMetadataMap, setGeoMetadataMap] = useState(null);

    const [analysisOptions, setAnalysisOptions] = useState({
        runPpi: true, runVenn: true, runSignature: true,
        runMedian: false, 
        runLoess: false, runCombat: false, runRuv: false, splitBatches: false,
        outlierMethod: 'none',
        runPairwiseQC: false,
        runDegHeatmaps: false
    });

    const [ppiParams, setPpiParams] = useState({
        cluster: 'louvain',
        hub: 'pagerank',
        score: 900
    });

    useEffect(() => {
        const meta = document.createElement('meta');
        meta.httpEquiv = "Content-Security-Policy";
        meta.content = "default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval' blob:; worker-src 'self' blob:; style-src 'self' 'unsafe-inline'; img-src 'self' data: blob:; connect-src 'self' https://maayanlab.cloud;";
        document.getElementsByTagName('head')[0].appendChild(meta);

        const removeError = window.electronAPI.onAnalysisError((msg) => {
            alert(`Error: ${msg}`);
            setIsLoading(false);
        });
        const removeComplete = window.electronAPI.onAnalysisComplete((res) => {
            setAnalysisResults(res);
            setIsLoading(false);
            setStep(3);
        });
        
        // UPGRADED geo-complete listener to respect manual grouping selections passed from the GeoModal
        const removeGeoComplete = window.electronAPI.onGeoComplete((res) => {
             setFilePath(res.filePath);
             setFileName(res.filePath.split(/[\\/]/).pop());
             setHeaders(res.headers); 
             setSelectedHeaders(new Set()); 
             setLastClickedIndex(null);     
             setGeoModalOpen(false);
             setIsLoading(false);
             
             // Extract optional data passed from backend
             const passedSampleMap = res.sampleMap || {};
             const passedMetaMap = res.metaMap || null;
             
             if (passedMetaMap) setGeoMetadataMap(passedMetaMap);

             const samples = res.headers.filter(h => h !== 'GeneSymbol' && h !== 'ID');
             const initialConfig = res.headers.reduce((acc, h) => {
                 acc[h] = { role: 'Ignore', finalName: h, group: null, batch: null, replicateID: null, runOrder: null };
                 return acc;
             }, {});
             
             const idCol = res.headers.includes('GeneSymbol') ? 'GeneSymbol' : res.headers[0];
             if(initialConfig[idCol]) initialConfig[idCol].role = 'ID';
             
             samples.forEach((s, i) => {
                 let assignedGroup = 'Group1';
                 // Map the explicitly chosen group if it exists
                 if (passedSampleMap[s]) assignedGroup = passedSampleMap[s];
                 
                 initialConfig[s] = { role: 'Sample', finalName: s, group: assignedGroup, batch: 'Batch1', replicateID: null, runOrder: i+1 };
             });
             
             setColumnConfig(initialConfig);
             setSampleConfig(samples.map((s, i) => {
                 let assignedGroup = 'Group1';
                 if (passedSampleMap[s]) assignedGroup = passedSampleMap[s];
                 return { originalName: s, finalName: s, group: assignedGroup, batch: 'Batch1', runOrder: i+1 };
             }));
             idSetColumn(idCol);
             setMode('raw');
             setStep(2);
        });
        const removeGeoSplitComplete = window.electronAPI.onGeoSplitComplete((res) => {
             alert(`Split Analysis Complete! Results saved to: ${res.resultsPath}`);
             setIsLoading(false);
             setGeoModalOpen(false);
        });

        const removeGeoIndependentComplete = window.electronAPI.onGeoIndependentComplete((res) => {
             alert(`GEO Independent Analysis complete!\nResults saved to: ${res.resultsPath}`);
             setIsLoading(false);
             setGeoModalOpen(false);
        });

        const removeCrossSpeciesComplete = window.electronAPI.onCrossSpeciesComplete((res) => {
            alert(`Cross-Species Analysis Complete! Results saved to: ${res.resultsPath}`);
            setIsLoading(false);
            setCrossSpeciesModalOpen(false);
        });

        const removeProgress = window.electronAPI.onAnalysisProgress((msg) => setLoadingMessage(msg));

        return () => {
            if (removeError) removeError(); 
            if (removeComplete) removeComplete(); 
            if (removeGeoComplete) removeGeoComplete(); 
            if (removeGeoSplitComplete) removeGeoSplitComplete(); 
            if (removeGeoIndependentComplete) removeGeoIndependentComplete(); 
            if (removeCrossSpeciesComplete) removeCrossSpeciesComplete(); 
            if (removeProgress) removeProgress();
        };
    }, []);

    useEffect(() => {
        const savedDir = localStorage.getItem('omics_working_dir');
        if (savedDir) {
            setWorkingDir(savedDir);
        } else if (window.electronAPI && window.electronAPI.getDefaultPath) {
            window.electronAPI.getDefaultPath().then(path => {
                if (path) setWorkingDir(path);
            });
        }
    }, []);
    
    const handleSelectWorkingDir = async () => {
        if (!window.electronAPI || !window.electronAPI.openDirectory) return;
        const dir = await window.electronAPI.openDirectory();
        if (dir) {
            setWorkingDir(dir);
            localStorage.setItem('omics_working_dir', dir);
        }
    };
    
    const handleOpenWorkingDir = () => {
        if (workingDir && window.electronAPI && window.electronAPI.openPath) {
            window.electronAPI.openPath(workingDir);
        } else {
            alert("Working directory not set or API unavailable.");
        }
    };

    const handleLoadExistingResults = async () => {
        if (!window.electronAPI || !window.electronAPI.openDirectory) {
            alert("Directory selection API not available.");
            return;
        }
        const dir = await window.electronAPI.openDirectory();
        if (dir) {
            let projectName = "Unknown";
            const folderName = dir.split(/[\\/]/).pop();
            if (folderName.startsWith("Analysis_Results_for_")) {
                projectName = folderName.replace("Analysis_Results_for_", "");
            }
            const sep = dir.includes("\\") ? "\\" : "/";
            const vennPath = `${dir}${sep}${projectName}_Global_Summary_Analysis${sep}venn_sets.json`;

            setAnalysisResults({ resultsPath: dir, vennSetsPath: vennPath });
            setStep(3);
        }
    };

    const handleSelectFile = async (selectedMode) => {
        if (selectedMode === 'geo') {
            setGeoModalOpen(true);
            return;
        }

        const path = await window.electronAPI.openFile();
        if (path) {
            setFilePath(path);
            setFileName(path.split(/[\\/]/).pop());
            const loadedHeaders = await window.electronAPI.getFileHeaders(path);
            setHeaders(loadedHeaders);
            setSelectedHeaders(new Set()); 
            setLastClickedIndex(null);    
            setGeoMetadataMap(null); // Reset metadata on standard load
            setMode(selectedMode);

            const session = await window.electronAPI.loadSession(path);
            if (session) {
                let loadedConfig = session.columnConfig || {};
                const completeConfig = {};
                loadedHeaders.forEach(h => {
                    if (loadedConfig[h]) {
                        completeConfig[h] = loadedConfig[h];
                    } else {
                        completeConfig[h] = { role: 'Ignore', finalName: h, group: null, batch: null, replicateID: null, runOrder: null };
                    }
                });
                setColumnConfig(completeConfig);

                const savedId = session.idColumn || Object.keys(completeConfig).find(k => completeConfig[k].role === 'ID');
                idSetColumn(savedId || loadedHeaders[0]);
                
                const loadedSamples = [];
                loadedHeaders.forEach((h, i) => {
                     const cfg = completeConfig[h];
                     if (cfg && cfg.role === 'Sample') {
                         loadedSamples.push({
                             originalName: h,
                             finalName: cfg.finalName,
                             group: cfg.group,
                             batch: cfg.batch,
                             runOrder: cfg.runOrder || (i + 1)
                         });
                     }
                });
                setSampleConfig(loadedSamples);
                setMetadataColumns(Object.keys(completeConfig).filter(k => completeConfig[k].role === 'Metadata'));
                
                if(session.analysisSets) setAnalysisSets(session.analysisSets);
                if(session.annotationKeywords) setAnnotationKeywords(session.annotationKeywords);
                if(session.analysisOptions) setAnalysisOptions(prev => ({ ...prev, ...session.analysisOptions }));
                if(session.ppiParams) setPpiParams(session.ppiParams);
                if (selectedMode === 'meta' && session.metaContrast) {
                     setMetaContrast(session.metaContrast);
                }

                if (session.contrasts) {
                    const migratedContrasts = session.contrasts.map(c => {
                        if (typeof c === 'string') return c;
                        return c;
                    });
                    setContrasts(migratedContrasts);
                }

                if (session.vennContrasts) {
                    const migratedVenn = session.vennContrasts.map(combo => {
                        return combo.map(c => {
                            if (typeof c === 'string') return `legacy_${c}`;
                            return c;
                        });
                    });
                    setVennContrasts(migratedVenn);
                }

                if(session.signatureConfig) setSignatureConfig(session.signatureConfig);

            } else {
                const initialConfig = loadedHeaders.reduce((acc, header) => {
                    acc[header] = { role: 'Ignore', finalName: header, group: null, batch: null, replicateID: null, runOrder: null };
                    return acc;
                }, {});
                
                const idCol = loadedHeaders[0];
                if(initialConfig[idCol]) initialConfig[idCol].role = 'ID';
                
                setColumnConfig(initialConfig);
                idSetColumn(idCol);
                
                setSampleConfig([]);
                setMetadataColumns([]);
                setAnalysisSets([]);
                setAnnotationKeywords([]);
                setContrasts([]);
                setVennContrasts([]);
                setSignatureConfig([]);
                setAnalysisOptions({
                    runPpi: true, runVenn: true, runSignature: true,
                    runMedian: false,
                    runLoess: false, runCombat: false, runRuv: false, splitBatches: false,
                    outlierMethod: 'none',
                    runPairwiseQC: false, runDegHeatmaps: false
                });
                setMetaContrast({ name: '', geneCol: '', logFCCol: '', pValCol: '', countCol: '', filterOp: '>', filterVal: '0', hrFile: null, hrThreshold: 1.5 });
            }
            
            setStep(2);
        }
    };

    const handleSelectMetaFile = async () => {
        const path = await window.electronAPI.openFile();
        if (path) {
            setFilePath(path);
            setFileName(path.split(/[\\/]/).pop());
            const loadedHeaders = await window.electronAPI.getFileHeaders(path);
            setHeaders(loadedHeaders);
            setSelectedHeaders(new Set()); 
            setLastClickedIndex(null); 
            
            idSetColumn(loadedHeaders[0]);
            setMode('meta');
            setStep(2); 
            
            const session = await window.electronAPI.loadSession(path);
            if (session && session.metaContrast) {
                 setMetaContrast(session.metaContrast);
                 if (session.analysisOptions) setAnalysisOptions(prev => ({ ...prev, ...session.analysisOptions }));
                 if(session.ppiParams) setPpiParams(session.ppiParams);
            } else {
                 setMetaContrast({ name: '', geneCol: '', logFCCol: '', pValCol: '', countCol: '', filterOp: '>', filterVal: '0', hrFile: null, hrThreshold: 1.5 });
            }
        }
    };

    const handleSaveConfig = () => {
        if (!filePath) return;
        window.electronAPI.saveSession({
            filePath,
            sessionData: {
                columnConfig, 
                idColumn, sampleConfig, metadataColumns, analysisSets,
                annotationKeywords, contrasts, vennContrasts, signatureConfig, analysisOptions,
                metaContrast,
                ppiParams 
            }
        });
    };
    
    useEffect(() => {
        if (filePath && step === 2) {
             const timer = setTimeout(() => {
                 window.electronAPI.saveSession({
                    filePath,
                    sessionData: {
                        columnConfig, idColumn, sampleConfig, metadataColumns, analysisSets,
                        annotationKeywords, contrasts, vennContrasts, signatureConfig, analysisOptions,
                        metaContrast,
                        ppiParams
                    }
                });
             }, 1000); 
             return () => clearTimeout(timer);
        }
    }, [columnConfig, idColumn, contrasts, metaContrast, vennContrasts, signatureConfig, analysisSets, annotationKeywords, analysisOptions, filePath, step, ppiParams]);


    const handleRunAnalysis = (hrConfigOverride = null, pCutoffOverride = null, fcCutoffOverride = null, ppiParamsOverride = null) => {
        setIsLoading(true);
        if (mode === 'meta') {
            window.electronAPI.runMetaAnalysis({
                filePath, 
                metaContrasts: [metaContrast], 
                vennContrasts, 
                signatureConfig, 
                analysisOptions,
                countCol: metaContrast.countCol,
                filterOp: metaContrast.filterOp,
                filterVal: metaContrast.filterVal,
                hrFile: metaContrast.hrFile, 
                hrThreshold: metaContrast.hrThreshold,
                ppiCluster: ppiParamsOverride?.cluster,
                ppiHub: ppiParamsOverride?.hub,
                ppiScore: ppiParamsOverride?.score
            });
        } else {
            const finalSampleConfig = Object.keys(columnConfig)
                .filter(key => columnConfig[key].role === 'Sample')
                .map(key => ({
                    originalName: key,
                    finalName: columnConfig[key].finalName,
                    group: columnConfig[key].group,
                    batch: columnConfig[key].batch,
                    replicateID: columnConfig[key].replicateID,
                    runOrder: columnConfig[key].runOrder
                }));

            const finalMetadataColumns = Object.keys(columnConfig).filter(key => columnConfig[key].role === 'Metadata');

            window.electronAPI.runFullAnalysis({
                filePath, idColumn, 
                sampleConfig: finalSampleConfig, 
                metadataColumns: finalMetadataColumns, 
                contrasts,
                vennContrasts, signatureConfig, analysisSets, annotationKeywords, analysisOptions,
                hrFile: hrConfigOverride?.hrFile,
                hrThreshold: hrConfigOverride?.hrThreshold,
                pCutoff: pCutoffOverride, 
                fcCutoff: fcCutoffOverride,
                ppiCluster: ppiParamsOverride?.cluster,
                ppiHub: ppiParamsOverride?.hub,
                ppiScore: ppiParamsOverride?.score
            });
        }
    };

    const handleStopAnalysis = () => {
        if(confirm("Are you sure you want to stop the current analysis?")) {
            window.electronAPI.haltAnalysis();
            setIsLoading(false);
            setLoadingMessage('Analysis Stopped.');
        }
    };

    const handleRunGeoPipeline = (input) => {
         setIsLoading(true);
         window.electronAPI.runGeoPipeline({ gseIds: input, targetDir: workingDir });
    };

    // Upgraded payloads to support the manual sample groupings provided by GeoModal
    const handleRunGeoSplit = (input, controlKeyword, caseKeyword, sampleMap = null) => {
         setIsLoading(true);
         window.electronAPI.runGeoSplitAnalysis({ 
             gseIds: input, 
             targetDir: workingDir,
             control: controlKeyword || 'Normal', 
             case: caseKeyword || 'Tumor',
             sampleMap: sampleMap
         });
    };

    const handleRunGeoIndependent = (input, sampleMap = null) => {
         setIsLoading(true);
         setLoadingMessage(`Starting Independent GEO Analysis for ${input}...`);
         window.electronAPI.runGeoIndependent({ 
             gseIds: input, 
             targetDir: workingDir,
             sampleMap: sampleMap
         });
    };

    const handleRunGeoManualFetch = (input, sampleMap = null) => {
         setIsLoading(true);
         setLoadingMessage(`Fetching dataset ${input} safely for manual configuration...`);
         window.electronAPI.runGeoManualFetch({ 
             gseIds: input, 
             targetDir: workingDir,
             sampleMap: sampleMap
         });
    };

    const handleRunCrossSpecies = (data) => {
        setIsLoading(true);
        window.electronAPI.runCrossSpecies({
            axolotl: data.axolotl,
            human: data.human,
            ortho: data.ortho,
            targetDir: workingDir
        });
    };
    
    const handleSkipToResults = () => {
       if (!filePath) return;
       const projectName = fileName.replace(/\.[^/.]+$/, "");
       const fileDir = filePath.substring(0, filePath.lastIndexOf(filePath.includes("\\") ? "\\" : "/"));
       const derivedPath = `${fileDir}${filePath.includes("\\") ? "\\" : "/"}Analysis_Results_for_${projectName}`;
       const vennPath = `${derivedPath}${filePath.includes("\\") ? "\\" : "/"}${projectName}_Global_Summary_Analysis${filePath.includes("\\") ? "\\" : "/"}venn_sets.json`;
       setAnalysisResults({ resultsPath: derivedPath, vennSetsPath: vennPath });
       setStep(3);
    };

    const resetApp = () => {
        setStep(1); setFilePath(null); setFileName(null); setAnalysisResults(null); setMode('raw'); setGeoMetadataMap(null);
        setSelectedHeaders(new Set()); setLastClickedIndex(null); 
        setAnalysisOptions({
            runPpi: true, runVenn: true, runSignature: true,
            runMedian: false,
            runLoess: false, runCombat: false, runRuv: false, splitBatches: false,
            outlierMethod: 'none',
            runPairwiseQC: false, runDegHeatmaps: false
        });
        setPpiParams({
            cluster: 'louvain',
            hub: 'pagerank',
            score: 900
        });
    };

    const renderStep = () => {
        switch (step) {
            case 2: 
                if (mode === 'standard') {
                    return <AnalysisSetupStep 
                        headers={headers} config={columnConfig} setConfig={setColumnConfig}
                        idColumn={idColumn} setIdColumn={idSetColumn}
                        sampleConfig={sampleConfig} setSampleConfig={setSampleConfig}
                        metadataColumns={metadataColumns} setMetadataColumns={setMetadataColumns}
                        analysisSets={analysisSets} setAnalysisSets={setAnalysisSets}
                        annotationKeywords={annotationKeywords} setAnnotationKeywords={setAnnotationKeywords}
                        contrasts={contrasts} setContrasts={setContrasts}
                        vennContrasts={vennContrasts} setVennContrasts={setVennContrasts}
                        signatureConfig={signatureConfig} setSignatureConfig={setSignatureConfig}
                        analysisOptions={analysisOptions} setAnalysisOptions={setAnalysisOptions}
                        onStart={handleRunAnalysis} onBack={resetApp} isLoading={isLoading}
                        onSaveConfig={handleSaveConfig} 
                        selectedHeaders={selectedHeaders} setSelectedHeaders={setSelectedHeaders}
                        lastClickedIndex={lastClickedIndex} setLastClickedIndex={setLastClickedIndex}
                        filePath={filePath} onSkipToResults={handleSkipToResults} 
                        ppiParams={ppiParams} setPpiParams={setPpiParams}
                        geoMetadataMap={geoMetadataMap}
                    />;
                } else {
                     return <MetaAnalysisSetupStep 
                        headers={headers} onStart={handleRunAnalysis} isLoading={isLoading}
                        metaContrast={metaContrast} setMetaContrast={setMetaContrast}
                        vennContrasts={vennContrasts} setVennContrasts={setVennContrasts}
                        signatureConfig={signatureConfig} setSignatureConfig={setSignatureConfig}
                        analysisOptions={analysisOptions} setAnalysisOptions={setAnalysisOptions} onBack={resetApp}
                        onSaveConfig={handleSaveConfig}
                        ppiParams={ppiParams} setPpiParams={setPpiParams}
                    />;
                }
            case 3: 
                return <ResultsStep results={analysisResults} onReset={resetApp} onBackToSetup={() => setStep(2)} />;
            default: 
                return <UploadStep 
                    onSelectFile={handleSelectFile} 
                    fileName={fileName} 
                    isLoading={isLoading} 
                    loadingMessage={loadingMessage} 
                    workingDir={workingDir}
                    onSelectDir={handleSelectWorkingDir}
                    onOpenDir={handleOpenWorkingDir}
                    onSelectMetaFile={handleSelectMetaFile}
                    onOpenCrossSpecies={() => setCrossSpeciesModalOpen(true)} 
                    onOpenKeggMapper={() => setKeggMapperModalOpen(true)}
                    onLoadExisting={handleLoadExistingResults}
                />;
        }
    };
    
    return (
        <div className="bg-gray-50 min-h-screen font-sans relative">
            <div className="container mx-auto px-4 py-8">
                <header className="text-center mb-8 border-b pb-4 relative">
                    <h1 className="text-4xl font-bold text-gray-800">Automated Omics Analysis Pipeline</h1>
                    <p className="text-lg text-gray-600 mt-2">From raw data or pre-computed results to biological insight.</p>
                    {isLoading && (
                        <button onClick={handleStopAnalysis} className="absolute right-0 top-2 bg-red-100 text-red-700 px-4 py-2 rounded-full border border-red-300 hover:bg-red-200 flex items-center shadow-lg animate-pulse">
                            <StopCircle size={20} className="mr-2" /> Stop Analysis
                        </button>
                    )}
                </header>
                <main className="bg-white rounded-xl shadow-lg p-6 md:p-8 h-[80vh] max-h-[900px] flex flex-col">
                    {isLoading ? (
                        <div className="flex flex-col items-center justify-center h-full">
                            <Loader2 size={64} className="text-blue-500 animate-spin mb-4" />
                            <h3 className="text-xl font-semibold text-gray-700">Processing...</h3>
                            <p className="text-gray-500">{loadingMessage || "Please wait while R scripts are running."}</p>
                        </div>
                    ) : renderStep()}
                </main>
            </div>
            <GeoModal 
                isOpen={geoModalOpen} 
                onClose={() => setGeoModalOpen(false)} 
                onRun={handleRunGeoPipeline} 
                onRunSplit={handleRunGeoSplit}
                onRunIndependent={handleRunGeoIndependent}
                onRunManualFetch={handleRunGeoManualFetch}
                isLoading={isLoading} 
            />
            <CrossSpeciesModal 
                isOpen={crossSpeciesModalOpen}
                onClose={() => setCrossSpeciesModalOpen(false)}
                onRun={handleRunCrossSpecies}
                isLoading={isLoading}
            />
            <KeggMapperModal 
                isOpen={keggMapperModalOpen}
                onClose={() => setKeggMapperModalOpen(false)}
                workingDir={workingDir}
            />
        </div>
    );
};

export default App;