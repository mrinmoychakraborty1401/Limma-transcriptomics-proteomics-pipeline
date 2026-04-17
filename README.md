# Prognostic Biomarker Identification Pipeline
This repository contains the bioinformatic workflow utilised to identify and validate prognostic biomarkers in Triple-Negative Breast Cancer (TNBC). The pipeline integrates transcriptomic analysis with functional genomics to pinpoint key drivers of disease progression.

# Analytical Workflow
The pipeline is structured into the following functional modules:

Differential Gene Expression Analysis (DGEA): Identification of significantly dysregulated genes across multiple TNBC microarray datasets using the limma framework.

Protein–Protein Interaction (PPI) Network & Clustering: Construction of interactome networks to identify hub genes and functional modules within the proliferative core.

Functional Enrichment Analysis:

Over-Representation Analysis (ORA): Determining significantly enriched biological processes and pathways.

Gene Set Enrichment Analysis (GSEA): Rank-based analysis to identify coordinated gene expression changes across biological categories.

Cluster Enrichment: Targeted functional annotation of specific PPI clusters to determine the biological relevance of identified sub-networks.

# Repository Structure
/scripts: R scripts for DGEA, PPI construction, and enrichment (ORA/GSEA).

# Prerequisites
To execute the scripts in this repository, ensure the following R packages are installed:

install.packages(c("tidyverse", "BiocManager"))
BiocManager::install(c("limma", "clusterProfiler", "org.Hs.eg.db", "enrichplot", "igraph"))
Data Availability
All primary transcriptomic data were sourced from the NCBI Gene Expression Omnibus (GEO). Specific accession IDs and processed results are detailed within the /data folder.
