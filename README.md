# Progressive Multiple Sclerosis Drug Prioritization Framework  

This repository contains code and datasets for prioritizing drug candidates in Progressive Multiple Sclerosis (PMS) research, including preclinical/clinical data processing pipelines and an interactive Shiny dashboard for evidence visualization.  


## Repository Structure  
```  
PMS-Drug-Prioritization/  
├── Preclinical/        # Preclinical study datasets and processing  
│   ├── OCTOPUS_preclinicalProducePublicationData.R  # Script to generate publication-ready preclinical data  
│   └── Preclinical.csv  # Curated preclinical dataset (140 studies)  
├── Clinical/           # Clinical study datasets and processing  
│   ├── clinicalreview.R  # Script for clinical data curation and quality checks  
│   ├── ClinicalPublicationList.csv  # List of included clinical publications  
│   └── ClinicalMSDrugSummary.csv  # Summary metrics (efficacy, safety) for clinical studies  
├── shiny_app/               # Interactive visualization tool  
│   ├── app.R                   # Main Shiny application code (combines UI and server logic)  
│   ├── server.R                # Backend logic: forest plots, radar charts, data filtering  
│   └── ui.R                    # User interface layout: input controls and output displays  
└── README.md                # Repository documentation  
```  


## Key Components  

### 1. Datasets  
- **Preclinical Data**:  
  - 140 studies on MS-relevant animal models (e.g., EAE, cuprizone-induced demyelination).  
  - Variables include: drug dose, administration route, treatment duration, behavioral outcomes (rotarod, grip strength), histological measures (myelin staining), and risk-of-bias scores (adapted CAMARADES checklist).  

- **Clinical Data**:  
  - 37 studies on human subjects with PMS (PPMS/SPMS).  
  - Variables include: study design, sample size, efficacy outcomes (EDSS progression, MRI lesion volume), safety profiles (adverse events), and quality scores (24-item checklist).  


### 2. Shiny Application  
An interactive dashboard to explore and compare drug evidence:  
- **Forest Plots**: Visualize effect sizes (standardized mean differences [SMD] for continuous outcomes; risk ratios [RR] for binary outcomes) with confidence intervals.  
- **Drug Profiles**: Compare candidates using radar charts across 4 domains: efficacy, safety, study quality, and feasibility.  
- **Filters**: Subset data by drug name, model type (preclinical), or outcome measure (e.g., "EDSS" for clinical studies).  

**To run the app**:  
```r  
setwd("shiny_app")  # Navigate to the shiny_app directory  
source("app.R")     # Launch the interactive dashboard  
```  


## Dependencies  
Required R packages are listed in `requirements.txt`. Install them using:  
```r  
install.packages(readLines("requirements.txt"))  
```  


## Limitations  
- **Data Timeliness**: Datasets are static snapshots from ReLiSyR/MS-SOLES (no real-time updates during the project), excluding post-project publications (e.g., PMID: 40371642).  
- **Preclinical Inconsistencies**: Some data may include pooled models (inflammatory + demyelinating) due to late-stage identification of this issue, which could not be resolved within the project timeline.  


