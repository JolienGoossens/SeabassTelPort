# SeabassTelPort

This repository contains the code to investigate habitat use and connectivity of European seabass, *Dicentrarchus labrax* in the Port of Zeebrugge (Belgium) in the southern North Sea.



Full details on the approach and the analysis are outlined in:  (submitted for publication)

Data are available through DOI:

The following protocol executes the analysis outlined in the publication. Scripts can be found in the folder src.


## 1) Get data
Script in folder **src/data**. 

*Make_folders.R*: Create necessary folders


## 2) Prepare data for exploration, analysis and visualization
Scripts in folder **src/features**. Execute the preparation scripts in the following order:

*Format1_Organisation.R*: Format raw detection data and metadata
*Format2_Hourlypresence.R*: Organize detection data in hourly time bins 
*Format3_Epistemic_hour.R*: Estimate likely locations


## 3) Explore habitat use
Scripts for visual exploration in folder **src/visualization**.

*Fig1_Studyarea.R*: Visualize the study area
*Fig2_Abacus.R*: Create abacus plot
*Fig3_RI.R*: Visualize monthly residency index (RI)

Script to calculate the RI in folder **src/features**, under *RI_sitefidelity.R*.

## 4) Model

Scripts in folder **src/models**.

Different types of network analyses were used:
*Network_CA.R*: Correspondence analysis using individual RI at each receiver station
*Network_spatial.R*: Visual representation of network with calculation of eigenvector centrality (EVC)
*Network_transitprobability.R*: Compute transition probability matrix
*Network_Co-occurrence.R*: Calculate hourly co-occurrence between all potential dyads of fish

Generalized linear mixed models (GLMM) and linear mixed models (LMM) were used in:
*GLMM_daynight.R*: Evaluate habitat use at the closed area of Vandamme shipping lock

*GLMM_protection.R*: Model the exposure of individual fish to fisheries under different management scenarios

