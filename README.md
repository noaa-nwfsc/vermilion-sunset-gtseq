# NWFSC Vermilion Sunset Rockfish Project

Primary Contact: Anita Wray, anita.wray@noaa.gov

## Objective:

The primary objective was to conduct a genomic analysis to distinguish the vermilion rockfish stock along the west coast from sunset rockfish using tissue samples previously collected during fishery-independent resource surveys. Specifically, we (1) used high-throughput sequencing technologies to identify single nucleotide polymorphisms (SNPs) which produced an assay that definitively separates the two species, (2) applied this SNP panel to over 27 thousand samples and finally (3) identified species-specific demographic and biological differences.

Species ID utilized Rubias, which was altered to allow for species specific calls. SNP panel was built by Gary Longo. ~36,000 fish were sequenced by GTSeek.


## Methods:

### 1. Genotyping
Run [Nate Campbell's GT-seq genotyper](https://github.com/GTseq/GTseq-Pipeline) to produce a .csv file with genotypes and summary statistics for all samples. I just ran GTseq_Genotyper_v3.pl and GTseq_GenoCompile_v3.pl.

### 2. Species Identification using Rubias
Run Rubias with a reference panel of known individuals to identify samples as either Vermilion or Sunset Rockfish. [Script here](https://github.com/anita-wray/vermilion_sunset_gtseq/blob/main/RUBIAS/VMRF_rubias_PCA.R)

This script needs 4 inputs:
1. a common loci RDS <- common_loci_path
2. a reference rockfish df for input to Rubias <- rf_reference_df_sans_SVH_path
3. a reference rockfish genind for PCA plots <- rf_GTseq_panel_genind_path
4. a excel file (not csv) with the 'unknown' samples <- rf_GTs_path
5. (optional) a non-reference rockfish genind to visualize flagged species with other rockfish species <- otro_rf_spp_sample_ID_path

### 3. Biological Data merging
Merged biological data collected in the field with genetic species ID. [Script here](https://github.com/noaa-nwfsc/vermilion-sunset-gtseq/blob/main/METADATA_FILTERING_AND_MERGING/Merge_Rubias_Structure_NewHybrids.R)

### 4. Statistics and Graphics
Code for all analyses and figures for the manuscript are included in [this folder](https://github.com/anita-wray/vermilion_sunset_gtseq/tree/main/FIGURES_STATISTICS). Major figure results and included below.

## Major Results:
- Vermilion rockfish were identified throughout the sampling region, from Hood Canal, WA to the US/Mexico border. In contrast, no sunset rockfish were encountered north of Point Arena, CA (38.91° N).
- Vermilion rockfish were caught at significantly shallower depths than sunset, although there was substantial overlap between the two species.
- Both vermilion and sunset rockfish demonstrated sexually dimorphic length-weight relationships, with females of both species generally heavier at longer lengths than males.
- Sunset rockfish of both sexes tended to be heavier at longer lengths than their vermilion rockfish counterparts.
- Both male and female sunset rockfish grew faster and reached substantially larger values of L∞ than their vermilion counterparts.
- Among all individuals captured, the mean length of sunset rockfish was significantly longer than vermilion rockfish.
- The weight and length of both species increased with depth, suggesting both species undergo ontogenetic migration into deeper waters.

### A snippet of the figures produced for the manuscript
![Fig6-SoCal_Map_w_points_final](https://github.com/user-attachments/assets/c03f9dd2-0c73-4233-aa51-96dc97992bb8)
  **Figure 6 - Spatial and depth distribution for vermilion rockfish (left) and sunset rockfish (right) specimens captured in Southern California used in this study. Circle size represents the number of specimens, and color represents the depth of capture in meters. Samples without depth data were not included in this plot.**

## Disclaimer: 
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
