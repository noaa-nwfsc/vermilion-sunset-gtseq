#Author: Anita Wray
#Purpose: Merge results from rubias, newhybrids, and structure together with
# raw sequence statistics to analyze all samples.

###### Load Libraries and set wd #####
packages <- c('tidyverse','readxl')
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd('~/Desktop/VermilionRF/VMSURF Species ID/')

###### Read in and merge Rubias, structure, newhybrids results ####

#Read in Rubias results
rubias <- read.csv(
  './assignment_output/QF_Data/rf_most_likely_repunit_GTseq_data_boc_rebs_ref_10-25.csv') %>%
  select(-c('mixture_collection', 'rep_pofz', 'PofZ', 'log_likelihood'
            ))

#Read in the structure vermilion/sunset analysis
structure <- read.csv(file = './structure_results/k2_R10_05_2025.csv')
colnames(structure) <- c('individual','% K1', '% K2')

#New Hybrids 
new_hybs <- read.csv("~/Desktop/VermilionRF/VMSURF Species ID/newhybrids/ver_sun_w_hybrids/PofZ_results_5_12_25.csv") %>%
  subset(hybridclass != 'PV') %>%
  subset(hybridclass != 'PS') %>%
  select(c('individuals', 'hybridclass'))

new_hybs$individuals <- gsub("_", "-",new_hybs$individuals)

merged_two <- merge(rubias, structure,
                         all.x = TRUE, by.x = 'sample_ID',
                         by.y = 'individual')

merged_allthree <- merge(merged_two, new_hybs,
                         all.x = TRUE, by.x = 'sample_ID',
                         by.y = 'individuals')

#merged_allthree <- merge(merged_allthree, canary_hybs,
 #                        all.x = TRUE, by.x = 'New_Sample_ID',
  #                       by.y = 'individual')

#Convert the missing loci column to a percent of missing loci
merged_allthree$missing_loci <- round(((merged_allthree$n_miss_loci/195)*100),
                                      digits = 1)

#Save file as csv
write.csv(merged_allthree,
          file = './metadata/structure_newhyb_rubias_results_2025_10.csv')

#Remove species call from samples that were flagged by Rubias
merged_allthree$repunit[merged_allthree$Z_flag == 'TRUE'] <- NA
merged_allthree$collection[merged_allthree$Z_flag == 'TRUE'] <- NA

#Add species, hybrid, population, on-target reads, and % genotyped results
merged_allthree_subsetted <- merged_allthree %>%
  select(c("On.Target.Reads", "X..GT","sample_ID","repunit",
           "collection","% K1","% K2","hybridclass"))


###### Load in metadata (THIS SHOULD ALREADY CONTAIN METADATA FROM THE NEW SAMPLES) ####
meta <- read_excel("metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_051525_AW.xlsx") %>%
  select(-c('repunit', 'collection', "% K1","% K2","hybridclass", "On-Target Reads", "% GT"))

meta_all <- merge(meta, merged_allthree_subsetted, 
                  all = T, 
                  by.x = "Specimen.Num", by.y = 'sample_ID')

#Look at sample names not included in metadata sheet - 
# check notes and confirm that it is okay that they don't have a species call
check_blanks <- meta_all %>% subset(is.na(`On.Target.Reads`))%>%
  filter(!str_detect(`Notes`, 'ALL HL147 samples')) %>% #HL147 was messed up and all had to be removed
  filter(!str_detect(`Notes`, 'sample was below sequencing threshold')) %>% #previously thrown out samples
  filter(!str_detect(`Notes`, 'ALL 2014 TRAWL CLIPS LOST')) #comment is self explanatory


###### Add in notes for the new years samples  ####

#change this list every year!!! there is probably a more sophisticated way to do this
samples_2024 <- c('TRAWL28', 'HL341', 'HL342','HL343','HL344',
                  'HL345','HL346','HL347','HL348','HL349','HL350',
                  'HL351','HL352','HL353','HL354','HL355','HL356',
                  'HL357','HL358','HL359','HL360','HL361','HL362',
                  'HL363', 'HL364', 'HL365')	

merged_allthree_2024 <- merged_allthree %>%
  subset(Plate %in% samples_2024)

## Add Z-flag note for potentially hybrid samples
true_z_flag <- merged_allthree_2024 %>%
  subset(Z_flag == TRUE)

true_z_flag_meta <- meta_all %>% 
  subset(Specimen.Num %in% true_z_flag$sample_ID)

true_z_flag_meta$Notes <- paste0(true_z_flag_meta$Notes, " ", "Z-flag is true, species ID not called")

#Add sequencing note for samples that were not run in Rubias due to poor sequencing quality
poor_sequencing_flag <- read.csv("QAQC/failed_samples_10_2025.csv") %>%
  subset(Plate %in% samples_2024)

poor_sequencing_flag_meta <- meta_all %>% 
  subset(Specimen.Num %in% poor_sequencing_flag$sample_ID)

poor_sequencing_flag_meta$Notes <- paste0(poor_sequencing_flag_meta$Notes, " ", "sample was below sequencing threshold, removed prior to rubias call")


#Remove annoying NAs that get added 
poor_sequencing_flag_meta$Notes <- gsub("NA", "", poor_sequencing_flag_meta$Notes)
true_z_flag_meta$Notes <- gsub("NA", "", true_z_flag_meta$Notes)



###### Merge existing metadata with new metadata to include notes & export ####
## Now re-add those notes
meta_all <- meta_all %>%
  subset(!Specimen.Num %in% poor_sequencing_flag_meta$Specimen.Num) %>%
  subset(!Specimen.Num %in% true_z_flag_meta$Specimen.Num)


meta_all <- rbind(meta_all, poor_sequencing_flag_meta, true_z_flag_meta)

writexl::write_xlsx(meta_all, path = './metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_101425_AW.xlsx')
