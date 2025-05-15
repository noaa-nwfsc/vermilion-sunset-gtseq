#Author: Anita Wray
#Date: Feb 2023
#Purpose: This script will convert the excel from Nate (previously converted from a rf_GTs_as_loci)
# into a genind, then into a structure file format

library(pegas)
library(readxl)
library(tidyverse)
library(adegenet)
library(data.table)

rf_GTs_path <- "~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/successful_samples_09_2024.xlsx"
# read path for common loci RDS
common_loci_path <- "~/Desktop/VermilionRF/VMSURF Species ID/rubias_inputs/rf_GTseq_common_loci.RDS"
# read in genotypes -------------------------------------------------------

# first, save genotyping file as .xlsx in excel
# `read_rf_GTs_as_loci` has odd behavior of changing values of every other column of loci with "0"
# to "00" - has consequences downstream that are avoided by reading in with `read_excel`
rf_GTs_QF <- read_excel(path = rf_GTs_path) %>%
  rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) %>% # replace "." in loci names with "_"
  rename("% GT" = "X_GT",
         "On-Target Reads" = "On_Target_Reads",
         "% On-Target" = "X_On_Target",
         "Raw Reads" = "Raw_Reads")

#remove non-sunset, canary, or vermilion samples (Blue, Deacon, Rougheye, Bocac)
non_sample_species <- read.csv('~/Desktop/VermilionRF/VMSURF Species ID/metadata/non-verm-species.csv',
                               header = F)

rf_GTs_QF <- rf_GTs_QF[!rf_GTs_QF$sample_ID %in% non_sample_species$V1,]

#### for this run we are only going to look at 2023 samples 
#rf_GTs_QF <- rf_GTs_QF [!grepl("CN",rf_GTs_QF$sample_ID),]
#rf_GTs_QF <- rf_GTs_QF[grepl("-23-",rf_GTs_QF$sample_ID),]

# create list of columns to drop for conversion to genind 
drop_col <- c("sample_ID", "Plate" ,"Raw Reads", "On-Target Reads","% On-Target","% GT" ,"IFI")

# pull plate column & sample_ID to match up after file format conversions below
rf_GTs_cols <- rf_GTs_QF %>%
  select(sample_ID, Plate) %>%
  mutate(pop = rep(NA, times=nrow(rf_GTs_QF)),
         Sample_ID = as.character(sample_ID))
# include individual sample names and pop as NA


#Create a new column with shorter names bc STRUCTURE doesn't like long sampleID names
#rf_GTs_cols$New_Sample_ID <- gsub("-",'', rf_GTs_cols$sample_ID)
#write.csv(rf_GTs_cols, file = '~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/newsampleID_allsamples_5_1_2025.csv')
new_sample_ID <- read.csv('~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/newsampleID_allsamples_5_1_2025.csv')
rf_GTs_cols <- merge(rf_GTs_cols, new_sample_ID, all = F) %>%
  select(-c('X'))

rf_GTs_cols <- subset(rf_GTs_cols, rf_GTs_cols$sample_ID %in% rf_GTs_QF$sample_ID)


# df2genind drops any rows (i.e., individuals with missing data)
# and any markers with no loci scored are dropped (i.e., SebC_36504) which
# did not perform well outside of panel testing 
rf_GTs_genind <- df2genind(rf_GTs_QF %>%
                             select(-drop_col), 
                           NA.char = "0", ploidy = 2, 
                           sep = "", ind.names = rf_GTs_QF$sample_ID)

# drop unwanted loci (i.e., "SebC_36504") see `rockfish_gtseq_messing.R`
# SebC_36504 did not genotype in most samples across runs
common_loci <- readRDS(file = common_loci_path)

# drop "SebC_36504" 
# likely already dropped from df2genind because of total lack of calls
rf_GTs_genind <- rf_GTs_genind[loc=common_loci]



#Use the as.loci() function to split the genind
rf_GTs_as_loci <- as.loci(rf_GTs_genind)
rf_GTs_as_loci$population <- 01

rf_GTs_as_loci <- rf_GTs_as_loci %>%
  relocate(population, .before = SebC_10437)
#Save as a .str file for structure
write.loci(rf_GTs_as_loci, 
           file = "~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/genotyped_2023_samples_newID_05_06_2025.str", 
           loci.sep ="\t", quote = FALSE,
           allele.sep ="\t", na ="-9\t-9", col.names = FALSE)

#Read it back in to filter the dataset
data <- read.table(file = "~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/genotyped_2023_samples_newID_05_06_2025.str",
                   header = FALSE, 
                   sep = '\t')


### Put the new sample names back in

data <- merge(y = data, x = (rf_GTs_cols %>% select(c("sample_ID", 'New_Sample_ID'))), 
              by.y = "V1", by.x = 'sample_ID') %>%
  select(-c('sample_ID'))

#Take the three columns we need for filtering
three_cols <- c('sample_ID', 'repunit', 'Z_flag', 'collection')
metadata <- read.csv("~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_Data/rf_most_likely_repunit_GTseq_data_boc_rebs_ref_09-24.csv") %>%
  select(three_cols)

##Create five iterations of datasets to output as structure files:
## 1. Sunset only
## 2. Vermilion only
## 3. Sunset/Vermilion only with flagged individuals
## 4. Sunset/Canary only with flagged individuals
## 5. Vermilion/Canary only with flagged individuals. 

#Filter out bad samples that we don't have a sample ID for
non_flagged <- metadata[metadata$Z_flag == FALSE,]
flagged <- metadata[metadata$Z_flag == TRUE,]

#Merge the new sample ID names with the old names
metadata_merged <- merge(x = rf_GTs_cols, y = non_flagged, 
                         all.y = FALSE,
                         by.y = 'sample_ID', 
                         by.x = 'Sample_ID')
metadata_merged_flag <- merge(x = rf_GTs_cols, y = flagged, 
                         all.y = FALSE,
                         by.y = 'sample_ID', 
                         by.x = 'Sample_ID')

mis_punched <- c('H-19-TO-V0001','H-11-MI-V0624','H-21-MI-V0237','T-17-MJ-CN-0053',
                 'T-15-NA-0230','H-21-AG-V0586','H-09-CN-AG-A0454','T-17-MJ-CN-0053',
                 'T-23-NA-0227','H-10-CN-AG-A0014','H-21-TO-V0681','H-19-TO-V0001',
                 'H-11-MI-V0624','T-15-NA-0230','H-21-MI-V0237','H-22-CN-AG-A0142',
                 'T-17-MJ-CN-0053','H-21-AG-V0586','H-14-MI-V0534','H-23-TO-V0198',
                 'H-15-TO-V0152','H-08-AG-V0155','H-23-TO-V0325','H-21-TO-V0194',
                 'H-23-MI-V0019','H-21-TO-V0319','H-19-AG-V0469','H-13-MI-V0318',
                 'H-21-AG-V0756','H-23-MI-V0182','H-14-AG-V0521','H-23-MI-V0372',
                 'H-14-AG-V0061','H-22-CN-AG-A0142')


metadata_merged_flag <- metadata_merged_flag %>%
  subset(!sample_ID %in% mis_punched)
#Get one dataset for each species
vermilion <- metadata_merged[metadata_merged$repunit == 'vermilion',]
canary <- metadata_merged[metadata_merged$repunit == 'canary',]
sunset <- metadata_merged[metadata_merged$repunit == 'sunset',]


sun_verm_with_flagged <- rbind(sunset, vermilion, metadata_merged_flag)
sun_can_with_flagged <- rbind(sunset, canary, metadata_merged_flag)
verm_can_with_flagged <- rbind(vermilion, canary, metadata_merged_flag)

##Create five iterations of datasets to output as structure files:
## 1. Sunset only
## 2. Vermilion only
## 3. Sunset/Vermilion only with flagged individuals
## 4. Sunset/Canary only with flagged individuals
## 5. Vermilion/Canary only with flagged individuals.
data_vermilion <- data[data$New_Sample_ID %in% vermilion$New_Sample_ID,]
data_sunset <- data[data$New_Sample_ID %in% sunset$New_Sample_ID,]
data_sun_verm_flagged <- data[data$New_Sample_ID %in% sun_verm_with_flagged$New_Sample_ID,]
data_sun_can_flagged <- data[data$New_Sample_ID %in% sun_can_with_flagged$New_Sample_ID,]
data_verm_can_flagged <- data[data$New_Sample_ID %in% verm_can_with_flagged$New_Sample_ID,]


############### TEST HERE VIA PCA TO MAKE SURE THE FILE IS WHAT YOU WANT #####
new_genind<-rf_GTs_genind %>%
  subset(indNames(rf_GTs_genind) %in% sun_verm_with_flagged$sample_ID)



rf_flagged_panel_GTs_scaled <- scaleGen(new_genind, NA.method="mean")
rf_flagged_panel_GTs_pca <- dudi.pca(rf_flagged_panel_GTs_scaled, cent = FALSE, scale = FALSE, scannf = F, nf = 10)

# visualize eigenvalues
barplot(rf_flagged_panel_GTs_pca$eig[1:50], main = "PCA eigenvalues")

# save PCA results as data frame
rf_flagged_panel_pcscores <- as.data.frame(rf_flagged_panel_GTs_pca$li) %>%
  rownames_to_column(var = "sample_id")

rf_flagged_panel_eig <- round((rf_flagged_panel_GTs_pca$eig/(sum(rf_flagged_panel_GTs_pca$eig)))*100,2)

ggplot(rf_flagged_panel_pcscores, aes(Axis1, Axis2)) + 
  geom_point() +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_flagged_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10)




######
#Write five datasets into .str files
write_delim(data_vermilion, 
            file = "~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/vermilion_only_05_06_2025.str",
            col_names = FALSE)         
write_delim(data_sunset, 
            file = "~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/sunset_only_05_06_2025.str",
            col_names = FALSE) 
write_delim(data_sun_verm_flagged, 
            file = "~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/sunset_vermilion_flagged_05_06_2025.str",
            col_names = FALSE) 
write_delim(data_sun_can_flagged,
            file = '~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/sunset_canary_flagged_05_06_2025.str',
            col_names = FALSE)
write_delim(data_verm_can_flagged,
            file = '~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/vermilion_canary_flagged_05_06_2025.str',
            col_names = FALSE)


