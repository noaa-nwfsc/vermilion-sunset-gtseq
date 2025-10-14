# goal here will be to have a generic protocol for calling individuals 
library(tidyverse)
library(adegenet)
library(readxl)
library(rubias)
library(ggrepel)
library(data.table)

# set read and write paths  -----------------------------------------------
setwd("~/Desktop/VermilionRF/VMSURF Species ID/")

###### these paths shouldn't change across runs unless these files move directories
# read path for common loci RDS
common_loci_path <- "~/Desktop/VermilionRF/VMSURF Species ID/rubias_inputs/rf_GTseq_common_loci.RDS"

# read path for reference rockfish df for Rubias without hybrids
rf_reference_df_sans_SVH_path <- "~/Desktop/VermilionRF/VMSURF Species ID/rubias_inputs/rf_reference_df_sans_SVH_avec_bocac_and_rebs.RDS"

# read path for reference rockfish genind for PCA plots (with hybrid)
rf_GTseq_panel_genind_path <- "~/Desktop/VermilionRF/VMSURF Species ID/rubias_inputs/rf_panel_GTseq_genind_common_loci_sans_shite_hybrids_avec_bocac_and_rebs.RDS"

# read path for other species included on plates Trawl-13 and Trawl-14
otro_rf_spp_sample_ID_path <- "~/Desktop/VermilionRF/VMSURF Species ID/rubias_inputs/2022-03-29_otro_spp._included_plates.RDS" 

######## these paths should be unique to each run
# read path for the genotype data saved as xlsx file
rf_GTs_path <- "./QF_genotypes/successful_samples_10_2025.xlsx"

# write path for Rubias calls with flagged individuals
rf_most_likely_repunit_GTseq_data_path <-  "~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_Data/rf_most_likely_repunit_GTseq_data_boc_rebs_ref_10-25.csv"

# write path for Rubias calls of only flagged individuals
rf_most_likely_repunit_GTseq_data_flagged_path <- "~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_Data/rf_most_likely_repunit_GTseq_data_flagged_boc_rebs_ref_10-25.csv"

# write path for PCA with flagged individuals and high quality rf panel
flagged_individuals_with_panel_PCA_path <- "~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_PCAs/flagged_individuals_with_panel_PCA_boc_rebs_ref_10-25.pdf"

# write path for PCA with flagged individuals and high quality rf panel
flagged_individuals_with_panel_PCA_path_unlabeled <- "~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_PCAs/flagged_individuals_with_panel_unlabeled_PCA_boc_rebs_ref_10-25.pdf"

# write path for PCA with QF individuals and high quality rf panel
QF_individuals_with_panel_PCA_path <- "~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_PCAs/QF_individuals_with_panel_PCA_boc_rebs_ref_10-25.pdf"

QF_individuals_with_panel_PCA_path_current_year_only <- "~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/QF_PCAs/QF_individuals_with_panel_PCA_boc_rebs_ref_10-25_2024ONLY.pdf"


# read in genotypes -------------------------------------------------------

# first, save genotyping file as .xlsx in excel
# `read_csv` has odd behavior of changing values of every other column of loci with "0"
# to "00" - has consequences downstream that are avoided by reading in with `read_excel`
rf_GTs_QF <- read_excel(path = rf_GTs_path) %>%
  rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) %>%
  subset()

#rf_GTs_QF$sample_ID <- sub(".*_", "", rf_GTs_QF$sample_ID)

colnames(rf_GTs_QF)[3:6] <- c("Raw Reads", "On-Target Reads","% On-Target","% GT")

# ======================== EDIT (03/2024) =====================================
##Remove non-vermilion sunsets from previous runs
non_verm <- read.csv('~/Desktop/VermilionRF/VMSURF Species ID/metadata/non-verm-species.csv', 
                     header = F)

#Remove non-vermilion, sunset or canary
rf_GTs_QF <- rf_GTs_QF[!rf_GTs_QF$sample_ID %in% non_verm$V1,]


# create list of columns to drop for conversion to genind 
drop_col <- c("sample_ID", "Plate" ,"Raw Reads", "On-Target Reads","% On-Target","% GT" ,"IFI")

# pull plate column & sample_ID to match up after file format conversions below
rf_GTs_cols <- rf_GTs_QF %>%
  select(sample_ID, Plate) %>%
  mutate(pop = rep(NA, times=nrow(rf_GTs_QF)),
         Sample_ID = as.character(sample_ID))
# include individual sample names and pop as NA

# df2genind drops any rows (i.e., individuals with missing data)
# and any markers with no loci scored are dropped (i.e., SebC_36504) which
# did not perform well outside of panel testing 
rf_GTs_genind <- df2genind(rf_GTs_QF %>%
                             select(-drop_col), 
                           NA.char = "0", ploidy = 2, sep = "",
                           pop = rf_GTs_cols$pop, ind.names = rf_GTs_cols$Sample_ID)
#This may output a warning --- ignore it

# ======================== EDIT (03/2024) =====================================
# I removed these two lines because bocaccio rf do sequence well at this locus, 
# and therefore can be very informative when we are ruling out mis-ID

# drop unwanted loci (i.e., "SebC_36504") see `rockfish_gtseq_messing.R`
# SebC_36504 did not genotype in most samples across runs
#common_loci <- readRDS(file = common_loci_path)

# drop "SebC_36504" 
# likely already dropped from df2genind because of total lack of calls
#rf_GTs_genind <- rf_GTs_genind[loc=common_loci]

# convert the genind back into a dataframe in prep for Rubias
#  split alleles into two columns (rubias requires this)
rf_GTs_df <- genind2df(rf_GTs_genind, oneColPerAll = TRUE, sep = "") 

# continue format prep for Rubias
# requires specific column names (i.e., repunit, sample_type, indiv, collection)
# collection is plate ID
# note: collection in the output will be population level ID if applicable (i.e., vermilion) 
rf_GTs_df <- left_join(rownames_to_column(rf_GTs_df, var = "Sample_ID") %>%
                         rename(repunit = pop, indiv = Sample_ID) %>%
                         add_column(sample_type = "mixture", .before = "indiv" ) %>%
                         mutate(sample_type = as.character(sample_type),
                                repunit = as.character(repunit)),
                       rf_GTs_cols %>%
                         select(Sample_ID, Plate) %>%
                         rename(indiv = Sample_ID,
                                collection = Plate)) %>%
  relocate(collection, .after = "repunit") %>%
  # mutate across all character columns NA(character) to actual NA
  mutate(across(where(is.character), ~na_if(., "NA")))

# read in reference rockfish dataset for Rubias
# used to call individuals in `rf_GTs_df`
rf_reference_df_sans_SVH <- readRDS(file = rf_reference_df_sans_SVH_path)%>%
  rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("_1", ".1", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("_2", ".2", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("SebC.1", "SebC_1", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("SV.1", "SV_1", .x, fixed = TRUE))%>%
  rename_with( ~ gsub("SebC.2", "SebC_2", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("SV.2", "SV_2", .x, fixed = TRUE))%>%
  rename_with( ~ gsub("Sni_scaffold.1287_bp76223_76379.1", "Sni_scaffold_1287_bp76223_76379_1", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("Sni_scaffold.1287_bp76223_76379.2", "Sni_scaffold_1287_bp76223_76379_2", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("Sni_scaffold_931_BP109316.109356.109540.1", "Sni_scaffold_931_BP109316_109356_109540_1", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("Sni_scaffold_931_BP109316.109356.109540.2", "Sni_scaffold_931_BP109316_109356_109540_2", .x, fixed = TRUE))

### Match column names
which(!colnames(rf_reference_df_sans_SVH) %in% colnames(rf_GTs_df))
reorder_idx <- match(colnames(rf_GTs_df), colnames(rf_reference_df_sans_SVH)) 
rf_reference_df_sans_SVH <- rf_reference_df_sans_SVH[,reorder_idx]


#make sure plate numbers are accurate (none should be > 96)
table(rf_GTs_QF$Plate)

#which(!colnames(rf_reference_df_sans_SVH) == colnames(rf_GTs_df))

##Test if reference panel can 'self-assign' aka assign each individual to its own reference
#sa_rockfish <- self_assign(reference = rf_reference_df_sans_SVH, gen_start_col = 5)
#print(sa_rockfish, n = 100)
#options(scipen=999)
#sa_to_repu <- sa_rockfish %>%
#  group_by(indiv, collection, repunit, inferred_repunit) %>%
#  summarise(repu_scaled_like = sum(scaled_likelihood))

#write.csv(sa_to_repu, file = '~/Desktop/VermilionRF/VMSURF Species ID/assignment_output/quality_of_reference_bocac_and_rebs.csv')
# estimate assignment with Rubias -----------------------------------------

# run Rubias for quality filtered individuals using the reference panel
# to estimate assignment to species 
rf_mix_est <- infer_mixture(reference = rf_reference_df_sans_SVH, mixture = rf_GTs_df,
                            gen_start_col = 5)  

# individual posteriors for each individual belonging to all possible collections
# 5 rows for each individual (S, V-A, V-B, V-C, & C)
# NOTE: individuals with all missing data are assigned a Z score of the mixing 
# proportions for the most common unit (not seen with QF'd individuals)
rf_indiv_post <- rf_mix_est$indiv_posteriors

# probability of reporting units (repunit) summarized. The probability of the vermilion
# repunit is the sum of the collections (i.e., V-A, V-B, V-C) probabilities 
# note: the three vermilion rows (different pops) will have all the same 
# rep_pofz scores
rf_indiv_post_sum_repunit <- left_join(rf_indiv_post,
                                       rf_indiv_post %>%
                                         group_by(indiv, repunit) %>%
                                         summarise(rep_pofz = sum(PofZ))) %>%
  relocate(rep_pofz, .after = repunit)

# get most likely repunit (i.e., species) and then filter the most likely PofZ 
# which basically selects the most likely vermilion population 
# really not necessary to first select top `rep_pofz` as top `PofZ`
# will always be associated with top `rep_pofz` BUT clarifies priority of IDing species first
rf_most_likely_repunit <- rf_indiv_post_sum_repunit %>%
  group_by(indiv) %>%
  top_n(rep_pofz, n=1) %>% # this line isn't really necessary 
  top_n(PofZ, n=1)

# add output info from Nate (Gtseek)
# added `Z_flag` for secondary inspection of individuals in PCA
# when Z score is less than -10 (likely hybrids or misidentifications)
# individuals that were dropped by quality filters and did not undergo assignment 
# estiamtes in Rubias will have missing data (i.e., NA or NULL) for Rubias outputs
rf_most_likely_repunit_GTseq_data <- left_join(rf_GTs_QF %>%
                                                 select("sample_ID" ,"Raw Reads", "On-Target Reads","% On-Target","% GT" ,"IFI", "Plate"),
                                               rf_most_likely_repunit %>% 
                                                 rename(sample_ID = indiv) %>%
                                                 mutate(Z_flag = if_else(z_score < -10, TRUE, FALSE)), 
                                               by="sample_ID")

# write csv (includes flagged)
# flagged inds should be inspected via PCA and
# assignments manually overriden if appropriate
write_csv(rf_most_likely_repunit_GTseq_data,
          file = rf_most_likely_repunit_GTseq_data_path)

good_samples <- rf_most_likely_repunit_GTseq_data %>%
          filter(Z_flag == "FALSE")

table(good_samples$Plate)
table(good_samples$repunit)

# write csv of just flagged
write_csv(rf_most_likely_repunit_GTseq_data %>%
            filter(Z_flag == "TRUE"),
          file = rf_most_likely_repunit_GTseq_data_flagged_path)


# individuals flagged for inspection in a PCA with panel inds should be manually called
rf_flagged_inds <- rf_most_likely_repunit_GTseq_data %>%
  filter(Z_flag == "TRUE") %>% 
  pull(sample_ID)

# PCA  --------------------------------------------------------------------
# the other rf species included for testing (blue, deacon, blackspotted, & rougheye)
# were sequenced in this production round. Going to plot them on on their own PCA
otro_rf_spp_sample_ID <- readRDS(file = otro_rf_spp_sample_ID_path)

# panel of high quality genotyped individuals used for marker development and testing
# excludes two previously ID'd hybrids (out of 3) that yielded low genotyping rate in panel tests
rf_GTseq_panel_genind <- readRDS(file = rf_GTseq_panel_genind_path)

# drop unwanted loci (i.e., "SebC_36504") see `rockfish_gtseq_messing.R`
# SebC_36504 did not genotype in most samples across runs
common_loci <- readRDS(file = common_loci_path)

# drop "SebC_36504" 
# likely already dropped from df2genind because of total lack of calls
rf_GTs_genind <- rf_GTs_genind[loc=common_loci]

# flagged individual(s) genind 
rf_flagged_genind <- rf_GTs_genind[rf_flagged_inds,]

# otro species flagged ind 
rf_flagged_otro_spp_genind <- rf_flagged_genind[otro_rf_spp_sample_ID,]

# drop otro species from the flagged genind
rf_flagged_panel_genind <- repool(rf_GTseq_panel_genind, rf_flagged_genind[!row.names(rf_flagged_genind@tab) %in% otro_rf_spp_sample_ID])

# QF inds without flagged individuals and panel genind
rf_QF_panel_genind <- repool(rf_GTseq_panel_genind, rf_GTs_genind[!row.names(rf_GTs_genind@tab) %in% rf_flagged_inds])

# reassuringly, none of the other species made it past the Z flag filter 
rf_QF_panel_genind[!row.names(rf_QF_panel_genind@tab) %in% otro_rf_spp_sample_ID]

### PCA  flagged inds
# really only need to retain 3 factors (nf = 3) if potential hybrids or mis id'd
# species are excluded from QF inds by Z flag (flagged inds)
library(ggrepel)

rf_flagged_panel_GTs_scaled <- scaleGen(rf_flagged_panel_genind, NA.method="mean")
rf_flagged_panel_GTs_pca <- dudi.pca(rf_flagged_panel_GTs_scaled, cent = FALSE, scale = FALSE, scannf = F, nf = 10)

# visualize eigenvalues
barplot(rf_flagged_panel_GTs_pca$eig[1:50], main = "PCA eigenvalues")

# save PCA results as data frame
rf_flagged_panel_pcscores <- as.data.frame(rf_flagged_panel_GTs_pca$li) %>%
  rownames_to_column(var = "sample_id")

# add in collection and species data for panel samples
# I manually added the only good "known" hybrid from the panel (VM-536) with a high GT rate 
# the other two (VM-263 & VM-464) were below 50% GT and should not be trusted
panel <- tibble(indNames(rf_flagged_panel_genind),
                pop(rf_flagged_panel_genind),
                .name_repair = ~ c("sample_id", "species"))
panel[panel == 'NA'] <- NA

test1 <- rf_reference_df_sans_SVH %>%
  select(indiv, collection) %>%
  rename(sample_id = indiv) %>%
  mutate(across(collection, na_if, "NA"))
test1 <- rbind(test1, tibble("VM-536", "SVH",
                    .name_repair = ~ c("sample_id", "collection")))
rf_flagged_panel_species <- merge(panel, test1)

rf_flagged_panel_pcscores <- merge(rf_flagged_panel_pcscores, rf_flagged_panel_species, all = T)

rf_flagged_panel_eig <- round((rf_flagged_panel_GTs_pca$eig/(sum(rf_flagged_panel_GTs_pca$eig)))*100,2)

# mutates NA to flagged and adds it as factor
# for both collection and species
rf_flagged_panel_pcscores <- rf_flagged_panel_pcscores %>%
  mutate(species = as.character(species),
         species = ifelse(is.na(species), "flagged", species),
         species = factor(species, levels = c("flagged", "sunset", 
                                              "vermilion", "sun_verm_hybrid", 
                                              "canary", 'bocaccio', 'rebs')),
         collection = ifelse(is.na(collection), "F", collection),
         collection = factor(collection, levels = c("F", "S", "V-A", "V-B", 
                                                    "V-C", "SVH", "C", 'B',
                                                    'R')))

# plot
species_colors <- c("sunset" = '#ff7f00',
                    "vermilion" = '#61599d', 
                    "sun_verm_hybrid" = '#1f78b4', 
                    "canary" = '#b2df8a',
                    'bocaccio' = '#FFCC00',
                    'rebs' = '#e31a1c')
pop_colors <- c("S" = '#ff7f00',
                "V-A" = '#a6cee3',
                "V-B" = '#33a02c',
                "V-C" = '#cab2d6',
                "SVH" = '#1f78b4',
                "C" = '#b2df8a',
                "B" = '#FFCC00',
                'R' = '#e31a1c')
species_colors_w_flagged <- c("sunset" = '#ff7f00',
                              "vermilion" = '#61599d', 
                              "sun_verm_hybrid" = '#1f78b4', 
                              "canary" = '#b2df8a',
                              'flagged' = 'black',
                              'bocaccio' = '#FFCC00',
                              'rebs' = '#e31a1c')
pop_colors_w_flagged <- c("S" = '#ff7f00',
                "V-A" = '#a6cee3',
                "V-B" = '#33a02c',
                "V-C" = '#cab2d6',
                "SVH" = '#1f78b4',
                "C" = '#b2df8a',
                'F' = 'black',
                "B" = '#FFCC00',
                'R' = '#e31a1c')

pdf(file = flagged_individuals_with_panel_PCA_path,
    width = 7, height = 6)
ggplot(rf_flagged_panel_pcscores, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=species)) +
  scale_shape_manual(values=1:nlevels(rf_flagged_panel_pcscores$species)) +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_flagged_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  geom_text_repel(data= subset(rf_flagged_panel_pcscores, sample_id %in% rf_flagged_inds),
                 aes(label = sample_id),
                 size = 2, max.overlaps = 100,  #increase if there are >100 overlap inds
                 force = 4) +
  ggtitle("Flagged individuals with panel")+
  scale_color_manual(values = species_colors_w_flagged)

ggplot(rf_flagged_panel_pcscores, aes(Axis1, Axis3)) + 
  geom_point(aes(col=collection, shape=collection)) +
  scale_shape_manual(values=1:nlevels(rf_flagged_panel_pcscores$collection)) +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC3 (",rf_flagged_panel_eig[3],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  geom_text_repel(data= subset(rf_flagged_panel_pcscores, sample_id %in% rf_flagged_inds),
                  aes(label = sample_id),
                  size = 2, max.overlaps = 100,  #increase if there are >100 overlap inds
                  force = 4) +
  ggtitle("Flagged individuals with panel")  +
  scale_color_manual(values = pop_colors_w_flagged)
dev.off()

# plot unlabeled
pdf(file = flagged_individuals_with_panel_PCA_path_unlabeled,
    width = 7, height = 6)
ggplot(rf_flagged_panel_pcscores, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=species)) +
  scale_shape_manual(values=1:nlevels(rf_flagged_panel_pcscores$species)) +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_flagged_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  ggtitle("Flagged individuals with panel")+
  scale_color_manual(values = species_colors_w_flagged)

ggplot(rf_flagged_panel_pcscores, aes(Axis1, Axis3)) + 
  geom_point(aes(col=collection, shape=collection)) +
  scale_shape_manual(values=1:nlevels(rf_flagged_panel_pcscores$collection)) +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC3 (",rf_flagged_panel_eig[3],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  ggtitle("Flagged individuals with panel") +
  scale_color_manual(values = pop_colors_w_flagged)
dev.off()

### PCA of quality filtered inds (sans flagged) with panel --------------------

# probably only need to retain 3 factors (nf = 3) if potential hybrids or mis id'd
# species are excluded from QF inds by Z flag (flagged inds)
rf_QF_panel_GTs_scaled <- scaleGen(rf_QF_panel_genind, NA.method="mean")
rf_QF_panel_GTs_pca <- dudi.pca(rf_QF_panel_GTs_scaled, cent = FALSE, scale = FALSE, scannf = F, nf = 10)

## visualize eigenvalues
barplot(rf_QF_panel_GTs_pca$eig[1:50], main = "PCA eigenvalues")

# save PCA results as data frame
rf_QF_panel_pcscores <- as.data.frame(rf_QF_panel_GTs_pca$li) %>%
  rownames_to_column(var = "sample_id")

# add in collection and species data for panel samples
rf_QF_panel_species <-  full_join(rf_most_likely_repunit %>%
                                    filter(!indiv %in% rf_flagged_inds) %>%
                                    select(indiv, repunit, collection) %>%
                                    rename(sample_id = indiv,
                                           species = repunit) %>%
                                    mutate(assignment = "rubias"),
                                  rf_flagged_panel_species %>%
                                    filter(sample_id %in% indNames(rf_GTseq_panel_genind)) %>%
                                    mutate(assignment = "panel"))

rf_QF_panel_pcscores <- merge(rf_QF_panel_pcscores, rf_QF_panel_species, all = T)

# for both collection and species
rf_QF_panel_pcscores <- rf_QF_panel_pcscores %>%
  mutate(species = factor(species, levels = c( "sunset", "vermilion", 
                                               "sun_verm_hybrid", "canary", 
                                               'bocaccio', 'rebs')),
         collection = factor(collection, levels = c( "S", "V-A", 
                                                     "V-B", "V-C", 
                                                     "SVH", "C", 
                                                     'B', 'R')),
         assignment = factor(assignment, levels = c("panel","rubias")))

# Calculate percent eigenvalues for each principle component
rf_QF_panel_eig <- round((rf_QF_panel_GTs_pca$eig/(sum(rf_QF_panel_GTs_pca$eig)))*100,2)

# plot
pdf(file = QF_individuals_with_panel_PCA_path,
    width = 7, height = 6)
ggplot(rf_QF_panel_pcscores, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=assignment)) +
  scale_shape_manual(values=1:nlevels(rf_QF_panel_pcscores$assignment)) +
  xlab(paste0("PC1 (",rf_QF_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_QF_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  ggtitle("QF individuals with panel")+
  scale_color_manual(values = species_colors)

ggplot(rf_QF_panel_pcscores, aes(Axis1, Axis3)) + 
  geom_point(aes(col=collection, shape=assignment)) +
  scale_shape_manual(values=1:nlevels(rf_QF_panel_pcscores$assignment)) +
  xlab(paste0("PC1 (",rf_QF_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC3 (",rf_QF_panel_eig[3],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  ggtitle("QF individuals with panel")+
  scale_color_manual(values = pop_colors) 
dev.off()



samples_2024 <- c('TRAWL28', 'HL341', 'HL342','HL343','HL344',
                  'HL345','HL346','HL347','HL348','HL349','HL350',
                  'HL351','HL352','HL353','HL354','HL355','HL356',
                  'HL357','HL358','HL359','HL360','HL361','HL362',
                  'HL363', 'HL364', 'HL365')		

samples_2024_ids <- rf_GTs_cols %>%
  subset(Plate %in% samples_2024)


rf_flagged_panel_pcscores_2024 <- rf_flagged_panel_pcscores %>%
  subset(sample_id %in% samples_2024_ids$sample_ID | collection != 'F')

pdf(file = QF_individuals_with_panel_PCA_path_current_year_only,
    width = 7, height = 6)

ggplot(rf_flagged_panel_pcscores_2024, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=species)) +
  scale_shape_manual(values=1:nlevels(rf_flagged_panel_pcscores_2024$species)) +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_flagged_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  geom_text_repel(data= subset(rf_flagged_panel_pcscores_2024, sample_id %in% rf_flagged_inds),
                  aes(label = sample_id),
                  size = 2, max.overlaps = 100,  #increase if there are >100 overlap inds
                  force = 4) +
  ggtitle("Flagged individuals with panel - 2024 samples")+
  scale_color_manual(values = species_colors_w_flagged)

rf_QF_panel_pcscores_2024 <- rf_QF_panel_pcscores %>%
  subset(sample_id %in% samples_2024_ids$sample_ID | assignment == 'panel')

ggplot(rf_QF_panel_pcscores_2024, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=assignment)) +
  scale_shape_manual(values=1:nlevels(rf_QF_panel_pcscores_2024$assignment)) +
  xlab(paste0("PC1 (",rf_QF_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_QF_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  ggtitle("QF individuals with panel - 2024 samples")+
  scale_color_manual(values = species_colors)

dev.off()


samples_hybrid_redos_ids <- c('H-04-MI-V0831', 'H-08-AG-V0155', 'H-09-CN-AG-A0454',
'H-10-CN-AG-A0014','H-11-MI-V0401','H-11-MI-V0624','H-13-MI-V0318','H-14-AG-V0061',
'H-14-MI-V0534','H-15-TO-V0152','H-19-AG-V0469','H-19-TO-V0001','H-21-AG-V0586',
'H-21-AG-V0756','H-21-MI-V0237','H-21-TO-V0194','H-21-TO-V0319','H-21-TO-V0681',
'H-22-CN-AG-A0142','T-15-NA-0230','T-17-MJ-CN-0053')

rf_flagged_panel_pcscores_hybrid_redos <- rf_flagged_panel_pcscores %>%
  subset(sample_id %in% samples_hybrid_redos_ids| collection != 'F')

ggplot(rf_flagged_panel_pcscores_hybrid_redos, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=species)) +
  scale_shape_manual(values=1:nlevels(rf_flagged_panel_pcscores_hybrid_redos$species)) +
  xlab(paste0("PC1 (",rf_flagged_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_flagged_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  geom_text_repel(data= subset(rf_flagged_panel_pcscores_hybrid_redos, sample_id %in% rf_flagged_inds),
                  aes(label = sample_id),
                  size = 2, max.overlaps = 100,  #increase if there are >100 overlap inds
                  force = 4) +
  ggtitle("Flagged individuals with panel - 2023 samples")+
  scale_color_manual(values = species_colors_w_flagged)

rf_QF_panel_pcscores_hybrid_redos <- rf_QF_panel_pcscores %>%
  subset(sample_id %in% samples_hybrid_redos_ids | assignment == 'panel')

ggplot(rf_QF_panel_pcscores_hybrid_redos, aes(Axis1, Axis2)) + 
  geom_point(aes(col=species, shape=assignment)) +
  scale_shape_manual(values=1:nlevels(rf_QF_panel_pcscores_hybrid_redos$assignment)) +
  xlab(paste0("PC1 (",rf_QF_panel_eig[1],")%", collapse = "")) + 
  ylab(paste0("PC2 (",rf_QF_panel_eig[2],")%", collapse = "")) +  
  theme_grey(base_size = 10) +
  ggtitle("QF individuals with panel - 2023 samples")+
  scale_color_manual(values = species_colors)
