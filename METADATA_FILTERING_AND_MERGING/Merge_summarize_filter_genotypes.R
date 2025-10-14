## The purpose of this script is to 
#       1. merge genotype data from the multiple genotyping runs
#       2. Run summary statistics on the samples
#.      3. Filter samples to only include good quality ones 


## FOR EACH NEW YEAR ------
# Change the 'samples_2024' and run, double checking that naming conventions
# have not changed from year to year

library(tidyverse)

setwd("~/Desktop/VermilionRF/VMSURF Species ID/input_genotypes/")


### STEP 1 - Merge Genotype Data ####
# This is the compiled list of all genotyped data. It was 
# filtered a bit to include samples that were sequencing blanks, errors, etc but 
# NOT filtered for quality

original_samples <- read.csv('genotypes_combined_09_2024.csv') %>%
  select(-c("X"))

#Add new samples from annual GTSEEK run, again not quality filtered
samples_2024 <- read.csv('rockfish_Prod092025_Genotypes.csv')

#Pull out the plate number and do a bit of cleaning up
samples_2024$Plate <- substr(samples_2024$Sample, 22, 27)
samples_2024$Plate <- gsub("_", "", samples_2024$Plate)
samples_2024$Plate <- gsub("Trawl2", 'TRAWL28', samples_2024$Plate) ## This will be a problem if we have more than 2 trawl plates in the future

#Shorten the sample name and change the column name to match the previous
samples_2024$Sample <- sub(".*_", "", samples_2024$Sample)
names(samples_2024)[names(samples_2024) == 'Sample'] <- 'sample_ID'



#Check to see if the colnames are the same, all should be true
colnames(original_samples) %in% colnames(samples_2024)

#Add them all together
combined <- rbind(original_samples, samples_2024)


#### STEP 2 - Plot some summary statistics ####
#Peek at the data to summarize
ggplot(data = combined, aes(x = `Raw.Reads`)) +
  geom_histogram()+
  geom_vline(xintercept = 10000, color = 'blue')+
  theme_bw()

ggplot(data = combined, aes(x = `On.Target.Reads`)) +
  geom_histogram()+
  geom_vline(xintercept = 10000, color = 'blue')+
  theme_bw() +
  ggtitle('Distribution of On-Target Read Count')

#How many are above the 10k threshold?
sum(combined$`On.Target.Reads` >= 10000) 

## This is for the new samples
sum(samples_2024$On.Target.Reads >= 10000)
nrow(samples_2024)
sum(samples_2024$On.Target.Reads >= 10000)/nrow(samples_2024)

#What does the Genotyping rate look like? 
ggplot(data = combined, aes(x = `X.GT`)) +
  geom_histogram()+
  geom_vline(xintercept = 75, color = 'blue')+
  theme_bw()+
  ggtitle('Percentage of Reads Genotyped')
#How many are above the 75% threshold?
sum(combined$`X.GT` >= 75) 
#How many fit both thresholds? 
sum(combined$`X.GT` >= 75 & combined$`On.Target.Reads` >= 10000)

## Subset of just the new samples
sum(samples_2024$X.GT >= 75 & samples_2024$On.Target.Reads >= 10000)
(sum(samples_2024$X.GT >= 75 & samples_2024$On.Target.Reads >= 10000)/nrow(samples_2024))


#What does the IFI look like? (Checks for contamination)
hist(combined$IFI)
sum(combined$IFI >= 20)

#Compare plot to original Campbell GT-seq paper
ggplot(data = combined, aes(x = `On.Target.Reads`, y = `X.GT`)) +
  geom_point(pch = 1) +
  theme_classic()+
  ylab('Percentage of Genotypes Collected')

###Look at data by plate - just new data 
ggplot(data = samples_2024, aes(x = `On.Target.Reads`, y = `X.GT`, col = Plate)) +
  geom_point(pch = 1) +
  theme_classic()+
  facet_wrap(~Plate)+
  ylab('Percentage of Genotypes Collected') +
  geom_rect(ymin = 75, ymax = Inf, 
            xmin = 10000, xmax = Inf, fill = "green", alpha=0.005)+
  theme(legend.position = 'none')

ggplot(data = samples_2024, aes(x = `IFI`, y = `X.GT`, col = Plate)) +
  geom_point(pch = 1) +
  theme_classic()+
  facet_wrap(~Plate)+
  ylab('Percentage of Genotypes Collected') +
  geom_rect(ymin = 75, ymax = Inf, 
            xmin = -Inf, xmax = 10, fill = "green", alpha=0.005)+
  theme(legend.position = 'none')

#### STEP 3 - Output Data into one megafile ####

#Output successful samples

successful_samples <- combined[combined$X.GT >= 75 &
                                 combined$On.Target.Reads >= 10000,]
failed_samples <- combined[combined$X.GT < 75 |
                             combined$On.Target.Reads < 10000,]

nrow(combined)
nrow(failed_samples) + nrow(successful_samples)

write.csv(combined, '~/Desktop/VermilionRF/VMSURF Species ID/input_genotypes/genotypes_combined_10_2025.csv')
write.csv(successful_samples, '~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/successful_samples_10_2025.csv')
writexl::write_xlsx(successful_samples, '~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/successful_samples_10_2025.xlsx')
write.csv(failed_samples,'~/Desktop/VermilionRF/VMSURF Species ID/QAQC/failed_samples_10_2025.csv' )
