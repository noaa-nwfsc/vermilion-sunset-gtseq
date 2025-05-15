#Author: Anita Wray
#Date: Feb 2023
#Purpose: This script will directly take the output (*_f) from STRUCTURE via the
#command line and output a stacked barplot of the results. It builds two functions
#of which the first one you should run twice.
#Adapted from https://github.com/DrewWham/Genetic-Structure-Tools/blob/master/plotSTR.r

#Load libraries
library(stringr)
library(ggplot2)
library(data.table)
library(patchwork)
library(forcats)
library(viridis)
library(dplyr)

#Load file names
structure_file <- '~/Desktop/VermilionRF/VMSURF Species ID/structure_results/sunset_vermilion_flagged_05_06_2025.str'
structure_result <- '~/Desktop/VermilionRF/VMSURF Species ID/structure_results/sunset_vermilion_flagged_05_06_2025.K2.R4_f'
setwd('~/Desktop/VermilionRF/VMSURF Species ID/structure_results/')


STR.in = structure_file
STR.out = structure_result
run = 4

#function for extracting the cluster Probs, 
#requires STR infile because STRUCTURE likes to chop off the ends of your 
#sample names so I have to use your original file to get your original names
read.STR<-function(STR.in,STR.out,run){
  #read in data
  str<-read.table(STR.in)	
  str.out<-readLines(STR.out, warn=FALSE)
  #the next part parses the structure outfile and grabs the q score part
  q.tab.id <- grep("Inferred ancestry of individuals:",str.out,
                   value=FALSE)
  num.ind <- grep("Run parameters:",str.out,
                  value=FALSE)+1
  k.ind<-grep("populations assumed",str.out,
              value=FALSE)
  k<-str.out[k.ind]
  k<-as.numeric(str_extract_all(k,"[0-9.A-Za-z_]+")[[1]][1])
  
  num.ind<-str.out[num.ind]	 				 
  num.ind<-as.numeric(str_extract_all(num.ind,"[0-9.A-Za-z_]+")[[1]][1])
  q.end.id<-1+num.ind+q.tab.id		
  q.tab<-str.out[(q.tab.id+2):q.end.id]
  qs<-t(sapply(str_extract_all(q.tab,"[0-9.A-Za-z_]+"),as.character))
  
  cols_qs<-dim(qs)[2]
  
  qs<-qs[,c(2,(cols_qs-k+1):(cols_qs))]
  qs<-data.frame(qs)
  nclust<- k
  nrun <- run
  
  meta <- readxl::read_xlsx("~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_050825_AW.xlsx")
  #Add in metadata
  newid <- read.csv("~/Desktop/VermilionRF/VMSURF Species ID/QF_genotypes/newsampleID_allsamples_5_1_2025.csv")%>%
    select(c('Sample_ID',	'New_Sample_ID'))
  meta <- merge(newid, meta, by.x = 'Sample_ID', by.y = 'Specimen.Num', 
                all = F) %>%
    select(c('New_Sample_ID', 'Sample_ID'))
  qs <- merge(qs, meta, by.x = 'X1', by.y = 'New_Sample_ID', all = F)
  
  qs <- qs[,c(ncol(qs),1:nclust+1)]
  
  write.csv(qs,paste("k",nclust,"_R",run,"_05_2025.csv",sep=""),row.names=F)
}

read.STR(structure_file, structure_result, 10)





firstK <- '~/Desktop/VermilionRF/VMSURF Species ID/structure_results/k2_R10_05_2025.csv'
secondK <- '~/Desktop/VermilionRF/VMSURF Species ID/structure_results/sunset_only/k3_R4_02_2024.csv'



#This will plot just one structure plot SORTED BY POPULATION
plotoneSTR_pop_sort <-function(firstK){  
  #I turn off warnings because it will tell you about duplicated factors below 
  oldw <- getOption("warn")
  options(warn = -1)
  #read data
  data<-fread(firstK)
  meta <- readxl::read_xlsx("~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_050825_AW.xlsx")
  nclust<-dim(data)[2]-1
  #relabel the columns
  labs<-c("Sample",paste("Cluster",seq(1:nclust),sep=""))
  og.labs<-c('Sample_ID',paste("X",seq(from = 2, to = (nclust+1)),sep=""))
  setnames(data,og.labs,labs)
  #remove duplicates, you shouldnt have duplicates anyway
  data<-data[!duplicated(data$Sample),]
  
  #Add in metadata
  meta <- meta %>%
    select(c('Specimen.Num', 'Rubias Pop Call'))
  data <- merge(data, meta, by.y = 'Specimen.Num', by.x = 'Sample', all = F)
  
  #I melt here to make the plotting easy
  mdata<-melt(data)
  names(mdata)<-c("Sample",'pop',"Cluster","Probability")
  mdata$Sample<-factor(mdata$Sample,levels=data$Sample)
  
  print('Creating Structure Plot!')
  kplot <-
    ggplot(mdata, aes(x=factor(Sample), y=Probability, fill = factor(Cluster),
                      color = factor(Cluster))) +
    geom_col() +
    facet_grid(~fct_inorder(pop), switch = "x", scales = "free", space = "free") +
    theme_minimal() + labs(x = "Individuals", y = "Ancestry") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_brewer(palette = 'Pastel1')+
    scale_color_brewer(palette = 'Pastel1')
  
  
  return(kplot)
}

plotoneSTR_pop_sort(firstK)

plotoneSTR <-function(firstK){  
  #I turn off warnings because it will tell you about duplicated factors below 
  oldw <- getOption("warn")
  options(warn = -1)
  print('Reading in Data')
  #read data
  data<-fread(firstK)
  meta <- readxl::read_xlsx("~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_120823_AW.xlsx")
  nclust<-dim(data)[2]-1
  #relabel the columns
  labs<-c("Sample",paste("Cluster",seq(1:nclust),sep=""))
  og.labs<-c('Sample_ID',paste("X",seq(from = 2, to = (nclust+1)),sep=""))
  setnames(data,og.labs,labs)
  #remove duplicates, you shouldnt have duplicates anyway
  data<-data[!duplicated(data$Sample),]
  
  #Add in metadata
  meta <- meta %>%
    select(c('Specimen.Num', 'Rubias Species Call'))
  hybs <- read.delim('~/Desktop/VermilionRF/VMSURF Species ID/metadata/sunset_vermilion_hybrids.txt', header = F)
  meta[meta$Specimen.Num %in% hybs$V1,2] <- 'Hybrid'
  data <- merge(data, meta, by.y = 'Specimen.Num', by.x = 'Sample', all = F)
  
  #I melt here to make the plotting easy
  mdata<-melt(data)
  names(mdata)<-c("Sample",'pop',"Cluster","Probability")
  mdata$Sample<-factor(mdata$Sample,levels=data$Sample)
  
  print('Creating Structure Plot!')
  kplot <-
    ggplot(mdata, aes(x=factor(Sample), y=Probability, fill = factor(Cluster),
                      color = factor(Cluster))) +
    geom_col() +
    facet_grid(~fct_inorder(pop), switch = "x", scales = "free", space = "free") +
    theme_minimal() + labs(x = "Individuals", y = "Ancestry") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_brewer(palette = 'Pastel1')+
    scale_color_brewer(palette = 'Pastel1')
  
  mdata2 <- subset(mdata, mdata$pop == 'Hybrid')
  kplot2 <-
    ggplot(mdata2, aes(x=factor(Sample), y=Probability, fill = factor(Cluster),
                      color = factor(Cluster))) +
    geom_col() +
    facet_grid(~fct_inorder(pop), switch = "x", scales = "free", space = "free") +
    theme_minimal() + labs(x = "Individuals", y = "Ancestry") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_brewer(palette = 'Pastel1')+
    scale_color_brewer(palette = 'Pastel1')
  
  return(kplot2)
}

plotoneSTR(firstK)

##This will plot two structure plots on top of each other. 
#Returning the plots OR saving the plots into a PDF within the function worked
# once and then gave up. Worth trying again.

plottwoSTR <-function(firstK,secondK){  
  #I turn off warnings because it will tell you about duplicated factors below 
  oldw <- getOption("warn")
  options(warn = -1)
  #read data
  data<-fread(firstK)
  meta <- readxl::read_xlsx(
    "~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_110323_AW.xlsx")  %>%
    select(c('Specimen.Num', 'collection'))
  nclust<-dim(data)[2]-1
  #relabel the columns
  labs<-c("Sample",paste("Cluster",seq(1:nclust),sep=""))
  colnames(data) <- labs
  #remove duplicates, you shouldnt have duplicates anyway
  data<-data[!duplicated(data$Sample),]
  
  #Add in metadata
  data <- merge(data, meta, by.x = 'Sample', by.y = 'Specimen.Num', all = FALSE)
  
  #I melt here to make the plotting easy
  mdata<-melt(data)
  names(mdata)<-c("Sample",'collection',"Cluster","Probability")
  mdata$Sample<-factor(mdata$Sample,levels=data$Sample)
  
  #mdata_hybrids <- mdata[mdata$Probability > 0.6, ]
  #mdata_hybrids_list <- mdata_hybrids$Sample
  #mdata_hybrids_plot <- mdata[mdata$Sample %in% mdata_hybrids_list,]
  
  print('Plotting 1st K!')
  k2plot <-
    ggplot(mdata, aes(x=factor(Sample), y=Probability, fill = factor(Cluster),
                      color = factor(Cluster))) +
    geom_col() +
    facet_grid(~fct_inorder(collection), switch = "x", scales = "free", space = "free") +
    theme_minimal() + labs(x = "Individuals", y = "Ancestry") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_brewer(palette = 'Pastel1')+
    scale_color_brewer(palette = 'Pastel1')
  
  
  #read data
  data<-fread(secondK)
  plt.order<-fread(secondK)
  meta <- readxl::read_xlsx(
    "~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_110323_AW.xlsx")  %>%
    select(c('Specimen.Num', 'collection'))
  nclust<-dim(data)[2]-1
  #relabel the columns
  labs<-c("Sample",paste("Cluster",seq(1:nclust),sep=""))
  colnames(data) <- labs
  #remove duplicates, you shouldnt have duplicates anyway
  data<-data[!duplicated(data$Sample),]
  
  #Add in metadata
  data <- merge(data, meta, by.x = 'Sample', by.y = 'Specimen.Num', all = FALSE)
  
  #I melt here to make the plotting easy
  mdata<-melt(data)
  names(mdata)<-c("Sample",'collection',"Cluster","Probability")
  mdata$Sample<-factor(mdata$Sample,levels=data$Sample)
  
  print('Plotting 2nd K!')
  k3plot <-
    ggplot(mdata, aes(x=factor(Sample), y=Probability, fill = factor(Cluster), 
                      color = factor(Cluster))) +
    geom_col() +
    facet_grid(~fct_inorder(collection), switch = "x", scales = "free", space = "free") +
    theme_minimal() + labs(x = "Individuals", y = "Ancestry") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_brewer(palette = 'Pastel1') +
    scale_color_brewer(palette = 'Pastel1')
  
  return(k2plot/k3plot)
  print('Done!')
  
  
}
plottwoSTR(firstK, secondK)  



##

#####
#This is to subset the K3 to only visualize the hybrids

k3_data <- read.csv(secondK)
labs<-c("Sample",paste("Cluster",seq(1:3),sep=""))
og.labs<-paste("X",seq(1:4),sep="")
setnames(k3_data,og.labs,labs)


k3_data_hybrids_only <- k3_data[((k3_data$Cluster1 > 0.15) &
                                   (k3_data$Cluster1 < 0.85))|
                                  ((k3_data$Cluster2 > 0.15) &
                                     (k3_data$Cluster2 < 0.85)) |
                                  ((k3_data$Cluster3 > 0.15) &
                                     (k3_data$Cluster3 < 0.85)), ]

write.csv(k3_data_hybrids_only, 'k3_hybrids_only.csv')


##this function plots the structure plot, it takes the result of read.STR, and 
# will add in the old sample names and rubias results.
plotSTR_oneK_sorted_by_species_ID <-function(k3_hybrids){  
  #I turn off warnings because it will tell you about duplicated factors below 
  oldw <- getOption("warn")
  options(warn = -1)
  #read data
  data<-fread(k3_hybrids)
  #remove duplicates, you shouldnt have duplicates anyway
  data<-data[!duplicated(data$Sample),]
  #I melt here to make the plotting easy
  mdata<-melt(data)
  names(mdata)<-c("Sample","Species","Probability")
  mdata$Sample<-factor(mdata$Sample,levels=data$Sample)
  
  #mdata_hybrids <- mdata[mdata$Probability > 0.6, ]
  #mdata_hybrids_list <- mdata_hybrids$Sample
  #mdata_hybrids_plot <- mdata[mdata$Sample %in% mdata_hybrids_list,]
  
  
  k2plot <-
    ggplot(mdata, aes(x=factor(Sample), y=Probability, fill = factor(Species),
                      color = factor(Species))) +
    geom_col() +
    #facet_grid(~fct_inorder(Species), switch = "x", scales = "free", space = "free") +
    theme_minimal() + labs(x = "Individuals", y = "Ancestry") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = expand_scale(add = 1)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_brewer(palette = 'Pastel1')+
    scale_color_brewer(palette = 'Pastel1')
  
  
  
  return(k2plot)
  
  
}
plotSTR_oneK_sorted_by_species_ID(k3_hybrids)  


