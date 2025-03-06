#Author: Anita Wray
#Date: Mar 2023
#Purpose: Hybrid Detection
library(hybriddetective)
library(genepopedit)
library(parallel)
library(tidyverse)

setwd("~/Desktop/VermilionRF/hybriddetective")
genepop <- '~/Desktop/VermilionRF/QF_genotypes/nocanary_genepop_w_hybrids.txt'

genepop_df <- read.delim(file = genepop,
                         header = FALSE,
                         quote = "",
                         stringsAsFactors = FALSE, 
                         skip = 197,
                         sep = ' ')
#Use this to double check that the genepop looks alright
#genepop_detective(genepop, variable = 'Loci')
#genepop_detective(genepop, variable = 'Pops')

#Now load in the structure file to get pure individuals
k2 <- read.csv('~/Desktop/VermilionRF/structure_results/sunset_and_verm/k2.csv')

#Make sure these are the same length
nrow(k2) == nrow(genepop_df)

#Get the names of samples with 100% ancestry
pure_indv_sun <- k2[k2$X2 > .95,1]
pure_indv_ver <- k2[k2$X3 > .95,1]
random_ver <- pure_indv_ver %>%
  sample(size = 10)
random_sun <- pure_indv_sun %>%
  sample(size = 10)

hybridk2 <- k2[k2$X2 <= .95 & k2$X3 <= .95,]

randoms <- k2[k2$X1 %in% random_sun | k2$X1 %in% random_ver,]

hybrid_plus_pure <- rbind(hybridk2, randoms)

write.csv(hybrid_plus_pure, file = '~/Desktop/newhybrids_tests/with_pure/structure_results_95_plus_pure.csv')

#Combine them into a vector

individuals <- genepop_df[genepop_df$V1 %in% hybrid_plus_pure$X1,1]


write.table(individuals, sep = '\n', file = '~/Desktop/newhybrids_tests/with_pure/individuals.txt', 
            quote = FALSE, row.names = FALSE, col.names = FALSE)

#Get two genepop files, one with the 200 pure references and one with the other
# ~21k individuals
subset_genepop_individual(genepop = genepop,
                          indiv = hybrid_plus_pure,
                          keep = TRUE,
                          path = '~/Desktop/newhybrids_tests/with_pure/hybrids_ID.txt')

##### For this part, you must copy the output from the above function to 
# parallelize new hybrids

#Run parallel new hybrids
parallelnh_OSX2(folder.data = '/Users/anita.wray/Desktop/newhybrids_tests/95/analysis/',
               where.NH = '/Users/anita.wray/Desktop/Applications/newhybrids/',
               burnin = 10000, 
               sweeps = 10000) #Ramp up once it is running

nh_multiplotR(NHResults = '~/Desktop/newhybrids_tests/95/analysis/NH.Results/')






parallelnh_OSX2 <- function(folder.data, where.NH, burnin, sweeps){
  
  useroptions <- options()
  
  options(scipen = 999) ### if the number of digits is too big in either burnin or sweeps
  ## R will output in scientific notation, which is not interpreted correctly by NHm - will change back on exit
  
  
  files.anal <- list.files(path = folder.data)
  
  path.indiv.file <- NULL
  indiv.file.exists <- grep(pattern = "individuals.txt", x = files.anal)
  
  if(indiv.file.exists > 0){
    path.indiv.file <- paste0(folder.data, files.anal[indiv.file.exists])
    files.anal <- files.anal[-indiv.file.exists]
  }
  if(indiv.file.exists == 0){
    writeLines("Note: An individual file has not been provided. Please refer to the help file for more information")
  }
  
  print("Creating Temp folder")
  dir.create(path = paste0(folder.data, "NH.Temp"))
  where.temp <- paste0(folder.data, "NH.Temp", "/")
  
  
  ## create a new folder to put the results in - remember the inception thing - be smart
  dir.create(path = paste0(folder.data, "NH.Results"))
  res.path.make <- paste0(folder.data, "NH.Results") ## get the path to the new results folder
  
  h.cores <- detectCores()
  
  ## Copy the entire NH folder i times to the temp folder you made
  ## use a loop here because then can iteratively name the copies so that the file.copy doesn't
  ## lose it because you have things named the same - also - how else are you goign to ID things later?
  
  NH.loc.vec <- paste(where.temp, "newhybrids", sep = "/") ##
  for(i in 1:length(files.anal)){
    
    file.copy(from = where.NH, to = where.temp, recursive = TRUE) ## copies whole folder
    
    NH.rename <- paste(NH.loc.vec, i, sep="_") ## get vector of where the folder is, then add a number to it
    
    file.rename(from = NH.loc.vec, to = NH.rename) ## rename
    
    print(paste0("Copying NH folder ", i))
  }
  
  
  ## Copy the files to be analzyed into the NH folder copies - one file per copy
  NH.copies <- list.files(where.temp) ## list of copies of NH in the temp folder
  
  for(j in 1:length(files.anal)){ ## for each file to be analyzed
    
    
    to.file <- NH.copies[j] ## what copy of NH do you copy the file to be analyzed to?
    from.file <- files.anal[j] ## the file to be copied
    
    to.dir <- paste0(where.temp, to.file, "/") ## where is the NH copy - this is a directory
    from.dir <- paste0(folder.data, from.file) ### where is the data file to be copied
    
    file.copy(from = from.dir, to = to.dir) ## copy it on ovah
    
  }
  
  ## once NH has run, need to extract (copy) the output
  
  ### get a list of all the copies of NH you have made etc.
  NH.copy.list <- list.files(path = where.temp)
  # NH.copy.list # for internal code checking
  
  
  ## now to execute the command
  
  ## add in randomized seeds - the NH guide says they should be small.
  
  r.seed <- sample(x = c(1:10), size = 2)
  
  do.seed <- c("--seeds", r.seed)
  burnin.do <- paste("--burn-in", burnin, sep=" ")
  sweeps.do <- paste("--num-sweeps", sweeps, sep=" ")
  ## replace with files.anal
  where.temp2 <- gsub(x = where.temp, pattern = " ", replacement = "\\ ", fixed = T)
  jobs.vector <- NULL
  
  for(b in 1:length(NH.copy.list)){
    
    b.copy <- NH.copy.list[b]
    file.do <- paste("-d", files.anal[b])
    what.temp <- paste0(where.temp2, b.copy)
    path.hold <- paste("cd", paste0(what.temp, ";"), "/Users/anita.wray/Desktop/Applications/newhybrids/newhybsng", file.do, burnin.do, sweeps.do, "--no-gui", sep = " ")
    jobs.vector <- c(jobs.vector, path.hold)
    
  }
  
  
  if(length(jobs.vector)<h.cores){
    mclapply(X = jobs.vector, FUN = system, mc.cores = length(jobs.vector))
  }
  if(length(jobs.vector)>h.cores){
    mclapply(X = jobs.vector, FUN = system, mc.cores = h.cores)
  }
  
  
  ## get the results generated for each copy of NH/file to be analyzed
  for(k in 1:length(NH.copy.list)){
    
    ## make a nice name for the folders of resutls we are goign to make - so so pretty
    res.name <- paste0(files.anal[k], "_Results") # file name + results - not as pretty as it could be
    dir.create(path = paste0(res.path.make, "/", res.name)) ## make that folder to put the resutls in
    
    where.the.results.to <- paste0(res.path.make, "/", res.name, "/") ## where did you make that folder?
    
    ## copy in the proper data file from which the sims were made
    file.copy(from = paste0(folder.data, files.anal[k]), to = where.the.results.to)
    ## copy in the individual file
    file.copy(from = paste0(path.indiv.file), to = where.the.results.to)
    
    NH.copy.to.get <- NH.copy.list[k] ## what copy of NH are we goign to look in?
    find.res.vec <- list.files(path = paste0(where.temp, NH.copy.to.get), pattern = "aa-") ## find all the files that
    ## begin with "aa-" inside that copy of NH - big shout out to Eric Anderson for using a regular string in the results name!
    
    find.res.vec.path <- paste0(paste0(where.temp, NH.copy.to.get, "/"), find.res.vec) ## find the path to all the files ID'd
    file.copy(from = find.res.vec.path, to = where.the.results.to) ## copy those files to the proper results folder
    
    ## keep making it pretty by renaming the results files so you know what data set they are associated with
    rename.vec <- gsub(x = find.res.vec, pattern = "aa-", replacement = paste0(files.anal[k], "_"))
    
    ## again - need a vectr that shows their computr postion
    rename.vec.path <- paste0(where.the.results.to, rename.vec) ## vector with new names
    old.name.vec.path <- paste0(where.the.results.to, find.res.vec) ## vector with old names
    
    file.rename(from = old.name.vec.path, to = rename.vec.path) ## check that vectorized functionality in file.rename - mad props!
  }
  
  unlink(paste0(folder.data, "NH.Temp", "/"), recursive = TRUE)
  
  
  on.exit(options(useroptions))
  
}
