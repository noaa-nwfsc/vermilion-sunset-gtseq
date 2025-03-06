###Statistics from Paper #1

## This script is adapted from a multitude of other scripts for publishing on GitHub.
### Author: Anita Wray
### Date: January 4, 2024

####Load required packages
packages <- c('tidyverse', 'huxtable')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

theme_set(theme_classic())

## Metadata reading ------------------------------------------------------------
#Read in the master metadata file
#And the metadata with depths
metadata <- read.csv("~/Desktop/VermilionRF/VMSURF Species ID/metadata/passed_rubias_samples_metadata.csv")

#And remove canary samples
metadata <- metadata[metadata$repunit != 'canary',]
table(metadata$collection)
#Make sure we have continuous variables
metadata$Depth..m. <- as.numeric(metadata$Depth..m.)
metadata$LatDD <- as.numeric(metadata$LatDD)
metadata$LonDD.v2 <- as.numeric(metadata$LonDD.v2)
metadata$Fork.length..cm. <- as.numeric(metadata$Fork.length..cm.)

breakz10 <- seq(from = 0, to = 320, by = 10)
breakz20 <- seq(from = 0, to = 320, by = 20)
metadata$Depth..m. <- as.numeric(metadata$Depth..m.)
metadata <- metadata %>% mutate(depthcat10 = cut(Depth..m., breaks=breakz10))
metadata <- metadata %>% mutate(depthcat20 = cut(Depth..m., breaks=breakz20))

### Depth Significance testing -------------------------------------------------
t.test(metadata$Depth..m. ~ `repunit`,
       data = metadata)





### LW Per year - Table 1 ------------------------------------------------------
LWsunsetmale04.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2004"))
LWsunsetmale05.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2005"))
LWsunsetmale06.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2006"))
LWsunsetmale07.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2007"))
LWsunsetmale08.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2008"))
LWsunsetmale09.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2009"))
LWsunsetmale10.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2010"))
LWsunsetmale11.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2011"))
LWsunsetmale12.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2012"))
LWsunsetmale13.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2013"))
LWsunsetmale14.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2014"))
LWsunsetmale15.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2015"))
LWsunsetmale16.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2016"))
LWsunsetmale17.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2017"))
LWsunsetmale18.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2018"))
LWsunsetmale19.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2019"))
LWsunsetmale21.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2021"))
LWsunsetmale22.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Male" & Year=="2022"))


huxreg('2004'=LWsunsetmale04.fn, '2005'=LWsunsetmale05.fn, 
       '2006'=LWsunsetmale06.fn, '2007'=LWsunsetmale07.fn, 
       '2008'=LWsunsetmale08.fn, '2009'=LWsunsetmale09.fn, 
       '2010'=LWsunsetmale10.fn, '2011'=LWsunsetmale11.fn, 
       '2012'=LWsunsetmale12.fn, '2013'=LWsunsetmale13.fn, 
       '2014'=LWsunsetmale14.fn, '2015'=LWsunsetmale15.fn, 
       '2016'=LWsunsetmale16.fn, '2017'=LWsunsetmale17.fn, 
       '2018'=LWsunsetmale18.fn, '2019'=LWsunsetmale19.fn, 
       '2021'=LWsunsetmale21.fn, '2022'=LWsunsetmale22.fn, 
       statistics = c("N" = "nobs", "R2" = "r.squared"))%>% 
  set_caption("Sunset Male LW per year")

LWsunsetfemale04.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2004"))
LWsunsetfemale05.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2005"))
LWsunsetfemale06.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2006"))
LWsunsetfemale07.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2007"))
LWsunsetfemale08.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2008"))
LWsunsetfemale09.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2009"))
LWsunsetfemale10.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2010"))
LWsunsetfemale11.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2011"))
LWsunsetfemale12.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2012"))
LWsunsetfemale13.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2013"))
LWsunsetfemale14.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2014"))
LWsunsetfemale15.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2015"))
LWsunsetfemale16.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2016"))
LWsunsetfemale17.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2017"))
LWsunsetfemale18.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2018"))
LWsunsetfemale19.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2019"))
LWsunsetfemale21.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2021"))
LWsunsetfemale22.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="sunset" & Sex=="Female" & Year=="2022"))

huxreg('2004'=LWsunsetfemale04.fn, '2005'=LWsunsetfemale05.fn, 
       '2006'=LWsunsetfemale06.fn, '2007'=LWsunsetfemale07.fn, 
       '2008'=LWsunsetfemale08.fn, '2009'=LWsunsetfemale09.fn, 
       '2010'=LWsunsetfemale10.fn, '2011'=LWsunsetfemale11.fn, 
       '2012'=LWsunsetfemale12.fn, '2013'=LWsunsetfemale13.fn, 
       '2014'=LWsunsetfemale14.fn, '2015'=LWsunsetfemale15.fn, 
       '2016'=LWsunsetfemale16.fn, '2017'=LWsunsetfemale17.fn, 
       '2018'=LWsunsetfemale18.fn, '2019'=LWsunsetfemale19.fn, 
       '2021'=LWsunsetfemale21.fn, '2022'=LWsunsetfemale22.fn, 
       statistics = c("N" = "nobs", "R2" = "r.squared"))%>% 
  set_caption("Sunset Female LW per year")

LWvermilionmale04.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2004"))
LWvermilionmale05.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2005"))
LWvermilionmale06.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2006"))
LWvermilionmale07.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2007"))
LWvermilionmale08.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2008"))
LWvermilionmale09.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2009"))
LWvermilionmale10.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2010"))
LWvermilionmale11.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2011"))
LWvermilionmale12.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2012"))
LWvermilionmale13.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2013"))
LWvermilionmale14.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2014"))
LWvermilionmale15.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2015"))
LWvermilionmale16.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2016"))
LWvermilionmale17.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2017"))
LWvermilionmale18.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2018"))
LWvermilionmale19.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2019"))
LWvermilionmale21.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2021"))
LWvermilionmale22.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Male" & Year=="2022"))

huxreg('2004'=LWsunsetmale04.fn, '2005'=LWvermilionmale05.fn, 
       '2006'=LWvermilionmale06.fn, '2007'=LWvermilionmale07.fn, 
       '2008'=LWvermilionmale08.fn, '2009'=LWvermilionmale09.fn, 
       '2010'=LWvermilionmale10.fn, '2011'=LWvermilionmale11.fn, 
       '2012'=LWvermilionmale12.fn, '2013'=LWvermilionmale13.fn, 
       '2014'=LWvermilionmale14.fn, '2015'=LWvermilionmale15.fn, 
       '2016'=LWvermilionmale16.fn, '2017'=LWvermilionmale17.fn, 
       '2018'=LWvermilionmale18.fn, '2019'=LWvermilionmale19.fn, 
       '2021'=LWvermilionmale21.fn, '2022'=LWvermilionmale22.fn, 
       statistics = c("N" = "nobs", "R2" = "r.squared"))%>% 
  set_caption("Vermilion Male LW per year")

LWvermilionfemale04.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2004"))
LWvermilionfemale05.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2005"))
LWvermilionfemale06.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2006"))
LWvermilionfemale07.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2007"))
LWvermilionfemale08.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2008"))
LWvermilionfemale09.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2009"))
LWvermilionfemale10.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2010"))
LWvermilionfemale11.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2011"))
LWvermilionfemale12.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2012"))
LWvermilionfemale13.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2013"))
LWvermilionfemale14.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2014"))
LWvermilionfemale15.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2015"))
LWvermilionfemale16.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2016"))
LWvermilionfemale17.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2017"))
LWvermilionfemale18.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2018"))
LWvermilionfemale19.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2019"))
LWvermilionfemale21.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2021"))
LWvermilionfemale22.fn <- lm(log(Wt..kg.) ~ log(Fork.length..cm.), data=subset(metadata, repunit=="vermilion" & Sex=="Female" & Year=="2022"))


huxreg('2004'=LWvermilionfemale04.fn, '2005'=LWvermilionfemale05.fn, 
       '2006'=LWvermilionfemale06.fn, '2007'=LWvermilionfemale07.fn, 
       '2008'=LWvermilionfemale08.fn, '2009'=LWvermilionfemale09.fn, 
       '2010'=LWvermilionfemale10.fn, '2011'=LWvermilionfemale11.fn, 
       '2012'=LWvermilionfemale12.fn, '2013'=LWvermilionfemale13.fn, 
       '2014'=LWvermilionfemale14.fn, '2015'=LWvermilionfemale15.fn, 
       '2016'=LWvermilionfemale16.fn, '2017'=LWvermilionfemale17.fn, 
       '2018'=LWvermilionfemale18.fn, '2019'=LWvermilionfemale19.fn, 
       '2021'=LWvermilionfemale21.fn, '2022'=LWvermilionfemale22.fn, 
       statistics = c("N" = "nobs", "R2" = "r.squared"))%>% 
  set_caption("Vermilion Female LW per year")






### LW for the eight species/sex/area permutations- Table 2 --------------------
metadata$logL <- log(metadata$Fork.length..cm.)
metadata$logW <- log(metadata$Wt..kg.)

##Nocal
NoCal <- metadata %>%
  subset(latcat == 'NoCal')
SoCal <- metadata %>%
  subset(latcat == 'SoCal')

lm_sunset_nocal_male <- lm(logW~logL, 
                           data = subset(NoCal, 
                                         repunit == 'sunset' & Sex == 'Male'))
lm_sunset_nocal_female <- lm(logW~logL, 
                             data = subset(NoCal, 
                                           repunit == 'sunset' & Sex == 'Female'))
lm_vermilion_nocal_male <- lm(logW~logL, 
                              data = subset(NoCal, 
                                            repunit == 'vermilion' & Sex == 'Male'))
lm_vermilion_nocal_female <- lm(logW~logL, 
                                data = subset(NoCal, 
                                              repunit == 'vermilion' & Sex == 'Female'))
lm_sunset_socal_male <- lm(logW~logL, 
                           data = subset(SoCal, 
                                         repunit == 'sunset' & Sex == 'Male'))
lm_sunset_socal_female <- lm(logW~logL, 
                             data = subset(SoCal, 
                                           repunit == 'sunset' & Sex == 'Female'))
lm_vermilion_socal_male <- lm(logW~logL, 
                              data = subset(SoCal, 
                                            repunit == 'vermilion' & Sex == 'Male'))
lm_vermilion_socal_female <- lm(logW~logL, 
                                data = subset(SoCal, 
                                              repunit == 'vermilion' & Sex == 'Female'))

huxreg(statistics = c("N" = "nobs", "R2" = "r.squared"),
       'VM-NC-M' = lm_vermilion_nocal_male,'VM-SC-M' = lm_vermilion_socal_male,
       'VM-NC-F' = lm_vermilion_nocal_female,'VM-SC-F' = lm_vermilion_socal_female,
       'SU-NC-M' = lm_sunset_nocal_male, 'SU-SC-M' = lm_sunset_socal_male,
       'SU-NC-F' = lm_sunset_nocal_female,'SU-SC-F' = lm_sunset_socal_female 
        ) %>% 
  set_caption("Table 2")



### VB Growth for the eight species/sex/area permutations- Table 3 ------------
removed_individuals <- c('H-05-AG-V0260', 'H-05-AG-V0242')

AgeDat <- metadata[!metadata$Specimen.Num %in% removed_individuals,]

AgeDat <- AgeDat %>% mutate(latcat = case_when(
  LatDD < 34.27 ~ 'SoCal',
  LatDD > 34.27 & LatDD < 42 ~ 'NoCal',
  LatDD > 42 & LatDD < 46 ~ 'Oregon',
  LatDD > 46 ~ 'Washington')
)

AgeDat <- AgeDat[!is.na(AgeDat$Ages),]
AgeDat <- AgeDat[!is.na(AgeDat$Fork.length..cm.),]
AgeDat <- AgeDat[!is.na(AgeDat$Wt..kg.),]

table(AgeDat$latcat)
# First I'm going to subset the data into the 4 permutations
VermM <- subset(AgeDat, Sex=="Male" & repunit=="vermilion")
VermF <- subset(AgeDat, Sex=="Female" & repunit=="vermilion")
SunM <- subset(AgeDat, Sex=="Male" & repunit=="sunset")
SunF <- subset(AgeDat, Sex=="Female" & repunit=="sunset")

theta <- c(55, 0.15, -1.0)  #theta is the vector of VB model parameters
SSQ <- function(theta, xdat) {
  Linf <- theta[1]
  K <- theta[2]
  t0 <- theta[3]
  epsilon <- rep(0, length(xdat$Ages))
  lpred <- rep(0, length(xdat$Ages))
  for (i in 1:length(xdat$Ages)) {
    lpred[i] <- Linf * (1 - exp(-K * (xdat$Ages[i] - t0)))
    epsilon[i] <- (xdat$Fork.length..cm.[i] - lpred[i])^2
  }
  ssq <- sum(epsilon)
  return(ssq)
}

#out <- optim(theta, fn = SSQ, method = "BFGS", x = Ages, hessian = TRUE)
#out$V <- solve(out$hessian)  #solve the hessian
#out$S <- sqrt(diag(out$V))  #Standard Error
#out$R <- out$V/(out$S %o% out$S)  #Correlation

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = VermM, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpVM <- out$par[1] * (1 - exp(-out$par[2] * (VermM$Ages - out$par[3])))

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = VermF, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpVF <- out$par[1] * (1 - exp(-out$par[2] * (VermF$Ages - out$par[3])))

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = SunM, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpSM <- out$par[1] * (1 - exp(-out$par[2] * (SunM$Ages - out$par[3])))

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = SunF, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S

#For plotting - may have to specify in advance for each permutation
lpSF <- out$par[1] * (1 - exp(-out$par[2] * (SunF$Ages - out$par[3])))

vb <- vbFuns(param="Typical")

NoCal <- subset(AgeDat, latcat == 'NoCal')
SoCal <- subset(AgeDat, latcat == 'SoCal')

#Nocal sunset male
f.starts_nocal_sunset_male <- vbStarts(data=subset(NoCal, 
                                                   Sex == 'Male' & repunit == 'sunset'), 
                                       Fork.length..cm.~Ages)
f.fit_nocal_sunset_male <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                               data = subset(NoCal, 
                                             Sex == 'Male' & repunit == 'sunset'), 
                               start = f.starts_nocal_sunset_male)

#Nocal sunset female
f.starts_nocal_sunset_female <- vbStarts(data=subset(NoCal, 
                                                     Sex == 'Female' & repunit == 'sunset'), 
                                         Fork.length..cm.~Ages)
f.fit_nocal_sunset_female <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                                 data = subset(NoCal, 
                                               Sex == 'Female' & repunit == 'sunset'), 
                                 start = f.starts_nocal_sunset_female)

#Socal sunset male
f.starts_socal_sunset_male <- vbStarts(data=subset(SoCal, 
                                                   Sex == 'Male' & repunit == 'sunset'), 
                                       Fork.length..cm.~Ages)
f.fit_socal_sunset_male <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                               data = subset(SoCal, 
                                             Sex == 'Male' & repunit == 'sunset'), 
                               start = f.starts_socal_sunset_male)

#Socal sunset female
f.starts_socal_sunset_female <- vbStarts(data=subset(SoCal, 
                                                     Sex == 'Female' & repunit == 'sunset'), 
                                         Fork.length..cm.~Ages)
f.fit_socal_sunset_female <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                                 data = subset(SoCal, 
                                               Sex == 'Female' & repunit == 'sunset'), 
                                 start = f.starts_socal_sunset_female)


#Nocal vermilion male
f.starts_nocal_vermilion_male <- vbStarts(data=subset(NoCal, 
                                                      Sex == 'Male' & repunit == 'vermilion'),
                                          Fork.length..cm.~Ages)
f.fit_nocal_vermilion_male <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                                  data = subset(NoCal, 
                                                Sex == 'Male' & repunit == 'vermilion'), 
                                  start = f.starts_nocal_vermilion_male)

#Nocal vermilion female
f.starts_nocal_vermilion_female <- vbStarts(data=subset(NoCal, 
                                                        Sex == 'Female' & repunit == 'vermilion'), 
                                            Fork.length..cm.~Ages)
f.fit_nocal_vermilion_female <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                                    data = subset(NoCal, 
                                                  Sex == 'Female' & repunit == 'vermilion'), 
                                    start = f.starts_nocal_vermilion_female)

#SoCal vermilion male
f.starts_socal_vermilion_male <- vbStarts(data=subset(
  SoCal, Sex == 'Male' & repunit == 'vermilion'), Fork.length..cm.~Ages)
f.fit_socal_vermilion_male <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                                  data = subset(
                                    SoCal, Sex == 'Male' & repunit == 'vermilion'), 
                                  start = f.starts_socal_vermilion_male)

#Socal vermilion female
f.starts_socal_vermilion_female <- vbStarts(data=subset(SoCal, Sex == 'Female' & repunit == 'vermilion'), 
                                            Fork.length..cm.~Ages)
f.fit_socal_vermilion_female <- nls(Fork.length..cm.~vb(Ages,Linf,K,t0),
                                    data = subset(SoCal, Sex == 'Female' & repunit == 'vermilion'), 
                                    start = f.starts_socal_vermilion_female)


huxreg(statistics = NULL,
       'VM-NC-M' = f.fit_nocal_vermilion_male,'VM-SC-M'= f.fit_socal_vermilion_male,
       'VM-NC-F' = f.fit_nocal_vermilion_female,'VM-SC-F'= f.fit_socal_vermilion_female,
       'SU-NC-M' = f.fit_nocal_sunset_male, 'SU-SC-M'= f.fit_socal_sunset_male,
       'SU-NC-F' = f.fit_nocal_sunset_female,'SU-SC-F'= f.fit_socal_sunset_female
        )


### Sex Ratio - Table 4 --------------------------------------------------------
sex_ratio <- metadata %>%
  subset(Survey=='H&L') %>%
  select(c('repunit', 'Year', 'Sex')) %>%
  subset(Sex!="") %>%
  subset(Sex!='Unknown')
sex_ratio$repunit <- gsub('vermilion', 'VM', sex_ratio$repunit)
sex_ratio$repunit <- gsub('sunset', 'SU', sex_ratio$repunit)
sex_ratio$Sex <- gsub('Female', 'F', sex_ratio$Sex)
sex_ratio$Sex <- gsub('Male', 'M', sex_ratio$Sex)
sex_ratio$species_sex <- paste(sex_ratio$repunit, sex_ratio$Sex, sep ='')

count_sex_per_year <- as.data.frame(table(sex_ratio$species_sex, sex_ratio$Year))
colnames(count_sex_per_year) <- c('SP', 'Year', 'Count')

proper_df <- count_sex_per_year %>%
  group_by(SP,Year) %>%
  pivot_wider(names_from = SP, values_from = Count, values_fill = 0)
proper_df$SUF_prop <- round((proper_df$SUF/(proper_df$SUF+proper_df$SUM)*100),3)
proper_df$SUM_prop <- round((proper_df$SUM/(proper_df$SUF+proper_df$SUM)*100),3)
proper_df$VMF_prop <- round((proper_df$VMF/(proper_df$VMF+proper_df$VMM)*100),3)
proper_df$VMM_prop <- round((proper_df$VMM/(proper_df$VMF+proper_df$VMM)*100),3)

col_order <- c('Year', 'VMF_prop', 'VMF',  'VMM_prop', 'VMM',
               'SUF_prop', 'SUF', 'SUM_prop', 'SUM')
table4 <- proper_df[, col_order]
table4

mean(table4$SUM_prop)
