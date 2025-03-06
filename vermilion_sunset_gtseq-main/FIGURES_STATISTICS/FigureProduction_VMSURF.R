### Plotting Figures from Paper #1

## This script is adapted from a multitude of other scripts for publishing on GitHub
### Author: Anita Wray
### Date: January 4, 2024

####Load required packages
packages <- c('rnaturalearth', 'sf', 'tidyverse', 'ggsignif', 'data.table', 'patchwork')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

theme_set(theme_classic())

#Set colors
pal <- PNWColors::pnw_palette('Sunset2', 4)
pal_species <- c('vermilion male' = "#C3B1E1",
                 'vermilion female' = "#882255",
                 'sunset male' = "#FEBA4F",
                 'sunset female' = "#F04F00",
                 'vermilion' = pal[1],
                 'sunset' = pal[3])
pal_pop_and_species <- PNWColors::pnw_palette('Cascades', 4)

##### Read in the master metadata file -------
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
metadata$Ages <- as.numeric(metadata$Ages)

breakz10 <- seq(from = 0, to = 320, by = 10)
breakz20 <- seq(from = 0, to = 320, by = 20)
metadata$Depth..m. <- as.numeric(metadata$Depth..m.)
metadata <- metadata %>% mutate(depthcat10 = cut(Depth..m., breaks=breakz10))
metadata <- metadata %>% mutate(depthcat20 = cut(Depth..m., breaks=breakz20))

##### Mapping - Fig 1 & Fig 5 ------------------------------------------------------------------
#Load in the base R map
world <- ne_countries(scale = "medium", returnclass = "sf")

fig1 <- ggplot(data = world)+
  geom_sf() +
  borders("state") +
  geom_point(data = metadata, aes(x = `LonDD.v2`, y = `LatDD`, col = `repunit`))+
  coord_sf(xlim = c(-115, -125.57), ylim = c(30, 48.84), expand = FALSE) +
  scale_x_continuous(breaks = c(-124, -120, -116)) +
  scale_color_manual(values = pal_species, breaks=c('vermilion', 'sunset'))+
  theme(strip.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        strip.text.x = element_text(
          size = 12, hjust = 0, face = "bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  facet_wrap(~factor(repunit, levels = c('vermilion', 'sunset')))+
  geom_text(label = 'sunset', x = 116, y = 47)

## Now let's look at just SoCal, where most of our datapoints are from
socal <- metadata %>% 
  filter(LatDD > 32 & LatDD < 36)

# Lets add in more depth information, and therefore we need a better/new color palette
cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc",
                             "#0D98BA","#4ab04a", "#C0FF02", "#ffd73e"))(10),
          colorRampPalette(c("#e29421", 
                             "#e29421", "#f05336",
                             "#ce472e", "#8B0000"))(10))

fig5 <- ggplot(data = world) +
  geom_sf() +
  geom_count(data = socal,
             aes(x = `LonDD.v2`, y = `LatDD`,
                 color = `Depth..m.`)) +
  coord_sf(xlim = c(-117, -121.5), ylim = c(31.75, 36), expand = FALSE)+
  facet_wrap(~repunit)+
  theme(legend.position = 'bottom',
        strip.background = element_blank(), 
        legend.title = element_blank(),
        strip.text.x = element_text(
          size = 12, hjust = 0, face = "bold"))+
  xlab('Longitude')+
  ylab('Latitude')+
  scale_x_continuous(breaks = c(-121, -120, -119, -118))+
  labs(color='Depth (m)')+
  scale_color_gradientn(colours=cols, limits=c(0, 315),
                        breaks=seq(0, 300, by=100), 
                        na.value=rgb(246, 246, 246, max=255))

fig1
fig5

##### Depth Distribution - Fig 2 & Fig 8 ---------------------------------------
metadata$repunit <- factor(metadata$repunit, levels = c('vermilion', 'sunset'))


fig8 <- ggplot(data = metadata[metadata$repunit != 'canary',],
       aes(x = `repunit`, y = `Depth..m.`,
           col = `repunit`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = pal_species, breaks = c('vermilion', 'sunset'))

#Create a age class column
sunverm <- metadata %>%
  mutate(ageclass = case_when(
    Ages <= 5 ~ '≤ 5 Years Old',
    Ages >5 ~ '> 5 Years Old')
  )
#Remove data absent individuals
sunverm_age <- sunverm[!is.na(sunverm$ageclass),]
#Plot
fig2 <- ggplot(data = sunverm_age,
       aes(x = `repunit`, y = `Depth..m.`)) +
  geom_boxplot(aes(fill = ageclass))+
  ylab('Depth (m)')+
  xlab('')+
  scale_color_manual(values = pal_species) +
  geom_signif(comparisons=list(c("vermilion", 'sunset')), 
              map_signif_level = TRUE,annotations="***",
              y_position = c(350,350)) +
  geom_signif(
    y_position = c(190, 330), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("***", "***"), tip_length = 0) +
  scale_y_continuous(limits = c(0,375))+
  guides(fill = guide_legend(title = "Age Class"))

fig2
fig8 

##### Cumulative and Relative Proportion - Fig 3 & Fig 4 --------

metadata <- subset(metadata, metadata$depthcat10 != "NA's")

sunset <- subset(metadata, metadata$repunit == 'sunset')
vermilion <- subset(metadata, metadata$repunit == 'vermilion')

df_sunset <- data.frame(summary(sunset$depthcat10))
colnames(df_sunset) <- 'Count Sunset'
df_sunset$depthcat10 <- row.names(df_sunset)
order <- row.names(df_sunset)
df_vermilion <- data.frame(summary(vermilion$depthcat10))
colnames(df_vermilion) <- 'Count Vermilion'
df_vermilion$depthcat10 <- row.names(df_vermilion)

df <- merge(df_vermilion, df_sunset)

df$perc_sun <- df$`Count Sunset`/(df$`Count Sunset`+df$`Count Vermilion`)
df$perc_ver <- df$`Count Vermilion`/(df$`Count Vermilion`+df$`Count Sunset`)

melt_df <- melt(df %>%
                  select(c('depthcat10', 'perc_sun', 'perc_ver')), 
                id = 'depthcat10')

melt_df$value <- gsub('NaN', 0, melt_df$value)
melt_df$value <- as.numeric(melt_df$value)
melt_df$variable <- gsub('perc_sun', 'sunset', melt_df$variable)
melt_df$variable <- gsub('perc_ver', 'vermilion', melt_df$variable)
melt_df <- melt_df %>%
  mutate(across(variable, factor, levels=c("vermilion","sunset")))



fig4 <- ggplot(data = melt_df, aes(x = factor(depthcat10, level = order), 
                           y = value, fill = variable)) +
  geom_bar(stat='identity', position="stack") +
  labs(x = 'Binned Depth', y = 'Relative Proportion')+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual(values = pal_species, name = 'Species') +
  scale_x_discrete(labels=c("(0,10]" = '0-10', "(10,20]" = '11-20',
                            "(20,30]" = '21-30', "(30,40]" = '31-40',
                            "(40,50]" = '41-50', "(50,60]" = '51-60',
                            "(60,70]" = '61-70', "(70,80]" = '71-80',
                            "(80,90]" = '81-90', "(90,100]" = '91-100',
                            "(100,110]" = '101-110', "(110,120]" = '111-120',
                            "(120,130]" = '121-130', "(130,140]" = '131-140',
                            "(140,150]" = '141-150', "(150,160]" = '151-160',
                            "(160,170]" = '161-170', "(170,180]" = '171-180',
                            "(180,190]" = '181-190', "(190,200]" = '191-200',
                            "(200,210]" = '201-210', "(210,220]" = '211-220',
                            "(220,230]" = '221-230', "(230,240]" = '231-240',
                            "(240,250]" = '241-250', "(250,260]" = '251-260',
                            "(260,270]" = '261-270', "(270,280]" = '271-280',
                            "(280,290]" = '281-290', "(290,300]" = '291-300',
                            "(300,310]" = '301-311', "(310,320]" = '311-320')) +
  scale_y_continuous(expand = c(0, 0))


df <- df %>%
  mutate(cum_ver = cumsum(`Count Vermilion`)/sum(`Count Vermilion`))
df <- df %>%
  mutate(cum_sun = cumsum(`Count Sunset`)/sum(`Count Sunset`))

df_sunset$cumulative_percentage <- 100*cumsum(df_sunset$`Count Sunset`)/sum(df_sunset$`Count Sunset`)
df_vermilion$cumulative_percentage <- 100*cumsum(df_vermilion$`Count Vermilion`)/sum(df_vermilion$`Count Vermilion`)

sunset_sorted_depth <- sort(sunset$Depth..m.)
d50_s <- sunset_sorted_depth[length(sunset_sorted_depth)/2]

vermilion_sorted_depth <- sort(vermilion$Depth..m.)
d50_v <- vermilion_sorted_depth[length(vermilion_sorted_depth)/2]

fig3 <- ggplot() +
  stat_ecdf(data = sunset, aes(Depth..m.), geom = "step", color = pal[3]) +
  stat_ecdf(data = vermilion, aes(Depth..m.), geom = 'step', color = pal[1]) +
  labs(x = 'Depth (m)') +
  scale_y_continuous(name = "Cumulative Proportion") +
  scale_x_continuous(name = 'Depth (m)') +
  labs(x = 'Depth (m)', y = 'Percent of Total Abundance', color = 'Species') +
  geom_vline(xintercept = d50_s, color = pal[3]) +
  geom_vline(xintercept = d50_v, color = pal[1])

fig3
fig4

##### Length Weight Curves - Fig 6 --------------------------------------------


fig6e <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'vermilion'&
                              metadata$Sex == 'Female'),
              aes(color = 'vermilion female'), method = 'loess') +
  stat_smooth(data = subset(metadata, metadata$repunit == 'vermilion'&
                              metadata$Sex == 'Male'),
              aes(color = 'vermilion male'), method = 'loess')+
  stat_smooth(data = subset(metadata, metadata$repunit == 'sunset' &
                              metadata$Sex == 'Female'),
              aes(color = 'sunset female'), method = 'loess') +
  stat_smooth(data = subset(metadata, metadata$repunit == 'sunset' &
                              metadata$Sex == 'Male'),
              aes(color = 'sunset male'), method = 'loess')  +
  theme_classic() +
  scale_color_manual(values = pal_species, breaks = c('vermilion female', 
                                                      'vermilion male',
                                                      'sunset female',
                                                      'sunset male')) +
  theme(legend.position = c(.1, .81)) +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')




fig6b <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'sunset' &
                             metadata$Sex == 'Female'),
             aes(color = 'sunset female')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'sunset' &
                              metadata$Sex == 'Female'),
              aes(color = 'sunset female'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'sunset' &
                             metadata$Sex == 'Male'),
             aes(color = 'sunset male')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'sunset' &
                              metadata$Sex == 'Male'),
              aes(color = 'sunset male'), method = 'loess') +
  theme_classic() +
  scale_color_manual(values = pal_species) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')

fig6a <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'vermilion' &
                             metadata$Sex == 'Female'),
             aes(color = 'vermilion female')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'vermilion' &
                              metadata$Sex == 'Female'),
              aes(color = 'vermilion female'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'vermilion' &
                             metadata$Sex == 'Male'),
             aes(color = 'vermilion male')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'vermilion' &
                              metadata$Sex == 'Male'),
              aes(color = 'vermilion male'), method = 'loess') +
  theme_classic() +
  scale_color_manual(values = pal_species) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')

fig6c <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'sunset' &
                             metadata$Sex == 'Male'),
             aes(color = 'sunset male')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'sunset' &
                              metadata$Sex == 'Male'),
              aes(color = 'sunset male'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'vermilion' &
                             metadata$Sex == 'Male'),
             aes(color = 'vermilion male')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'vermilion' &
                              metadata$Sex == 'Male'),
              aes(color = 'vermilion male'), method = 'loess') +
  theme_classic() +
  scale_color_manual(values = pal_species) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')

fig6d <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'sunset' &
                             metadata$Sex == 'Female'),
             aes(color = 'sunset female')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'sunset' &
                              metadata$Sex == 'Female'),
              aes(color = 'sunset female'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'vermilion' &
                             metadata$Sex == 'Female'),
             aes(color = 'vermilion female')) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'vermilion' &
                              metadata$Sex == 'Female'),
              aes(color = 'vermilion female'), method = 'loess') +
  theme_classic() +
  scale_color_manual(values = pal_species, breaks = c('vermilion female', 
                                                      'vermilion male',
                                                      'sunset female',
                                                      'sunset male')) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')


fig6 <- ((fig6a + fig6b)/(fig6c+fig6d))/fig6e + plot_annotation(tag_levels = 'a') +
  plot_layout(heights = c(1,1,2))

fig6



  ### Stats for VBG plot ---------------------
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

##### Von Bertalanffy Growth Curves - Fig 7 -----------------------------------
fig7e <- ggplot(data = AgeDat) +
  geom_line(data = VermM, aes(x = sort(VermM$Ages),
                              y = sort(lpVM), col = "vermilion male")) +
  geom_line(data = VermF, aes(x = sort(VermF$Ages),
                              y = sort(lpVF), col = "vermilion female")) +
  geom_line(data = SunM, aes(x = sort(SunM$Ages),
                             y = sort(lpSM), col = "sunset male")) +
  geom_line(data = SunF, aes(x = sort(SunF$Ages),
                             y = sort(lpSF), col = "sunset female")) +
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = c(.92, .35)) +
  scale_color_manual(values = pal_species, breaks = c('vermilion female', 
                                                      'vermilion male',
                                                      'sunset female',
                                                      'sunset male'))

fig7b <- ggplot(data = AgeDat) +
  geom_line(data = SunM, aes(x = sort(SunM$Ages),
                             y = sort(lpSM), col = "sunset male")) +
  geom_point(data = SunM, aes(x = SunM$Ages,
                              y = SunM$Fork.length..cm., col = "sunset male"))+
  geom_line(data = SunF, aes(x = sort(SunF$Ages),
                             y = sort(lpSF), col = "sunset female")) +
  geom_point(data = SunF, aes(x = SunF$Ages,
                              y = SunF$Fork.length..cm., col = "sunset female"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig7a <- ggplot(data = AgeDat) +
  geom_line(data = VermM, aes(x = sort(VermM$Ages),
                              y = sort(lpVM), col = "vermilion male")) +
  geom_point(data = VermM, aes(x = VermM$Ages,
                               y = VermM$Fork.length..cm., col = "vermilion male"))+
  geom_line(data = VermF, aes(x = sort(VermF$Ages),
                              y = sort(lpVF), col = "vermilion female")) +
  geom_point(data = VermF, aes(x = VermF$Ages,
                               y = VermF$Fork.length..cm., col = "vermilion female"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig7c <- ggplot(data = AgeDat) +
  geom_line(data = VermM, aes(x = sort(VermM$Ages),
                              y = sort(lpVM), col = "vermilion male")) +
  geom_point(data = VermM, aes(x = VermM$Ages,
                               y = VermM$Fork.length..cm., col = "vermilion male"))+
  geom_line(data = SunM, aes(x = sort(SunM$Ages),
                             y = sort(lpSM), col = "sunset male")) +
  geom_point(data = SunM, aes(x = SunM$Ages,
                              y = SunM$Fork.length..cm., col = "sunset male"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig7d <- ggplot(data = AgeDat) +
  geom_line(data = VermF, aes(x = sort(VermF$Ages),
                              y = sort(lpVF), col = "vermilion female")) +
  geom_point(data = VermF, aes(x = VermF$Ages,
                               y = VermF$Fork.length..cm., col = "vermilion female"))+
  geom_line(data = SunF, aes(x = sort(SunF$Ages),
                             y = sort(lpSF), col = "sunset female")) +
  geom_point(data = SunF, aes(x = SunF$Ages,
                              y = SunF$Fork.length..cm., col = "sunset female"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig7 <- ((fig7a + fig7b)/(fig7c+fig7d))/fig7e + plot_annotation(tag_levels = 'a') +
  plot_layout(heights = c(1,1,2))
              
fig7

#### Mean Weight & Length at Depth - Fig 9 ------------------------------------
sunset <- subset(metadata, metadata$repunit == 'sunset')
vermilion <- subset(metadata, metadata$repunit == 'vermilion')

df_sunset <- data.frame(summary(sunset$depthcat20))
colnames(df_sunset) <- 'Count Sunset'
df_sunset$depthcat <- row.names(df_sunset)
order <- row.names(df_sunset)
df_vermilion <- data.frame(summary(vermilion$depthcat20))
colnames(df_vermilion) <- 'Count Vermilion'
df_vermilion$depthcat <- row.names(df_vermilion)

sunset_wt <- aggregate(Wt..kg.~depthcat20, sunset, mean)
sunset_sd_wt <- aggregate(Wt..kg.~depthcat20, sunset, sd)

colnames(sunset_sd_wt) <- c('depthcat20', 'sd')

sunset_wt <- merge(sunset_wt, sunset_sd_wt)
colnames(sunset_wt) <- c('depthcat20', 'sun_wt', 'sun_sd')

vermilion_wt <- aggregate(Wt..kg.~depthcat20, vermilion, mean)
vermilion_sd_wt <- aggregate(Wt..kg.~depthcat20, vermilion, sd)

colnames(vermilion_sd_wt) <- c('depthcat20', 'sd')

vermilion_wt <- merge(vermilion_wt, vermilion_sd_wt)
colnames(vermilion_wt) <- c('depthcat20', 'verm_wt', 'verm_sd')

combined_wt <- merge(vermilion_wt, sunset_wt, all = TRUE)

fig9a <- ggplot(data = combined_wt) +
  geom_point(aes(y = sun_wt,
                 x = factor(depthcat20, level = order)),
             col = pal[3], alpha = 0.6) +
  geom_errorbar(data =combined_wt, aes(ymin = sun_wt-sun_sd,
                                       ymax = sun_wt+sun_sd,
                                       x = factor(depthcat20, level = order)),
                width = 0.1, col = pal[3], alpha = 0.6) +
  geom_point(data = combined_wt, aes(y = verm_wt,
                                     x = factor(depthcat20, level = order)),
             col = pal[1], alpha = 0.6) +
  geom_errorbar(data = combined_wt, aes(ymin = verm_wt-verm_sd,
                                        ymax = verm_wt+verm_sd,
                                        x = factor(depthcat20, level = order)),
                width = 0.1, col = pal[1], alpha = 0.6) +
  theme(axis.text.x = element_blank()) +
  labs(x = '', y = 'Average Weight (kg)') +
  scale_x_discrete(labels=c("(0,20]" = '0-21',
                            "(20,40]" = '21-40',
                            "(40,60]" = '41-60',
                            "(60,80]" = '61-80',
                            "(80,100]" = '81-100',
                            "(100,120]" = '101-120',
                            "(120,140]" = '121-140',
                            "(140,160]" = '141-160',
                            "(160,180]" = '161-180',
                            "(180,200]" = '181-200',
                            "(200,220]" = '201-220',
                            "(220,240]" = '221-240',
                            "(240,260]" = '241-260',
                            "(260,280]" = '261-280',
                            "(280,300]" = '281-300',
                            "(300,320]" = '301-320'))


sunset_ln <- aggregate(Fork.length..cm.~depthcat20, sunset, mean)
sunset_sd_ln <- aggregate(Fork.length..cm.~depthcat20, sunset, sd)

colnames(sunset_sd_ln) <- c('depthcat20', 'sd')

sunset_ln <- merge(sunset_ln, sunset_sd_ln)
colnames(sunset_ln) <- c('depthcat20', 'sun_ln', 'sun_sd')

vermilion_ln <- aggregate(Fork.length..cm.~depthcat20, vermilion, mean)
vermilion_sd_ln <- aggregate(Fork.length..cm.~depthcat20, vermilion, sd)

colnames(vermilion_sd_ln) <- c('depthcat20', 'sd')

vermilion_ln <- merge(vermilion_ln, vermilion_sd_ln)
colnames(vermilion_ln) <- c('depthcat20', 'verm_ln', 'verm_sd')

combined_ln <- merge(vermilion_ln, sunset_ln, all = TRUE)

fig9b <- ggplot(data = combined_ln) +
  geom_point(aes(y = sun_ln,
                 x = factor(depthcat20, level = order)),
             col = pal[3], alpha = 0.6) +
  geom_errorbar(data =combined_ln, aes(ymin = sun_ln-sun_sd,
                                       ymax = sun_ln+sun_sd,
                                       x = factor(depthcat20, level = order)),
                width = 0.1, col = pal[3], alpha = 0.6) +
  geom_point(data = combined_ln, aes(y = verm_ln,
                                     x = factor(depthcat20, level = order)),
             col = pal[1], alpha = 0.6) +
  geom_errorbar(data = combined_ln, aes(ymin = verm_ln-verm_sd,
                                        ymax = verm_ln+verm_sd,
                                        x = factor(depthcat20, level = order)),
                width = 0.1, col = pal[1], alpha = 0.6) +
  theme(axis.text.x = element_text(angle = 45, hjust =1)) +
  labs(x = 'Depth', y = 'Average Length (cm)') +
  scale_x_discrete(labels=c("(0,20]" = '0-21',
                            "(20,40]" = '21-40',
                            "(40,60]" = '41-60',
                            "(60,80]" = '61-80',
                            "(80,100]" = '81-100',
                            "(100,120]" = '101-120',
                            "(120,140]" = '121-140',
                            "(140,160]" = '141-160',
                            "(160,180]" = '161-180',
                            "(180,200]" = '181-200',
                            "(200,220]" = '201-220',
                            "(220,240]" = '221-240',
                            "(240,260]" = '241-260',
                            "(260,280]" = '261-280',
                            "(280,300]" = '281-300',
                            "(300,320]" = '301-320'))

fig9a/fig9b
