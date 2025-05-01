### Creates presentation ready figures
library(readxl)
library(dplyr)
library(viridis)
library(tidyverse)
library(patchwork)
library(plotrix)

setwd("~/Desktop/VermilionRF/VMSURF Species ID/")

in_file <- "~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_040125_AW.xlsx"

#Set colors
pal <- PNWColors::pnw_palette('Sunset2', 4)
color_pal_hybrids <- c(
                 'vermilion' = pal[1],
                 'sunset' = pal[3], 
                 'hybrid' = '#ee6b6e')

#Now lets load in metadata
passed_metadata <- read_xlsx(in_file) 

table(passed_metadata$`Rubias Species Call`)

length(!is.na(passed_metadata$`Rubias Species Call`))


meta <- passed_metadata[(passed_metadata$`Rubias Species Call` != 'canary' | is.na(passed_metadata$`Rubias Species Call`)),]
meta <- meta[(meta$`Rubias Species Call` != 'bocaccio' | is.na(meta$`Rubias Species Call`)),]

#Make sure depth is a continuous variable
meta$Depth..m. <- as.numeric(meta$Depth..m.)
meta$Fork.length..cm. <- as.numeric(meta$Fork.length..cm.) 

#Convert hybrid samples to hybrid ID

hybrids <- read.csv("~/Desktop/VermilionRF/VMSURF Species ID/newhybrids/ver_sun_w_hybrids/PofZ_results.csv") %>%
  subset(hybridclass != 'PV') %>%
  subset(hybridclass != 'PS') %>%
  select(c('individuals', 'hybridclass'))

hybrid_id <- hybrids$individuals

hybrid_id <- gsub("_", "-", hybrid_id)


meta$`Rubias Species Call`[meta$Specimen.Num %in% hybrid_id] <- 'hybrid'
meta$`Rubias Pop Call`[meta$Specimen.Num %in% hybrid_id] <- 'hybrid'

hybs_2023 <- c('H-23-AG-V0393','H-23-AG-V0406','H-23-AG-V0556','H-23-MI-V0386','H-23-MI-V0437',
               'H-23-MI-V0562','H-23-MI-V0652','H-23-TO-V0487','H-23-TO-V0492')

meta$`Rubias Species Call`[meta$Specimen.Num %in% hybs_2023] <- 'hybrid'
meta$`Rubias Pop Call`[meta$Specimen.Num %in% hybs_2023] <- 'hybrid'

table(meta$`Rubias Species Call`)

meta <- meta %>%
  drop_na(`Rubias Species Call`) %>%
  subset(`Rubias Species Call` != 'rebs')


### map

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  borders("state") +
  geom_point(data = meta, aes(x = `LonDD.v2`, 
                                         y = `LatDD`, 
                                         col = factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion'))))+
  coord_sf(xlim = c(-115, -125.57), ylim = c(30, 48.84), expand = FALSE) +
  scale_x_continuous(breaks = c(-124, -120, -116)) +
  scale_color_manual(values = color_pal_hybrids)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = c(0.9, 0.87),
    legend.title=element_blank(),
    legend.spacing = unit(0, "pt"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())+
  facet_wrap(~factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion')))

df <- as.data.frame.matrix(table(meta$Site.Cell.ID, meta$`Rubias Species Call`, useNA = "ifany")) %>%
  subset(hybrid > 0 & vermilion == 0) 

#Create a column which groups the samples into three size classes
#Based on email convo with John Harms
sunverm_hybrid <- meta %>% 
  mutate(sizeclass = case_when(
    Fork.length..cm. >= 35 ~ 'Adult',
    Fork.length..cm. < 35 ~ 'Subadult')
  )
#Remove data absent samples
sunverm_hybrid_size <- sunverm_hybrid[!is.na(sunverm_hybrid$sizeclass),]

df2 <- as.data.frame.matrix(table(sunverm_hybrid_size$Site.Cell.ID, sunverm_hybrid_size$`Rubias Species Call`, useNA = "ifany"))

sunverm_hybrid_adult <- subset(sunverm_hybrid_size, sunverm_hybrid_size$sizeclass == 'Adult')
#Plot
depth <- ggplot(data = meta,
       aes(x = factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion')), 
           y = `Depth..m.`,
           col = `Rubias Species Call`)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = 'none') +
  #facet_wrap(~sizeclass) + 
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)+
  ylim(0,350)

(depth_and_size <- ggplot(data = sunverm_hybrid_size,
       aes(x = factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion')), 
           y = `Depth..m.`,
           col = `Rubias Species Call`)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = 'none') +
  facet_wrap(~sizeclass) + 
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)+
  ylim(0,350))

#Create a age class column
sunverm_hybrid <- sunverm_hybrid %>% 
  mutate(ageclass = case_when(
    Ages <= 5 ~ 'Under 5',
    Ages >5 ~ 'Over 5')
  )
#Remove data absent individuals
sunverm_hybrid_age <- sunverm_hybrid[!is.na(sunverm_hybrid$ageclass),]
#Plot
depth_and_age <- ggplot(data = sunverm_hybrid_age,
       aes(x = factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion')),
           y = `Depth..m.`,
           col = `Rubias Species Call`)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = 'none') +
  facet_wrap(~ageclass) + 
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)

#Two-way anova of adult and subadult species depth differences
adult <- sunverm_hybrid_size[sunverm_hybrid_size$sizeclass == 'Adult',]
subadult <- sunverm_hybrid_size[sunverm_hybrid_size$sizeclass == 'Subadult',]
adult_aov <- aov(adult$Depth..m. ~ adult$`Rubias Species Call`)
subadult_aov <- aov(subadult$Depth..m. ~ subadult$`Rubias Species Call`)
summary(adult_aov)
summary(subadult_aov)

pairwise.t.test(x = adult$Depth..m.,
                g = adult$`Rubias Species Call`)

pairwise.t.test(x = subadult$Depth..m.,
                g = subadult$`Rubias Species Call`)

#Two-way anova of old and young species depth differences
old <- sunverm_hybrid_age[sunverm_hybrid_age$ageclass == 'Over 5',]
young <- sunverm_hybrid_age[sunverm_hybrid_age$ageclass == 'Under 5',]

pairwise.t.test(x = old$Depth..m.,
                g = old$`Rubias Species Call`)

pairwise.t.test(x = young$Depth..m.,
                g = young$`Rubias Species Call`)

pairwise.t.test(x = meta$Fork.length..cm.,
                g = meta$`Rubias Species Call`)

pairwise.t.test(x = meta$Wt..kg.,
                g = meta$`Rubias Species Call`)

length <- ggplot(data = meta,
       aes(x = factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion')),
           y = `Fork.length..cm.`,
           col = `Rubias Species Call`)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = 'none') +
  ylab('Fork Length (cm)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)

weight <- ggplot(data = meta,
       aes(x = factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion')),
           y = `Wt..kg.`,
           col = `Rubias Species Call`)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = 'none') +
  ylab('Weight (kg)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)

design <- "
BC
DE
"

depth_and_age + depth_and_size + length + weight + plot_layout(design = design)



### Statistics

names(meta)[names(meta) == 'Rubias Species Call'] <- 'species'

# analysis of variance
anova_depth <- aov(`Depth..m.` ~ species, data = meta)
anova_length <- aov(`Fork.length..cm.` ~ species, data = meta)
anova_weight <- aov(`Wt..kg.` ~ species, data = meta)

# Tukey's test
TukeyHSD(anova_depth)
TukeyHSD(anova_length)
TukeyHSD(anova_weight)


meta %>%
  group_by(species) %>% 
  summarise(mean_length = mean(Fork.length..cm., na.rm = TRUE),
            se_length = sd(Fork.length..cm., na.rm = TRUE)/sqrt(n()),
            mean_depth = mean(Depth..m., na.rm = TRUE),
            se_depth = sd(Depth..m., na.rm = TRUE)/sqrt(n()),
            mean_wt = mean(Wt..kg., na.rm = TRUE),
            se_wt = sd(Wt..kg., na.rm = TRUE)/sqrt(n()))



table(meta$species)
