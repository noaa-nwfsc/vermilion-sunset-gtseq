
##### Load required packages & color pals --------------------------------------
packages <- c('rnaturalearth', 'sf', 'patchwork', 'reshape', 'rnaturalearthdata',
              'dplyr', 'tidyverse','readxl', "multcompView", 'ggsignif', 'data.table', 
              'patchwork', 'ggOceanMaps', "tools", "dplyr", 'sp', 'readr',
              'mapproj', 'cowplot', 'stringr')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
theme_set(theme_classic())

setwd("~/Desktop/VermilionRF/VMSURF Species ID/")

in_file <- "~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_040125_AW.xlsx"

#Set colors
color_pal_hybrids <- c(
  'vermilion' = '#481567FF',
  'sunset' = '#2D708EFF', 
  'hybrid' = '#55C667FF')

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

hybrids <- read.csv("~/Desktop/VermilionRF/VMSURF Species ID/newhybrids/ver_sun_w_hybrids/PofZ_results_5_12_25.csv") %>%
  subset(hybridclass != 'PV') %>%
  subset(hybridclass != 'PS') %>%
  select(c('individuals', 'hybridclass'))

hybrid_id <- hybrids$individuals

hybrid_id <- gsub("_", "-", hybrid_id)


meta$`Rubias Species Call`[meta$Specimen.Num %in% hybrid_id] <- 'hybrid'
meta$`Rubias Pop Call`[meta$Specimen.Num %in% hybrid_id] <- 'hybrid'


table(meta$`Rubias Species Call`)

meta <- meta %>%
  drop_na(`Rubias Species Call`) %>%
  subset(`Rubias Species Call` != 'rebs')


##### map #####

world <- ne_countries(scale = "large", returnclass = "sf")
#states <- ne_states(country = 'united states of america', returnclass = "sf") 
path.eez.usa <- ("~/Desktop/VermilionRF/VMSURF Species ID/metadata/shapefiles/EEZ_land_union_v3_202003/")
fnam.eez.usa <- "EEZ_Land_v3_202030.shp"
eez.usa <- st_read(dsn = path.eez.usa, layer = file_path_sans_ext(fnam.eez.usa))
# eez.usa has 259 features and 16 fields
# A Large SpatialLinesDataFrame object with 259 features and 16 fields (3.3 Mb)

# Fortify the shapefile data using `fortify.shape()`:
dat.eez.usa1 <- fortify(eez.usa) # a 180400x22 dataframe
labels <- read.csv(file = "~/Desktop/VermilionRF/VMSURF Species ID/metadata/paper2_labels.csv")
map_labels <- read.csv(file = "~/Desktop/VermilionRF/VMSURF Species ID/metadata/paper2_labels_2.csv")

map <- ggplot() +
  geom_sf(data = dat.eez.usa1, size = 1.5, color = "grey", fill = 'white')+
  geom_sf(data = world) +
  borders("state") +
  geom_point(data = meta, aes(x = `LonDD.v2`, 
                                         y = `LatDD`, 
                                         col = factor(`Rubias Species Call`, 
                                                      levels = c('sunset','hybrid','vermilion'))),
             size = 0.5)+
  coord_sf(xlim = c(-113, -128), ylim = c(29, 50.5), expand = FALSE)+
  scale_x_continuous(breaks = c(-126, -122, -118)) +
  scale_color_manual(values = color_pal_hybrids)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = c(0.915, 0.87),
    legend.title=element_blank(),
    legend.spacing = unit(0, "pt"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))+
  facet_wrap(~factor(`Rubias Species Call`, levels = c('sunset','hybrid','vermilion'))) +
  guides(color = guide_legend(override.aes = list(size = 2)))+
  geom_text(data = labels, aes(label = label, x = long, y = lat), size.unit = 'mm', size = 2)+
  geom_segment(data = labels, 
               aes(x = start.arrow.long, y = start.arrow.lat, xend = end.arrow.long, yend = end.arrow.lat), 
               color = 'black', size = 0.5, alpha = 0.6, arrow = arrow(length = unit(0.15, "cm"))) +
  geom_text(data = map_labels, aes(label = label, x= long, y = lat, color = color, angle = angle), size.unit = 'mm', size = 2)

map


ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/pop_struct_MS/Fig3-HybridMap.jpeg",
      width = 174,
       height = 110,
       units = c("mm"),
       dpi = 900)
map
dev.off()



######################





df <- as.data.frame.matrix(table(meta$Site.Cell.ID, meta$`Rubias Species Call`, useNA = "ifany")) %>%
  subset(hybrid > 0 & vermilion == 0) 


#Change column names
names(meta)[names(meta) == 'Rubias Species Call'] <- 'repunit'
names(meta)[names(meta) == 'Rubias Pop Call'] <- 'collection'

#Create a column which groups the samples into three size classes
#Based on email convo with John Harms
sunverm_hybrid <- meta %>% 
  mutate(sizeclass = case_when(
    Fork.length..cm. >= 35 ~ 'Adult',
    Fork.length..cm. < 35 ~ 'Subadult')
  )
#Remove data absent samples
sunverm_hybrid_size <- sunverm_hybrid[!is.na(sunverm_hybrid$sizeclass),]

#Create a age class column
sunverm_hybrid <- sunverm_hybrid %>% 
  mutate(ageclass = case_when(
    Ages <= 5 ~ 'Under 5',
    Ages >5 ~ 'Over 5')
  )
#Remove data absent individuals
sunverm_hybrid_age <- sunverm_hybrid[!is.na(sunverm_hybrid$ageclass),]



#### stats
# analysis of variance - weight
anova <- aov(`Wt..kg.` ~ repunit, data = sunverm_hybrid)
# Tukey's test
tukey <- TukeyHSD(anova)
# compact letter display
cld <- multcompLetters4(anova, tukey)
# table with factors and 3rd quantile
wt_tbl <- group_by(sunverm_hybrid, repunit) %>%
  summarise(depth=mean(`Wt..kg.`, na.rm = T), sd = sd(`Wt..kg.`, na.rm = T))
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
cld <- tibble::rownames_to_column(cld, "repunit")
wt_tbl <- merge(wt_tbl, cld)

# analysis of variance - length
anova <- aov(`Fork.length..cm.` ~ repunit, data = sunverm_hybrid)
# Tukey's test
tukey <- TukeyHSD(anova)
# compact letter display
cld <- multcompLetters4(anova, tukey)
# table with factors and 3rd quantile
length_tbl <- group_by(sunverm_hybrid, repunit) %>%
  summarise(depth=mean(`Fork.length..cm.`, na.rm = T), sd = sd(`Fork.length..cm.`, na.rm = T))
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
cld <- tibble::rownames_to_column(cld, "repunit")
length_tbl <- merge(length_tbl, cld)


# analysis of variance - depth
anova <- aov(`Depth..m.` ~ repunit, data = sunverm_hybrid)
# Tukey's test
tukey <- TukeyHSD(anova)
# compact letter display
cld <- multcompLetters4(anova, tukey)
# table with factors and 3rd quantile
depth_tbl <- group_by(sunverm_hybrid, repunit) %>%
  summarise(depth=mean(`Depth..m.`, na.rm = T), se = (sd(`Depth..m.`, na.rm = T)/sqrt(length(na.omit(`Depth..m.`)))))
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
cld <- tibble::rownames_to_column(cld, "repunit")
depth_tbl <- merge(depth_tbl, cld)

#Two-way anova of adult and subadult species depth differences
adult <- sunverm_hybrid_size[sunverm_hybrid_size$sizeclass == 'Adult',]
subadult <- sunverm_hybrid_size[sunverm_hybrid_size$sizeclass == 'Subadult',]
adult_aov <- aov(`Depth..m.` ~ repunit, data = adult)
tukey <- TukeyHSD(adult_aov)
# compact letter display
cld <- multcompLetters4(adult_aov, tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
large_depth_tbl <- tibble::rownames_to_column(cld, "repunit")%>%
  select(c('repunit', 'Letters'))
large_depth_tbl$sizeclass <- 'Adult'

adult_depth_tbl <- group_by(adult, repunit) %>%
  summarise(depth=mean(`Depth..m.`, na.rm = T), se = (sd(`Depth..m.`, na.rm = T)/sqrt(length(na.omit(`Depth..m.`)))))

juv_depth_tbl <- group_by(subadult, repunit) %>%
  summarise(depth=mean(`Depth..m.`, na.rm = T), se = (sd(`Depth..m.`, na.rm = T)/sqrt(length(na.omit(`Depth..m.`)))))

subadult_aov <- aov(`Depth..m.` ~ repunit, data = subadult)
tukey <- TukeyHSD(subadult_aov)
# compact letter display
cld <- multcompLetters4(subadult_aov, tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
small_depth_tbl <- tibble::rownames_to_column(cld, "repunit") %>%
  select(c('repunit', 'Letters'))
small_depth_tbl$sizeclass <- 'Subadult'


size_depth_tbl <- rbind(large_depth_tbl, small_depth_tbl)

#Two-way anova of old and young species depth differences
old <- sunverm_hybrid_age[sunverm_hybrid_age$ageclass == 'Over 5',]
old_aov <- aov(`Depth..m.` ~ repunit, data = old)
tukey <- TukeyHSD(old_aov)
# compact letter display
cld <- multcompLetters4(old_aov, tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
old_depth_tbl <- tibble::rownames_to_column(cld, "repunit")%>%
  select(c('repunit', 'Letters'))
old_depth_tbl$ageclass <- 'Over 5'


young <- sunverm_hybrid_age[sunverm_hybrid_age$ageclass == 'Under 5',]
tukey <- TukeyHSD(subadult_aov)
# compact letter display
cld <- multcompLetters4(subadult_aov, tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$repunit)
young_depth_tbl <- tibble::rownames_to_column(cld, "repunit")%>%
  select(c('repunit', 'Letters'))
young_depth_tbl$ageclass <- 'Under 5'

age_depth_tbl <- rbind(old_depth_tbl, young_depth_tbl)



df2 <- as.data.frame.matrix(table(sunverm_hybrid_size$Site.Cell.ID, sunverm_hybrid_size$repunit, useNA = "ifany"))

sunverm_hybrid_adult <- subset(sunverm_hybrid_size, sunverm_hybrid_size$sizeclass == 'Adult')
#Plot
depth <- ggplot(data = meta,
       aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')), 
           y = `Depth..m.`,
           col = repunit)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  #facet_wrap(~sizeclass) + 
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)+
  ylim(0,350)

(depth_and_size <- ggplot(data = sunverm_hybrid_size,
       aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')), 
           y = `Depth..m.`,
           col = repunit)) +
  geom_boxplot()+
  geom_text(data = size_depth_tbl, aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')),
                                y = c(330, 225, 190, 205, 120, 150), label = Letters),
            size = 4, color = "black") +
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  facet_wrap(~sizeclass) + 
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids)+
  ylim(0,350))


#Plot
(depth_and_age <- ggplot(data = sunverm_hybrid_age,
       aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')),
           y = `Depth..m.`,
           col = repunit)) +
  geom_boxplot()+
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  geom_text(data = age_depth_tbl, aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')),
                                       y = c(330, 200, 180, 220, 175, 150), label = Letters),
            size = 4, color = "black") +
  facet_wrap(~ageclass) + 
  ylab('Depth (m)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids))








(length <- ggplot(data = meta,
       aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')),
           y = `Fork.length..cm.`,
           col = repunit)) +
  geom_boxplot()+
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Fork Length (cm)')+
  xlab('Species')+
  scale_color_manual(values = color_pal_hybrids) +
  geom_text(data = length_tbl, aes(x = repunit, y = c(62, 70, 67), label = Letters), 
            size = 4, color = "black"))

(weight <- ggplot(data = meta,
       aes(x = factor(repunit, levels = c('sunset','hybrid','vermilion')),
           y = `Wt..kg.`,
           col = repunit)) +
  geom_boxplot()+
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Weight (kg)')+
  scale_color_manual(values = color_pal_hybrids)+
  geom_text(data = wt_tbl, aes(x = repunit,
                               y = c(4,6.5,4.5), label = Letters), 
            size = 4, color = "black"))

design <- "
BC
DE
"

hybrid_biological <- depth_and_age + depth_and_size + length + weight + plot_layout(design = design) +
  plot_annotation(tag_levels = 'A')


#ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/pop_struct_MS/SuplFig1-HybridBiological.jpeg",
#       width = 180,
#       height = 150,
#       units = c("mm"),
#       dpi = 900)
#hybrid_biological
#dev.off()


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
