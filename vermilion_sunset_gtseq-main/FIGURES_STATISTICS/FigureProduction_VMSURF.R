### Plotting Figures from Paper #1

## This script is adapted from a multitude of other scripts for publishing on GitHub
### Author: Anita Wray
### Date: January 4, 2024

##### Load required packages & color pals --------------------------------------
packages <- c('rnaturalearth', 'sf', 'tidyverse', 'ggsignif', 'data.table', 
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

#Set colors
pal <- PNWColors::pnw_palette('Sunset2', 4)
pal_species <- c('Vermilion Rockfish male' = "#C3B1E1",
                 'Vermilion Rockfish female' = "#882255",
                 'Sunset Rockfish male' = "#FEBA4F",
                 'Sunset Rockfish female' = "#F04F00",
                 'Vermilion Rockfish' = pal[1],
                 'Sunset Rockfish' = pal[3], 
                 '≤ 5 Years Old Vermilion' = '#AC94F4',
                 '≤ 5 Years Old Sunset' = '#ee9f27',
                 '> 5 Years Old Vermilion' = '#643b9f',
                 '> 5 Years Old Sunset' = '#d24e01')
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

metadata$repunit <- gsub('vermilion', 'Vermilion Rockfish', metadata$repunit)
metadata$repunit <- gsub('sunset', 'Sunset Rockfish', metadata$repunit)
##### Mapping - Fig 1, 2 & 6 ---------------------------------------------------
#Load in the base R map

path.eez.usa <- ("~/Desktop/VermilionRF/VMSURF Species ID/metadata/shapefiles/EEZ_land_union_v3_202003/")
fnam.eez.usa <- "EEZ_Land_v3_202030.shp"
eez.usa <- st_read(dsn = path.eez.usa, layer = file_path_sans_ext(fnam.eez.usa))
# eez.usa has 259 features and 16 fields
# A Large SpatialLinesDataFrame object with 259 features and 16 fields (3.3 Mb)

# Fortify the shapefile data using `fortify.shape()`:
dat.eez.usa1 <- fortify(eez.usa) # a 180400x22 dataframe


path.cca <- ("~/Desktop/VermilionRF/VMSURF Species ID/metadata/shapefiles/Cowcod_Conservation_Areas_-_R7_-_CDFW_[ds3165]/")
fnam.cca <- "Cowcod_Conservation_Areas_-_R7_-_CDFW_[ds3165].shp"
cca <- st_read(dsn = path.cca, layer = file_path_sans_ext(fnam.cca))

path.60fa <- ("~/Desktop/VermilionRF/VMSURF Species ID/metadata/shapefiles/2024_07024/")
fnam.60fa <- "c60fm_01012024_XYTableToPoint_PointsToLine.shp"
fathom60 <- st_read(dsn = path.60fa, layer = file_path_sans_ext(fnam.60fa))

#JOHN - Not sure we need to add the 60 fm curve around San Nick/SBI just because 
#those areas were already protected by the CCAs, but the rest of the Channel 
#Islands + Catalina and Clemente were not.

fathom60 <- fathom60[!fathom60$area_name %like% "San Nicolas", ]
fathom60 <- fathom60[!fathom60$area_name %like% "Santa Barbara", ]
fathom60 <- fathom60[!fathom60$area_name %like% "Cortes", ]
fathom60 <- fathom60[!fathom60$area_name %like% "Tanner", ]

# to visualize
# mapview::mapview(fathom_30, zcol = "Region")
#Load in the base R map
world <- ne_countries(scale = "large", returnclass = "sf")
labels <- read.csv(file = "~/Desktop/VermilionRF/VMSURF Species ID/metadata/labels_for_map_fig1.csv")
wc_labels <- labels %>%
  subset(repunit == 'westcoast')
sc_labels <- labels %>%
  subset(repunit == 'socal')

(basic_map <- ggplot() +
    geom_sf(data = world) +
    geom_sf(data = cca, size = 1.5, color = 'blue', fill = 'transparent')+
    #geom_sf(data = fathom_30, size = 1, color = 'lightgreen')+
    geom_sf(data = fathom60, size = 1, color = 'lightblue') +
    coord_sf(xlim = c(-117, -121.5), ylim = c(31.75, 36), expand = FALSE)+
    scale_x_continuous(breaks = c(seq(from = -140, to = -114, by = 5))) +
    scale_y_continuous(breaks = c(seq(from = 29, to = 50, by = 2))))


p_wc <- ggplot()+
  geom_sf(data = dat.eez.usa1, size = 1.5, color = "grey", fill = 'white')+
  geom_sf(data = world)+
  borders("state") +
  coord_sf(xlim = c(-113, -140), ylim = c(29, 50.5), expand = FALSE)+
  scale_x_continuous(breaks = c(seq(from = -140, to = -114, by = 5))) +
  scale_y_continuous(breaks = c(seq(from = 29, to = 50, by = 2))) +
  theme(strip.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank(),
        strip.text.x = element_text(
          size = 12, hjust = 0, face = "bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_text(data = wc_labels, aes(label = label, x = long, y = lat), size.unit = 'mm', size = 2.5)+
  geom_segment(data = wc_labels, 
               aes(x = start.arrow.long, y = start.arrow.lat, xend = end.arrow.long, yend = end.arrow.lat), 
               color = 'black', size = 0.5, alpha = 0.6, arrow = arrow(length = unit(0.15, "cm"))) +
  geom_rect(aes(xmin = -121.5, xmax = -117, ymin = 31.75, ymax = 36),
            fill = "transparent", color = "red", size = 0.5) +
  geom_text(aes(x = -127, y = 48.1, angle=29), label = "Canada", color = 'grey', size = 2)+
  geom_text(aes(x = -126.75, y = 47.4, angle=29), label = "USA", color = 'grey', size = 2)+
  geom_text(aes(x = -120, y = 30.51, angle=13), label = "Mexico", color = 'grey', size = 2)+
  geom_text(aes(x = -120.2, y = 31.2, angle=13), label = "USA", color = 'grey', size = 2)+
  geom_text(aes(x = -120.5, y = 47.2), label = "WA", color = 'darkgrey', size = 3)+
  geom_text(aes(x = -120.5, y = 44), label = "OR", color = 'darkgrey', size = 3)+
  geom_text(aes(x = -119.8, y = 37.6), label = "CA", color = 'darkgrey', size = 3)+
  # Add color legend using an invisible geom_point
  geom_point(
    data = data.frame(color = 1),
    aes(color = "cca"), x = NA, y = NA, na.rm = TRUE,
    key_glyph = "polygon"
  ) +
  geom_point(
    data = data.frame(color = 2),
    aes(color = "fathom60"), x = NA, y = NA, na.rm = TRUE,
    key_glyph = "polygon"
  ) +
  scale_color_manual(
    name = "",
    values = c('blue', "lightblue"),
    labels = c("CCA Boundary", "60 fm (110 m) RCA Boundary")
  )


socal_map <- ggplot() +
  geom_sf(data = dat.eez.usa1, size = 1.5, color = "grey", fill = 'transparent')+
  geom_sf(data = world) +
  geom_sf(data = cca, size = 1.5, color = 'blue', fill = 'transparent')+
  #geom_sf(data = fathom_30, linewidth = 0.75, color = "lightgreen") +
  geom_sf(data = fathom60, size = 1, color = 'lightblue') +
  #scale_color_manual(name = "", values = c(fathom_30 = "lightgreen"), labels = c('30 Fathoms')) +
  coord_sf(xlim = c(-117, -121.5), ylim = c(31.75, 36), expand = FALSE)+
  scale_x_continuous(breaks = c(seq(from = -121, to = -117, by = 2))) +
  scale_y_continuous(breaks = c(seq(from = 32, to = 36, by = 2)))+
  theme(legend.position = 'none',
        strip.background = element_blank(), 
        strip.text.x = element_text(
          size = 12, hjust = 0, face = "bold"),
        panel.border = element_rect(color = "red", 
                                    fill = NA, 
                                    linewidth = 1),
        axis.line.y = element_blank(), axis.line.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))+
  geom_text(data = sc_labels, aes(label = label, x = long, y = lat), size.unit = 'mm', size = 2.5)+
  geom_segment(data = sc_labels, 
               aes(x = start.arrow.long, y = start.arrow.lat, xend = end.arrow.long, yend = end.arrow.lat), 
               color = 'black', size = 0.5, alpha = 0.6, arrow = arrow(length = unit(0.15, "cm")))

#socal_map

p2 <- ggdraw(p_wc) +
  draw_plot(socal_map,
            # The distance along a (0,1) x-axis to draw the left edge of the plot
            x = .086, 
            # The distance along a (0,1) y-axis to draw the bottom edge of the plot
            y = 0.17,
            # The width and height of the plot expressed as proportion of the entire ggdraw object
            width = 0.45, 
            height = 0.45)

fig1 <- p2 +
  draw_line(x = c(0.521, 0.7),
            y = c(0.22, 0.252),
            color ="red", size = 0.5) +
  draw_line(x = c(0.521, 0.7),
            y = c(0.594, 0.415),
            color ="red", size = 0.5)

#inset_finished 

ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig1-DetailedMap.jpeg",
       width = 140,
       height = 160,
       units = c("mm"),
       dpi = 900)
fig1
dev.off()






labels_fig2 <- readxl::read_excel("~/Desktop/VermilionRF/VMSURF Species ID/metadata/labels_for_fig2.xlsx")


fig2 <- ggplot(data = world)+
  geom_sf() +
  borders("state") +
  geom_point(data = metadata, aes(x = `LonDD.v2`, y = `LatDD`, col = `repunit`), size = 1, pch = 1)+
  coord_sf(xlim = c(-115, -128), ylim = c(29, 50.5), expand = FALSE) +
  scale_x_continuous(breaks = c(-124, -120, -116)) +
  scale_color_manual(values = pal_species, breaks=c('Vermilion Rockfish', 'Sunset Rockfish'))+
  theme(strip.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size = 8),
        legend.box.spacing = margin(0.2)) +
  facet_wrap(~factor(repunit, levels = c('Vermilion Rockfish', 'Sunset Rockfish')))+
  geom_text(data = labels_fig2, 
            aes(label = label, x = long, y = lat), 
            size.unit = 'mm', size = 1.5)+
  geom_segment(data = labels_fig2, 
               aes(x = `start arrow long`, y = `start arrow lat`, 
                   xend = `end arrow long`, yend = `end arrow lat`), 
               color = 'black', linewidth = 0.2, 
               arrow = arrow(length = unit(0.1, "cm")))

ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig2-Map_w_points_open_circles.jpeg",
       width = 90,
       height = 100,
       units = c("mm"),
       dpi = 300)
fig2
dev.off()

## Now let's look at just SoCal, where most of our data points are from
socal <- metadata %>% 
  filter(LatDD > 32 & LatDD < 36)

socal$LatDD <- socal$LatDD %>%
  round(digits = 2)
socal$LonDD.v2 <- socal$LonDD.v2 %>%
  round(digits = 2)

mround <- function(x,base){
  base*round(x/base)
}


socal$Depth_rounded <- mround(socal$Depth..m.,5)

socal$Depth..m. <- socal$Depth..m. %>%
  round(digits = 0)

# Lets add in more depth information, and therefore we need a better/new color palette
cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc",
                             "#0D98BA","#4ab04a", "#C0FF02", "#ffd73e"))(10),
          colorRampPalette(c("#e29421", 
                             "#e29421", "#f05336",
                             "#ce472e", "#8B0000"))(10))

labels_fig6 <- readxl::read_excel("~/Desktop/VermilionRF/VMSURF Species ID/metadata/labels_for_fig6.xlsx")

fig5 <- ggplot(data = world) +
  geom_sf() +
  geom_count(data = socal,
             aes(x = `LonDD.v2`, y = `LatDD`,
                 color = `Depth_rounded`)) +
  coord_sf(xlim = c(-117, -121.5), ylim = c(31.75, 36), expand = FALSE)+
  facet_wrap(~factor(repunit, levels = c('Vermilion Rockfish', 'Sunset Rockfish')))+
  theme(legend.position = 'bottom',
        text = element_text(size = 4.5),
        strip.background = element_blank(), 
        #legend.text = element_text(size=2),
        legend.box.spacing = margin(0.2),
        plot.margin=grid::unit(c(1,1,1,1), "mm"),
        legend.key.size = unit(3.2, 'mm'),
        strip.text = element_text(size = 5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_x_continuous(breaks = c(-121, -120, -119, -118))+
  labs(color='Depth (m)')+
  scale_color_gradientn(colours=cols, limits=c(0, 315),
                        breaks=seq(0, 300, by=100), 
                        na.value=rgb(246, 246, 246, max=255))+
  geom_text(data = labels_fig6, 
            aes(label = label, x = long, y = lat), 
            size.unit = 'mm', size = 1)+
  geom_segment(data = labels_fig6, 
               aes(x = `start arrow long`, y = `start arrow lat`, 
                   xend = `end arrow long`, yend = `end arrow lat`), 
               color = 'black', linewidth = 0.15,
               arrow = arrow(length = unit(0.09, "cm")))+
  scale_size_area(max_size = 5)

ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig6-SoCal_Map_w_points_final.jpeg",
       width = 90,
       height = 70,
       units = c("mm"),
       dpi = 3000)
fig5
dev.off()






##### Depth Distribution - Fig 3-----------------------------------------------
metadata$repunit <- factor(metadata$repunit, levels = c('Vermilion Rockfish', 'Sunset Rockfish'))


#Create a age class column
sunverm <- metadata %>%
  mutate(ageclass = case_when(
    Ages <= 5 & repunit == 'Vermilion Rockfish' ~ '≤ 5 Years Old Vermilion',
    Ages <= 5 & repunit == 'Sunset Rockfish' ~ '≤ 5 Years Old Sunset',
    Ages >5 & repunit == 'Vermilion Rockfish' ~ '> 5 Years Old Vermilion',
    Ages >5 & repunit == 'Sunset Rockfish' ~ '> 5 Years Old Sunset')
  )
#Remove data absent individuals
sunverm_age <- sunverm[!is.na(sunverm$ageclass),]

is.outlier <- function (x) {
  x < quantile(x, .25) - 1.5 * IQR(x) |
    x > quantile(x, .75) + 1.5 * IQR(x)
}

sunverm_age %>% group_by(cut) %>%
  mutate(outlier.p = is.outlier(price)) %>%
  ungroup() -> diamonds

#Plot
(fig3 <- ggplot(data = sunverm_age,
       aes(x = `repunit`, y = `Depth..m.`,
           color = factor(ageclass, levels = c('≤ 5 Years Old Vermilion',
                                               '> 5 Years Old Vermilion' ,
                                               '≤ 5 Years Old Sunset',
                                               '> 5 Years Old Sunset'
                                               )))) +
  geom_boxplot(lwd = .25, outlier.size = 0.5, show.legend = FALSE)+
  stat_summary(fun.y="mean",
               position = position_dodge2(width = 0.75, preserve = "single"), 
               size = 0.2, pch = 17, lwd = 0.00001)+
  ylab('Depth (m)')+
  xlab('')+
  scale_color_manual(values = pal_species) +
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')), 
              map_signif_level = TRUE,
              y_position = c(350,350), size = 0.3,
              textsize = 2, color = 'black') +
  geom_signif(
    y_position = c(190, 330), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("***", "***"), tip_length = 0,
    size = 0.3,
    textsize = 2, color = 'black') +
  scale_y_continuous(limits = c(0,375))+
    theme(
      text = element_text(size = 5),
      axis.line = element_line(colour = 'black', size = .2),
      axis.ticks = element_line(colour = "black", size = .2),
      legend.position = 'top',
      legend.title = element_text(size = 3), 
      legend.text  = element_text(size = 3.5),
      legend.key.size = unit(0.1, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5, shape = 15)),
           color = guide_legend(override.aes = list(size = 0.5),
                                title = '', nrow = 1)) +
  geom_point(size = 0, color = 'white'))


ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig3-Depth_distribution.jpeg",
       width = 80,
       height = 70,
       units = c("mm"),
       dpi = 3000)
fig3
dev.off()

##### Cumulative and Relative Proportion - Fig 4 & 5 -----------------------

metadata <- subset(metadata, metadata$depthcat10 != "NA's")

sunset <- subset(metadata, metadata$repunit == 'Sunset Rockfish')
vermilion <- subset(metadata, metadata$repunit == 'Vermilion Rockfish')

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
melt_df$variable <- gsub('perc_sun', 'Sunset Rockfish', melt_df$variable)
melt_df$variable <- gsub('perc_ver', 'Vermilion Rockfish', melt_df$variable)
melt_df <- melt_df %>%
  mutate(across(variable, factor, levels=c('Vermilion Rockfish', 'Sunset Rockfish')))



fig5 <- ggplot(data = melt_df, aes(x = factor(depthcat10, level = order), 
                           y = value, fill = variable)) +
  geom_bar(stat='identity', position="stack") +
  labs(x = 'Depth Stratum (m)', y = 'Relative Proportion')+
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
  scale_y_continuous(expand = c(0, 0))+
  theme(
    text = element_text(size = 5),
    axis.line = element_line(colour = 'black', size = .2),
    axis.ticks = element_line(colour = "black", size = .2),
    legend.key.size = unit(2, 'mm'),
    legend.box.spacing = margin(0.2), 
    legend.position = 'bottom'
  )


ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig5-CumulativeDepth_2.jpeg",
       width = 90,
       height = 50,
       units = c("mm"),
       dpi = 300)
fig5
dev.off()

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

fig4 <- ggplot() +
  stat_ecdf(data = sunset, aes(Depth..m.), geom = "step", color = pal[3], lwd = .3) +
  stat_ecdf(data = vermilion, aes(Depth..m.), geom = 'step', color = pal[1], lwd = .3) +
  labs(x = 'Depth (m)') +
  scale_y_continuous(name = "Cumulative Proportion") +
  scale_x_continuous(name = 'Depth (m)') +
  labs(x = 'Depth (m)', y = 'Percent of Total Abundance', color = 'Species') +
  geom_vline(xintercept = d50_s, color = pal[3], lwd = .3) +
  geom_vline(xintercept = d50_v, color = pal[1], lwd = .3)+
  theme(
    text = element_text(size = 6),
    axis.line = element_line(colour = 'black', size = .2),
    axis.ticks = element_line(colour = "black", size = .2)
  )



ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig4-CumulativeDepth_1.jpeg",
       width = 90,
       height = 50,
       units = c("mm"),
       dpi = 300)
fig4
dev.off()



##### Length Weight Curves - Fig 7 ---------------------------------------------


fig6e <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish'&
                              metadata$Sex == 'Female'),
              aes(color = 'Vermilion Rockfish female'), method = 'loess') +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish'&
                              metadata$Sex == 'Male'),
              aes(color = 'Vermilion Rockfish male'), method = 'loess')+
  stat_smooth(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                              metadata$Sex == 'Female'),
              aes(color = 'Sunset Rockfish female'), method = 'loess') +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                              metadata$Sex == 'Male'),
              aes(color = 'Sunset Rockfish male'), method = 'loess')  +
  theme_classic() +
  scale_y_continuous(limits = c(0,6), n.breaks = 4) +
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish female', 
                                                      'Vermilion Rockfish male',
                                                      'Sunset Rockfish female',
                                                      'Sunset Rockfish male')) +
  theme(legend.position = c(.21, .81)) +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')




fig6b <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                             metadata$Sex == 'Female'),
             aes(color = 'Sunset Rockfish female'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                              metadata$Sex == 'Female'),
              aes(color = 'Sunset Rockfish female'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                             metadata$Sex == 'Male'),
             aes(color = 'Sunset Rockfish male'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                              metadata$Sex == 'Male'),
              aes(color = 'Sunset Rockfish male'), method = 'loess') +
  theme_classic() +
  scale_y_continuous(limits = c(0,6), n.breaks = 4) +
  scale_color_manual(values = pal_species) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')

fig6a <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                             metadata$Sex == 'Female'),
             aes(color = 'Vermilion Rockfish female'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                              metadata$Sex == 'Female'),
              aes(color = 'Vermilion Rockfish female'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                             metadata$Sex == 'Male'),
             aes(color = 'Vermilion Rockfish male'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                              metadata$Sex == 'Male'),
              aes(color = 'Vermilion Rockfish male'), method = 'loess') +
  theme_classic() +
  scale_color_manual(values = pal_species) +
  scale_y_continuous(limits = c(0,6), n.breaks = 4) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')

fig6c <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                             metadata$Sex == 'Male'),
             aes(color = 'Sunset Rockfish male'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                              metadata$Sex == 'Male'),
              aes(color = 'Sunset Rockfish male'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                             metadata$Sex == 'Male'),
             aes(color = 'Vermilion Rockfish male'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                              metadata$Sex == 'Male'),
              aes(color = 'Vermilion Rockfish male'), method = 'loess') +
  theme_classic() +
  scale_color_manual(values = pal_species) +
  scale_y_continuous(limits = c(0,6), n.breaks = 4) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')

fig6d <- ggplot(data = metadata, aes(x = Fork.length..cm., y = Wt..kg., color = repunit)) +
  geom_point(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                             metadata$Sex == 'Female'),
             aes(color = 'Sunset Rockfish female'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Sunset Rockfish' &
                              metadata$Sex == 'Female'),
              aes(color = 'Sunset Rockfish female'), method = 'loess') +
  geom_point(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                             metadata$Sex == 'Female'),
             aes(color = 'Vermilion Rockfish female'), pch = 1) +
  stat_smooth(data = subset(metadata, metadata$repunit == 'Vermilion Rockfish' &
                              metadata$Sex == 'Female'),
              aes(color = 'Vermilion Rockfish female'), method = 'loess') +
  theme_classic() +
  scale_y_continuous(limits = c(0,6), n.breaks = 4) +
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish female', 
                                                      'Vermilion Rockfish male',
                                                      'Sunset Rockfish female',
                                                      'Sunset Rockfish male')) +
  theme(legend.position = 'none') +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')


fig7 <- ((fig6a + fig6b)/(fig6c+fig6d))/fig6e + plot_annotation(tag_levels = 'a') +
  plot_layout(heights = c(1,1,2))

#fig7


ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig7-Length_Weight.jpeg",
       width = 190,
       height = 240,
       units = c("mm"),
       dpi = 300)
fig7
dev.off()


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
VermM <- subset(AgeDat, Sex=="Male" & repunit=="Vermilion Rockfish")
VermF <- subset(AgeDat, Sex=="Female" & repunit=="Vermilion Rockfish")
SunM <- subset(AgeDat, Sex=="Male" & repunit=="Sunset Rockfish")
SunF <- subset(AgeDat, Sex=="Female" & repunit=="Sunset Rockfish")

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

##### Von Bertalanffy Growth Curves - Fig 8 ------------------------------------
fig8e <- ggplot(data = AgeDat) +
  geom_line(data = VermM, aes(x = sort(VermM$Ages),
                              y = sort(lpVM), col = "Vermilion Rockfish male")) +
  geom_line(data = VermF, aes(x = sort(VermF$Ages),
                              y = sort(lpVF), col = "Vermilion Rockfish female")) +
  geom_line(data = SunM, aes(x = sort(SunM$Ages),
                             y = sort(lpSM), col = "Sunset Rockfish male")) +
  geom_line(data = SunF, aes(x = sort(SunF$Ages),
                             y = sort(lpSF), col = "Sunset Rockfish female")) +
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,72), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = c(.85, .3)) +
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish female', 
                                                      'Vermilion Rockfish male',
                                                      'Sunset Rockfish female',
                                                      'Sunset Rockfish male'))

fig8b <- ggplot(data = AgeDat) +
  geom_line(data = SunM, aes(x = sort(SunM$Ages),
                             y = sort(lpSM), col = "Sunset Rockfish male")) +
  geom_point(data = SunM, aes(x = SunM$Ages,
                              y = SunM$Fork.length..cm., col = "Sunset Rockfish male"), pch = 1)+
  geom_line(data = SunF, aes(x = sort(SunF$Ages),
                             y = sort(lpSF), col = "Sunset Rockfish female")) +
  geom_point(data = SunF, aes(x = SunF$Ages,
                              y = SunF$Fork.length..cm., col = "Sunset Rockfish female"), pch = 1)+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,72), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig8a <- ggplot(data = AgeDat) +
  geom_line(data = VermM, aes(x = sort(VermM$Ages),
                              y = sort(lpVM), col = "Vermilion Rockfish male")) +
  geom_point(data = VermM, aes(x = VermM$Ages,
                               y = VermM$Fork.length..cm., col = "Vermilion Rockfish male"), pch = 1)+
  geom_line(data = VermF, aes(x = sort(VermF$Ages),
                              y = sort(lpVF), col = "Vermilion Rockfish female")) +
  geom_point(data = VermF, aes(x = VermF$Ages,
                               y = VermF$Fork.length..cm., col = "Vermilion Rockfish female"), pch = 1)+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,72), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig8c <- ggplot(data = AgeDat) +
  geom_line(data = VermM, aes(x = sort(VermM$Ages),
                              y = sort(lpVM), col = "Vermilion Rockfish male")) +
  geom_point(data = VermM, aes(x = VermM$Ages,
                               y = VermM$Fork.length..cm., col = "Vermilion Rockfish male"), pch = 1)+
  geom_line(data = SunM, aes(x = sort(SunM$Ages),
                             y = sort(lpSM), col = "Sunset Rockfish male")) +
  geom_point(data = SunM, aes(x = SunM$Ages,
                              y = SunM$Fork.length..cm., col = "Sunset Rockfish male"), pch = 1)+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,72), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig8d <- ggplot(data = AgeDat) +
  geom_line(data = VermF, aes(x = sort(VermF$Ages),
                              y = sort(lpVF), col = "Vermilion Rockfish female")) +
  geom_point(data = VermF, aes(x = VermF$Ages,
                               y = VermF$Fork.length..cm., col = "Vermilion Rockfish female"), pch = 1)+
  geom_line(data = SunF, aes(x = sort(SunF$Ages),
                             y = sort(lpSF), col = "Sunset Rockfish female")) +
  geom_point(data = SunF, aes(x = SunF$Ages,
                              y = SunF$Fork.length..cm., col = "Sunset Rockfish female"), pch = 1)+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7) +
  scale_x_continuous(limits = c(0,72), n.breaks = 7) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_color_manual(values = pal_species)

fig8 <- ((fig8a + fig8b)/(fig8c+fig8d))/fig8e + plot_annotation(tag_levels = 'a') +
  plot_layout(heights = c(1,1,2))

ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig8-VB_Curves.jpeg",
       width = 190,
       height = 240,
       units = c("mm"),
       dpi = 300)
           
fig8
dev.off()  


##### Length Distribution - Fig 10 ----------------------------------------------
fig10e <- ggplot(data = metadata[metadata$repunit != 'canary' & metadata$Survey == 'H&L',],
               aes(x = `repunit`, y = `Fork.length..cm.`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Fork Length (cm)')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,75))+
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')),
              map_signif_level = TRUE,
              y_position = c(68,68),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17)+
  ggtitle('a: Combined')


metadata <- metadata %>%
  drop_na(LatDD)

north <- metadata[metadata$LatDD >= 34.44,]
south <- metadata[metadata$LatDD < 34.44,]

per_location <- metadata %>%
  mutate(locclass = case_when(
    LatDD >= 34.44 ~ 'North',
    LatDD < 34.44 ~ 'South')
  )

fig10a <- ggplot(data = north,
                aes(x = `repunit`, y = `Fork.length..cm.`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Fork Length (cm)')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,75))+
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')),
              map_signif_level = TRUE,
              y_position = c(66,66),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17)+
  ggtitle('b: Northern California')


fig10b <- ggplot(data = south,
                aes(x = `repunit`, y = `Fork.length..cm.`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank())+
  ylab('Fork Length (cm)')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,75))+
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')),
              map_signif_level = TRUE,
              y_position = c(68,68),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17)+
  ggtitle('c: Southern California')

fig10c <- ggplot(data = per_location %>%
         subset(repunit == 'Vermilion Rockfish'),
                aes(x = `locclass`, y = `Fork.length..cm.`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Fork Length (cm)')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,75))+
  geom_signif(comparisons=list(c("North", 'South')),
              map_signif_level = TRUE,
              y_position = c(66,66),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17) +
  ggtitle('d: Vermilion Rockfish Only')+
  scale_x_discrete(labels=c("North of Pt. Conception","South of Pt. Conception"))

fig10d <- ggplot(data = per_location %>%
                  subset(repunit == 'Sunset Rockfish'),
                aes(x = `locclass`, y = `Fork.length..cm.`, color = repunit )) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Fork Length (cm)')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,75))+
  geom_signif(comparisons=list(c("North", 'South')),
              map_signif_level = TRUE,
              y_position = c(68,68),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17) +
  ggtitle('e: Sunset Rockfish Only')+
  scale_x_discrete(labels=c("North of Pt. Conception","South of Pt. Conception"))


north_OR <- metadata[metadata$LatDD >= 42,]
south_OR <- metadata[metadata$LatDD < 42,]

per_location <- metadata %>%
  mutate(locclass_OR = case_when(
    LatDD >= 42 ~ 'North',
    LatDD < 42 ~ 'South')
  )

fig10f <- ggplot(data = per_location %>%
                  subset(repunit == 'Vermilion Rockfish'),
                aes(x = `locclass_OR`, y = `Fork.length..cm.`, color = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Fork Length (cm)')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,75))+
  geom_signif(comparisons=list(c("North", 'South')),
              map_signif_level = TRUE,
              y_position = c(66,66),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17) +
  ggtitle('f: Vermilion Rockfish Only')+
  scale_x_discrete(labels=c("North of OR/CA Border","South of OR/CA Border"))

fig10 <- fig10e + fig10a + fig10b + fig10c + fig10d + fig10f +
  plot_layout(ncol = 2)


fig10


ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig9-Length_by_location.jpeg",
       width = 190,
       height = 240,
       units = c("mm"),
       dpi = 300)
fig10
dev.off() 

quantile(subset(per_location, locclass == 'North' & repunit == 'Vermilion Rockfish')$Fork.length..cm., prob=c(.25,.5,.75), type=1, na.rm = TRUE)

hist(subset(per_location, locclass == 'North' & repunit == 'Vermilion Rockfish')$Fork.length..cm.)

t.test(metadata$Fork.length..cm. ~ `repunit`,
       data = metadata)


##### Age Distribution Fig 9------------------------------------------------
fig9a <- ggplot(data = metadata[metadata$repunit != 'canary' & metadata$Survey == 'H&L',],
                aes(x = `repunit`, y = `Ages`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Age')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70), expand = c(0, 0))+
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')),
              map_signif_level = TRUE,
              y_position = c(72,72),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17)+
  ggtitle('a: Combined')

fig9b <- ggplot(data = north,
                aes(x = `repunit`, y = `Ages`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Age')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70), expand = c(0, 0))+
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')),
              map_signif_level = TRUE,
              y_position = c(72,72),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17)+
  ggtitle('b: Northern California')


fig9c <- ggplot(data = south,
                aes(x = `repunit`, y = `Ages`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank())+
  ylab('Age')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70), expand = c(0, 0))+
  geom_signif(comparisons=list(c('Vermilion Rockfish', 'Sunset Rockfish')),
              map_signif_level = TRUE,
              y_position = c(72,72),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17)+
  ggtitle('c: Southern California')

fig9d <- ggplot(data = per_location %>%
                  subset(repunit == 'Vermilion Rockfish'),
                aes(x = `locclass`, y = `Ages`, col = repunit)) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Age')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70), expand = c(0, 0))+
  geom_signif(comparisons=list(c("North", 'South')),
              map_signif_level = TRUE,
              y_position = c(72,72),
              show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17) +
  ggtitle('d: Vermilion Rockfish Only')+
  scale_x_discrete(labels=c("North of Pt. Conception","South of Pt. Conception"))

fig9e <- ggplot(data = per_location %>%
                  subset(repunit == 'Sunset Rockfish'),
                aes(x = `locclass`, y = `Ages`, col = repunit )) +
  geom_boxplot(width = 0.4)+
  theme(legend.position = 'none',
        axis.title.x=element_blank()) +
  ylab('Age')+
  scale_color_manual(values = pal_species, breaks = c('Vermilion Rockfish', 'Sunset Rockfish'))+
  scale_y_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70), expand = c(0, 0))+
  geom_signif(comparisons=list(c("North", 'South')),
              map_signif_level = TRUE,
              y_position = c(68,68),
              #show.legend = FALSE,
              test = 't.test', color = 'black')+
  stat_summary(fun.y="mean", size = 0.2, pch = 17) +
  ggtitle('e: Sunset Rockfish Only')+
  scale_x_discrete(labels=c("North of Pt. Conception","South of Pt. Conception"))

fig9 <- fig9a + fig9b + fig9c + fig9d + fig9e + plot_spacer() +
  plot_layout(ncol = 2)


fig9

t.test(`Ages` ~ `locclass`,
       data = per_location %>%
         subset(repunit == 'Sunset Rockfish'))

t.test(`Ages` ~ `locclass`,
       data = per_location %>%
         subset(repunit == 'Vermilion Rockfish'))

t.test(`Ages` ~ `repunit`,
       data = north)

t.test(`Ages` ~ `repunit`,
       data = south)

t.test(`Ages` ~ `repunit`,
       data = metadata)


ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig9-age_by_location.jpeg",
       width = 190, height = 240,
       units = c("mm"),
       dpi = 300)
fig9
dev.off() 

##### Mean Weight & Length at Depth - Fig 11 ------------------------------------
sunset <- subset(metadata, metadata$repunit == 'Sunset Rockfish')
vermilion <- subset(metadata, metadata$repunit == 'Vermilion Rockfish')

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

fig11a <- ggplot(data = combined_wt) +
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
  theme(axis.text.x = element_blank(),
        text = element_text(size = 8)) +
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

fig11b <- ggplot(data = combined_ln) +
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
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        text = element_text(size = 8)) +
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

fig11 <- fig11a/fig11b

ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/Fig10-Weight_Length_at_depths.jpeg",
       width = 190,
       height = 110,
       units = c("mm"),
       dpi = 300)
fig11
dev.off() 

