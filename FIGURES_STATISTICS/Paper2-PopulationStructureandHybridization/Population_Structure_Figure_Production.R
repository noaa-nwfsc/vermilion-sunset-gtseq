### Figure Production for Paper #2 on Pop Structure

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

#Set colors
pal <- pnw_palette('Sunset2', 4)
pal_species <- wes_palette("Zissou1", 4)
color_pal_hybrids <- c('vermilion' = pal[1],
                       'sunset' = pal[3],
                       'SVH' = 'lightgreen')
color_pal_hybrids2 <- c('S' = pal[1],
                        'V-C' = '#FBCEB1',
                        'V-B' = '#FEBA4F',
                        'V-A' = '#F94D00',
                        'SVH' = 'lightgreen')

pal_pop_and_species <- pnw_palette('Cascades', 4)

pops_pal <- pnw_palette('Cascades', 3)


##### Metadata ----------------------------------------------------------------
in_file <- "~/Desktop/VermilionRF/VMSURF Species ID/metadata/H&L-WCGBTS Combined_Vermilion RF Finclip Metadata_SK_051525_AW.xlsx"

#Now lets load in metadata
meta <- read_xlsx(in_file) 

table(meta$repunit)
table(meta$collection)
nrow(meta %>% subset(is.na(repunit) & Year != 2024))
table(meta$Year)
#length(!is.na(meta$`Rubias Species Call`))

meta <- meta %>%
  drop_na(`repunit`) %>%
  subset(`repunit` != 'rebs') %>%
  subset(`repunit` != 'bocaccio') %>%
  subset(`repunit` != 'canary')

#Change column names
names(meta)[names(meta) == 'Rubias Species Call'] <- 'repunit'
names(meta)[names(meta) == 'Rubias Pop Call'] <- 'collection'

#Make sure depth is a continuous variable
meta$Depth..m. <- as.numeric(meta$Depth..m.)
meta$LatDD <- as.numeric(meta$LatDD)
meta$LonDD.v2 <- as.numeric(meta$LonDD.v2)
meta$Fork.length..cm. <- as.numeric(meta$Fork.length..cm.)
meta$collection <- as.factor(meta$collection)

#### Mapping ------------------------------------------------------------------
#pdf(file = "~/Desktop/VermilionRF/VMSURF Species ID/scripts/MS_2_Figures.pdf")
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

# And now we graph those ones

map_passedsamples <- ggplot() +
  geom_sf(data = dat.eez.usa1, size = 1.5, color = "grey", fill = 'white')+
  geom_sf(data = world) +
  borders("state") +
  geom_point(data = meta, aes(x = `LonDD.v2`, y = `LatDD`, col = `repunit`))+
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

#Splitting the dataset between vermilion and sunset to get a closer look
vermilion <- subset(meta, meta$repunit == 'vermilion')
sunset <- subset(meta, meta$repunit == 'sunset')
combined <- rbind(vermilion, sunset)

combined <- combined %>%
  mutate(across(repunit, factor, levels=c("vermilion","sunset")))


## Plot the geographic distribution of the three populations of vermilion, 
#### which we colloquially called A,B, and C :)
vermilion_pop <- ggplot() +
  geom_sf(data = dat.eez.usa1, size = 1.5, color = "grey", fill = 'white')+
  geom_sf(data = world) +
  borders("state") +
  geom_text(data = labels, aes(label = label, x = long, y = lat), size.unit = 'mm', size = 2)+
  geom_segment(data = labels, 
               aes(x = start.arrow.long, y = start.arrow.lat, xend = end.arrow.long, yend = end.arrow.lat), 
               color = 'black', size = 0.5, alpha = 0.6, arrow = arrow(length = unit(0.15, "cm"))) +
  geom_point(data = vermilion, aes(x = `LonDD.v2`, y = `LatDD`, col = `collection`), size 0.5)+
  coord_sf(xlim = c(-113, -128), ylim = c(29, 50.5), expand = FALSE)+
  scale_x_continuous(breaks = c(-126, -122, -118)) +
  facet_wrap(~collection)+
  scale_color_manual(values = pops_pal) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.title=element_blank(),
    legend.position = c(0.93, 0.78),
    legend.spacing = unit(0, "pt"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))+
  geom_text(data = map_labels, aes(label = label, x= long, y = lat, angle = angle), color = 'lightgrey', size.unit = 'mm', size = 2)


vermilion_pop

ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/pop_struct_MS/Fig1-PopulationMap.jpeg",
       width = 174,
       height = 110,
       units = c("mm"),
       dpi = 900)
vermilion_pop
dev.off()

### Do we see overlap in where these samples were caught? #####
df <- as.data.frame(table(vermilion$Site.Cell.ID,vermilion$collection)) %>%
  pivot_wider(names_from = Var2, values_from = Freq ) %>%
  select(-c(S)) 

df <- df[-1,]

HL_df <- df[1:130,] %>%
  select(-c('V-A'))

non_HL_df <- df[130:142,]


df2 <- HL_df %>%
  pivot_longer(!Var1, names_to = "Pop", values_to = 'Count')

df3 <- non_HL_df %>%
  pivot_longer(!Var1, names_to = "Pop", values_to = 'Count')

unique(df2$Var1)

order <- c('Cambria', 'San Simeon', 'Piedras Blancas', 'Half Moon Bay',
           'Salmon Creek Beach', 'Gorda Rock', 'Brookings', 'Gold Beach',
           'Port Orford', 'Bandon', 'Depoe Bay', 'Strait of Juan de Fuca',
           'Kyuquot Sound, Canada')


df3 <- df3 %>%
  mutate(state = case_when(
  Var1 == 'Cambria' ~ 'CA',
  Var1 == 'San Simeon' ~ 'CA',
  Var1 == 'Piedras Blancas' ~ 'CA',
  Var1 == 'Half Moon Bay' ~ 'CA',
  Var1 == 'Salmon Creek Beach' ~ 'CA',
  Var1 == 'Gorda Rock' ~ 'CA',
  Var1 == 'Brookings' ~ 'OR',
  Var1 == 'Gold Beach' ~ 'OR',
  Var1 == 'Port Orford' ~ 'OR',
  Var1 == 'Cambria' ~ 'OR',
  Var1 == 'Bandon' ~ 'OR',
  Var1 == 'Depoe Bay' ~ 'OR',
  Var1 == 'Strait of Juan de Fuca' ~ 'WA',
  Var1 == 'Kyuquot Sound, Canada' ~ 'BC')
)

ggplot(data = df2, aes(y = Count, x = Var1)) +
  geom_bar(stat='identity', fill = 'grey')+
  facet_wrap(~Pop, ncol = 1, scales = "free") +
  xlab('Site Number') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(data = df3, aes(y = Count, x = Var1, color = state)) +
  geom_bar(stat='identity', fill = 'grey')+
  facet_wrap(~Pop, ncol = 1, scales = "free") +
  xlab('Site Number') +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_x_discrete(limits = order)

gorda <- vermilion %>%
  subset(Site.Cell.ID == 'Gorda Rock')

table(gorda$collection, gorda$Year)


#### Biological Characteristics between groups ########
vermilion$Sex <- gsub('Female', 'F', vermilion$Sex )
vermilion$Sex <- gsub('Male', 'M', vermilion$Sex )
vermilion_with_sex <- subset(vermilion, vermilion$Sex != 'Unknown')

vermilion$collection <- gsub("V-A", "VA", vermilion$collection)
vermilion$collection <- gsub("V-B", "VB", vermilion$collection)
vermilion$collection <- gsub("V-C", "VC", vermilion$collection)

old <- ggplot(data = subset(vermilion, vermilion$Sex != ''),
       aes(x = `collection`, y = `Depth..m.`,
           col = `collection`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Depth (m)')+
  stat_summary(fun="mean") +
  scale_y_continuous(limits = c(0,200), n.breaks = 4, expand = c(0,0)) +
  scale_color_manual(values = pops_pal) +
  theme(axis.title.x=element_blank())

pairwise.t.test(x = vermilion$Depth..m.,
                g = vermilion$`collection`)

youngings <- vermilion %>% 
  subset(Fork.length..cm. <= 35)
young <- ggplot(data = youngings,
       aes(x = `collection`, y = `Depth..m.`,
           col = `collection`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Depth (m)')+
  stat_summary(fun="mean") +
  scale_y_continuous(limits = c(0,200), n.breaks = 4, expand = c(0,0)) +
  scale_color_manual(values = pops_pal)+
  theme(axis.title.x=element_blank())

pairwise.t.test(x = youngings$Depth..m.,
                g = youngings$`collection`)


sumtable(vermilion, group = 'collection', 
         vars = c("Depth..m.", "Fork.length..cm.", "Wt..kg.", "LatDD"),
         digits = 3, group.test = TRUE)

young+old

# Weight versus collection

# analysis of variance
anova <- aov(`Wt..kg.` ~ collection, data = vermilion)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
wt_tbl <- group_by(vermilion, collection) %>%
  summarise(depth=mean(`Wt..kg.`, na.rm = T), sd = sd(`Wt..kg.`, na.rm = T))


# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$collection)
wt_tbl$cld <- cld$Letters

(weight <- ggplot(data = vermilion,
       aes(x = `collection`, y = `Wt..kg.`,
           col = `collection`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Weight (kg)')+
  stat_summary(fun.y="mean") +
  scale_color_manual(values = pops_pal) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.6))+
    theme(axis.title.x=element_blank(),
          text = element_text(size = 7.5))+
    geom_text(data = wt_tbl, aes(x = collection, y = c(4.4, 3.9, 4.0), label = cld), 
              size = 3, color = "black"))

pairwise.t.test(x = vermilion$Wt..kg.,
                g = vermilion$`collection`)


#Fork Length

# analysis of variance
anova <- aov(`Fork.length..cm.` ~ collection, data = vermilion)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
length_tbl <- group_by(vermilion, collection) %>%
  summarise(depth=mean(`Fork.length..cm.`, na.rm = T), sd = sd(`Fork.length..cm.`, na.rm = T))


# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$collection)
length_tbl$cld <- cld$Letters


(length <- ggplot(data = vermilion,
       aes(x = `collection`, y = `Fork.length..cm.`,
           col = `collection`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Fork Length (cm)')+
  stat_summary(fun.y="mean") +
  scale_color_manual(values = pops_pal)+
  scale_y_continuous(expand = c(0, 0), limits = c(0,75))+
  theme(axis.title.x=element_blank(),
        text = element_text(size = 7.5))+
  geom_text(data = length_tbl, aes(x = collection, y = c(69, 68, 68), label = cld), 
            size = 3, color = "black"))

pairwise.t.test(x = vermilion$Fork.length..cm.,
                g = vermilion$`collection`)



# analysis of variance
anova <- aov(`Depth..m.` ~ collection, data = vermilion)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
depth_tbl <- group_by(vermilion, collection) %>%
  summarise(depth=mean(`Depth..m.`, na.rm = T), sd = sd(`Depth..m.`, na.rm = T))


# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$collection)
depth_tbl$cld <- cld$Letters

#Depth total 
(depth <- ggplot(data = vermilion,
                aes(x = `collection`, y = `Depth..m.`,
                    col = `collection`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Depth (m)')+
  stat_summary(fun.y="mean") +
  scale_color_manual(values = pops_pal)+
  scale_y_continuous(expand = c(0, 0), limits = c(0,200))+
  theme(axis.title.x=element_blank(),
        text = element_text(size = 7.5)) +
  geom_text(data = depth_tbl, aes(x = collection, y = c(68,128, 173), label = cld), 
            size = 3, color = "black"))

pop_biological <- depth + length + weight + plot_annotation(tag_levels = 'A')



ggsave(file = "~/Desktop/VermilionRF/VMSURF Species ID/figures/pop_struct_MS/Fig2-PopulationBiological.jpeg",
       width = 174,
       height = 90,
       units = c("mm"),
       dpi = 900)
pop_biological
dev.off()

#######



# analysis of variance
anova <- aov(`Depth..m.` ~ collection, data = vermilion)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
depth_tbl <- group_by(vermilion, collection) %>%
  summarise(depth=mean(`Depth..m.`, na.rm = T), sd = sd(`Depth..m.`, na.rm = T))


# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$collection)
depth_tbl$cld <- cld$Letters



#Age
ggplot(data = vermilion,
       aes(x = `collection`, y = `Ages`,
           col = `collection`)) +
  geom_boxplot()+
  theme(legend.position = 'none') +
  ylab('Age (years)')+
  xlab('Population') +
  ggtitle('Age versus collection')+
  stat_summary(fun.y="mean") +
  scale_color_manual(values = pops_pal)+
  scale_y_continuous(expand = c(0, 0))

pairwise.t.test(x = vermilion$Ages,
                g = vermilion$`collection`)


#Length weight
ggplot(data = vermilion, aes(x = Fork.length..cm., y = Wt..kg.)) +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-B' &
                              vermilion$Sex == 'F'),
              aes(color = 'V-B female'), method = 'loess') +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-B' &
                              vermilion$Sex == 'M'),
              aes(color = 'V-B male'), method = 'loess')  +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-C' &
                              vermilion$Sex == 'F'),
              aes(color = 'V-C female'), method = 'loess') +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-C' &
                              vermilion$Sex == 'M'),
              aes(color = 'V-C male'), method = 'loess')  +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-A'&
                              vermilion$Sex == 'F'),
              aes(color = 'V-A female'), method = 'loess') +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-A'&
                              vermilion$Sex == 'M'),
              aes(color = 'V-A male'), method = 'loess')+
  theme_classic() +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))

ggplot(data = vermilion, aes(x = Fork.length..cm., y = Wt..kg.)) +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-B' &
                              vermilion$Sex == 'F'),
              aes(color = 'V-B female'), method = 'loess') +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-B' &
                              vermilion$Sex == 'M'),
              aes(color = 'V-B male'), method = 'loess')  +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-C' &
                              vermilion$Sex == 'F'),
              aes(color = 'V-C female'), method = 'loess') +
  stat_smooth(data = subset(vermilion, vermilion$collection == 'V-C' &
                              vermilion$Sex == 'M'),
              aes(color = 'V-C male'), method = 'loess')  +
  theme_classic() +
  labs(x = 'Fork Length (cm)', y = 'Weight (kg)', color = '')+
  scale_color_manual(values = pal_species)+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))


#table(VA$Wt..kg., VA$Sex)
#table(VA$Fork.length..cm., VA$Sex)

VCF <- subset(vermilion, vermilion$collection == 'V-C' &
                vermilion$Sex == 'F')%>%
  drop_na(Ages) %>%
  drop_na(`Fork.length..cm.`)
VCM <- subset(vermilion, vermilion$collection == 'V-C' &
                vermilion$Sex == 'M')%>%
  drop_na(Ages) %>%
  drop_na(`Fork.length..cm.`)
VBF <- subset(vermilion, vermilion$collection == 'V-B' &
                vermilion$Sex == 'F')%>%
  drop_na(Ages) %>%
  drop_na(`Fork.length..cm.`)
VBM <- subset(vermilion, vermilion$collection == 'V-B' &
                vermilion$Sex == 'M') %>%
  drop_na(Ages) %>%
  drop_na(`Fork.length..cm.`)

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


out <- optim(theta, fn = SSQ, method = "BFGS", xdat = VCF, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpVCF <- out$par[1] * (1 - exp(-out$par[2] * (VCF$Ages - out$par[3])))

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = VCM, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpVCM <- out$par[1] * (1 - exp(-out$par[2] * (VCM$Ages - out$par[3])))

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = VBF, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpVBF <- out$par[1] * (1 - exp(-out$par[2] * (VBF$Ages - out$par[3])))

out <- optim(theta, fn = SSQ, method = "BFGS", xdat = VBM, hessian = TRUE)
out$V <- solve(out$hessian)  #solve the hessian
out$S <- sqrt(diag(out$V))  #Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation
out$par
out$S
#For plotting - may have to specify in advance for each permutation
lpVBM <- out$par[1] * (1 - exp(-out$par[2] * (VBM$Ages - out$par[3])))

ggplot() +
  geom_line(data = VBM, aes(x = sort(Ages),
                              y = sort(lpVBM), col = "V-B male")) +
  geom_point(data = VBM, aes(x = Ages,
                              y = `Fork.length..cm.`, col = "V-B male"))+
  geom_line(data = VBF, aes(x = sort(Ages),
                              y = sort(lpVBF), col = "V-B female")) +
  geom_point(data = VBF, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-B female"))+
  geom_line(data = VCM, aes(x = sort(Ages),
                             y = sort(lpVCM), col = "V-C male")) +
  geom_point(data = VCM, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-C male"))+
  geom_line(data = VCF, aes(x = sort(Ages),
                             y = sort(lpVCF), col = "V-C female")) +
  geom_point(data = VCF, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-C female"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7, expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7, expand = c(0, 0)) +
  theme_classic()+
  scale_color_manual(values = pal_species)

ggplot() +
  geom_line(data = VBM, aes(x = sort(Ages),
                            y = sort(lpVBM), col = "V-B male")) +
  geom_point(data = VBM, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-B male"))+
  geom_line(data = VBF, aes(x = sort(Ages),
                            y = sort(lpVBF), col = "V-B female")) +
  geom_point(data = VBF, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-B female"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7, expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7, expand = c(0, 0)) +
  theme_classic()+
  scale_color_manual(values = pal_species)

ggplot() +
  geom_line(data = VCM, aes(x = sort(Ages),
                            y = sort(lpVCM), col = "V-C male")) +
  geom_point(data = VCM, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-C male"))+
  geom_line(data = VCF, aes(x = sort(Ages),
                            y = sort(lpVCF), col = "V-C female")) +
  geom_point(data = VCF, aes(x = Ages,
                             y = `Fork.length..cm.`, col = "V-C female"))+
  labs(x ="Age (yrs)", y ="Fork length (cm)", color = '') +
  scale_y_continuous(limits = c(0,65), n.breaks = 7, expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,65), n.breaks = 7, expand = c(0, 0)) +
  theme_classic()+
  scale_color_manual(values = pal_species[3:4])

ggplot(data = meta, aes(y = LatDD, x = Depth..m.)) +
  geom_point(alpha = 0.7, aes(col = collection)) +
  stat_smooth(method=lm)+
  scale_color_viridis(discrete = TRUE)

ggplot(data = meta, aes(y = LatDD, x = Depth..m., col = collection)) +
  geom_point(alpha = 0.7) +
  stat_smooth(method=lm)+
  facet_wrap(~collection)+
  scale_color_viridis(discrete = TRUE)

#dev.off()

