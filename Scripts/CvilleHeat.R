# .............................................
# Using heat measurements from the City and block census 
# polygons to get heat scores for each block 
# Last updated: 2023-08-09
# Michele Claibourn and Lee LeBoeuf
# .............................................
# 0. Load packages
# 1. Define Cville and load data 
# 2. Extract summaries
# 3. Reduce, Join
# 4. Merge with population data and generate csv files
# .............................................

# 0. Load packages ----
library(raster)
library(sf)
library(sp)
library(tigris)
library(tidyverse)
options(tigris_use_cache = TRUE)
library(leaflet)
library(tidycensus)

# 1. Define Cville and download data 

# a. Morning
am_img <- raster("rasters_chw_charlottesville_010622/am_t_f.tif")
am_img # WGS84
nlayers(am_img)
plot(am_img)
# image(am_img)

# b. Afternoon
af_img <- raster("rasters_chw_charlottesville_010622/af_t_f.tif")
af_img # WGS84
# nlayers(af_img)
# plot(af_img)
# image(af_img)

# c. Evening
pm_img <- raster("rasters_chw_charlottesville_010622/pm_t_f.tif")
pm_img # WGS84
# nlayers(pm_img)
# plot(pm_img)
# image(pm_img)

# define region (generated in spatial_units/get_spatial.R)
# block polygons
CvilleBlocks <- blocks(state = "51", county = "540", year = 2020)
crs(CvilleBlocks) # NAD83

# align crs in tract polygons and nlcd raster
CvilleBlocks1 <- st_transform(CvilleBlocks, proj4string(am_img))
CvilleBlocks2 <- st_transform(CvilleBlocks, proj4string(af_img))
CvilleBlocks3 <- st_transform(CvilleBlocks, proj4string(pm_img))

# Crop heat data (not sure if this is necessary)
# am 
am_img_est <- crop(am_img, CvilleBlocks1)
# plot(am_img_est)

# af 
af_img_est <- crop(af_img, CvilleBlocks2)
# plot(af_img_est)

# pm 
pm_img_est <- crop(pm_img, CvilleBlocks3)
# plot(pm_img_est)


# 2. Extract summaries ----
# am
am_extract <- raster::extract(am_img_est, CvilleBlocks1, sp = TRUE, df = TRUE, fun = mean, na.rm = TRUE) 

# af 
af_extract <- raster::extract(af_img_est, CvilleBlocks2, sp = TRUE, df = TRUE, fun = mean, na.rm = TRUE) 

# pm 
pm_extract <- raster::extract(pm_img_est, CvilleBlocks3, sp = TRUE, df = TRUE, fun = mean, na.rm = TRUE) 

# the above returned a spatal polygon object; change it back to sf
# but only need to retain the geometry for one of them. 
am_extractf <- as.data.frame(am_extract)
af_extract_blocks <- st_as_sf(af_extract)
pm_extractf <- as.data.frame(pm_extract)

# set CRS to WGS84
af_extractf <- st_transform(af_extract_blocks, crs(CvilleBlocks))

# 3. Reduce, Join and Generate csv files ----
amaf <- merge(af_extractf, am_extractf[c("GEOID20", "am_t_f")], 
                by = "GEOID20")

alltempdat <- merge(amaf, pm_extractf[,c("GEOID20","pm_t_f")], by = "GEOID20")

# 4. Merge with population data and generate csv files ----

# Pull variables
blockpop20 <- get_decennial(geography = "block",
                            table = "P4",
                            show_call = T,
                            state = "51", 
                            county = "540", 
                            year = 2020, 
                            output = "wide")

blockpop20 <- blockpop20 %>%
  dplyr::select("GEOID", "NAME", "P4_001N", "P4_002N", "P4_003N", "P4_004N", "P4_005N", "P4_006N", "P4_007N", "P4_008N", "P4_009N", "P4_010N", "P4_011N")

prettynames20 <- c("GEOID", "NAME", "Totalpop", "Hisp", "NonHisp", "OneRacepop", "WhiteAlone", "BlackAlone", "AIANalone", "AsianAlone", "NHPIalone", "OtherRaceAlone", "TwoOrMorepop")

colnames(blockpop20) <- prettynames20

# merge pop dat with heat data 

alldat <- alltempdat %>%
  left_join(blockpop20, by = c("GEOID20" = "GEOID"))

alldat <- alldat %>%
  mutate(percHisp = round(Hisp/Totalpop * 100, 2),
         percNonHisp = round(NonHisp/Totalpop * 100, 2),
         percWhite = round(WhiteAlone/Totalpop * 100, 2),
         percBlack = round(BlackAlone/Totalpop * 100, 2))

# Write rds and csv
write_rds(alldat, "cvillheat_blocks.RDS")
write_csv(alldat, "cvillheat_blocks.csv")

# save everything for updates
# save.image("cvillheat_blocks.RData")

# useful tutorials
# https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-data-r

##### Mapping data, runnning correlations, making checks #################################################################
dat <- readRDS("cvillheat_blocks.RDS")
crs(dat)
dat <- st_transform(dat, crs = 4326)

#################### Single dimension maps ############################################################

### Morning 
pal <- colorNumeric("viridis", domain = dat$am_t_f)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = dat,
              fillColor = ~pal(dat$am_t_f),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", dat$GEOID10, "<br>",
                             "Average temperature: ", round(dat$am_t_f))) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (dat$am_t_f), opacity = 0.7)


### Afternoon 
pal <- colorNumeric("viridis", domain = dat$af_t_f)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = dat,
              fillColor = ~pal(dat$af_t_f),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", dat$GEOID10, "<br>",
                             "Average temperature: ", dat$af_t_f)) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (dat$af_t_f), opacity = 0.7)

### Evening 
pal <- colorNumeric("viridis", domain = dat$pm_t_f)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = dat,
              fillColor = ~pal(dat$pm_t_f),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", dat$GEOID10, "<br>",
                             "Average temperature: ", dat$pm_t_f)) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (dat$pm_t_f), opacity = 0.7)

### Trying to figure out NA's 
pal <- colorNumeric("viridis", domain = dat$Totalpop)

leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(data = dat,
              fillColor = ~pal(dat$Totalpop),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", dat$GEOID10, "<br>",
                             "Total population: ", dat$Totalpop)) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (dat$Totalpop), opacity = 0.7)

### Trying to figure out NA's

pal <- colorNumeric("viridis", domain = missing$af_t_f)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = missing,
              fillColor = ~pal(missing$af_t_f),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("Average temperature: ", round(missing$af_t_f))) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (missing$af_t_f), opacity = 0.7)


#################### Correlations ############################################################

### Morning
# heat and percent of black residents --- NS
cor.test(dat$am_t_f, dat$percBlack)
# heat and percent of Hispanic residents --- NS
cor.test(dat$am_t_f, dat$percHisp)
# heat and percent of White residents --- Significant (r = -0.11)
cor.test(dat$am_t_f, dat$percWhite)

### Afternoon
# heat and percent of black residents --- Significant (r = 0.05)
cor.test(dat$af_t_f, dat$percBlack)
# heat and percent of Hispanic residents --- Significant (r = 0.11)
cor.test(dat$af_t_f, dat$percHisp)
# heat and percent of White residents --- Significant (r = -0.12)
cor.test(dat$af_t_f, dat$percWhite)

### Evening 
# heat and percent of black residents --- Significant (r = 0.17)
cor.test(dat$pm_t_f, dat$percBlack)
# heat and percent of Hispanic residents --- Significant (r = 0.09)
cor.test(dat$pm_t_f, dat$percHisp)
# heat and percent of White residents --- Significant (r = -0.22)
cor.test(dat$pm_t_f, dat$percWhite)


#################### Bi-chlorpleth maps ############################################################
# creating one for the strongest positive correlation above -- evening temp and percent black residents

library(leafem)
library(biscale)
library(patchwork)
library(scales)

### Legend creation
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
                    "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
                    "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-2
                    
# to show
bipal2 <- c("#be64ac", "#8c62aa", "#3b4994",
                     "#dfd0d6", "#a5add3", "#5698b9",
                     "#e8e8e8", "#ace4e4", "#5ac8c8") 
                     # A-3, B-3, C-3
# A-2, B-2, C-2
# A-1, B-1, C-1

# create class
df <- dat %>%
  mutate(temp = ntile(pm_t_f, 3),
         percBlack = ntile(percBlack, 3)) %>%
  mutate(percBlack = if_else(percBlack == 1, 'A', 
                             if_else(percBlack == 2, 'B', 'C')),
         biclass = paste0(percBlack, temp)) 

# need to deal with NA's
navalues <- c("NA2", "NA3", "NA1", "NANA", "ANA")
df$biclass <- ifelse(df$biclass %in% navalues, NA, df$biclass)

map2 <- ggplot(df) + 
  geom_sf(aes(fill = biclass), color = "white", size = 0.1, show.legend = FALSE) + 
  scale_fill_manual(values = bipal) +
  labs(title = "Percent Black and Evening Temp") +
  theme_void()

# make legend
bipal3 <- tibble(
  "3-3" = "#3b4994", # high percent Black, high temp
  "2-3" = "#8c62aa",
  "1-3" = "#be64ac", # low percent Black, high temp
  "3-2" = "#5698b9",
  "2-2" = "#a5add3", # medium percent Black, medium temp
  "1-2" = "#dfd0d6",
  "3-1" = "#5ac8c8", # high percent Black, low temp
  "2-1" = "#ace4e4",
  "1-1" = "#e8e8e8" # low percent Black, low temp
) %>%
  gather("group", "fill")

bipal3 <- bipal3 %>% 
  separate(group, into = c("percentBlack", "temp"), sep = "-") %>%
  mutate(percentBlack = as.integer(percentBlack),
         temp = as.integer(temp))

legend2 <- ggplot() +
  geom_tile(
    data = bipal3,
    mapping = aes(
      x = percentBlack,
      y = temp,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = expression("Higher percent Black" %->%  ""),
       y = expression("Higher temp" %->% "")) +
  theme_void() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6),
    axis.title.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()

# plot
map2 + inset_element(legend2, left = 0.8, bottom = 0.8, right = 1, top = 1)

hist(dat$percBlack)
hist(dat$pm_t_f)





