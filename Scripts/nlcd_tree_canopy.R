# .............................................
# Tree canopy data from NLCD - needs significant clean-up
# Last updated: 2023-10-10
# Beth Mitchell
# .............................................

library(FedData) 
library(sf)
library(raster)
library(tidyverse)
library(tidycensus)
library(viridis)

library(sp)
library(tigris)
options(tigris_use_cache = TRUE)
library(leaflet)

cville_blocks <- blocks(state = "51", county = "540", year = 2020)
st_crs(cville_blocks) # NAD83

# a. get tree canopy ----

tree_img <- raster("Data/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif")
tree_img
nlayers(tree_img)
plot(tree_img)

tree <- get_nlcd(
  template = cville_blocks,
  label = "Charlottesville",
  dataset = "canopy",
  year = 2021, 
  landmass = "L48"
)

plot(tree)

# # b. Check projections ----
# # Convert raster to df
tree_df <- as.data.frame(tree, xy = TRUE)
names(tree_df) <- c("x", "y", "tree_can")
# 
st_crs(tree) # check coordinate system: WGS84
st_crs(cville_blocks) # check locality polygons for reference: NAD83
# 
# # reproject polygons to same CRS as impervious

CvilleBlocks1 <- st_transform(cville_blocks, proj4string(tree_img))
tree_img_est <- crop(tree_img, CvilleBlocks1)

tree_extract <- raster::extract(tree_img_est, CvilleBlocks1, sp = TRUE, df = TRUE, fun = mean, na.rm = TRUE) 
tree_extractf <- as.data.frame(tree_extract)
tree_extract_blocks <- st_as_sf(tree_extract)

tree_extract_blocks<- st_transform(tree_extract_blocks, crs = 4326)

write_rds(tree_extract_blocks, "cvilltree_blocks.RDS")

pal <- colorNumeric("Greens", domain = tree_extract_blocks$Histogram)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = tree_extract_blocks,
              fillColor = ~pal(tree_extract_blocks$Histogram),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", tree_extract_blocks$GEOID20, "<br>",
                             "Tree Canopy: ", round(tree_extract_blocks$Histogram),"%")) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (tree_extract_blocks$Histogram), opacity = 0.7)

# didnt use the below ----

# # c. extract tree canopy ----
# # (not yet aggregated, check)
# tree_extract <- raster::extract(tree, cville_blocks, sp = TRUE, df = TRUE, fun = mean, na.rm = TRUE)
# names(tree_extract) <- c("block", "tree_can")
# 
# # the above keeps all cell values within a polygon
# # check variation within tracts/spatial units
# tree_extract %>% group_by(block) %>% 
#   summarize(cells = n(), 
#             miss = sum(is.na(tree_can)),
#             mean = mean(tree_can, na.rm = TRUE), 
#             std = sd(tree_can, na.rm = TRUE))
# 
# # replace NA with 0
# # given equal grid sizes, omitting NAs that have no tree canopy will
# # generate inflated estimates of percent tree canopy
# tree0 <- tree
# tree0[is.na(tree0[])] <- 0 
# 
# tree0_extract <- raster::extract(tree0, cville_blocks, df = TRUE)
# names(tree0_extract) <- c("block", "tree_can")
# 
# # use the mean as the summary function for the moment
# tree_mean <- raster::extract(tree0, cville_blocks, df = TRUE, 
#                              fun = mean, na.rm = TRUE,
#                              sp = TRUE)
# 
# # tree_mean_blkgps <- raster::extract(tree0, cville_blkgps, df = TRUE,
# #                                     fun = mean, na.rm = TRUE,
# #                                     sp = TRUE)
# # 
# # tree_mean_blocks <- raster::extract(tree0, cville_blocks, df = TRUE,
# #                                     fun = mean, na.rm = TRUE,
# #                                     sp = TRUE)
# 
# # the above returned a spatal polygon object; change it back to sf
# tree_blocks <- st_as_sf(tree_mean)
# tree_blocks <- tree_blocks %>% 
#   rename(tree_can = layer)
# 
# ggplot() + 
#   geom_sf(data = tree_blocks, aes(fill = tree_can)) +
#   scale_fill_viridis_c()
# # this produces the tract level data for Cville
# 
# # tree_blkgps <- st_as_sf(tree_mean_blkgps)
# # tree_blkgps <- tree_blkgps %>% 
# #   rename(tree_can = layer)
# # 
# # ggplot() + 
# #   geom_sf(data = tree_blkgps, aes(fill = tree_can)) +
# #   scale_fill_viridis_c()
# # # this produces the block group level data for Cville
# # 
# # tree_blocks <- st_as_sf(tree_mean_blocks)
# # tree_blocks <- tree_blocks %>% 
# #   rename(tree_can = layer)
# 
# ggplot() + 
#   geom_sf(data = tree_blocks, aes(fill = tree_can)) +
#   scale_fill_viridis_c()
# # this produces the block level data for Cville
# 
# 
# # d. write to csv file ----
# # remove unncessary variables and geomstry
# treet_tract_reduced <- tree_tracts %>% 
#   select(STATEFP:NAMELSAD, tree_can) %>% 
#   st_drop_geometry()
# 
# tree_blkgps_reduced <- tree_blkgps %>% 
#   select(STATEFP:NAMELSAD, tree_can) %>% 
#   st_drop_geometry()
# 
# tree_blocks_reduced <- tree_blocks %>% 
#   select(STATEFP10:NAME10, tree_can) %>% 
#   st_drop_geometry()
# 
# write_csv(treet_tract_reduced, "data/nlcd_tree_cville_tracts.csv")
# write_csv(tree_blkgps_reduced, "data/nlcd_tree_cville_blkgps.csv")
# write_csv(tree_blocks_reduced, "data/nlcd_tree_cville_blocks.csv")
# 
# # save everything for updates
# save.image("data/nlcd_generate.RData")
# 

