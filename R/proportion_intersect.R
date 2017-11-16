library(tidyverse)
library(sf)

# bbox of california
cali_bbox <- st_bbox(california_sf)

# read bluesky shape
bluesky <- read_sf(dsn="./data/bluesky_grid", layer="bluesky_grid")

bluesky2 <- st_intersection(bluesky, california_sf)

ggplot(bluesky2) +
  geom_sf()

grid_test <- bluesky %>% slice(1)

ggplot(grid_test) +
  geom_sf()

crs <- st_crs(california_sf)$proj4string



x <- c(cali_bbox[1], cali_bbox[1], cali_bbox[3], cali_bbox[3], cali_bbox[1])
y <- c(cali_bbox[2], cali_bbox[4], cali_bbox[4], cali_bbox[2], cali_bbox[2])
coords <- matrix(cbind(x, y), ncol=2)

bbox_poly <- sp::Polygon(coords)
bbox_poly2 <- sp:: SpatialPolygons(list(sp::Polygons(list(bbox_poly), ID = "a")),
  proj4string = sp::CRS(crs))

# convert to sf feature
bbox_sf <- st_as_sf(bbox_poly2)

# clip bluesky grid
clip_bluesky <- bluesky[bbox_sf,]

ggplot(bbox_sf) +
  geom_sf() +
  geom_sf(data=california_sf, alpha = 0.0) +
  geom_sf(data=clip_bluesky, alpha = 0.0)


cali_county <- california_sf %>% slice(1)

unlist(cali_county$geometry)


