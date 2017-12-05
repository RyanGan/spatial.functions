library(tidyverse)
library(sf)

# define my custom intersection function as it stands now ----
# consider using only sf and base r instead of tidyverse. if this moves far enough
# along, take a look at other sf functions to see how they go about the problem
proportion_intersect <- function(poly_sf, poly_id, grid_sf, grid_id){
  # enquo lazy eval
  poly_id <- enquo(poly_id)
  grid_id <- enquo(grid_id)
  # subset grid that contains poly_i
  grid_i <- grid_sf[poly_sf,]
  # proportion intersect
  intersect_sf <- st_intersection(grid_i, poly_sf) %>%
    # filter only to polygon or multipolygon type
    # to avoid errors with point or line types
    filter(st_is(., c("POLYGON", "MULTIPOLYGON"))==T)
  # calculation of proportion intersect
  proportion <- as.numeric(st_area(intersect_sf)/st_area(grid_i)) %>%
    data_frame() %>% rename(proportion = ".")
  # column bind the proportion to the intersect sf object
  output_df <- intersect_sf %>%
    # eventually replace these with generic names
    dplyr::select(!!grid_id, !!poly_id) %>%
    bind_cols(proportion)
  # remove geometry
  st_geometry(output_df) <- NULL
  return(output_df)
}

# prepare example sf objects --------
# read california sf object
california_sf <- read_sf(dsn="./data/california", layer="california")

# bbox of california
cali_bbox <- st_bbox(california_sf)

# read bluesky shape
bluesky <- read_sf(dsn="./data/bluesky_grid", layer="bluesky_grid")
# subset to california intersection

bluesky2 <- st_intersection(bluesky, california_sf)

ggplot(bluesky2) +
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

# i now have a smaller grid area that covers only california.
# i plan to go county by county to find the intersection with grids in that county
# i believe this is likely a better approach as some grids that partially overlapped
# smaller counties were only capturing grids that were 100% in the county.

# take first county (Los Angles county)
la_county <- california_sf %>% filter(NAME == "Los Angeles")

# plot la county
ggplot(la_county) + geom_sf() + theme_minimal()

# clip to grids in the LA bounding box area
la_bbox <- st_bbox(la_county)
la_bbox
# apply custom bbox clip function
la_grid <- bbox_clip(clip_bluesky, la_bbox)

# plot
ggplot(la_grid) +
  geom_sf(alpha = 0) +
  geom_sf(data=la_county, alpha = 0) +
  theme_minimal()


# find intersection of one grid
la_pi <- proportion_intersect(poly_sf = la_county, poly_id = GEOID,
                              grid_sf = la_grid, grid_id = )

?proportion_intersect
