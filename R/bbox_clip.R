#' Clip an sf object by a bounding box


# clip by bbox function ------
bbox_clip <- function(sf, bbox) {
  # find the CRS of the sf object
  crs <- sf::st_crs(sf)$proj4string
  # create matrix
  x <- c(bbox[1], bbox[1], bbox[3], bbox[3], bbox[1])
  y <- c(bbox[2], bbox[4], bbox[4], bbox[2], bbox[2])
  coords <- matrix(cbind(x, y), ncol=2)
  # create polygon and assign same coord crs as sf object
  coords_poly <- sp::Polygon(coords)
  bbox_poly <- sp:: SpatialPolygons(list(sp::Polygons(list(coords_poly),
    ID = "bbox")), proj4string = sp::CRS(crs))
  # convert to sf feature
  bbox_sf <- st_as_sf(bbox_poly)
  # clip sf object
  clipped_sf <- sf[bbox_sf,]
  return(clipped_sf)
}


