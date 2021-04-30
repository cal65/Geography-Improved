require(data.table)
require(geosphere)

geographic_midpoint <- function(lon, lat, weight = NULL) {
  if (is.null(weight)) {
    weight <- rep(1, length(lon))
  }
  # degrees to radians
  lat <- lat * pi / 180
  lon <- lon * pi / 180
  # cartesian coordinates
  x <- cos(lat) * cos(lon)
  y <- cos(lat) * sin(lon)
  z <- sin(lat)
  # weighted mean
  x <- weighted.mean(x, w = weight)
  y <- weighted.mean(y, w = weight)
  z <- weighted.mean(z, w = weight)
  # convert to lat and lon
  lon <- atan2(y, x) * 180 / pi
  hyp <- sqrt(x * x + y * y)
  lat <- atan2(z, hyp) * 180 / pi
  
  data.frame(lon = lon, lat = lat)
}
calculate_midpoint <- function(df, lat, lon, agg, weight){
  df <- df[!is.na(lat) & !is.na(lon)]
  agg_list <- vector('list')
  geo_list <- vector('list')
  aggregates <- unique(df[[agg]])
  for (ag in aggregates){
    agg_list[[ag]] <- df[get(agg) == ag]
    geo_list[[ag]] <- geographic_midpoint(agg_list[[ag]][[lon]], 
                                          agg_list[[ag]][[lat]], 
                                          agg_list[[ag]][[weight]]
    )
    geo_list[[ag]][[agg]] <- ag
  }
  midpoint_df <- do.call('rbind', geo_list)
  names(midpoint_df) <- mapvalues(names(midpoint_df), from = c(lat, lon),
                                  to = c('lat_mid', 'lon_mid'))
  return(midpoint_df)
}



