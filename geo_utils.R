require(data.table)
require(geosphere)
require(ggmap)
move_origin <- function(df, long='long', lat='lat', orig_x, orig_y, height){
  lat_range <- range(df[[lat]], na.rm=T)
  long_range <- range(df[[long]], na.rm=T)
  if ((long_range[2] - long_range[1]) > 180){
    df[[long]] <- ifelse(df[[long]] <0, 
                         df[[long]] + 360,
                         df[[long]])
    long_range <- range(df[[long]], na.rm=T)
  }
  transform_mult <- max(height/(lat_range[2] - lat_range[1]), 4)

  df[[long]] <- (df[[long]] - min(df[[long]], na.rm=T)) * transform_mult + orig_x
  df[[lat]] <- (df[[lat]] - min(df[[lat]], na.rm=T)) * transform_mult + orig_y
  return (df)
}

ignore_regions <- c('Macquarie Island', 'Hawaii', 'Alaska', 'Easter Island') 
# these outlying areas ruin the graph normalization
top_lang_map_copy <- top_lang_map[!subregion %in% ignore_regions][!is.na(long)]
top_lang_map_copy <- top_lang_map_copy[order(total)]
country_ranges = vector('list')
unique_langs <- unique(top_lang_map_copy$Languages)
# loop through each language
for (i in 1:length(unique_langs)){
  l = unique_langs[i]
  lang_df <- top_lang_map_copy[Languages == l]
  orig_x <- 0
  unique_countries <- unique(lang_df$Country)
  # for each language, loop through each country
  for (cy in unique_countries){
    top_lang_map_copy[Country == cy & Languages == l] <- move_origin(lang_df[Country == cy], 
                                                orig_x=orig_x, orig_y = i*6,  height=20)
    # add 2 to the original x axis
    orig_x <- max(top_lang_map_copy[Country == cy & Languages == l]$long, na.rm=T) + 5000/(length(unique_countries)^2)
    country_ranges[[cy]] <- range(top_lang_map_copy[Country == cy]$long)
  }
}
country_range_df <- as.data.frame(unlist(lapply(country_ranges, function(x) x[2] - x[1])))
palette <- c('White', brewer.pal(7, 'Blues')[-1]) # use blues palette but a real white for "never" color
ggplot(top_lang_map_copy) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=total_bucket, 
                   text=paste(Country, total, sep='\n')), color='black') +
  facet_grid(Languages ~ ., scales='free', space = 'free') +
  scale_fill_manual(values = palette, 'Total Nights') +
  ggtitle('Countries by Official Languages') +
  theme(plot.title = element_text(hjust=0.5),
        strip.text.y = element_text(angle=0),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = 'bottom')
ggsave('Official_lang_outlines.jpeg', width=12, height=15)
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
lang_html <- ggplotly(tooltip = c('text')) %>% 
                      layout(legend = list(orientation = "h", y=0), 
                             autosize=F, width = 900, height = 900, margin=m)
saveWidget(as_widget(lang_html), "language_outlines.html")


ggplot(top_lang_map_copy[Languages != 'English']) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=total_bucket), color='black') +
  facet_grid(Languages ~ ., scales='free', space = 'free') +
  scale_fill_manual(values = palette, 'Total Nights') +
  ggtitle('Countries by Official Languages') +
  theme(plot.title = element_text(hjust=0.5),
        strip.text.y = element_text(angle=0),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())
ggsave('Official_lang_outlines_noE.jpeg', width=12, height=11)

extract_countries(vec){
  country_splits <- strsplit(vec, ',(\\s)')
}

library(sp)
library(sf)
library(xlsx)
unesco <- read.xlsx('external/whc-sites-2019.xls', sheetIndex = 1)
crs_wgs84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spatial_unesco <- SpatialPointsDataFrame(coords = unesco[,c('longitude', 'latitude')],
  data = unesco,
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
spatial_unesco <- st_as_sf(spatial_unesco)
st_crs(spatial_unesco) <- crs_wgs84
spatial_unesco_km <- st_transform(spatial_unesco, "+proj=utm +zone=42N +datum=WGS84 +units=km")
spatial_unesco_buffer <- st_buffer(spatial_unesco_km, 60)
total_nights_sdf <- SpatialPointsDataFrame(coords = total_nights[!is.na(lon),c('lon', 'lat')],
                                           data = total_nights[!is.na(lon)],
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
total_nights_sdf <- st_as_sf(total_nights_sdf)
st_crs(total_nights_sdf) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
total_nights_sdf_km <- st_transform(total_nights_sdf, "+proj=utm +zone=42N +datum=WGS84 +units=km")
indices <- sapply(st_contains(spatial_unesco_buffer, total_nights_sdf_km), function(z) if (length(z)==0) NA_integer_ else z[1])
indices <- unique(indices)
unesco[indices, c('name_en', 'category_short', 'states_name_en')]

joined_df <- st_join(spatial_unesco_buffer, total_nights_sdf_km, join = st_intersects)
setDT(joined_df)
my_df <- unique(joined_df[!is.na(lon), c('name_en', 'category_short', 
                                         'states_name_en', 'Location', 'total')])



climate <- read.csv('external/other_climate_2007_koppen_geiger.csv')
climate$geometry <- st_as_sfc(climate$geometry)
climate <- st_as_sf(climate)
st_crs(climate) <- crs_wgs84
climate_km <- st_transform(climate, "+proj=utm +zone=42N +datum=WGS84 +units=km")
climate_joined_df <- st_join(climate, total_nights_sdf, join = st_intersects)
setDT(climate_joined_df)
climate_joined_df <- unique(climate_joined_df[!is.na(lon), 
                           c('climate',  'Location', 'State', 'Country', 'total')])
climate_joined_df$climate_simp <- substr(climate_joined_df$climate, 1,2)
climate_joined_df[, .(total=sum(total), n_countries=length(unique(Country))),
                  by='climate_simp']
ggplot(climate_joined_df) +
  geom_col(aes(x=climate_simp, y=total)) +
  coord_flip()

### Function to regroup split lines and polygons
# Takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) { # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1 # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2 # parts that are moved
  }
  g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# Takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df)) # rassign the order variable
  df[,ordercol] <- o
  df
}

getWorldMap <- function(limits_x=c(-150, 170), limits_y = c(-15,85)){
  worldmap <- map_data ("world", resolution=0, xlim=limits_x, ylim=limits_y)
  worldmap$long.recenter <- ifelse(worldmap$long < center - 180 , worldmap$long + 360, worldmap$long)
  # now regroup
  worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
  # close polys and use the new grouping var
  worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order") 
  return(worldmap.cp)
}

arcMap <- function(df, lon_col='lon', lat_col='lat', zoom=2,
                   year=2023) {
  l_cols <- c(lon_col, lat_col)
  G_coords<-df[(!is.na(get(lon_col)))]
  n <- nrow(G_coords)-1
  lines <- vector('list', length=n)
  center <- 180
  for(i in 1:(n)){
    line_df <- as.data.frame(gcIntermediate(G_coords[i, l_cols, with=F], 
                                             G_coords[i+1, l_cols,with=F]))
    if (names(line_df)[1] != 'lon') {# this can happen if back to back coordinates are the same
      names(line_df) <- c('lon', 'lat')
      }
    lines[[i]]<- line_df
    # for(j in 1:nrow(lines[[i]])){
    #   lines[[i]]$lon[j] <- ifelse(lines[[i]]$lon[j]< center - 180, lines[[i]]$lon[j] + 360, lines[[i]]$lon[j]) 
    # }
  }
  limits_y <- range(G_coords[,get(lat_col)]) + c(-20, 20)
  limits_x <- range(G_coords[,get(lon_col)]) + c(-30, 240)
  print(limits_x)
  G_coords$lon_recenter <- ifelse(G_coords[,get(lon_col)] < center - 180, G_coords[,get(lon_col)] + 360, G_coords[,get(lon_col)])
  # Plot worldmap using data from worldmap.cp
  worldmap.cp <- getWorldMap(limits_x=limits_x)
  lat_mean = mean(range(G_coords[,get(lat_col)]))
  lon_mean = mean(range(G_coords[,get(lon_col)]))
  worldmap = get_map(location = c(lon_mean, lat_mean), maptype="terrain", zoom=zoom)
  sweetmap <- ggmap(worldmap) + 
    geom_point(data=G_coords, aes(x=lon, y=get(lat_col), size=sqrt(Nights), color=Country), 
               alpha=0.5) +
    scale_color_brewer(palette='Set1')
  
  line_palette <- colorRampPalette(c('black', 'darkorange1'), alpha = TRUE)(n)
  
  for(i in 1:(n)){
    sweetmap <-sweetmap + geom_path(data=lines[[i]], aes(x=lon, y=lat), 
                                    linewidth=.4, color=line_palette[i], alpha=0.5)
  }
  sweetmap <- sweetmap + ggtitle(paste0("Cal Travels -", as.character(year))) +
    theme(plot.title = element_text(hjust=0.5))
  return(sweetmap)
}

arcMap(geo_coords[Year==2023][order(Start.Date)], zoom=3)
x