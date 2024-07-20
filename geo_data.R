require(googlesheets4)
require(data.table)
require(countrycode)
require(readxl)

preprocess <- function(title = 'Geography of Cal', sheet=NA, google=T, sleep=10){
  if (google == T){
    geogr <- googledrive::drive_get(title)
  }
  end_year <- as.numeric(format(Sys.Date(), '%Y'))
  ws_names <- as.character(2008:end_year)
  if (is.na(sheet)){
    geo_dfs = {}
    for(n in ws_names){
      if (google == T){
        current_sheet <- geogr %>% read_sheet(sheet = n, skip=2, col_types = 'cccDDcii?')
        Sys.sleep(sleep)
      }
      else {
        current_sheet  <- read_xlsx(path=title, sheet=n, skip=2)
      }
      #search the first column for table head
      geo_dfs[[n]] <- as.data.frame(current_sheet)
    }
    geo_all <- do.call('rbind.fill', geo_dfs)
    names(geo_all) <- c('Location', 'Country', 'State', 'Start.Date', 'End.Date', 'Color', 
                        'Nights', 'Total', 'Notes') # TODO: refactor this
  } else {
    geogr <- googledrive::drive_get(title)
    geo_all <- geogr %>% read_sheet(sheet = 2)
    geo_all <- as.data.frame(geo_all)
    names(geo_all) <- gsub(' ', '.', names(geo_all)) 
  }
  
  geo_all$Start.Date <- as.Date(geo_all$Start.Date, format='%m/%d/%Y')
  geo_all$End.Date <- as.Date(geo_all$End.Date, format='%m/%d/%Y')
  geo_all$Nights <- as.numeric(geo_all$End.Date - geo_all$Start.Date) + 1
  geo_all$Year <- format(geo_all$Start.Date, '%Y')
  setDT(geo_all)
  geo_all <- geo_all[!is.na(Location)]
  return(geo_all)
}

agg <- function(df, agg_cols){
  df <- df[, .(total = sum(eval(Total), na.rm=T)), by = eval(agg_cols)]
  return(df)
}


get_repeats <- function(df, min){
  
  repeats <- df[,.(times = length(unique(Year))), 
                c('Location', 'Country', 'State')][times>=min]$Location
  repeats_df <- df[Location %in% repeats]
  repeats_df$id <- 1:nrow(repeats_df)
  repeats_df[Location=='Red Eye', Country:='International']
  repeats_df$Location <- factor(repeats_df$Location, unique(repeats_df$Location))
  repeats_df$Country <- factor(repeats_df$Country, unique(repeats_df$Country))
  repeats.m <- data.table::melt(repeats_df[,c('Location', 'Country', 'State', 'id', 'Start.Date', 'End.Date')], 
                    id.vars = c('Location', 'Country', 'State', 'id'), value.name = 'Date')
  
  return(repeats.m)
}

unfold <- function(row, colnames, date_start, date_end){
  d1 <- as.Date(row[[date_start]])
  d2 <- as.Date(row[[date_end]])
  dates <- seq.Date(from=d1, to=d2, by='day')
  df <- data.frame(matrix(row[1:length(colnames)], nrow=1))
  names(df) <- colnames
  df <- df[rep(1, length(dates)),]
  df$Date.Start <- dates
  return(df)
}

convert_states <- function(dt, states_table, country_col='Country', state_col='State', 
                           year_col='Year', us='USA'){
  dt_us <- dt[get(country_col) == us]
  states_dt <- dt_us[, .(Nights = sum(Nights)), by = c(year_col, state_col)]
  states_dt <- merge(states_dt, states_table, by.x=state_col, by.y='Abbr')
  return(states_dt)
}

country_dates <- function(dt, date_start, continent_csv_path='country_count.csv'){
  country_count <- dt[, .(first_date = min(Start.Date)), by=c('Country')]
  country_count$count <- 1:nrow(country_count)
  
  country_continent <- read.csv(continent_csv_path)
  country_count$continent <- mapvalues(country_count$Country, 
                                       from=country_continent$Country, 
                                       to=country_continent$continent)
  country_count$iso3 <- countrycode(country_count$Country, "country.name", 'iso3c')
  country_count <- country_count[!is.na(Country)]
  country_count[is.na(iso3)]$iso3 <- 'GBR'
  country_count$year <- format(country_count$first_date, "%Y")
  return(country_count)
}

world_cities_graph <- function(df, cities_csv = 'AlphaBetaGamma.csv'){
  
}

latlon_barplot <- function(df, col, cutoff, save=T){
  extreme_indices <- c(which.min(df[,get(col)]), which.max(df[,get(col)]))
  cutoff_indices <- which(df$total > cutoff)
  text_df <- df[c(extreme_indices, cutoff_indices),]
  max_days <- max(df$total)
  max_ex <- round(log(max_days/2)/log(5))
  scale_breaks <- 5^c(1:max_ex) * 2
  p <- ggplot(df) + 
    geom_col(aes(x=get(col), y=total, fill=abs(get(col))), width=0.1)  + 
    geom_text_repel(data=text_df, aes(x=get(col), 
                                      y=ifelse(total > cutoff, total, cutoff), 
                                      label=Location), size=2, color='orange',
                    box.padding = 0.1) +
    scale_y_sqrt("Total Number of Days (sqrt)",
                 breaks = scale_breaks,
                 labels = scale_breaks) +
    scale_fill_gradient(low='dark red', high='dark blue', guide="none") +
    theme_few() + xlab(col) +
    ggtitle(paste0("Distribution by ",  col)) +
    theme(plot.title = element_text(hjust=0.5), 
          panel.background = element_rect(fill='grey90'))
  if (col == 'lat'){
    p + coord_flip()
  }
  if (save == T){
    ggsave(paste0('Plots/cumulative_', col, '.jpeg'), width = 7.8, height=6)
  }
}

get_location_on_date <- function(df, date_str, date_col='End.Date', location_col='Location'){
  ###
  # input, date_str should be "02-04" or "11-17"
  # date_col should be End.Date
  ###
  years <- range(year(df[,get(date_col)]))
  year_start <- years[1]
  year_end <- years[2]
  dates <- rep(as.Date('2020-01-01'), year_end - year_start + 1)
  locations <- rep(0, year_end - year_start + 1)
  for (year in year_start:year_end){
    i <- year - year_start + 1
    dates[i] <- as.Date(paste0(year, '-', date_str))
    if (i == 20){
      print(dates[i])
    }
    date_index <- which.max(df[,get(date_col)] >= dates[i])
    if (date_index == 1 & year > year_start){ # this happens when date is in future
      locations[i] <- NA
    } else{
      locations[i] <- df[date_index][,get(location_col)]
    }
  }
  locations <- locations[!is.na(locations)]
  return (locations)
  
}

get_locations_year <- function(df) {
  dates <- seq.Date(from = as.Date('2021-01-01'), to = as.Date('2021-12-31'), by = '1 day')
  date_strs <- sapply(dates, function(x) format(x, '%m-%d'))
  date_locations <- vector('list')
  for (i in 1:length(date_strs)){
    date <- date_strs[i]
    date_locations[[date]] <- get_location_on_date(df, date)
  }
  return (date_locations)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

plot_locations_year <- function(df){
  date_locations <- get_locations_year(df)
  num_locations <- sapply(date_locations, function(x) length(unique(x)))
  geo_df <- data.frame(date_raw = names(date_locations), n = num_locations)
  geo_df$date <- as.Date(paste0('2021', '-', geo_df$date_raw))
  geo_df$location <- sapply(date_locations, function(x) Mode(x))
  return (geo_df)
}

