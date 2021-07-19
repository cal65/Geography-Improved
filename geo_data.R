require(googlesheets4)
require(data.table)

preprocess <- function(title = 'Geography of Cal', sheet=NA){
  geogr <- googledrive::drive_get(title)
  end_year <- as.numeric(format(Sys.Date(), '%Y'))
  ws_names <- as.character(2008:end_year)
  if (is.na(sheet)){
    geo_dfs = {}
    for(n in ws_names){
      current_sheet <- geogr %>% read_sheet(sheet = n, skip=2, col_types = 'cccDDcii?')
      #search the first column for table head
      geo_dfs[[n]] <- as.data.frame(current_sheet)
      Sys.sleep(10.5)
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
  
  repeats <- df[,.(times = length(unique(Year))), Location][times>=min]$Location
  repeats_df <- df[Location %in% repeats]
  repeats_df$id <- 1:nrow(repeats_df)
  repeats_df[Location=='Red Eye', Country:='International']
  repeats_df$Location <- factor(repeats_df$Location, unique(repeats_df$Location))
  repeats_df$Country <- factor(repeats_df$Country, unique(repeats_df$Country))
  repeats.m <- melt(repeats_df[,c('Location', 'Country', 'State', 'id', 'Start.Date', 'End.Date')], 
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

