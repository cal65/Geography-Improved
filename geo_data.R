require(googlesheets)
require(data.table)

preprocess <- function(title = 'Geography of Cal'){
  geogr <- gs_title(title)
  end_year <- as.numeric(format(Sys.Date(), '%Y'))
  ws_names <- as.character(2008:end_year)
  geo_dfs = {}
  for(n in ws_names){
    current_sheet <- gs_read(ss=geogr, ws = n)
    #search the first column for table head
    start_row <- which(current_sheet[,1] == 'Location')
    #bit awkward, but find the first missing data in first column after Location
    end_row <- min(which(is.na(current_sheet[start_row:nrow(current_sheet),1])))
    end_row <- ifelse(!is.finite(end_row), nrow(current_sheet), end_row)
    google_colnames <- as.character(current_sheet[start_row,])
    geo_dfs[[n]] <- as.data.frame(current_sheet[(start_row+1):(end_row+1),], 
                                  col.names = google_colnames)
    Sys.sleep(10.5)
  }
  geo_all <- do.call('rbind.fill', geo_dfs)
  names(geo_all) <- c('Location', 'Country', 'State', 'Start.Date', 'End.Date', 'Color', 'Nights', 'Total')
  geo_all$Start.Date <- as.Date(geo_all$Start.Date, format='%m/%d/%Y')
  geo_all$End.Date <- as.Date(geo_all$End.Date, format='%m/%d/%Y')
  geo_all$Nights <- as.numeric(geo_all$Nights)
  geo_all$Year <- format(geo_all$Start.Date, '%Y')
  geo_all$Total <- as.numeric(geo_all$Total)
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
