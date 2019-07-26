library(googlesheets)
library(ggplot2)
library(ggmap)
library(plyr)
library(data.table)
library(scales)
library(rworldmap)
library(countrycode)
library(RColorBrewer)
setwd('~/Documents/CAL/Real_Life/Geography-Improved/')
options(stringsAsFactors = F)
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = Sys.getenv(x='GOOGLE_API'))  

# which google sheets do you have access to?
# may ask you to authenticate in a browser!
gs_ls()

geogr <- gs_title('Geography of Cal')
end_year <- 2019
ws_names <- as.character(2008:end_year)
for(n in ws_names){
  current_sheet <- gs_read(ss=geogr, ws = n)
  #search the first column for table head
  start_row <- which(current_sheet[,1] == 'Location')
  #bit awkward, but find the first missing data in first column after Location
  end_row <- min(which(is.na(current_sheet[start_row:nrow(current_sheet),1])))
  end_row <- ifelse(!is.finite(end_row), nrow(current_sheet), end_row)
  google_colnames <- as.character(current_sheet[start_row,])
  assign(paste0('geo_df', n), as.data.frame(current_sheet[(start_row+1):(end_row+1),], 
                                            col.names = google_colnames))
  Sys.sleep(10.5)
}
#initiate geo_all by  combining first two dataframes
geo_all <- rbind(geo_df2008[,1:8], geo_df2009[,1:8])
for (year in 2010:end_year){
  geo_all <- rbind(geo_all, get(paste0('geo_df', year))[,1:8])
}
names(geo_all) <- c('Location', 'Country', 'State', 'Start.Date', 'End.Date', 'Color', 'Nights', 'Total')
geo_all$Start.Date <- as.Date(geo_all$Start.Date, format='%m/%d/%Y')
geo_all$End.Date <- as.Date(geo_all$End.Date, format='%m/%d/%Y')
geo_all$Nights <- as.numeric(geo_all$Nights)
geo_all <- data.table(geo_all)
geo_all <- geo_all[!is.na(Location)]

#repeats <- names(which(table(geo_all$Location)>1))
geo_all$Location <- mapvalues(geo_all$Location, from='New York City', to='New York')
geo_all$Location <- mapvalues(geo_all$Location, from='Airplane', to='Red Eye')
repeats <- geo_all[,.(times = length(unique(format(Start.Date, "%Y")))), Location][times>3]$Location

total_nights <- geo_all[, .(total=sum(Nights, na.rm=T), uni = length(unique(Start.Date)), sd_d=sd(Start.Date),
                            first_year = min(format(Start.Date, '%Y')), 
                            last_year=max(format(Start.Date, '%Y'))), 
                        by=c('Location', 'Country')][order(total, decreasing = T)]

repeats_geo <- geo_all[Location %in% repeats, 1:5]
repeats_geo$id <- 1:nrow(repeats_geo)
repeats_geo.m <- melt(repeats_geo, id.vars = c('Location', 'Country', 'State', 'id'), value.name = 'Date')
ggplot(repeats_geo.m) + geom_line(aes(x=Date, y=Location, group=id, color=Location)) + 
  geom_point(aes(x=Date, y=Location, color=Location)) +
  scale_x_date(labels = date_format("%d-%m-%Y")) + scale_color_discrete(guide=F) +
   ggtitle('Repeated Locations Over the Years') 
  

loc_refs <- read.csv('total_nights4.csv')
total_nights <- merge(total_nights, loc_refs[, c('Location', 'Country', 'lon', 'lat')], by = c('Location', 'Country'), all.x=T)


m1 <- borders('world', fill='black', size=0.2)
m2 <- borders('state', fill='black', size=0.2, colour='dark blue')
ggplot() + m1 + geom_point(data=total_nights, aes(x=lon, y=lat, size=total), color='sky blue', alpha=0.6) +
  scale_size_continuous(range = c(0.3,5))

missing_coords <- intersect(which(is.na(total_nights$lat)), which(!total_nights$Location %in% c('Bus', 'Red Eye', 'Train')))
for (i in missing_coords){
  total_nights[i,c('lon', 'lat')] <- geocode(paste(total_nights$Location[i], total_nights$Country[i],sep=', '))
}

bp <- colorRampPalette(brewer.pal(11, 'PiYG'))(length(unique(total_nights$first_year)))

ggplot() + m1 + m2 + geom_point(data=total_nights[last_year>2007], 
                           aes(x=lon, y=lat, size=total, color=as.factor(last_year),
                           fill=as.factor(first_year)), shape=21, alpha=0.6) +
  scale_size_continuous(range = c(0.1,6)) +
  scale_color_manual('Year Last', values=bp, guide=F) +
  scale_fill_manual('Year First', values=bp) +
  ggtitle('Geography of Cal') + theme(plot.title = element_text(hjust=0.5, size=12))
ggsave('Geography_Cal4.jpeg', width=13.5, height=8, dpi=750)
  #Country chart
country_count <- geo_all[, .(first_date = min(Start.Date)), by=c('Country')]
country_count$count <- 1:nrow(country_count)

country_continent <- read.csv('country_count.csv')
country_count$continent <- mapvalues(country_count$Country, from=country_continent$Country, to=country_continent$continent)
country_count$continent <- mapvalues(country_count$continent, from='Brazil', to='South America')

ggplot(country_count) + geom_step(aes(x=first_date, y=count)) +
  geom_text(aes(x=first_date, y=count, label=Country, color=continent), hjust=0, vjust=1.2) +
  ggtitle('New Country Progression') + theme(legend.position = 'bottom',
  plot.title = element_text(hjust=0.5, size=12), panel.background = element_blank()) +
  xlab('') + expand_limits(x=as.Date('2018-09-01'))
ggsave('Country_Count.jpeg', width=9, height=6, dpi=300)

country_count$iso3 <- countrycode(country_count$Country, "country.name", 'iso3c')
country_count <- country_count[!is.na(Country)]
country_count[is.na(iso3)]$iso3 <- 'GBR'
country_count$year <- format(country_count$first_date, "%Y")
malMap <- joinCountryData2Map(country_count, joinCode = "ISO3",
                              nameJoinColumn = "iso3")
mapCountryData(malMap, nameColumnToPlot="year", catMethod = "categorical",
               missingCountryCol = gray(.8))
library(googleVis)
geo_world <- gvisGeoChart(country_count, 'Country', 'year') +
  gvisGeoChart(total_nights, 'latlong', 'first_year')
plot(geo_world)

#city classification plot
alpha <- read.csv('AlphaBetaGamma.csv')
alpha$City.Name <- gsub('^ ', '', alpha$City.Name)
setDT(alpha)
geo_simp <- geo_all
geo_simp$Location <- mapvalues(geo_simp$Location, from=c('Kowloon', 'Aberdeen', 'Brooklyn', 'Newton', 'Cambridge', 'Santa Monica', 
                     'Washington', 'Arlington', 'Encinitas', 'Huntington Beach', 'Manhattan', 'Indian Rocks Beach'), 
                     to=c('Hong Kong', 'Hong Kong', 'New York', 'Boston', 'Boston', 'Los Angeles', 'Washington, D.C.',
                                    'Washington, D.C.', 'San Diego', 'Los Angeles', 'New York', 'Tampa'))
geo_simp$Year <- format(geo_simp$Start.Date, '%Y')
geo_years <- geo_simp[, .(Nights = sum(Nights)), by=c('Location', 'Country', 'Year')]
#manual add cause I spent a day in Miami
added_df <- data.frame(Location= c('Tianjin', 'Miami', 'Philadelphia', 'Philadelphia', 'Cincinnati', 'Seoul'),
                       Country= c('China', 'USA', 'USA', 'USA', 'USA', 'South Korea'), Year= c(2010, 2011, 2013, 2017, 2018, 2018), 
                       Nights=c(1, 1, 1,1, 2, 1))
geo_years <- rbind(geo_years, added_df)
alpha$Rank <- factor(alpha$Rank, levels = unique(alpha$Rank)) #this works because of the order of the spreadsheet
major_cities <- merge(geo_years, alpha, by.x='Location', by.y='City.Name')
#some manual fixes
major_cities <- major_cities[Location != 'Portland'] #I haven't been to that Portland
#tile plot
ggplot(major_cities) + geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=Continent), color='black') +
  facet_grid(Rank ~ ., scales='free', space='free') +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('CityYears2.jpeg', width=10, height=7.4, dpi=330)
