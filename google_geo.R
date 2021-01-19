library(googlesheets)
library(ggplot2)
library(ggmap)
library(plyr)
library(data.table)
library(scales)
library(rworldmap)
library(countrycode)
library(RColorBrewer)
library(plotly)
setwd('~/Documents/CAL/Real_Life/Geography-Improved/')
options(stringsAsFactors = F)
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = Sys.getenv(x='GOOGLE_API'))  

# which google sheets do you have access to?
# may ask you to authenticate in a browser!
gs_ls()

geogr <- gs_title('Geography of Cal')
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
#initiate geo_all by  combining first two dataframes
geo_all <- do.call('rbind.fill', geo_dfs)

names(geo_all) <- c('Location', 'Country', 'State', 'Start.Date', 'End.Date', 'Color', 'Nights', 'Total')
geo_all$Start.Date <- as.Date(geo_all$Start.Date, format='%m/%d/%Y')
geo_all$End.Date <- as.Date(geo_all$End.Date, format='%m/%d/%Y')
geo_all$Nights <- as.numeric(geo_all$Nights)
geo_all <- data.table(geo_all)
geo_all <- geo_all[!is.na(Location)]

# Manual mapping of some similar location names
geo_all$Location <- mapvalues(geo_all$Location, from='New York City', to='New York')
geo_all$Location <- mapvalues(geo_all$Location, from='Airplane', to='Red Eye')
geo_all$Location <- mapvalues(geo_all$Location, from='Aberdeen', to='Hong Kong')

squash <- TRUE
if (squash){
  geo_all$Location <- mapvalues(geo_all$Location, from='Kowloon', to='Hong Kong')
  geo_all$Location <- mapvalues(geo_all$Location, from='Cambridge', to='Boston')
}

#if the year ends in the same place as the next year begins, we have an extra spot
for (i in 2:nrow(geo_all)){
  if (geo_all$Location[i] == geo_all$Location[i-1]){
    replacement <- geo_all[i-1,]
    replacement$End.Date = geo_all$End.Date[i]
    replacement$Nights = as.numeric(replacement$End.Date - replacement$Start.Date) + 1
    geo_all <- rbind(geo_all[1:(i-2),], replacement, geo_all[(i+1):nrow(geo_all),])
    print(replacement)
    if (i >= nrow(geo_all)){
      break
    }
  }
}

repeats <- geo_all[,.(times = length(unique(format(Start.Date, "%Y")))), Location][times>=3]$Location

total_nights_step <- geo_all[, .(total=sum(Nights, na.rm=T), uni = length(unique(Start.Date)), sd_d=sd(Start.Date),
                            first_year = min(format(Start.Date, '%Y')), 
                            last_year=max(format(Start.Date, '%Y'))), 
                        by=c('Location', 'Country', 'State')][order(total, decreasing = T)]

repeats_geo <- geo_all[Location %in% repeats, 1:5]
repeats_geo$id <- 1:nrow(repeats_geo)
repeats_geo[Location == 'Red Eye']$Country <- 'International'
repeats_geo$Location <- factor(repeats_geo$Location, unique(repeats_geo$Location))
repeats_geo$Country <- factor(repeats_geo$Country, unique(repeats_geo$Country))
repeats_geo.m <- melt(repeats_geo, id.vars = c('Location', 'Country', 'State', 'id'), value.name = 'Date')
ggplot(repeats_geo.m) + geom_line(aes(x=Date, y=Location, group=id, color=Country), size=0.5) + 
  geom_point(aes(x=Date, y=Location, color=Country), size=.4, shape=23) +
  scale_x_date(labels = date_format("%Y"), breaks='year') + 
  scale_color_brewer(palette='RdBu') + 
  theme(legend.position="bottom", plot.title = element_text(hjust=0.5),
        panel.background = element_rect(fill='grey30'),
        panel.grid.major = element_blank()) +
   ggtitle('Repeated Locations Over the Years') 
ggsave('Repeats.jpeg', width=13.5, height=5, dpi=550)


loc_refs <- read.csv('total_nights4.csv')
total_nights <- merge(total_nights_step, loc_refs[, c('Location', 'Country', 'lon', 'lat')], by = c('Location', 'Country'), all.x=T)


m1 <- borders('world', fill='black', size=0.2, alpha=0.8)
m2 <- borders('state', fill='black', size=0.2, colour='dark blue', alpha=0.2)
ggplot() + m1 + geom_point(data=total_nights, aes(x=lon, y=lat, size=total), color='sky blue', alpha=0.6) +
  scale_size_continuous(range = c(0.3,5))

missing_coords <- intersect(which(is.na(total_nights$lat)), which(!total_nights$Location %in% c('Bus', 'Red Eye', 'Train')))
for (i in missing_coords){
  address <- with(total_nights, if (!is.na(State[i])) paste(Location[i], State[i], Country[i],sep=', ') else 
    paste(Location[i], Country[i], sep =', '))
  total_nights[i,c('lon', 'lat')] <- geocode(address)
}

bp <- colorRampPalette(brewer.pal(11, 'PiYG'))(length(unique(total_nights$first_year)))

ggplot() + m1 + m2 + geom_point(data=total_nights[last_year>2007], 
                           aes(x=lon, y=lat, size=sqrt(total+1), color=last_year,
                           fill=first_year, text=Location), shape=21, alpha=0.8) +
  scale_size_continuous('Total Nights (sq rt)', range = c(0.05,5),
                        breaks = c(2, 10, 30)) +
  scale_color_manual('Year Last', values=bp, guide=F) +
  scale_fill_manual('Year First', values=bp) +
  ggtitle('Geography of Cal') + 
  theme(plot.title = element_text(hjust=0.5, size=12), 
        panel.background = element_rect(fill=alpha('blue', 0.2)))
ggsave('Geography_Cal6.jpeg', width=13.5, height=8, dpi=750)

#plotly
ggplotly()
  
#Country chart
country_count <- geo_all[, .(first_date = min(Start.Date)), by=c('Country')]
country_count$count <- 1:nrow(country_count)

country_continent <- read.csv('country_count.csv')
country_count$continent <- mapvalues(country_count$Country, from=country_continent$Country, to=country_continent$continent)

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
geo_simp$Location <- mapvalues(geo_simp$Location, from=c('Kowloon', 'Aberdeen', 'Brooklyn', 'Newton', 'Cambridge', 
                     'Santa Monica', 'Washington', 'Arlington', 'Encinitas', 'Huntington Beach', 
                     'Manhattan', 'Indian Rocks Beach', 'Sandy Springs', 'Ontario'), 
                     to=c('Hong Kong', 'Hong Kong', 'New York', 'Boston', 'Boston', 'Los Angeles', 'Washington',
                          'Washington', 'San Diego', 'Los Angeles', 'New York', 'Tampa', 
                          'Atlanta', 'Los Angeles'))
geo_simp$Year <- format(geo_simp$Start.Date, '%Y')
geo_years <- geo_simp[, .(Nights = sum(Nights, na.rm=T)), by=c('Location', 'Country', 'Year')]
#manual add cause I spent a day in Miami
added_df <- data.frame(Location= c('Tianjin', 'Miami', 'Philadelphia', 'Philadelphia', 
                                   'Cincinnati', 'Seoul', 'Bratislava'),
                       Country= c('China', 'USA', 'USA', 'USA', 'USA', 'South Korea', 'Slovakia'), 
                       Year= c(2010, 2011, 2013, 2017, 2018, 2018, 2014), 
                       Nights=c(1, 1, 1,1, 2, 1, 1))
geo_years <- rbind(geo_years, added_df)
alpha$Rank <- factor(alpha$Rank, levels = unique(alpha$Rank)) #this works because of the order of the spreadsheet
major_cities <- merge(geo_years, alpha[,-c('Country')], by.x='Location', by.y='City.Name')
#some manual fixes
major_cities <- major_cities[Location != 'Portland'] #I haven't been to that Portland
#tile plot
ggplot(major_cities) + geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=Continent), color='black') +
  facet_grid(Rank ~ ., scales='free', space='free') +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('CityYears2.jpeg', width=12, height=8.5, dpi=330)

#UN Area
UN <- read.csv('UNSD — Methodology.csv')
UN_mapper <- data.frame(Country = c('Hong Kong', 'USA', 'Vietnam', 'England', 'Taiwan', 'South Korea',
                       'Laos', 'Scotland', 'Czech Republic', 'Northern Ireland', 'Macau'),
           UN_Country = c('China', 'United States of America', 'Viet Nam', 
                          'United Kingdom of Great Britain and Northern Ireland',
                          'China', 'Republic of Korea', "Lao People's Democratic Republic",
                          'United Kingdom of Great Britain and Northern Ireland', 'Czechia', 
                          'United Kingdom of Great Britain and Northern Ireland', 'China'))
total_nights$UN_Country <- mapvalues(total_nights$Country, from=UN_mapper$Country,
                                     to = UN_mapper$UN_Country)
total_nights[Location == 'San Juan']$UN_Country <- 'Puerto Rico'
total_nights$UN.Sub.region <- mapvalues(total_nights$UN_Country, from = UN$Country.or.Area,
                                    UN$Sub.region.Name)
total_nights$Status <- mapvalues(total_nights$UN_Country, from = UN$Country.or.Area,
                                 UN$Developed...Developing.Countries)
total_nights <- unique(total_nights)
total_region <- total_nights[!is.na(UN.Sub.region), .(total=sum(total)), 
                             by=c('UN_Country', 'UN.Sub.region', 'Status')]
names(total_region) <- mapvalues(names(total_region), from='UN_Country', to='Country')
total_region <- total_region[order(total, decreasing = T)]
total_region$Country <- factor(total_region$Country, levels = total_region$Country)
ggplot(total_region) + 
  geom_col(aes(x=Country, y=total+0.5, fill=UN.Sub.region), color='white') +
  geom_text(aes(x=Country, y=sqrt(total+0.5), label=total), hjust=1) +
  facet_grid(UN.Sub.region ~ ., scales='free', space='free') + coord_flip() +
  theme(strip.text.y = element_text(angle=0), plot.title = element_text(hjust=0.5),
        panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette='Set1', guide=F) + 
  scale_y_log10('Total Number of Nights') + 
  ggtitle('Region Chart')
ggsave('Region_Chart.jpeg', width=12, height=9)

major_cities$UN_Country <- mapvalues(major_cities$Country, from=UN_mapper$Country,
                                     to = UN_mapper$UN_Country)
major_cities <- merge(major_cities, 
                      UN[,c('Country.or.Area', 'Sub.Region.Name', 'Developed...Developing.Countries')],
                      by.x = 'UN_Country', by.y = 'Country.or.Area')
major_cities$UN.Sub.region <- mapvalues(major_cities$UN_Country, from = UN$Country.or.Area,
                                        UN$Sub.region.Name)


#tile plot
ggplot(major_cities) + geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=Continent), color='black') +
  facet_wrap( ~ Year, scales='free', nrow=2) +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('CityYearsView.jpeg', width=12, height=8.5, dpi=330)

ggplot(major_cities) + 
  geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=UN.Sub.region), color='black') +
  facet_grid(Rank ~ ., scales='free', space='free') +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('CityYears_UN.jpeg', width=12, height=8.5, dpi=330)

#step plots of top cities

geo_all[, Running := cumsum(Nights), by = c('Location', 'Country', 'State')]

top_cities <- c('Beijing','Boston', 'New York', 'Dublin', 'London', 'Bangkok', 'Hong Kong', 
                'Shanghai', 'Washington')

sub_geo <- geo_all[Location %in% top_cities, c('Location', 'End.Date', 'Running')]
min(sub_geo$End.Date)

ggplot(sub_geo) + 
  geom_step(aes(x=End.Date, y=Running, color=Location)) +
  scale_y_log10() +
  scale_color_brewer(palette = 'Set1')

###
geo_years <- merge(geo_years, total_nights, by = c('Location', 'Country'), all.x=T)
source('utils.R')
mid_df <- calculate_midpoint(geo_years, 'lat', 'lon', 'Year', 'Nights')
ggplot(mid_df) + geom_point(aes(x=lon_mid, y=lat_mid, color=Year))

geo_all$Year <- format(geo_all$End.Date, '%Y')
geo_years_all <- geo_all[, .(Nights = sum(Nights, na.rm=T)), by=c('Location', 'Country', 'Year')]
geo_merged <- merge(geo_years_all, 
                    total_nights[, c('Location', 'Country', 'State', 'lon', 'lat', 'UN.Sub.region')],
                    by = c('Location', 'Country'))
geo_merged <- merge(geo_merged, mid_df, by = 'Year')
base_df <- data.frame(Location = c('Newton', 'Washington'))

geo_merged$distance <- diag(distm(geo_merged[,c('lon', 'lat')], 
                                  geo_merged[,c('lon_mid', 'lat_mid')]))
geo_merged <- geo_merged[Nights > 0 & !is.na(UN.Sub.region)]
geo_merged <- geo_merged[Year > 2007 & Year < 2021]
ggplot(geo_merged, aes(x=Year, y=distance/1000)) +
  geom_point(aes(size=log(Nights), fill=UN.Sub.region, color = Nights > 30), alpha=0.6, shape=21) +
  geom_text_repel(data=geo_merged[!(Nights==1 & distance < 500000)], 
                  aes(label=Location, color = Nights > 30), 
                  size=2.5, hjust=0, nudge_x=-0.5, force=0.5, segment.alpha = 0.2) +
  ggtitle('Geographic Distance Plot') +
  scale_fill_brewer('Region', palette = 'Set1') +
  scale_color_manual('Base', values = c('black', 'blue')) +
  scale_y_continuous(labels = comma, 'Distance (km)') +
  scale_size_continuous('Nights', labels=round(exp(0:5)), breaks = c(0:5), range = c(1,8)) +
  theme(legend.position = 'bottom', plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill='white', color='black'))
ggsave('geodist.jpeg', width=15, height=9)


# language analysis
wiki_lang <- read.csv('wikipedia_language_table.csv')
wiki_mapper <- data.frame(Country = c('USA', 'England','Scotland', 'Northern Ireland'),
                        Wiki_Country = c('United States',
                                       'United Kingdom',
                                       'United Kingdom',
                                       'United Kingdom'))
total_nights$Country <- mapvalues(total_nights$Country, from=wiki_mapper$Country,
                                     to = wiki_mapper$Wiki_Country)
# sorry, I'm removing sign languages for brevity sake
wiki_lang <- wiki_lang[!grepl('Sign Language', wiki_lang$Languages),]
total_languages <- setDT(merge(total_nights, wiki_lang, by = 'Country'))
language_sum <- total_languages[, .(total = sum(total), Cal=length(unique(Country))), 
                                by = 'Languages'][order(total)]
language_sum$Languages <- factor(language_sum$Languages, levels = language_sum$Languages)
ggplot(language_sum, aes(x=Languages, y=Cal)) + 
  geom_col(fill='dark red', color='black') +
  geom_text(aes(label=n), hjust=-1, color='blue') +
  coord_flip() +
  ggtitle('Languages of Cal')

world_langs <- read.csv('World_Languages.csv')
world_langs_comb <- merge(world_langs, language_sum, by.x = 'Language', by.y='Languages',
                          all.x=T)
world_lang_compare <- melt(world_langs_comb[, c('Language', 'World', 'Cal')], value.name = 'Count')
ggplot(world_lang_compare, aes(x=Language, y = Count)) + 
  geom_col(aes(fill=variable), position='dodge') +
  geom_text(aes(y=Count/2, label=Count, group=variable), position=position_dodge(width = 1)) +
  scale_fill_brewer(palette = 'Set1') +
  coord_flip() +
  ggtitle('Country Language Comparison')
ggsave('World_Language_Comp.jpeg', width=10, height=8)
