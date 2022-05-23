library(googlesheets4)
library(ggplot2)
library(ggmap)
library(plyr)
library(ggrepel)
library(data.table)
library(scales)
library(rworldmap)
library(countrycode)
library(RColorBrewer)
library(plotly)
library(htmlwidgets)
library(ggthemes)
library(forcats)
setwd('~/Documents/CAL/Real_Life/Geography-Improved/')
source('geo_data.R')
options(stringsAsFactors = F)
register_google(key = Sys.getenv(x='GOOGLE_API'))  
# authenticate google sheet 

# pull from google one sheet at a time
geo_all <- preprocess('Geography of Cal', sleep = 4)

# Map to Cal schema
# Manual mapping of some similar location names
geo_all$Location <- mapvalues(geo_all$Location, from='New York City', to='New York')
geo_all$Location <- mapvalues(geo_all$Location, from='Airplane', to='Red Eye')
geo_all$Location <- mapvalues(geo_all$Location, from='Aberdeen', to='Hong Kong')

geo_all$Location_raw <- geo_all$Location

squash <- T
if (squash){
  geo_all$Location <- mapvalues(geo_all$Location, from='Kowloon', to='Hong Kong')
  geo_all$Location <- mapvalues(geo_all$Location, from='Cambridge', to='Boston')
}
geo_simp <- geo_all # this will be used later, for location simplifying 
# and to avoid the crossing new year's decision that we make next

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

total_nights_step <- geo_all[, .(total=sum(Nights, na.rm=T), uni = length(unique(Start.Date)), 
                                 sd_d=sd(Start.Date),
                            first_year = min(format(Start.Date, '%Y')), 
                            last_year=max(format(Start.Date, '%Y'))), 
                        by=list(Location, Country, State)][order(total, decreasing = T)]

repeats_geo.m <- get_repeats(geo_all, 3)
ggplot(repeats_geo.m) + 
  geom_line(aes(x=Date, y=Location, group=id, color=Country), size=0.5) + 
  geom_point(aes(x=Date, y=Location, color=Country), size=0.3, shape=23) +
  scale_x_date(labels = date_format("%Y"), breaks='year') + 
  scale_color_brewer(palette='Paired') + 
  theme_clean() +
  theme(legend.position="bottom", plot.title = element_text(hjust=0.5)) +
   ggtitle('Repeated Locations Over the Years') 
ggsave('Plots/Repeats.jpeg', width=13.5, height=5, dpi=550)

repeats_geo.m <- get_repeats(geo_simp, 3)
repeats_geo.m$Date_2012 <- as.Date(format(repeats_geo.m$Date, format='2012-%m-%d'))
repeats_geo.m$Year <- as.numeric(format(repeats_geo.m$Date, '%Y'))
ggplot(repeats_geo.m[Location %in% 
       c('Boston', 'Hong Kong', 'Seattle', 'New York',
         'Washington', 'Bangkok', 'Beijing', 'Shanghai', 'Tokyo')]) + 
  geom_line(aes(x=Date_2012, y=Year, group=id, color=Country)) + 
  geom_point(aes(x=Date_2012, y=Year, fill=Country), size=1, shape=23) +
  facet_grid(Location ~ .) + 
  scale_color_brewer(palette='Dark2') + 
  scale_fill_brewer(palette='Dark2') + 
  scale_x_date('Date', date_breaks = '2 months', date_labels = "%B") + 
  scale_y_reverse(breaks=pretty_breaks()) +
  ggtitle("Repeated Locations") +
   theme_pander() +
  theme(panel.border = element_rect(fill=NA, color='black'),
        legend.position="bottom", plot.title = element_text(hjust=0.5),
        strip.text.y = element_text(angle=0))
ggsave('Plots/Repeats2.jpeg', width=12, height=9, dpi=550)

loc_refs <- read.csv('total_nights4.csv')
total_nights <- merge(total_nights_step, loc_refs[, c('Location', 'Country', 'State', 'lon', 'lat')], 
                      by = c('Location', 'Country', 'State'), all.x=T)

## country border
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
write.csv(unique(total_nights), 'total_nights4.csv', row.names=F)

bp <- colorRampPalette(brewer.pal(11, 'PiYG'))(length(unique(total_nights$first_year)))

ggplot() + m1 + m2 + geom_point(data=total_nights[last_year>2007], 
                           aes(x=lon, y=lat, size=sqrt(total+1), 
                           fill=first_year, text=paste(Location, Country, sep='\n')), 
                           shape=21, alpha=0.8) +
<<<<<<< HEAD
  scale_size_continuous('Total Nights (sq rt)', range = c(0.1, 3),
=======
  scale_size_continuous('Total Nights (sq rt)', range = c(0.1,4),
>>>>>>> f22c766 (fix: plot readability)
                        breaks = c(3, 10, 30)) +
  scale_fill_manual('Year First', values=bp) +
  ggtitle('Geography of Cal') + 
  theme(plot.title = element_text(hjust=0.5, size=12), 
        panel.background = element_rect(fill=alpha('blue', 0.2)))
ggsave('Plots/Geography_Cal6.jpeg', width=13.5, height=8, dpi=750)

#lats and lons
latlon_barplot(total_nights, 'lat', 16)
latlon_barplot(total_nights, 'lon', 16)

#plotly
map_html <- ggplotly(tooltip = c('text', 'first_year'))
saveWidget(as_widget(map_html), "Geography_Cal.html")

#Country chart
country_count <- country_dates(geo_all, date_start = 'Start.Date')
ggplot(country_count) + geom_step(aes(x=first_date, y=count)) +
  geom_text(aes(x=first_date, y=count, label=Country, color=continent), hjust=0, vjust=1.2) +
  ggtitle('New Country Progression') + theme(legend.position = 'bottom',
  plot.title = element_text(hjust=0.5, size=12), panel.background = element_blank()) +
  xlab('') + expand_limits(x=as.Date('2021-09-01'))
ggsave('Country_Count.jpeg', width=9, height=6, dpi=300)


malMap <- joinCountryData2Map(country_count, joinCode = "ISO3",
                              nameJoinColumn = "iso3")
mapCountryData(malMap, nameColumnToPlot="year", catMethod = "categorical",
               missingCountryCol = gray(.8))
library(googleVis)
geo_world <- gvisGeoChart(country_count, 'Country', 'year') +
  gvisGeoChart(total_nights, 'latlong', 'first_year')
plot(geo_world)

#city classification plot
alpha <- setDT(read.csv('AlphaBetaGamma.csv'))
alpha$City.Name <- gsub('^ ', '', alpha$City.Name)
simp_city_df <- data.frame(city = c('Kowloon', 'Aberdeen', 'Brooklyn', 'Newton', 'Cambridge', 
                                    'Santa Monica', 'Washington', 'Arlington', 'Encinitas', 
                                    'Huntington Beach', 'Manhattan', 'Indian Rocks Beach', 'Sandy Springs', 'Ontario',
                                    'Decatur', 'Jersey City', 'Bloomington'),
                           simp_city = c('Hong Kong', 'Hong Kong', 'New York', 'Boston', 'Boston', 
                                         'Los Angeles', 'Washington', 'Washington', 'San Diego', 
                                         'Los Angeles', 'New York', 'Tampa', 'Atlanta', 
                                         'Los Angeles', 
                                         'Atlanta', 'New York', 'Minneapolis'))
geo_simp$Location <- mapvalues(geo_simp$Location, 
                               from=simp_city_df$city, to=simp_city_df$simp_city)

geo_years <- geo_simp[, .(Nights = sum(Nights, na.rm=T)), by=c('Location', 'Country', 'Year')]
#manual add cause I spent a day in Miami
added_df <- data.frame(Location= c('Tianjin', 'Miami', 'Philadelphia', 'Philadelphia', 
                                   'Cincinnati', 'Seoul', 'Bratislava', 'Los Angeles'),
                       Country= c('China', 'USA', 'USA', 'USA', 'USA', 'South Korea', 
                                  'Slovakia', 'USA'), 
                       Year= c(2010, 2011, 2013, 2017, 2018, 2018, 2014, 2022), 
                       Nights=c(1, 1, 1,1, 2, 1, 1, 1))
geo_years <- rbind(geo_years, added_df)
alpha$Rank <- factor(alpha$Rank, levels = unique(alpha$Rank)) #this works because of the order of the spreadsheet
major_cities <- merge(geo_years, alpha[,-c('Country')], by.x='Location', by.y='City.Name')
#some manual fixes
major_cities <- major_cities[Location != 'Portland'] #I haven't been to that Portland
major_cities <- major_cities[!(Location == 'Washington' & Year == 2021)] # Wrong Arlington | TODO
#tile plot
ggplot(major_cities) + geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=Continent), color='black') +
  facet_grid(Rank ~ ., scales='free', space='free') +
  scale_fill_brewer(palette='Set1') + 
  scale_y_discrete(limits=rev) +
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('Plots/CityYears2.jpeg', width=12, height=10, dpi=800)

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
                                    UN$Sub.region.Name, warn_missing=F)
total_nights$Status <- mapvalues(total_nights$UN_Country, from = UN$Country.or.Area,
                                 UN$Developed...Developing.Countries, warn_missing=F)
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
ggsave('Plots/Region_Chart.jpeg', width=12, height=9)

geo_simp$UN_Country <- mapvalues(geo_simp$Country, from=UN_mapper$Country,
                 to = UN_mapper$UN_Country)
geo_simp[Location == 'San Juan']$UN_Country <- 'Puerto Rico'
geo_simp$UN.Sub.region <- mapvalues(geo_simp$UN_Country, from = UN$Country.or.Area,
                                        UN$Sub.region.Name, warn_missing=F)
country_years <- geo_simp[Year > 2007, .(total = sum(Nights, na.rm=T)), 
                         by=list(Country,Year,UN.Sub.region)]
ggplot(country_years) +
  geom_col(aes(x=Year, y=total, fill=UN.Sub.region, group=Country), color='black') +
  coord_flip() +
  scale_fill_brewer(palette = 'Set1', 'UN Region') +
  theme_pander()

major_cities$UN_Country <- mapvalues(major_cities$Country, from=UN_mapper$Country,
                                     to = UN_mapper$UN_Country)
major_cities <- merge(major_cities, 
                      UN[,c('Country.or.Area', 'Sub.Region.Name', 'Developed...Developing.Countries')],
                      by.x = 'UN_Country', by.y = 'Country.or.Area')
major_cities$UN.Sub.region <- mapvalues(major_cities$UN_Country, from = UN$Country.or.Area,
                                        UN$Sub.region.Name, warn_missing = F)


#tile plot
ggplot(major_cities) + geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=Continent), color='black') +
  facet_wrap( ~ Year, scales='free', nrow=2) +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('Plot/CityYearsView.jpeg', width=12, height=8.5, dpi=330)

ggplot(major_cities) + 
  geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=UN.Sub.region), color='black') +
  facet_grid(Rank ~ ., scales='free', space='free') +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
  ggtitle('Major Cities over the Years') +
  geom_text(aes(x=Year, y=Location, label=Nights), size=3)
ggsave('CityYears_UN.jpeg', width=12, height=8.5, dpi=330)

#alpha all
geo_simp$Country <- mapvalues(geo_simp$Country, 
                               from = c('Northern Ireland', 'Scotland', 'England', 'Hong Kong'),
                               to = c('UK', 'UK', 'UK', 'China'))
 
total_nights_simp <- geo_simp[, .(total = sum(Nights)), by = c('Location', 'Country')]
added_df2 <- data.frame(Location= c('Tianjin', 'Miami', 'Philadelphia', 'Bratislava'),
                       Country= c('China', 'USA', 'USA', 'Slovakia'), 
                       total=c(1, 1, 2, 1))
total_nights_simp <- rbind.fill(total_nights_simp, added_df2)

alpha$City.Name <- mapvalues(alpha$City.Name, from = c('Washington, D.C.', 'San Jose (CR)'),
                             to = c('Washington', 'San Jose'))
alpha2 <- merge(alpha, total_nights_simp, by.x=c('City.Name', 'Country'), 
                by.y=c('Location', 'Country'), all.x=T)
alpha2$total[is.na(alpha2$total)] <- 0
setorderv(alpha2, c('total', 'Rank'), c(1, -1))
alpha2$City.Name <- factor(alpha2$City.Name, levels = unique(alpha2$City.Name))
alpha2$Rank_Simp <- gsub('[^a-zA-Z]+', '', alpha2$Rank)
ggplot(alpha2, aes(x=Rank_Simp, y=City.Name)) + 
  geom_tile(aes(alpha=sqrt(total), fill=Continent), color='black') +
  facet_wrap(Continent ~ ., scales='free', nrow=1) +
  scale_fill_brewer(palette='Set1') + 
  theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), 
        strip.text.y = element_text(angle=0)) + xlab('Classification') +
  ggtitle('World Cities') +
  geom_text(aes(label=total), size=3)
ggsave('Alpha_All.jpeg', width=16, height=8.5, dpi=330)

#step plots of top cities

geo_all[, Running := cumsum(Nights), by = c('Location', 'Country', 'State')]

top_cities <- c('Beijing','Boston', 'New York', 'Dublin', 'London', 'Bangkok', 'Hong Kong', 
                'Shanghai', 'Washington', 'Seattle')

sub_geo <- geo_all[Location %in% top_cities, c('Location', 'End.Date', 'Running')]
min(sub_geo$End.Date)

ggplot(sub_geo) + 
  geom_step(aes(x=End.Date, y=Running, color=Location)) +
  scale_y_log10() +
  scale_color_brewer(palette = 'Set3')

###
geo_years <- merge(geo_years, total_nights, by = c('Location', 'Country'), all.x=T)
source('utils.R')
mid_df <- calculate_midpoint(geo_years, 'lat', 'lon', 'Year', 'Nights')

geo_all$Year <- format(geo_all$End.Date, '%Y')
geo_years_all <- geo_all[, .(Nights = sum(Nights, na.rm=T)), by=c('Location', 'Country', 'Year')]
geo_merged <- merge(geo_years_all, 
                    total_nights[, c('Location', 'Country', 'State', 'lon', 'lat', 'UN.Sub.region')],
                    by = c('Location', 'Country'))
geo_merged <- merge(geo_merged, mid_df, by = 'Year')
geo_merged$distance <- diag(distm(geo_merged[,c('lon', 'lat')], 
                                  geo_merged[,c('lon_mid', 'lat_mid')]))
geo_merged <- geo_merged[Nights > 0 & !is.na(UN.Sub.region)]
geo_merged <- geo_merged[Year > 2007 ]
ggplot(geo_merged, aes(x=Year, y=distance/1000)) +
  geom_point(aes(size=log(Nights), fill=UN.Sub.region, color = Nights > 30, 
                 text=paste(Location, Country, sep='\n')), 
             alpha=0.6, shape=21) +
  geom_text_repel(data=geo_merged[!(Nights==1 & distance < 500000)], 
                  aes(label=Location, color = Nights > 30), 
                  size=2.5, hjust=0, nudge_x=-0.5, force=0.5, segment.alpha = 0.2,
                  max.overlaps = 15) +
  ggtitle('Geographic Distance Plot') +
  scale_fill_brewer('Region', palette = 'Set1') +
  scale_color_manual('Base', values = c('black', 'blue')) +
  scale_y_continuous(labels = comma, 'Distance (km)') +
  scale_size_continuous('Nights', labels=round(exp(0:5)), breaks = c(0:5), range = c(1,8)) +
  theme(legend.position = 'bottom', plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill='white', color='black'))
ggsave('geodist.jpeg', width=16, height=12, dpi=350)

geo_dist_html <- ggplotly(tooltip = c('text'))
saveWidget(as_widget(geo_dist_html), "geodist.html")

# language analysis
wiki_lang <- fread('external/wikipedia_language_table.csv')
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
language_sum <- total_languages[, .(total = sum(total), n_country=length(unique(Country))), 
                                by = 'Languages'][order(total)]
language_sum$Languages <- factor(language_sum$Languages, levels = language_sum$Languages)
ggplot(language_sum, aes(x=Languages, y=n_country)) + 
  geom_col(fill='dark red', color='black') +
  geom_text(aes(label=total), hjust=2, color='white') +
  coord_flip() +
  theme_pander() +
  ggtitle('Languages of Cal')

world_langs <- read.csv('World_Languages.csv')
world_langs_comb <- merge(world_langs, language_sum, by.x = 'Language', by.y='Languages',
                          all.x=T)
world_lang_compare <- melt(world_langs_comb[, c('Language', 'World', 'Cal')], value.name = 'Count')
world_lang_compare$Count[is.na(world_lang_compare$Count)] <- 0
ggplot(world_lang_compare, aes(x=Language, y = Count)) + 
  geom_col(aes(fill=variable), position='dodge') +
  geom_text(aes(y=Count/2, label=Count, group=variable), position=position_dodge(width = 1)) +
  scale_fill_brewer(palette = 'Set1') +
  coord_flip() +
  ggtitle('Country Language Comparison')
ggsave('World_Language_Comp.jpeg', width=10, height=8)

## 
top_langs <- wiki_lang[Languages %in% world_langs$Language]
language_country_sum <- total_languages[, .(total = sum(total)), by = c('Languages', 'Country')]

top_langs <- merge(top_langs, language_country_sum, all.x=T)
top_langs$total[is.na(top_langs$total)] <- 0
top_langs[Country=='Vatican City']$total <- 1


world_df <- setDT(map_data('world'))
world_df$region <- mapvalues(world_df$region, from = c('UK', 'USA'),
                             to = c('United Kingdom', 'United States'))
world_df[subregion == 'Hong Kong']$region <- 'Hong Kong'
world_lang_map <- merge(language_country_sum, world_df, by.x='Country', by.y='region', all.x=T)
ggplot(world_lang_map) +
  geom_polygon(aes(x=long, y=lat, group=group, alpha=sqrt(total), fill=Languages), color='black') +
  ggtitle("Language Map")

## World languages
top_langs$Country <- mapvalues(top_langs$Country,
                               from = c('Antigua and Barbuda', 'East Timor', 
                                        'Republic of the Congo', 'Eswatini',
                                        'Saint Kitts and Nevis', 'Saint Vincent and the Grenadines'),
                               to = c('Antigua', 'Timor-Leste', 'Republic of Congo',
                                      'Swaziland', 'Saint Kitts', 'Saint Vincent'))
top_lang_map <- merge(top_langs, world_df, by.x='Country', by.y='region', all.x=T)
top_lang_map$total_bucket <- cut(top_lang_map$total, breaks = c(-Inf,0, 2, 8, 20, 100, 1000, Inf),
       labels = c('Never', '<3', '<8', '<21', '<100', '<1000', '>1000'))



## 
unfolded_df <- do.call('rbind.fill', 
                       apply(geo_all[,c('Location', 'Country', 'State', 'Start.Date', 'End.Date')], 
                             1, 
                             function(x) unfold(x, c('Location', 'Country', 'State'), 
                                                'Start.Date', 'End.Date')))
unfolded_df <- merge(unfolded_df, loc_refs[, c('Location', 'Country', 'lon', 'lat')], 
                     by = c('Location', 'Country'), all.x=T)
ggplot(world_df) + geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_hex(data=unfolded_df, aes(x=lon, y=lat), bins=150, alpha=0.8) +
  scale_fill_gradient(low='blue', high='red', trans='log', guide=F) +
  theme_fivethirtyeight()
ggsave('world_density_visited.jpeg', width=12, height=8)

ggplot(world_df) + geom_polygon(aes(x=long, y=lat, group=group)) +
  stat_density2d(data=unfolded_df, aes(x=lon, y=lat), bins=150, alpha=0.2) 

ggplot(world_df) + geom_polygon(aes(x=long, y=lat, group=group), fill='grey90', color='black') +
  geom_hex(data=unfolded_df, aes(x=lon, y=lat), bins=250, alpha=0.8) +
  scale_fill_gradient(low='pink', high='dark red', trans='log', guide=F) +
  theme_fivethirtyeight()
ggsave('world_density_visited.jpeg', width=12, height=8)


## states map
states_table <- read.csv('states.csv')
states_dt <- convert_states(geo_all, states_table)
states_totals <- states_dt[, .(Total = sum(Nights, na.rm=T)), by=State]
states_dt <- merge(states_dt, states_totals, by='State')
states_dt$Prop <- with(states_dt, Nights/Total)
states_dt$TotalSqrt <- with(states_dt, sqrt(Total))
states_dt$NightsSqrt <- with(states_dt, Prop * TotalSqrt)
year_greens <- colorRampPalette(brewer.pal(9, "Greens"))(length(unique(states_dt$Year)))
ggplot(states_dt,aes(x=State.y)) + 
  geom_col(aes(y=NightsSqrt, fill=fct_rev(Year)), color='grey', position=position_stack()) + 
  geom_text(aes(y=NightsSqrt, label=Nights, fct_rev(fct_inorder(State.y))), 
            position=position_stack(0.5), 
            size=3, hjust=0.5, color='red', alpha=0.5) +
  facet_grid(Region ~ ., scales='free', space='free') + 
  coord_flip() + 
  scale_fill_manual(values=year_greens, 'Year') +
  ggtitle("States over the Years") +
  theme_few() +
  theme(plot.title=element_text(hjust=0.5))
ggsave('Plots/states_years.jpeg', width=11, height=7)

## elev
library(elevatr)
ll_prj <- "EPSG:4326"
sp <- SpatialPoints(total_nights[!is.na(lon),c('lon', 'lat')])
df_elev_epqs <- get_elev_point(sp, prj = ll_prj, src = 'aws')
elev_df <- data.frame(Location =  total_nights[!is.na(lon)]$Location, 
           Elevation = df_elev_epqs$elevation,
           State=total_nights[!is.na(lon)]$State,
           Country=total_nights[!is.na(lon)]$Country)
total_nights <- merge(total_nights, elev_df, by = c('Location', 'State', 'Country'))
ggplot(total_nights) +
  geom_point(aes(x=Elevation, y=lat)) +
  geom_text_repel(data=total_nights[Elevation > 1000], 
            aes(x=Elevation, y=lat, label=Location, color=Country)) +
  scale_color_brewer(palette='Set1') +
  theme_dark()
ggplot(total_nights) +
   geom_point(aes(x=lon, y=lat, size=Elevation, color=log(total)), alpha=0.4) + 
  scale_color_gradient(low='blue', high='red') +
  theme_clean()

ggplot(total_nights) +
  geom_point(aes(x=lon, y=lat, size=log(total), color=Elevation), alpha=0.4) + 
  scale_color_gradient(low='blue', high='red') +
  scale_size_continuous(breaks = c(1, 3, 5, 7), 
                        labels=round(exp(c(1, 3, 5, 7))),
                        'Total Nights') +
  theme_clean() +
  ggtitle('Elevation Map') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Plots/Elevation_Map.jpeg', width=12, height=9)
