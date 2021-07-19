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
setwd('~/Documents/CAL/Real_Life/Geography-Improved/')
source('geo_data.R')
options(stringsAsFactors = F)
name <- 'Maggie'
title = paste0('Geography of ', name)
geo_all <- preprocess(title=title, sheet=2)
#geo_all$Nights <- with(geo_all, as.numeric(difftime(End.Date, Start.Date, units='days')) + 1)
total_nights <- geo_all[, .(total=sum(Nights, na.rm=T), uni = length(unique(Start.Date)), 
                                 sd_d=sd(Start.Date),
                                 first_year = min(format(Start.Date, '%Y')), 
                                 last_year=max(format(Start.Date, '%Y')),
                                 lon = mean(lon, na.rm=T),
                                 lat=mean(lat, na.rm=T)), 
                             by=list(Location, Country, State)][order(total, decreasing = T)]
#
repeats_geo.m <- get_repeats(geo_all, 2)
ggplot(repeats_geo.m[!is.na(Location)]) + geom_line(aes(x=Date, y=Location, group=id, color=Country), size=0.5) + 
  geom_point(aes(x=Date, y=Location, color=Country), size=.4, shape=23) +
  scale_x_date(labels = date_format("%Y"), breaks='year') + 
  scale_color_brewer(palette='RdBu') + 
  theme(legend.position="bottom", plot.title = element_text(hjust=0.5),
        panel.background = element_rect(fill='grey30'),
        panel.grid.major = element_blank()) +
  ggtitle('Repeated Locations Over the Years') 
ggsave(paste0(name, '/Repeats.jpeg'), width=13.5, height=5, dpi=550)


m1 <- borders('world', fill='black', size=0.2, alpha=0.8)
m2 <- borders('state', fill='black', size=0.2, colour='dark blue', alpha=0.2)

bp <- colorRampPalette(brewer.pal(11, 'PiYG'))(length(unique(total_nights$first_year)))

ggplot() + m1 + m2 + geom_point(data=total_nights[last_year>2007], 
                                aes(x=lon, y=lat, size=sqrt(total+1), color=last_year,
                                    fill=first_year, text=Location), shape=21, alpha=0.8) +
  scale_size_continuous('Total Nights (sq rt)', range = c(0.005,4),
                        breaks = c(3, 10, 30)) +
  scale_color_manual('Year Last', values=bp, guide=F) +
  scale_fill_manual('Year First', values=bp) +
  ggtitle(paste0('Geography of ', name)) + 
  theme(plot.title = element_text(hjust=0.5, size=12), 
        panel.background = element_rect(fill=alpha('blue', 0.2)))
ggsave(paste0(name, '/Geography_', name, '.jpeg'), width=13.5, height=8, dpi=750)

alpha <- setDT(read.csv('AlphaBetaGamma.csv'))
alpha$City.Name <- gsub('^ ', '', alpha$City.Name)

alpha_chart <- function(df, renamer, alpha_cities_db, name, save=T){
 df$Location <- mapvalues(df$Location, 
                          from=renamer$city, 
                          to=renamer$simp_city, warn_missing=F) 
 geo_years <- df[, .(Nights = sum(Nights, na.rm=T)), by=c('Location', 'Country', 'Year')]
 alpha$Rank <- factor(alpha$Rank, levels = unique(alpha$Rank)) #this works because of the order of the spreadsheet
 major_cities <- merge(geo_years, alpha[,-c('Country')], by.x='Location', by.y='City.Name')
 ggplot(major_cities) + geom_tile(aes(x=Year, y=Location, alpha=log(Nights), fill=Continent), color='black') +
   facet_grid(Rank ~ ., scales='free', space='free') +
   scale_fill_brewer(palette='Set1') + 
   theme(plot.title=element_text(hjust=0.5), panel.grid = element_blank(), strip.text.y = element_text(angle=0)) +
   ggtitle('Major Cities over the Years') +
   geom_text(aes(x=Year, y=Location, label=Nights), size=3)
 if (save == T){
   ggsave(paste0(name,'/CityYears.jpeg'), width=12, height=8.5, dpi=330)
 }
}

simp_city_df <- data.frame(city = c('Brooklyn', 'Newton', 'Cambridge','Berkeley',
                                    'Santa Monica', 'Washington', 'Arlington', 'Encinitas', 
                                    'Carlsbad', 'Manhattan', 'Indian Rocks Beach', 'Sandy Springs', 'Ontario'),
                           simp_city = c('New York', 'Boston', 'Boston', 'San Francisco',
                                         'Los Angeles', 'Washington, D.C.', 'Washington, D.C.', 'San Diego', 
                                         'San Diego', 'New York', 'Tampa', 
                                         'Atlanta', 'Los Angeles'))
alpha_chart(geo_all, simp_city_df, name=name)
#tile plot


#UN Area
UN <- read.csv('UNSD — Methodology.csv')
UN_mapper <- data.frame(Country = c('United States', 'South Korea', 'United Kingdom'),
                        UN_Country = c('United States of America', 'Republic of Korea',
                                       'United Kingdom of Great Britain and Northern Ireland'))
total_nights$UN_Country <- mapvalues(total_nights$Country, from=UN_mapper$Country,
                                     to = UN_mapper$UN_Country)
total_nights$UN.Sub.region <- mapvalues(total_nights$UN_Country, from = UN$Country.or.Area,
                                        UN$Sub.region.Name, warn_missing = F)
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
ggsave(paste0(name, '/Region_Chart.jpeg'), width=12, height=9)