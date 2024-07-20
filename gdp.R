
load_gdp <- function(year=NA){
  gdp <- fread('Global/gdp-per-capita-worldbank.csv')
  pop <- fread('Global/population-and-demography.csv')
  if (!is.na(year)){
    gdp_year <- gdp[Year==year]
    pop_year <- pop[Year==year]
    gdp_pop <- merge(gdp_year, pop_year)
  }
  else {
    gdp_pop <- merge(gdp, pop)
  }
    
  return (gdp_pop)
}

gdp_pop_2022 <- load_gdp(year=2022)
gdp_pop_2022 <- merge(gdp_2022, pop_2022)
ggplot(gdp_pop_2022) + 
  geom_density(aes(x=GDPpc, weight = Population), fill='dark red') +
  ggtitle("Weighted GDP/capita of World Population") +
  xlab("GDP per Capita") + ylab('') + theme_economist() +
  scale_x_continuous(labels = scales::dollar_format())

entity_mapper <- fread('Global/entity_schema.csv')
total_region_gran$Entity <- mapvalues(total_region_gran$Country, 
                                      from=entity_mapper$Country_Cal,
                                      to=entity_mapper$Entity)
region_gdp_mapped <- merge(total_region_gran, gdp_pop_2022, by='Entity', all.x=T)
ggplot(region_gdp_mapped) + 
  geom_density(aes(x=GDPpc, weight = total), fill='dark red') +
  ggtitle("Weighted GDP/capita of World Population") +
  xlab("GDP per Capita") + ylab('') + theme_economist() +
  scale_x_continuous(labels = scales::dollar_format())


gdp_pop_2022$weight <- gdp_pop_2022$Population / sum(gdp_pop_2022$Population)
gdp_pop_2022$type <- 'Global'
region_gdp_mapped$weight <- region_gdp_mapped$total / sum(region_gdp_mapped$total)
region_gdp_mapped$type <- 'Personal'
gdp_comparison <- rbind(gdp_pop_2022, region_gdp_mapped, fill=T)

ggplot(gdp_comparison) + 
  geom_density(aes(x=GDPpc, weight = weight, fill=type), alpha=0.7) +
  ggtitle("Weighted GDP/capita - Travels vs World") +
  xlab("GDP per Capita") + ylab('') + theme_pander() +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_fill_brewer(palette='Set1') +
  theme(plot.title = element_text(hjust=0.5), legend.position = 'bottom')
ggsave('Plots/gdp_vs_world.jpeg', width=12, height=8.5, dpi=330)


alpha_countries <- alpha[,.(t=.N), by=Country]
alpha_countries$Alpha <- T
c_check <-merge(alpha_countries, gdp_comparison, all.x=T, all.y=T, by.x="Country", by.y="Entity")
c_check <- merge(c_check, total_region, by='Country', all.x=T, all.y=T)
c_check <- c_check[, c('Country', 'Population', 'Country.y', 'Alpha', 'UN.Sub.region.y')]
