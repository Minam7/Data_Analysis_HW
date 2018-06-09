library(rworldmap)
library(countrycode)

disaster_sum <- read_delim("data/disaster.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(lat = LATITUDE,long = LONGITUDE, magnit = INTENSITY,country = COUNTRY,year = YEAR, death=TOTAL_DEATHS) %>% 
  dplyr::select(lat, long, magnit, country, year, death)

disaster_sum = na.omit(disaster_sum)
disaster_sum <- disaster_sum %>% group_by(country) %>% summarise(fatality_sum = sum(death), fatality_mean = mean(death))

disaster_sum$country <- countrycode(disaster_sum$country, "country.name", "iso3c", warn = TRUE, nomatch = NA,
                                    custom_dict = NULL, custom_match = NULL, origin_regex = FALSE)

heatMap <- joinCountryData2Map(disaster_sum, joinCode = "ISO3",
                              nameJoinColumn = "country")

mapCountryData(heatMap, nameColumnToPlot = "fatality_sum", 
               mapTitle="Earthquakes total fatality", oceanCol=gray(0.3),
               colourPalette=c("cadetblue1","cadetblue3", "deepskyblue2", "deepskyblue3", "deepskyblue4", "dodgerblue4", "navy"), missingCountryCol = "white")


heatMap <- joinCountryData2Map(disaster_sum, joinCode = "ISO3",
                               nameJoinColumn = "country")

mapCountryData(heatMap, nameColumnToPlot = "fatality_mean", 
               mapTitle="Earthquakes fatality rate", oceanCol=gray(0.3),
               colourPalette=c("cadetblue1","cadetblue3", "deepskyblue2", "deepskyblue3", "deepskyblue4", "dodgerblue4", "navy"), missingCountryCol = "white")

