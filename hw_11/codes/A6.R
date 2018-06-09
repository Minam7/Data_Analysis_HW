disaster_death <- read_delim("data/disaster.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(lat = LATITUDE,long = LONGITUDE, magnit = INTENSITY, depth = FOCAL_DEPTH, country = COUNTRY,year = YEAR, death=TOTAL_DEATHS) %>% 
  dplyr::select(lat, long, magnit, depth, country, year, death)

# generalized regression model
glm_model <- glm(death ~ lat + long + magnit + depth, family = Gamma(link = "inverse"), data = disaster_death)
summary(glm_model)

glm_model <- glm(death ~ magnit + depth, family = Gamma(link = "inverse"), data = disaster_death)
summary(glm_model)

cat("Test GLM model using null and model deviances: ",1-pchisq(3965.6 - 3409.4, df=(429 - 427)))