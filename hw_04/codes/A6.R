apply_geo <- bsa %>% select(country= idcntry, student= idstud, sex= itsex, contains("bsmgeo")) %>% 
  mutate(score = rowMeans(.[, 4:8])) %>% 
  mutate(gender = ifelse(sex == 1, "female",
                         ifelse(sex == 2, "male", NA))) %>% 
  select(country, student, gender, score) %>% 
  filter(!is.na(country) & !is.na(student) & !is.na(gender) & !is.na(score))


ggplot(apply_geo, aes(x = score, fill = gender)) + geom_density(alpha= 0.4) + ggtitle("Density of geometry score based on gender") + 
  ylab("geometry apply score") + 
  xlab("gender") + 
  guides(fill=guide_legend(title="gender"))

group1 <- apply_geo %>% filter(gender == "female")
group2 <- apply_geo %>% filter(gender == "male")

hchart(density(group1$score), type = "area", name=list("female")) %>%
  hc_add_series(density(group2$score), type = "area", name=list("male")) %>%
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) 

t.test(score~gender, data = apply_geo, alt = 'greater')


  

