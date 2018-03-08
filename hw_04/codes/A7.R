schl_meal <- bcg %>% select(country= idcntry, school= idschool, contains("bcbg06")) %>% 
  filter(!is.na(country) & !is.na(school) & !is.na(bcbg06a) & !is.na(bcbg06b)) %>% 
  mutate(meal = ifelse(bcbg06a == 1 & bcbg06b == 1, "free breakfast and lunch for all",
                       ifelse(bcbg06a == 1 & bcbg06b == 2, "free breakfast for all but free lunch for some",
                              ifelse(bcbg06a == 1 & bcbg06b == 3, "free breakfast for all but no lunch",
                                     ifelse(bcbg06a == 2 & bcbg06b == 1, "free breakfast for some but free lunch for all",
                                            ifelse(bcbg06a == 2 & bcbg06b == 2, "free breakfast and lunch for some",
                                                   ifelse(bcbg06a == 2 & bcbg06b == 3, "free breakfast for some but no lunch",
                                                          ifelse(bcbg06a == 3 & bcbg06b == 1, "no breakfast but free lunch for all",
                                                                 ifelse(bcbg06a == 3 & bcbg06b == 2, "no breakfast but free lunch for some",
                                                                        ifelse(bcbg06a == 3 & bcbg06b == 3, "no breakfast and no lunch", NA)))))))))) %>% 
  filter(!is.na(meal)) %>% 
  select(country, school, meal)

schl_score <- bsg %>% select(country= idcntry, school=idschool, student= idstud, contains("bsssci"), contains("bsmmat")) %>% 
  mutate(score = rowMeans(.[, 4:13])) %>% 
  select(country, school, student, score)

meal_score <- schl_score %>% full_join(schl_meal, by = c("country", "school")) %>% 
  filter(!is.na(country) & !is.na(school) & !is.na(score) & !is.na(meal))

meal_score %>% ggplot(mapping = aes(meal, score, fill = meal)) +
  geom_boxplot(notch=FALSE) +
  ylab("score") +
  xlab("meal") +
  ggtitle("Density of score based on school meal") +
  guides(fill=guide_legend(title="school meal")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


hchart(density(filter(meal_score, meal == "free breakfast and lunch for all")$score), name=list("free breakfast and lunch for all")) %>%
  hc_add_series(density(filter(meal_score, meal == "free breakfast for all but free lunch for some")$score), name=list("free breakfast for all but free lunch for some")) %>% 
  hc_add_series(density(filter(meal_score, meal == "free breakfast for all but no lunch")$score), name=list("free breakfast for all but no lunch")) %>% 
  hc_add_series(density(filter(meal_score, meal == "free breakfast for some but free lunch for all")$score), name=list("free breakfast for some but free lunch for all")) %>% 
  hc_add_series(density(filter(meal_score, meal == "free breakfast and lunch for some")$score), name=list("free breakfast and lunch for some")) %>% 
  hc_add_series(density(filter(meal_score, meal == "free breakfast for some but no lunch")$score), name=list("free breakfast for some but no lunch")) %>% 
  hc_add_series(density(filter(meal_score, meal == "no breakfast but free lunch for all")$score), name=list("no breakfast but free lunch for all")) %>% 
  hc_add_series(density(filter(meal_score, meal == "no breakfast but free lunch for some")$score), name=list("no breakfast but free lunch for some")) %>% 
  hc_add_series(density(filter(meal_score, meal == "no breakfast and no lunch")$score), name=list("no breakfast and no lunch")) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on teacher education", style = list(fontWeight = "bold"))

summary(aov(score ~ education, data = tchr_bg_std_perf))
