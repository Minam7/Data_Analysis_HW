tchr_std_perf <- bst %>% select(country= idcntry, teacher= idteach, student= idstud, contains("bsssci"), contains("bsmmat"), school= idschool) %>% 
  mutate(score = rowMeans(.[, 4:13])) %>% 
  select(country, teacher, student, school, score) %>% 
  filter(!is.na(country) & !is.na(teacher) & !is.na(student) & !is.na(school) & !is.na(score))

tchr_bg <- btg %>% select(country= idcntry, teacher= idteach, school= idschool, edu= btbg04) %>% 
  filter(!is.na(country) & !is.na(teacher) & !is.na(school) & !is.na(edu))

tchr_bg_std_perf <- tchr_std_perf %>% inner_join(tchr_bg, by= c("country", "teacher", "school"))

group1 <- tchr_bg_std_perf %>% filter(edu == 1)
group2 <- tchr_bg_std_perf %>% filter(edu == 2)
group3 <- tchr_bg_std_perf %>% filter(edu == 3)
group4 <- tchr_bg_std_perf %>% filter(edu == 4)
group5 <- tchr_bg_std_perf %>% filter(edu == 5)
group6 <- tchr_bg_std_perf %>% filter(edu == 6)
group7 <- tchr_bg_std_perf %>% filter(edu == 7)

tchr_bg_std_perf <- tchr_bg_std_perf %>% 
  mutate(education = ifelse(edu == 1, "Did not complete Upper secondary",
                ifelse(edu == 2, "Upper secondary",
                       ifelse(edu == 3, "Post-secondary, non-tertiary",
                              ifelse(edu == 4, "Short-cycle tertiary",
                                     ifelse(edu == 5, "Bachelor’s or equivalent",
                                            ifelse(edu == 6, "Master’s or equivalent",
                                                   ifelse(edu == 7, "Doctor or equivalent", NA)))))))) %>% 
  select(country, teacher, student, school, score, education) %>% 
  filter(!is.na(education))

tchr_bg_std_perf %>% ggplot(mapping = aes(education, score, fill = education)) +
  geom_boxplot(notch=FALSE) +
  ylab("score") +
  xlab("teacher education") +
  ggtitle("Density of score based on teacher education") +
  guides(fill=guide_legend(title="teacher education")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


hchart(density(group1$score), name=list("Did not complete Upper secondary")) %>%
  hc_add_series(density(group2$score), name=list("Upper secondary")) %>% 
  hc_add_series(density(group3$score), name=list("Post-secondary, non-tertiary")) %>% 
  hc_add_series(density(group4$score), name=list("Short-cycle tertiary")) %>% 
  hc_add_series(density(group5$score), name=list("Bachelor’s or equivalent")) %>% 
  hc_add_series(density(group6$score), name=list("Master’s or equivalent")) %>% 
  hc_add_series(density(group7$score), name=list("Doctor or equivalent")) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on teacher education", style = list(fontWeight = "bold"))

summary(aov(score ~ education, data = tchr_bg_std_perf))
t.test(group7$score, group1$score, alt="greater")
