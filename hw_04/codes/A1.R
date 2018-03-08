tchr_std_perf <- bst %>% select(country= idcntry, teacher= idteach, student= idstud, contains("bsssci"), contains("bsmmat")) %>% 
  mutate(score = rowMeans(.[, 4:13])) %>% 
  select(country, teacher, student, score) %>% 
  filter(!is.na(country) & !is.na(teacher) & !is.na(student) & !is.na(score))

tchr_sat <-  btg %>% select(country= idcntry, teacher= idteach, sat=btdgtjs) %>% 
  filter(!is.na(country) & !is.na(teacher) & !is.na(sat))

tchr_sat_std_perf <- tchr_std_perf %>% inner_join(tchr_sat, by= c("country", "teacher")) %>% 
  mutate(satisfaction = ifelse(sat == 1, "Very Satisfied",
                ifelse(sat == 2, "Satisfied",
                       ifelse(sat == 3, "Less than Satisfied", NA)))) %>% 
  select(country, teacher, score, satisfaction)

tchr_sat_std_perf %>% ggplot(mapping = aes(satisfaction, score, fill = satisfaction)) +
  geom_boxplot(notch=FALSE) +
  ylab("score") +
  xlab("teacher satisfaction") +
  ggtitle("Density of score based on teacher satisfaction") +
  guides(fill=guide_legend(title="teacher satisfaction"))

group1 <- tchr_sat_std_perf %>% filter(satisfaction == "Very Satisfied")
group2 <- tchr_sat_std_perf %>% filter(satisfaction == "Satisfied")
group3 <- tchr_sat_std_perf %>% filter(satisfaction == "Less than Satisfied")

hchart(density(group1$score), name=list("Very Satisfied")) %>%
  hc_add_series(density(group2$score), name=list("Satisfied")) %>% 
  hc_add_series(density(group3$score), name=list("Less than Satisfied")) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on teacher satisfaction", style = list(fontWeight = "bold"))

summary(aov(score ~ satisfaction, data = tchr_sat_std_perf))

