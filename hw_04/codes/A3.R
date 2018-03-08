edu_resources <- bsg %>% select(idstud, idcntry, std_rsc = bsdg06s, contains("bsssci"), contains("bsmmat"), edu_rsc = bsdgher) %>% 
  mutate(score = rowMeans(.[, 4:13])) %>% 
  select(idstud, idcntry, std_rsc, edu_rsc, score) %>% 
  mutate(study_resource = ifelse(std_rsc < 1, "No Resource", "With Resource")) %>% 
  select(idstud, idcntry, study_resource, edu_resource=edu_rsc, score) %>% 
  filter(!is.na(study_resource)) %>% 
  filter(!is.na(edu_resource))


edu_resources %>% ggplot(mapping = aes(study_resource, score, fill = study_resource)) +
  geom_boxplot(notch=FALSE) +
  ylab("score") +
  xlab("resource") +
  ggtitle("Density of score based on home resources") +
  guides(fill=guide_legend(title="home resources"))

no_resource <- edu_resources %>% filter(study_resource == "No Resource")
with_resource <- edu_resources %>% filter(study_resource == "With Resource")


hchart(density(no_resource$score), type = "area", name=list("No Resource")) %>%
  hc_add_series(density(with_resource$score), type = "area", name=list("With Resource")) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on home resources", style = list(fontWeight = "bold"))

t.test(no_resource$score, with_resource$score, alt = "less")

# other data with anova
edu_resources <- edu_resources %>% mutate(edu_stuff = ifelse(edu_resource == 1, "Many Resources", ifelse(edu_resource == 2, "Some Resources", "Few Resources")))
edu_resources %>% ggplot(mapping = aes(edu_stuff, score, fill = edu_stuff)) +
  geom_boxplot(notch=FALSE) +
  ylab("score") +
  xlab("resource") +
  ggtitle("Density of score based on home resources") +
  guides(fill=guide_legend(title="home resources")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

few_resource <- edu_resources %>% filter(edu_resource == 3)
some_resource <- edu_resources %>% filter(edu_resource == 2)
many_resource <- edu_resources %>% filter(edu_resource == 1)

hchart(density(few_resource$score), name=list("Few Resources")) %>%
  hc_add_series(density(some_resource$score), name=list("Some Resources")) %>% 
  hc_add_series(density(many_resource$score), name=list("Many Resources")) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on home resources", style = list(fontWeight = "bold"))

summary(aov(score ~ edu_resource, data = edu_resources))
