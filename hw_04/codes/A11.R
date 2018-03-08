#1.
# lonely students effect their score
lone <- bsg %>% select(student = idstud, country = idcntry, left= bsbg16b, contains("bsssci"), contains("bsmmat")) %>% 
  mutate(score = rowMeans(.[, 4:13])) %>% 
  filter(left < 5) %>% 
  mutate(alone= ifelse(left < 3, "yes", "no")) %>% 
  select(student, country, alone, score)

ggplot(lone, aes(x = score, fill = alone)) + geom_density(alpha= 0.4) + ggtitle("Density of score based on loneliness") + 
  ylab("density") + 
  xlab("score") + 
  guides(fill=guide_legend(title="lonely"))

group1 <- lone %>% filter(alone == "yes")
group2 <- lone %>% filter(alone == "no")

hchart(density(group1$score), type = "area", name=list("alone")) %>%
  hc_add_series(density(group2$score), type = "area", name=list("not alone")) %>%
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on loneliness", style = list(fontWeight = "bold")) %>% 
  hc_add_theme(hc_theme_db())

t.test(score~alone, data = lone, alt="greater")

#2
# teachers using discussion get better results
tchr_inq <- btg %>% select(country= idcntry, teacher= idteach, discus = btbg14d) %>% 
  filter(!is.na(country) & !is.na(teacher) & !is.na(discus)) %>% 
  filter(discus < 5)

discus_res <- tchr_inq %>% inner_join(tchr_std_perf, by=c("country", "teacher")) %>% 
  mutate(discussion = ifelse(discus == 1, "Every or almost every lesson",
                             ifelse(discus == 2, "About half the lessons",
                                    ifelse(discus == 3, "Some lessons", "Never"))))

discus_res %>% ggplot(mapping = aes(discussion, score, fill = discussion)) +
  geom_boxplot(notch=FALSE) +
  ylab("score") +
  xlab("disscusion in class") +
  ggtitle("Density of score based on class discussion") +
  guides(fill=guide_legend(title="class discussion"))


hchart(density(filter(discus_res, discussion == "Every or almost every lesson")$score), name=list("Every or almost every lesson")) %>%
  hc_add_series(density(filter(discus_res, discussion == "About half the lessons")$score), name=list("About half the lessons")) %>% 
  hc_add_series(density(filter(discus_res, discussion == "Some lessons")$score), name=list("Some lessons")) %>% 
  hc_add_series(density(filter(discus_res, discussion == "Never")$score), name=list("Never")) %>% 
  hc_add_theme(hc_theme_ft()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on class discussion", style = list(fontWeight = "bold"))

summary(aov(score ~ discussion, data = discus_res))

discus_all <- discus_res %>% filter(discussion == "Every or almost every lesson")
discuss_no <- discus_res %>% filter(discussion == "Never")
t.test(discus_all$score, discuss_no$score, alt= "less")
