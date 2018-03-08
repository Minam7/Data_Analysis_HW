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


