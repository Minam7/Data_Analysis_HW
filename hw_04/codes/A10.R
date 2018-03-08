irn <- tims %>% filter(Country == "Iran, Islamic Rep. of") %>% 
  select(score = correct_ratio_per_question, type= cognitive_domain) %>% 
  filter(type == "Applying" | type == "Reasoning")

ggplot(irn, aes(x = score, fill = type)) + geom_density(alpha= 0.4) + ggtitle("Density of score based on cognitive domain in Iran") + 
  ylab("density") + 
  xlab("score") + 
  guides(fill=guide_legend(title="cognitive domain"))

group1 <- irn %>% filter(type == "Applying")
group2 <- irn %>% filter(type == "Reasoning")

hchart(density(group1$score), type = "area", name=list("Applying")) %>%
  hc_add_series(density(group2$score), type = "area", name=list("Reasoning")) %>%
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) 

t.test(score~type, data = irn, alt = 'greater')
