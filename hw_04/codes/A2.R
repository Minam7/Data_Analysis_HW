library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)
library(tidyr)

bcg = read_rds("tims/data/bcg.rds") # school background
bsg = read_rds("tims/data/bsg.rds") # student background
btg = read_rds("tims/data/btm.rds") # Teacher background
bsa = read_rds("tims/data/bsa.rds") # student result
bst = read_rds("tims/data/bst.rds") # student-teacher linkage
bsr = read_rds("tims/data/bsr.rds") # within-country
tims = read_rds("tims/timss_2015.rds")

edu_perf <- bsg %>% select(idstud, idcntry, edu = bsdgedup, contains("bsssci"), contains("bsmmat")) %>% 
  mutate(sci = rowMeans(.[, 4:8]), math = rowMeans(.[, 9:13])) %>% 
  select(idstud, idcntry, edu, sci, math) %>% 
  mutate(score = rowMeans(.[,4:5])) %>% 
  select(idstud, idcntry, edu, score) %>% 
  filter(edu != 6) %>% 
  mutate(educate = ifelse(edu < 3, "Ba Tahsilat", "Bi Tahsilat"))

ggplot(edu_perf,aes(x = score,fill = educate)) + geom_density(alpha= 0.4) + ggtitle("Density of score based on parents' education")

non_edu <- edu_perf %>% filter(educate == "Bi Tahsilat")
with_edu <- edu_perf %>% filter(educate == "Ba Tahsilat")

hchart(density(non_edu$score), type = "area", name=list("Bi Tahsilat")) %>%
  hc_add_series(density(with_edu$score), type = "area", name=list("Ba Tahsilat")) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_yAxis(title = list(text = "density")) %>% 
  hc_xAxis(title = list(text = "score")) %>% 
  hc_title(text = "Density of score based on parents' education", style = list(fontWeight = "bold"))

t.test(non_edu$score, with_edu$score, alt = "less")
