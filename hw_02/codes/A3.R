library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

sim_num_price = mobile %>% group_by(sim_no) %>% summarise(average= mean(price, na.rm = TRUE))

p = ggplot(data = sim_num_price, aes(y = average, x = sim_no, fill = average)) + geom_bar(stat="identity") + ggtitle('average price based on number of simcards') + xlab('number of simcards') + ylab('average price') + guides(fill=guide_legend(title="price"))
p

lte_price = mobile %>% group_by(LTE) %>% summarise(average= mean(price, na.rm = TRUE))

p_lte = ggplot(data = lte_price, aes(y = average, x = LTE, fill = average)) + geom_bar(stat="identity") + ggtitle('average price based on LTE support') + xlab('LTE support') + ylab('average price') + guides(fill=guide_legend(title="price"))
p_lte

mobile = mobile %>% mutate(sim_lte = interaction(sim_no, LTE))
View(mobile)

mobile_lte_price = mobile %>% group_by(sim_lte) %>% summarise(average= mean(price, na.rm = TRUE))

p_lte_price = ggplot(data = mobile_lte_price, aes(y = average, x = sim_lte, fill = average)) + geom_bar(stat="identity") + ggtitle('average price based on number of simcards and having LTE') + xlab('having LTE and number of simcards') + ylab('average price') + guides(fill=guide_legend(title="price"))
p_lte_price
