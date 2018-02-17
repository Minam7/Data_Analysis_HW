library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")
most_devices = mobile %>% group_by(company) %>% summarise(count = n())

top_vendor = head(most_devices[order(most_devices$count, decreasing=TRUE), ], 20)

p = ggplot(data = top_vendor, aes(y = count, x = company, fill = count)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=0.1,vjust=0.1)) + scale_fill_gradient(low="red", high="green")
p
cat("most cellphone vendor: ", (most_devices$company[which.max(most_devices$count)]))