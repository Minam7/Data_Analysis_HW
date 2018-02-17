library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
mobile = read_csv("data/mobile_data.csv")

last_year_announce = mobile[which(str_detect(mobile$announced, "2017") == TRUE), ]

p = ggplot(data = subset(last_year_announce, !is.na(audio_jack)), aes(y = dim_thickness, x = audio_jack)) + geom_boxplot() + xlab("audio jack") + ylab(" thickness") + ggtitle("Cellphone thickness based on having audio jack")
p
