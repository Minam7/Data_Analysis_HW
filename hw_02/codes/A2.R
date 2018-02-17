library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

device_length = mobile %>% group_by(year) %>% summarise(average= mean(dim_length, na.rm = TRUE))

p_length = ggplot(data = device_length, mapping = aes(y = average, x = year)) + geom_point(stat = "identity") + ggtitle('average length based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE)
p_length

device_breath = mobile %>% group_by(year) %>% summarise(average= mean(dim_breadth, na.rm = TRUE))

p_breath = ggplot(data = device_breath, mapping = aes(y = average, x = year)) + geom_point(stat = "identity") + ggtitle('average breath based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE)
p_breath

device_thickness = mobile %>% group_by(year) %>% summarise(average= mean(dim_thickness, na.rm = TRUE))

p_thickness = ggplot(data = device_thickness, mapping = aes(y = average, x = year)) + geom_point(stat = "identity") + ggtitle('average thickness based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE)
p_thickness

device_cam_px = mobile %>% group_by(year) %>% summarise(average= mean(cam_px, na.rm = TRUE))

p_cam_px = ggplot(data = device_cam_px, mapping = aes(y = average, x = year)) + geom_point(stat = "identity") + ggtitle('average camera pixel based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE)
p_cam_px
