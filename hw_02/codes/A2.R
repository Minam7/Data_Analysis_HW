library(readr)
library(dplyr)
library(ggplot2)
mobile = read_csv("data/mobile_data.csv")

# device_length = mobile %>% group_by(year) %>% summarise(average= mean(dim_length, na.rm = TRUE))

p_length = ggplot(data = mobile, mapping = aes(y = dim_length, x = year)) + geom_point(stat = "identity") + ggtitle('device length based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE) + ylab("length")
p_length

# device_breath = mobile %>% group_by(year) %>% summarise(average= mean(dim_breadth, na.rm = TRUE))

p_breath = ggplot(data = mobile, mapping = aes(y = dim_breadth, x = year)) + geom_point(stat = "identity") + ggtitle('device breath based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE) + ylab("breath")
p_breath

# device_thickness = mobile %>% group_by(year) %>% summarise(average= mean(dim_thickness, na.rm = TRUE))

p_thickness = ggplot(data = mobile, mapping = aes(y = dim_thickness, x = year)) + geom_point(stat = "identity") + ggtitle('thickness based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE) + ylab("thickness")
p_thickness

# device_cam_px = mobile %>% group_by(year) %>% summarise(average= mean(cam_px, na.rm = TRUE))

p_cam_px = ggplot(data = mobile, mapping = aes(y = cam_px, x = year)) + geom_point(stat = "identity") + ggtitle('camera pixel based on production year') + geom_smooth(method = "lm", se = FALSE, colour="red") + geom_smooth(se = FALSE) + ylab("camera pixel")
p_cam_px
