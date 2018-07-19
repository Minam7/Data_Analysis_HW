worldwide <- read_csv("data/worldwide.csv")

worldwide <- worldwide %>% mutate(year = as.numeric(format(time, format = "%Y")), 
         month = as.numeric(format(time, format = "%m")), 
         day = as.numeric(format(time, format = "%d")))

x <- str_split_fixed(worldwide$place, ",", 2) %>% as.data.frame(stringsAsFactors = F)
colnames(x) = c("", "location")
x <- x %>% select(location)

worldwide <- bind_cols(worldwide, x)

worldwide_fore <- worldwide %>% filter(location != "") %>% 
  select(time, year, month, day, latitude, longitude, depth, mag, location) %>% 
  group_by(year, month, location) %>% arrange(time) %>% 
  mutate(max_mag = max(mag))

x <- worldwide_fore %>% filter(mag == max_mag)
x$max_time <- x$time

s <- worldwide_fore %>% filter(mag != max_mag)
s <- s %>% full_join(x, by = c("max_mag", "month", "year", "location")) %>% 
  select(time = time.x, year, month, day = day.x, latitude = latitude.x, longitude = longitude.x,
         depth = depth.x, mag = mag.x, location,max_mag, max_time = time.y)

worldwide_fore_aft <- bind_rows(x, s) %>% filter(!is.na(time)) %>% 
  mutate(diff = as.numeric(max_time - time, units="days"),
         state = ifelse(diff > 0, 1, ifelse(
           diff == 0, 0, -1
         )))

prob = nrow(worldwide_fore_aft %>% filter(state == 1) %>% 
              distinct(year, month, location))/
  nrow(worldwide_fore_aft %>% distinct(year, month, location))

cat("Probability of happening earthquake after foreshock is:", prob)

# predict main earthquake
worldwide_fore_mean <- worldwide_fore_aft %>% group_by(year, month, location, state) %>% summarise(count = n(), mean_mag = mean(mag), mean_dep = mean(depth))
ww_fore <- worldwide_fore_mean %>% filter(state == 1)
ww_eq <- worldwide_fore_mean %>% filter(state == 0)
ww_merge <- inner_join(ww_fore, ww_eq, by = c("year", "month", "location"))
glm_foreshock <- glm(mean_mag.y ~ mean_mag.x + count.x, data = ww_merge)
summary(glm_foreshock)
