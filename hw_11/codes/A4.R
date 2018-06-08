iran_sig_eq <- iran_eq %>% filter(Mag >= 7) %>% arrange(desc(OriginTime))

last <- iran_sig_eq %>% slice(1)
diff = as.numeric(Sys.time() -last$OriginTime, units="days")
diff = diff %/% 365

diff_year = diff + 5
after_year = 1 # for first one
for (i in 1:nrow(iran_sig_eq)) {
  if (i > 1) {
    differ = as.numeric(iran_sig_eq[i-1,]$OriginTime - iran_sig_eq[i,]$OriginTime, units="days") %/% 365
    if (differ >= diff_year) {
      after_year = after_year + 1
    }
  }
}
after_year = after_year / nrow(iran_sig_eq)

after_two = 1 # for first one
for (i in 1:nrow(iran_sig_eq)) {
  if (i > 1) {
    differ = as.numeric(iran_sig_eq[i-1,]$OriginTime - iran_sig_eq[i,]$OriginTime, units="days") %/% 365
    if (differ >= 2) {
      after_two = after_two + 1
    }
  }
}
after_two = after_two / nrow(iran_sig_eq)

p = (after_year)/(after_two)
cat("Probability of earthquake in 5 years is", p)
