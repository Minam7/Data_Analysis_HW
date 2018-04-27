death_mat <- data.matrix(death_num)

# sex impact
cor.test(death_mat[,"Sex"], death_mat[,"MannerOfDeath"], method = 'spearman')
chisq.test(death_num$Sex, death_num$MannerOfDeath)

# race impact
cor.test(death_mat [,"RaceRecode3"], death_mat [,"MannerOfDeath"], method = 'spearman')
chisq.test(death_num$RaceRecode3, death_num$MannerOfDeath)

# education impact
cor.test(death_mat [,"edu"], death_mat [,"MannerOfDeath"], method = 'spearman')
chisq.test(death_num$edu, death_num$MannerOfDeath)

# age impact
cor.test(death_mat [,"AgeRecode27"], death_mat [,"MannerOfDeath"], method = 'spearman')
chisq.test(death_num$AgeRecode27, death_num$MannerOfDeath)

# method of disposition impact
cor.test(death_mat [,"MethodOfDisposition"], death_mat [,"MannerOfDeath"], method = 'spearman')
chisq.test(death_num$MethodOfDisposition, death_num$MannerOfDeath)
