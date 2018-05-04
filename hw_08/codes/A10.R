u_dickens <- glm(log(tf) ~ log(index), family="gaussian", data=unigram_data_all)
b_dickens <- glm(log(tf) ~ log(index), family="gaussian", data=bigram_data_all)
u_woolf <- glm(log(tf) ~ log(index), family="gaussian", data=woolf_unigram_data_all)
b_woolf <- glm(log(tf) ~ log(index), family="gaussian", data=woolf_bigram_data_all)

fit<-cbind(coef(u_dickens), coef(b_dickens), coef(u_woolf), coef(b_woolf))
fit

