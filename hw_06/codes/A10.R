final_predict = predict(edit_reg_model, house_test)
house_test$SalePrice = final_predict

res <- house_test %>% select(Id, SalePrice) %>% 
  mutate(SalePrice = ifelse(!is.na(SalePrice),SalePrice,mean(SalePrice, na.rm = TRUE)))

write.csv(res, file = "ans.csv",row.names=FALSE)

final_predict = predict(gam_model, house_test)
house_test$SalePrice = final_predict

res <- house_test %>% select(Id, SalePrice) %>% 
  mutate(SalePrice = ifelse(!is.na(SalePrice),SalePrice,mean(SalePrice, na.rm = TRUE)))

write.csv(res, file = "gam_ans.csv",row.names=FALSE)
