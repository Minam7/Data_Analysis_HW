library(car)

tops <- house_numeric %>% select(OverallQual,GrLivArea,GarageCars,GarageArea,TotalBsmtSF
                                ,`1stFlrSF`,FullBath,TotRmsAbvGrd,YearBuilt,YearRemodAdd
                                ,SalePrice)

scatterplotMatrix(tops, spread=FALSE, smoother.args=list(lty=2), 
                  main="Scatter Plot Matrix of house")
