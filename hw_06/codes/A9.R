library(gam)
second = tops$OverallQual^2

cor.test(tops$SalePrice, second)

edit_reg_model = lm(SalePrice~ I(OverallQual^2) + GrLivArea + GarageCars + TotalBsmtSF + I(1.1^`1stFlrSF`)
                    + FullBath + YearBuilt + YearRemodAdd + GarageArea, data = tops)
summary(edit_reg_model)

gam_model = gam(SalePrice~ s(OverallQual) + s(GrLivArea) + s(GarageCars) + s(GarageArea) + s(TotalBsmtSF) + 
                  + s(`1stFlrSF`) + (FullBath) + s(TotRmsAbvGrd) + s(YearBuilt) + s(YearRemodAdd), data=tops)
summary(gam_model)

gam_model = gam(SalePrice~ s(OverallQual) + s(GrLivArea) + s(TotalBsmtSF) + 
                  + s(`1stFlrSF`) + (FullBath) + s(YearBuilt) + s(YearRemodAdd), data=tops)
summary(gam_model)
