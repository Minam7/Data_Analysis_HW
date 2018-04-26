library(corrplot)
library(car)

death_num <- death %>% mutate(sex = ifelse(Sex == 'M', 1, 0) ) %>% 
  mutate(MannerOfDeath = ifelse(MannerOfDeath == 2, 1, 0)) %>% 
  mutate(InjuryAtWork = ifelse(InjuryAtWork == 'Y', 1, ifelse(InjuryAtWork == 'N', 0, -1))) %>% 
  mutate(MethodOfDisposition = ifelse(MethodOfDisposition == 'U', 0 , ifelse(MethodOfDisposition == 'O', 1 , ifelse(MethodOfDisposition == 'B', 2 , 3)))) %>% 
  mutate(MaritalStatus = ifelse(MaritalStatus == 'S', 0, ifelse(MaritalStatus == 'M', 1, ifelse(MaritalStatus == 'W', 2, 
                                                                                                ifelse(MaritalStatus == 'D', 3, 9))))) %>% 
  select_if(is.numeric)

all_cors = cor(death_num)
corrplot(all_cors, method = "color", tl.cex = 0.5/par("cex"), cl.cex = 0.5/par("cex"))

#### fixing education
death_num1989 <- death_num %>% filter(EducationReportingFlag == 0) %>% 
  mutate(edu = ifelse(Education1989Revision == 10, 2, ifelse(
    Education1989Revision == 11, 2, ifelse(
      Education1989Revision == 12, 3, ifelse(
        Education1989Revision == 13, 4, ifelse(
          Education1989Revision == 14, 5, ifelse(
            Education1989Revision == 15, 5, ifelse(
              Education1989Revision == 16, 6, ifelse(
                Education1989Revision == 17, 7, ifelse(
                  Education1989Revision == 99, 9, ifelse(
                    Education1989Revision == 9, 2, 1
                  )
                )
              )
            )
          )
        )
      )
    )))) %>% select(-EducationReportingFlag, -Education1989Revision, -Education2003Revision)

death_num2003 <- death_num %>% filter(EducationReportingFlag == 1) %>% 
  rename(edu = Education2003Revision) %>% select(-EducationReportingFlag, -Education1989Revision)

death_num2003$edu <- ifelse(death_num2003$edu == 8, 7, death_num2003$edu)

death_num <- rbind(death_num2003, death_num1989)

#### selecting data
death_num <- death_num %>% select(ResidentStatus, edu, MonthOfDeath, AgeRecode27, PlaceOfDeathAndDecedentsStatus, 
                                  DayOfWeekOfDeath, InjuryAtWork, MannerOfDeath, MethodOfDisposition, MaritalStatus,
                                  ActivityCode, PlaceOfInjury, sex, RaceRecode3, CauseRecode39)
all_cors = cor(death_num)
corrplot(all_cors, method = "color", tl.cex = 0.75/par("cex"), cl.cex = 0.75/par("cex"))

knitr::kable(sort(abs(all_cors['MannerOfDeath',]), decreasing = TRUE)[2:15])

death_num_sample = sample_n(death_num, 10000)
scatterplotMatrix(death_num_sample, spread=FALSE, smoother.args=list(lty=2), 
                  main="Scatter Plot Matrix of murder of suicide")
             