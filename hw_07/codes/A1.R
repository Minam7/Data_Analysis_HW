library(corrplot)

death_num <- death %>% mutate(Sex = factor(Sex) ) %>% 
  mutate(MannerOfDeath = ifelse(MannerOfDeath == 2, 1, 0)) %>% 
  mutate(InjuryAtWork = factor(InjuryAtWork)) %>% 
  mutate(MethodOfDisposition = factor(MethodOfDisposition)) %>% 
  mutate(MaritalStatus = factor(MaritalStatus))

all_cors = cor(data.matrix(death_num))
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
                                  ActivityCode, PlaceOfInjury, Sex, RaceRecode3)
death_num_new <- death_num

all_cors = cor(data.matrix(death_num))
corrplot(all_cors, method = "color", tl.cex = 0.75/par("cex"), cl.cex = 0.75/par("cex"))

knitr::kable(sort(abs(all_cors['MannerOfDeath',]), decreasing = TRUE)[2:14])

death_num_sample = sample_n(death_num, 10000)
scatterplotMatrix(death_num_sample, spread=FALSE, smoother.args=list(lty=2), 
                  main="Scatter Plot Matrix of murder of suicide")
             