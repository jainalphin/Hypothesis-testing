library(dplyr)
library(readxl)
df= read_excel("E:/DATA SCIENCE- Fingertips/Assigenment/R/hr-analytics/hr-analytics_data_4.xlsx")
View(df)

# HR manager wants to know which gender has more monthly income.

df %>% group_by(Gender) %>% summarise(average = mean(MonthlyIncome,,na.rm = TRUE))
  
#Q2. HR manager wants, Is there any relation between Gender and job role?
unique(df$JobRole)

chi_sqr = chisq.test(df$JobRole,df$Gender)
chi_sqr
chi_sqr$p.value

#Q3. Which Marital status prefers business travel?
unique(df$BusinessTravel)
dt = table(df %>% filter(BusinessTravel=="Travel_Frequently" | BusinessTravel=='Travel_Rarely')%>% select(MaritalStatus))
dt
names(sort(dt,decreasing = TRUE)[1])

#Q4. Which education filed helps you to earn more?
unique(df$EducationField)
tbl_df(df%>% group_by(EducationField) %>% summarise(average = mean(MonthlyIncome)) %>% arrange(-average))[1,1]

# Q5. Which Gender prefers working after the job time?
ans = table(df$Gender,df$OverTime)[,"Yes"]
ans
names(ans[order(-as.numeric(ans))][1])

names(sort(table(df$Gender,df$OverTime)[,2],decreasing = TRUE)[1])

#Q6. Employees from which department are more satisfied with their job?
sort(table(df$Department,df$JobSatisfaction)[,4],decreasing = TRUE)[1]
names(sort(table(df$Department,df$JobSatisfaction)[,4],decreasing = TRUE)[1])

#Q.7 which department has high daily rates?
ans =df %>% group_by(Department) %>% summarise(Average = mean(DailyRate)) %>% arrange(-Average)
ans[1,1]

# Q.8 Is there any relation between job role and age?
anova = aov(Age~JobRole,df)
p_val = anova(anova)$'Pr(>F)'[1]
anova(anova)


# Q9. Is there any relation between MaritalStatus and MonthlyIncome
anova = aov(MonthlyIncome~MaritalStatus,df)
p_val = anova(anova)$'Pr(>F)'[1]
anova(anova)

# Q10. Is there any relation between MaritalStatus and OverTime?
chi_sqr = chisq.test(df$MaritalStatus,df$OverTime)
chi_sqr$p.value

