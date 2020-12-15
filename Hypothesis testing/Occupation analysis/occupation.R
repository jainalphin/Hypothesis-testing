library(dplyr)

df= read.csv("E:/DATA SCIENCE- Fingertips/Assigenment/R/Occupation analysis/occupation-analytics-dataset.csv")
View(df)

#Q1. Is there any relation between Marital Status and Education?
unique(df$Marital.status)
unique(df$Education)

chi_sqr = chisq.test(df$Marital.status,df$Education)
chi_sqr
p_value = chi_sqr$p.value

if(p_value < 0.05){
  print("There is Significant Relation")
}else{
  print("There is no Significant Relation")
}

#Q2. Is there any relation between Occupation and Education?
unique(df$Occupation)
chi_sqr =chisq.test(df$Occupation,df$Education)
p_value=chi_sqr$p.value
if(p_value < 0.05){
  print("There is Significant Relation")
}else{
  print("There is no Significant Relation")
}
#-------------------question value diff--------------------
#Q3. Is there any relationship between Balance and Education
anova = aov(Balance~Education,df)
anova(anova)
p_value = anova(anova)$'Pr(>F)'[1]
p_value
if(p_value < 0.05){
  print("There is Significant Relation")
}else{
  print("There is no Significant Relation")
}


#Q4. Is there any relation between House Ownership Status and Balance?
unique(df$House.Ownership.Status)

ttest = t.test(Balance~House.Ownership.Status,df)
ttest
p_value = ttest$p.value
p_value
if(p_value < 0.05){
  print("There is Significant Relation")
}else{
  print("There is no Significant Relation")
}

#Q5. Is there any relation between Previous Default status and Balance?
unique(df$Previous.Default.status)
ttest = t.test(Balance~Previous.Default.status,df)
p_value = ttest$p.value
p_value
if(p_value < 0.05){
  print("There is Significant Relation")
}else{
  print("There is no Significant Relation")
}

#Q6. Which education qualification has most aged student/professional?
unique(df$Education)

anova = aov(Age~Education,df)
anova(anova)
p_val = anova(anova)$'Pr(>F)'[1]
p_val
if(p_val < 0.05){
  print("There is Significant Relation")
}else{
  print("There is no Significant Relation")
}

anova2 = TukeyHSD(anova)
anova2

res=data.frame(anova2$Education)
View(res)
res = res %>% arrange(p.adj)
View(res)

item_type = row.names(res[1,])
item_type

p_val <- res$p.adj[1]
p_val

if(p_val < 0.05){
  cat('There is Significant relation between ',item_type,"with p_val",p_val)
}else{
  cat('There is no Significant relation between ',item_type,"with p_val",p_val)
}


# string manipulation
library(stringr)
lower_val = word(item_type,start = 1,sep = '-')
lower_val

higer_val = word(item_type,start = 2,sep = '-')
higer_val

cat(lower_val,"lower Profit")
cat(higer_val,"Higher Profit")
