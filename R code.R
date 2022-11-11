library(haven)
library(sjmisc)
library(psych)
library(dplyr)

salary <- read_sav("C:/Users/sotos/Desktop/Data Science/Elements Statistics and Probabaility/Assignment/salary.sav")
View(salary)

gender=factor( salary$sex, labels=c('male', 'female'))
gender_table=table(gender)
gender_table
frq(gender, out = "v")
barplot(gender_table, main = paste("Barplot of Gender"))

jobs=factor( salary$jobcat, labels=c('clerical', 'office_trainee', 'security_officer', 'college_trainee', 'exempt_employee', 'MBA_trainee', 'technical'))
job_table=table(jobs)
job_margin_table=prop.table(job_table)
job_margin_table
frq(jobs, out = "v")

pie(job_margin_table, labels = paste(names(job_margin_table), "\n", round(job_margin_table*100, digits=2), "%" ,sep=""))

as.numeric(job_margin_table['clerical']+job_margin_table["security_officer"])*100

final_salary <- cut(salary$salnow, breaks=seq( min(salary$salnow), max(salary$salnow), (max(salary$salnow)-min(salary$salnow))/12), include.lowest = TRUE, dig.lab=5)
frq(final_salary, out = "v")

salary[salary$salnow==max(salary$salnow), 'salnow' ]

mean(salary$salnow)
median(salary$salnow)

quantile(salary$salnow, probs=0.9)

table(salary$edlevel)
hist(salary$edlevel, main= "Histogram of Education Level", xlab= "education level")
describe(salary$edlevel)

q=sapply(salary, class)
q
numeric_data=salary[, (q== "numeric")]
numeric_data=select(numeric_data,-('id'))
numeric_data
sapply(numeric_data, mean)
sapply(numeric_data, sd)
sapply(numeric_data, range)

z=scale(numeric_data)
z_numeric_data=as.data.frame(z)
z_numeric_data
describe(z_numeric_data)
sapply(z_numeric_data, mean)
sapply(z_numeric_data, sd)
sapply(z_numeric_data, range)

female_sample=cbind(z_numeric_data, sex=salary$sex)
female_sample=female_sample[female_sample$sex==1,]
mean(female_sample$edlevel)

salary$raise=salary$salnow-salary$salbeg
mean(salary$raise)
median(salary$raise)
sd(salary$raise)

salary[salary$raise==max(salary$raise), c('id','raise')]

salary$salbeg2=0
for (i in 1:as.numeric(count(salary))) {
  if(salary$salbeg[i]<5000) {
    salary$salbeg2[i]=0
    } else if(salary$salbeg[i]<10000) {
      salary$salbeg2[i]=1
      } else if(salary$salbeg[i]<15000) {
        salary$salbeg2[i]=2
        } else if (salary$salbeg[i]<20000) {
          salary$salbeg2[i]=3
          } else if (salary$salbeg[i]<25000) {
            salary$salbeg2[i]=4
          } else salary$salbeg2[i]=5
            }

frq(salary$salbeg2, out = 'v')

plot(salary$edlevel,salary$salnow, main='Scatter Plot Education level-Salary', xlab = 'Education Level', ylab = 'Salary', pch=10)
# Linear fit
abline(lm(salary$salnow ~ salary$edlevel), col = "orange", lwd = 3)
# Smooth fit
lines(lowess(salary$edlevel, salary$salnow), col = "blue", lwd = 3)
# Legend
legend("topleft", legend = c("Linear", "Smooth"), lwd = 3, lty = c(1, 1), col = c("orange", "blue"))

library(car)
scatterplot(salary$salnow ~ salary$edlevel,main='Scatter Plot Education level-Salary', xlab = 'Education Level', ylab = 'Salary',  col = 1, pch = 15, regLine = list(col = "orange",  lwd = 3), smooth = list(col.smooth = "red", col.spread = "white"))
legend("topleft", legend = c("Linear", "Smooth"), lwd = 6, lty = c(1, 1), col = c("orange", "red"))