#loading required libraries
library('ggplot2')
library('Rmisc')
library(mice)
library(VIM)
library(dplyr)
library(rAmCharts)
library(tidyverse)
library(gridExtra)
library(knitr)

#setting working directory
setwd("C:/Users/talha/Downloads/NU/MS/project")

#fill=T, na.strings="" represents that if there is any blank cell in any column replace it with NA
original <- read.csv(file = "CityPayrollDataset.csv",header=TRUE, fill = T, na.strings = "")

#copy of original dataset
dataset=original

#ploting null values
aggr_plot <- aggr(original, col=c('Green','red'),
                  numbers=TRUE, sortVars=TRUE, labels=names(data),
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

###########################################################----functions start-----######################################
#A generic function has been made to find sum of null values in any dataset
count_null = function(df)
{
  for (i in 1:ncol(df))
  {
    cat(paste("Null Values in ",names(df[i]))," are:\t\t",sum(is.na(df[i])))
    cat("\n")
  }
}


###########################################################----functions end-----######################################


#calling count_null function to count the null values in the dataset by columns
count_null(dataset)

#the following built in funtion can also be used to count the null values in dataset
sapply(dataset, function(x) sum(is.na(x)))

#plotting null values
md.pattern(dataset)
mice_plot <- aggr(dataset, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(dataset), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
str(dataset)

#making list of columns with $ sign
col_list = list('Hourly.or.Event.Rate',
'Projected.Annual.Salary',
'Q1.Payments',
'Q2.Payments',
'Q3.Payments',
'Q4.Payments',
'Payments.Over.Base.Pay',
'Total.Payments',
'Base.Pay',
'Permanent.Bonus.Pay',
'Longevity.Bonus.Pay',
'Temporary.Bonus.Pay',
'Lump.Sum.Pay',
'Overtime.Pay',
'Other.Pay...Adjustments',
'Average.Health.Cost',
'Average.Dental.Cost',
'Average.Basic.Life',
'Average.Benefit.Cost',
'Other.Pay..Payroll.Explorer.'
)

#unlisting the listed column names to use them in loop through index
col_list = unlist(col_list)

#converting columns with $ sign to integer type also removing their $ sign
for (i in 1:length(col_list)) {
  dataset[col_list[i]] <- lapply(dataset[col_list[i]], function(x) as.integer(gsub("[,$]", "", x)))
}

#removing variables from memory
rm(i)
rm(col_list)

#removing % sigb from x.over.base.pay column
dataset[['X..Over.Base.Pay']] <- lapply(dataset[['X..Over.Base.Pay']], function(x) as.integer(gsub("[,%]", "", x)))

#converting MOU as integer from character
dataset[['MOU']] <- as.integer(dataset[['MOU']] )

#making new dataframe of integer values
int_data = select(dataset,
          Payroll.Department,
          Hourly.or.Event.Rate,
          Lump.Sum.Pay,
          Overtime.Pay,
          MOU)

#predicting missing values
imputed_Data <- mice(int_data, m=5, maxit = 10, method = 'pmm', seed = 500)

#for better performance removing int_data dataset from memory
rm(int_data)

#filling those predicted missing values in their places
complete_data = complete(imputed_Data,5)

#for better performance removing imputed_Data dataset from memory
rm(imputed_Data)

#replacing NA cells with predicted values
dataset[['Payroll.Department']] <- complete_data$Payroll.Department
dataset[['Hourly.or.Event.Rate']] <- complete_data$Hourly.or.Event.Rate
dataset[['Lump.Sum.Pay']] <- complete_data$Lump.Sum.Pay
dataset[['Overtime.Pay']] <- complete_data$Overtime.Pay
dataset[['MOU']] <- complete_data$MOU

#for better performance removing complete_data dataset from memory
rm(complete_data)

#filling categorical variables by their mode
b.p <- dataset$Benefits.Plan
val <- unique(b.p[!is.na(b.p)])              
mode <- val[which.max(tabulate(match(b.p, val)))]                                 
b.p[is.na(b.p)] <- mode   
dataset$Benefits.Plan <- b.p

#for better performance removing variables from memory
rm(b.p)

m.t <- dataset$MOU.Title
val <- unique(m.t[!is.na(m.t)])              
mode <- val[which.max(tabulate(match(m.t, val)))]                                 
m.t[is.na(m.t)] <- mode
dataset$MOU.Title <- m.t
 
m.t <- dataset$Pay.Grade
val <- unique(m.t[!is.na(m.t)])              
mode <- val[which.max(tabulate(match(m.t, val)))]                                 
m.t[is.na(m.t)] <- mode
dataset$Pay.Grade <- m.t

#for better performance removing variables from memory
rm(m.t)
rm(val)
rm(mode)

#Again checking null values in dataframe
count_null(dataset)
print('There is no NULL value present in the dataset')

#Again Visualizing null values
aggr_plot <- aggr(dataset, col=c('Green','red'),
                  numbers=TRUE, sortVars=TRUE, labels=names(data),
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#clearing console
cat("\014")
#++++++++++++++----------------preprocessing ends here---------------+++++++++++

#----------------------------------TASK-1---------------------------------------
#Assumptions:

  #As explained in the class lectures, we compare our sample(altenative hyp) with
  #population(null hyp). Therefore, in null hypothesis "police Officers II" are included from the 
  #whole dataset and in sample all class.titles are assumed who are "not police officers II"

#Null Hyp: 
  #employees who get permanent bonus are "likely" to be from police officers-II
#Altr Hyp: 
  #employees who get permanent bonus are "not likely" to be from police officers-II

#Null Hypothesis 
task1.pop = subset(dataset, Job.Class.Title=="Police Officer II" & dataset$Temporary.Bonus.Pay > 0 )
task1.pop.mean = task1.pop$Temporary.Bonus.Pay
task1.popu.mean = mean(task1.pop.mean)

#Alternative Hypothesis
task1.sample = sample_n(dataset, 40, replace = T)
sample.req.data = subset(task1.sample, Job.Class.Title !="Police Officer II" & task1.sample$Temporary.Bonus.Pay > 0 )
task1.sample.mean = sample.req.data$Temporary.Bonus.Pay

#Applying hypothesis testing
t.test(task1.sample.mean,mu = task1.popu.mean)
print("P-values is greater than 0.05 and mean of sample is between CI therefore, we failed to reject null hypothesis")

#----------------------------------TASK-2---------------------------------------

#Assumptions:

#As explained in the class lectures, we compare our sample(altenative hyp) with
#population(null hyp). Therefore, in null hypothesis "Public Works - Sanitation" are included from the 
#whole dataset and in sample all class.titles are assumed who are "Public Works - Sanitation"

#Null Hypothesis 
#null hyp: employees who get permanent bonus are "likely" to be from Public Works - Sanitation
#altr hyp: employees who get permanent bonus are "not likely" to be from Public Works - Sanitation

#null Hypothesis
task2.pop = subset(dataset, Department.Title=="Public Works - Sanitation", Permanent.Bonus.Pay > 0 )
task2.pop.mean = task2.pop$Permanent.Bonus.Pay
task2.popu.mean = mean(task2.pop.mean)
task2.popu.mean
#Alter Hypothesis
task2.sample = sample_n(dataset, 40, replace = T)
task2.sample
sample2.req.data = subset(task2.sample, Department.Title !="Public Works - Sanitation" & task2.sample$Permanent.Bonus.Pay > 0 )
sample2.req.data
task2.sample.mean = sample2.req.data$Permanent.Bonus.Pay

#Hypothesis Testing
t.test(task2.sample.mean,mu = task2.popu.mean)
print("P-values is greater than 0.05 and mean of sample is between CI therefore, 
      Null hypothesis has not been rejected")


#----------------------------------TASK-3---------------------------------------
#2/1/21

#null hyp: employees working in Water and Power (DWP) Department likely being employed overtime
#altr hyp: employees working in Water and Power (DWP) Department not likely being employed overtime

#null
task3.pop = subset(dataset, Department.Title=="Water And Power (DWP)", Overtime.Pay > 0 )
task3.pop.mean = task3.pop$Overtime.Pay
task3.popu.mean = mean(task3.pop.mean)
task3.popu.mean
#alter
task3.sample = sample_n(dataset, 40, replace = T)
task3.sample
sample3.req.data = subset(task3.sample, Department.Title !="Water And Power (DWP)" & Overtime.Pay > 0 )
sample3.req.data
task3.sample.mean = sample3.req.data$Overtime.Pay
mean(task3.sample.mean)
CI(task3.sample.mean)

t.test(task3.sample.mean,mu = task3.popu.mean)
print("Null hypothesis has not been rejected")


#----------------------------------TASK-4---------------------------------------
#2/1/21


#hypothesis Test
#Null hyp: Airports (LAWA) Department works Part Time instead of Full Time.
#Alter Hyp Airports (LAWA) Department works Full Time instead of Part Time.
# sampling
task4.sample <- sample_n(dataset, 1000)

table(task4.sample$Employment.Type, task4.sample$Department.Title)

chisq.test(task4.sample$Employment.Type, task4.sample$Department.Title)
cat(sprintf(" chisq.test show that p-value < 0.005 that mean Hypothesis Alter Airports (LAWA) Department works Full Time instead of Part Time"))





################################ TAsk 5
#Hypothesis Test
#Null Hypothesis Police (LAPD) Department has experienced the highest pay raise
#Alternate Hypothesis Police (LAPD) Department has not experienced the highest pay raise

# making sample at condition after 2013 records  
task5.sample <- sample_n(dataset[(dataset$Year > 2013),], 500)

#dataframes containing and not containing police
task5.full <- task5.sample[(Police$Department.Title != 'Police (LAPD)'),]
task5.police <- task5.sample[(Police$Department.Title == 'Police (LAPD)'),]
#filtering columns
task5.full = task5.full$Projected.Annual.Salary
task5.police = task5.police$Projected.Annual.Salary

#test
chisq.test(task5.sample$Projected.Annual.Salary, task5.sample$Department.Title)
#Results
boxplot(task5.full,task5.police, horizontal = TRUE)
print('the results are same as previous')


#----------------------------------------------Task 6---------------------------
#Null hypothesis
#there is relationship between Department and Employment type
#Alternate Hypothesis 
#No relationship between Department and Employment type.

#sample of 100
task6.sample <- sample_n(dataset, 70,replace = T)
#making table
table(task6.sample$Department.Title, task6.sample$Employment.Type)
#applying test
chisq.test(task6.sample$Department.Title, task6.sample$Employment.Type)

print('from the results it can be seen that p value is very small than 0.005 therefore there 
      is no relation between these two variables')
#------------------------------------ Cross Check------------------------------
ggscatter(task6.sample, x = "Department.Title", y = "Employment.Type", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Department.Title", ylab = "Employment.Type")


#----------------------------------TASK7----------------------------------------


#NUll Hyp
#Average Base Pay of employees in General Services Department is $20,746
#Alt Hyp
#Average Base Pay of employees in General Services Department is not more than $20,746

task7.sample <- sample_n(Data_set[(dataset$Department.Title == 'General Services'),], 70,replace = T)
#selecting column
g_pay = task7.sample$Base.Pay
#appling condition 20746
t.test(g_pay, mu = 20746)
print('it has been observed that p is less than 0.005 therefore alternative hypothesis 
      Average Base Pay of employees in General Services Department is not more than $20,746
      is accepted')

#----------------------------- Cross Checking_________________________________
boxplot(g_pay, horizontal = TRUE)
print("from the box plot similar results have been observed as above therefore above results are
      ratained")

#----------------------------------TASK8----------------------------------------

#Null hyp 
#other than Water and Power (DWP) pay more benefit to employees
#Alter Hyp
#Water and Power (DWP) Department pays on average more Benefit
#Cost to its employees than all other departments.

task8.sample = sample_n(dataset[(dataset$Department.Title == 'Water And Power (DWP)'),], 70)
task8.sample_cost = task8.sample$Average.Benefit.Cost
#population average

task8.pop <- dataset$Average.Benefit.Cost
t.test(task8.pop, mu = mean(task8.sample_cost))
print('it has been observed that p is less than 0.005 therefore alternative hypothesis 
      Water and Power (DWP) Department pays on average more Benefit Cost to its 
      employees than all other departments is accepted')
#cross Check
boxplot(task8.pop,task8.sample_cost, horizontal = TRUE)
print("from the box plot similar results have been observed as above therefore above results are
      ratained")


#-------------------------------------task 9=-------------------------------

#Null hypothesis 
#employees of Recreation and Parks Department have denied of their Longevity Bonus Pay
#Alternate Hypothesis
#employees of Recreation and Parks Department haven't denied of their Longevity Bonus Pay
task9.sample <- sample_n(dataset[(dataset$Year == 2014),], 70)

chisq.test(task9.sample$Department.Title, task9.sample$Longevity.Bonus.Pay)

cat(sprintf("chisq.test showed P-Value == 1; Therefore employees of Recreation and Parks Department have denied of their Longevity Bonus Pay"))

#----------------------------------- Cross Check
hist( task9.sample$Longevity.Bonus.Pay)
print('Bonus pay is denied to most of the employees')

#__--------------------------------------------Task 10

#Null hypothesis 
#Harbor have more average health cost than water and power dept
#Alternate Hypothesis 
#Water Dept has more average health cost than Harbor dept.

h <- dataset[(dataset$Department.Title == 'Harbor (Port of LA)'),]
w <- dataset[(dataset$Department.Title == 'Water And Power (DWP)'),]

# average cost of both
ha <- h$Average.Health.Cost
wa <-  w$Average.Health.Cost

t.test(wa, mu = mean(ha))
print("chisq.test showed P-Value <0.005; DWP Dept. has more average health cost than Harbor dept")
#Cross Check
boxplot(wa,ha, horizontal = TRUE)






