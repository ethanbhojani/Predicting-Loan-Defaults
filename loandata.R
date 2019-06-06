#####loan data project
##Clear the workspace
ls()
rm(list=ls())


##get working directory
getwd()
setwd("C:/Users/ethan/Desktop/loan project/")
list.files()

#install.packages("openxlsx")

library("openxlsx")

?read.xlsx
loans <- read.xlsx("loan trial .xlsx", 1, colNames = TRUE)

##compile packages
library("dplyr")
library("stringr")
library("tidyr")
#install.packages("gmodels")
library("gmodels")
head(loans)
colnames(loans)

##First row is column names. Fixing this
tempDF <- loans
tempDF[] <- lapply(loans, as.character)
colnames(loans) <- tempDF[1, ]
loans <- loans[-1 ,]
tempDF <- NULL
##check that it worked
colnames(loans)

#subset columns of use - we are primarily concerned about how the loan amount, risk grade, employment length, home ownership stats, and interest rate affects loan default rates.
##These correspond to columns 3 7 9 12 13 14 17


loans2 <- loans[, c(3, 7, 9, 12, 13, 14, 17)]
summary(loans2)
colnames(loans2)
##We can see that the formatting (character vs factor vs numeric) is wrong for many of these. Start correcting them.

#We will convert loan amount to numeric, grade to a factor, emp length as factor, home ownership is a factor, annual inc is a numeric, loan status is a factor, 

loans3 <- loans2

loans3$loan_amnt <- as.numeric(loans2$loan_amnt)
loans3$annual_inc <- as.numeric(loans2$annual_inc)
loans3$int_rate <- as.numeric(loans2$int_rate)

loans3$home_ownership <- as.factor(loans2$home_ownership)
loans3$loan_status <- as.factor(loans2$loan_status)
loans3$grade <- as.factor(loans2$grade)
loans3$emp_length <- as.factor(loans2$emp_length)
##To make interest rates a factor, we will create bins... First remove all rows with NAs
summary(loans3)
loans4 <- na.omit(loans3)
summary(loans4)

##assign values to repaid and default (charged off)
##We can see that some values are written as simply charged off or paid, others are longer. We will extract all columns that contain the word charged off and replace them with 1's, and 0's for repaid. 
## We will make sure that the total charged off (1's) is 5670+761=6431 and paid (0's) is 1984+34116 = 36100
#First make all the strings lowercase, then search for "charged" and paid

loans4$loan_status <- sapply(loans4$loan_status, tolower)
#search for paid, replace with zeros

temp_list <- loans4$loan_status
temp_list[grepl("paid", loans4$loan_status)] <- 0
##search for charged, replace with 1's
temp_list[grepl("charged", loans4$loan_status)] <- 1
head(temp_list)
loans4$loan_status <- temp_list
temp_list <- NULL

loans4$loan_status <- as.numeric(loans4$loan_status)


summary(loans4)
##Probabilities for different categorical factors
CrossTable(loans4$grade , loans4$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

CrossTable(loans4$emp_length, loans4$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

CrossTable(loans4$home_ownership, loans4$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
##grade plot
grade_plot <- CrossTable(loans4$grade , loans4$loan_status, prop.r = TRUE, 
                         prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
grade_rate <- grade_plot$prop.row[,2]
barplot(grade_rate, xlab= "Credit Grade", ylab = "Default Rate", main = "Credit Grade and Default Probability", col = "lightblue")

##employment length
el_plot <- CrossTable(loans4$emp_length, loans4$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
el_rate <- el_plot$prop.row[,2]
el_rate2 <- el_rate[order(el_rate)]
barplot(el_rate2, xlab= "Employment Length", ylab = "Default Rate", main = "Employment Length and Default Probability", col = "darkblue")

#homeownership
home_plot <- CrossTable(loans4$home_ownership, loans4$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
home_rate <- home_plot$prop.row[,2]
home_rate2 <- home_rate[order(home_rate)]
barplot(home_rate2, xlab= "Home Ownership", ylab = "Default Rate", main = "Home Ownership and Default Probability", col = "lightgreen")

##Since the rest are numeric relationships, a graph wont help us much, rather we will observe the relationship as we run regression

##before we continue to the plot, we will observe for any outliers. 
plot(loans4$loan_amnt, ylab = "Loan Amount")
plot(loans4$int_rate, ylab = "Interest Rate")
plot(loans4$annual_inc, ylab = "Annual Income")

#We can see two outliers that are dramatically above the rest of the distribution, one some 4 million and one some 6 million. We will remove those for the sake of this analysis

income_outliers <- which(loans4$annual_inc > 3000000)
loans4 <- loans4[-income_outliers,]
##Outliers removed, we will continue with the analysis

##To create a model, we must split the model into testing and training group - this is to prevent overfitting
nrow(loans4)
##shuffle the rows ramdomly, then use the first set of rows to train the model, and the last 10,000 rows to test our model. Never use the testing rows for training the model.
loans5 <- loans4[sample(nrow(loans4)),]
head(loans4)
head(loans5)

##seperate training model -- 42529 - 10000 = 32529 rows 
training <- loans5[1:32529,]
testing <- loans5[32530:42529,]
nrow(training)
nrow(testing)

#test the general logistic model on one variable, we chose the family as binomial as there can be only two results, independent in each trial, default or pay back
glm(formula = loan_status ~ grade, family = "binomial", data = training)
#generate the model for all variables
glm(loan_status ~ ., family = "binomial", data = training)
#save the model
test_model <- glm(loan_status ~ ., family = "binomial", data = training)
summary(test_model)

##notice the significance levels, we can see that grade heavily affects default rates, as does annual income and interest rate.
##notice how home ownership has absolutey no correlation with defaut rate, so we will remove it from the model. 
complete_model <- glm(loan_status ~ loan_amnt + int_rate + grade + annual_inc + emp_length, family = "binomial", data = training)

##test the first value of the testing set.
predict(complete_model, newdata = testing[1,], type = "response")
testing$loan_status[1]
#We find that the probablilty of defaulting to be .001807 or .1807%, which is very low. 
#In reality, the person did not default, which would make sense since they have a low chance of defaulting

##We will now evaluate for all
model_results <- predict(complete_model, newdata = testing, type = "response")
head(model_results)

###Evaluation of the model

##Giving everyone a loan, cutoff = 1
cutoff <- 1
#We are removing all that have a default probability over the cutoff. 
results_1 <- ifelse(model_results > cutoff,1,0)
sum(results_1)
outcomes_1 <- table(results_1, testing$loan_status)

barplot(outcomes_1, xlab = "Loan Outcome, Fully Paid or Defaulted", col = "pink", main = "Model Outcome With Cutoff = 0")
accuracy_1 <- sum(diag(outcomes_1)) / nrow(testing)
accuracy_1

##accuracy of removing defaulters... = rejected correctly/ total actually defaulted
#since zero rejected
accuracy_1d <- 0 / sum(testing$loan_status)
accuracy_1d

##Slightly more conservative, cutoff = .15
cutoff <- .15
#We are removing all that have a default probability over the cutoff. 
results_15 <- ifelse(model_results > cutoff,1,0)
sum(results_15)
outcomes_15 <- table(results_15, testing$loan_status)

barplot(outcomes_15, xlab = "Loan Outcome, Fully Paid or Defaulted", col = "yellow", main = "Model Outcome With Cutoff = 0.15")
accuracy_15 <- sum(diag(outcomes_15)) / nrow(testing)
accuracy_15

accuracy_15d <- outcome_15[2,2] / sum(testing$loan_status)
accuracy_15d 

##Aggressive, cutoff = .05
cutoff <- .05
#We are removing all that have a default probability over the cutoff. 
results_05 <- ifelse(model_results > cutoff,1,0)
sum(results_05)
outcomes_05 <- table(results_05, testing$loan_status)

barplot(outcomes_05, xlab = "Loan Outcome, Fully Paid or Defaulted", col = "cyan", main = "Model Outcome With Cutoff = 0.05")
accuracy_05 <- sum(diag(outcomes_05)) / nrow(testing)
accuracy_05


accuracy_05d <- outcomes_05[2,2] / sum(testing$loan_status)
accuracy_05d 

##reject all, cutoff = 0
cutoff <- .00
#We are removing all that have a default probability over the cutoff. 
results_0 <- ifelse(model_results > cutoff,1,0)
sum(results_0)
outcomes_0 <- table(results_0, testing$loan_status)

barplot(outcomes_0, xlab = "Loan Outcome, Fully Paid or Defaulted", col = "cyan", main = "Model Outcome With Cutoff = 0")
accuracy_0 <- outcomes_0[1,2] / nrow(testing)
accuracy_0

accuracy_0d <- outcomes_0[1,2] / sum(testing$loan_status)
accuracy_0d 
