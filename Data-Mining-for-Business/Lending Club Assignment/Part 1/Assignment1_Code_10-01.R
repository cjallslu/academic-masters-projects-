#### IDS 572 - Group Assignment 1 ####
# CJ All, Eric Reitz, Vanessa Rodriguez
# Due Date: 10/2/2020
#--------------------------------------

rm(list = ls())

# Load Various packages and initial dataset ####
library(tidyverse)
library(lubridate)
library(rpart)
library(recipes)
library(caret)
library(ROCR)
library(dplyr)
library(rpart.plot)
library(C50) 
library(e1071)

lcdf <- read_csv('lcData5m.csv')

# Missing Data Adjustments ####

# Delete row with a loan status of current (not relevant to our purposes since it's still active)
lcdf <- subset(lcdf, loan_status != "Current")

#  Remove variables which have more than 60% missing values
nm <- names(lcdf)[colMeans(is.na(lcdf)) > 0.6]
lcdf <- lcdf %>% select(-nm)

# Replace missing values for the variables with less than 60% missing values with the means of the existing values
lcdf <- lcdf %>% replace_na(list(mths_since_last_delinq = mean(lcdf$mths_since_last_delinq, na.rm = TRUE), revol_util = mean(lcdf$revol_util, na.rm = TRUE), bc_open_to_buy = mean(lcdf$bc_open_to_buy, na.rm = TRUE), bc_util = mean(lcdf$bc_util, na.rm = TRUE), mo_sin_old_il_acct = mean(lcdf$mo_sin_old_rev_tl_op, na.rm = TRUE), mths_since_recent_bc = mean(lcdf$mths_since_recent_bc, na.rm = TRUE), mths_since_recent_inq = mean(lcdf$mths_since_recent_inq, na.rm = TRUE), num_tl_120dpd_2m = mean(lcdf$num_tl_120dpd_2m, na.rm = TRUE), percent_bc_gt_75 = mean(lcdf$percent_bc_gt_75, na.rm = TRUE)))

# Purpose Redistribution - when reviewing the data there are some entries that are extremely small, bundling them together all in the blanket other category will give these small entries more value in data comparisons
lcdf$purpose <- fct_recode(lcdf$purpose, other ="wedding", other ="educational", other ="renewable_energy")

# Data Exploration ####

# What are the proportions of defaults ("charged off" vs "fully paid" loans) in the data?
total_proportion <- lcdf %>% group_by(loan_status) %>% tally(name = "Count")
total_proportion$prop <- total_proportion$Count / sum(total_proportion$Count) * 100

total_proportion


# How does the rate vary with loan grade? - Make different counts for Each Grace
grade_proportion <- lcdf %>% group_by(loan_status, grade) %>% filter(grade =="A") %>% tally(name = "Count") # swap out for each grade
grade_proportion$prop <- grade_proportion$Count / sum(grade_proportion$Count) * 100

grade_proportion


# How does it vary with sub-grade? - Use same Grade filtering breakdown
subg_proportion <- lcdf %>% group_by(loan_status, grade, sub_grade) %>% filter(sub_grade =="F5") %>% tally(name = "Count")
subg_proportion$prop <- subg_proportion$Count / sum(subg_proportion$Count) * 100

subg_proportion %>% print(n= Inf)


# How many loans are in each grade?
lcdf %>% group_by(grade) %>% tally()

# Do loan amounts vary by grade?
lcdf %>% group_by(grade) %>% summarise(sum(loan_amnt))
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram(aes(fill=grade)) +
  ggtitle("# of Loans Vs. Loan Amounts - By Grade") +
  labs(y = "# of Loans", x = "Loan Amount")

# Does the Interest Rate Vary by grade? Subgrade? 
lcdf %>% group_by(grade) %>% summarise(mean(int_rate))
ggplot(lcdf, aes( x = int_rate)) + geom_histogram(aes(fill=grade)) +
  ggtitle("# of Loans Vs. Interest Rate - By Grade") +
  labs(y = "# of Loans", x = "Interest Rate")



# What are people borrowing money for? How many loans, average amounts, etc. Repeat within Grade as well -- Make some sort of plot for this
lcdf %>% group_by(purpose) %>% tally() %>% print(n = Inf)
lcdf %>% group_by(purpose, grade) %>% tally() %>% print(n = Inf)
lcdf %>% group_by(purpose) %>% summarise(nLoans=n(), avgLoanAMt=mean(loan_amnt), defaults=sum(loan_status=="Charged Off")) %>% print(n= Inf)
lcdf %>% group_by(purpose, grade) %>% summarise(nLoans=n(), avgLoanAMt=mean(loan_amnt), defaults=sum(loan_status=="Charged Off")) %>% print(n= Inf)

ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram(aes(fill = purpose))+
  ggtitle("# of Loans Vs. Loan Amount - By Purpose") +
  labs(y = "# of Loans", x = "Loan Amount")


#lcdf %>% group_by(purpose, grade) %>% summarise(nLoans=n(), avgLoanAMt=mean(loan_amnt), defaults=sum(loan_status=="Charged Off"), defaultsprop = (sum(loan_status=="Charged Off") / nLoans=n()) ) %>% print(n= Inf)
lcdf %>% group_by(loan_status, grade) %>% summarise(avgTotalBc = mean(num_bc_tl) ,avgSatisCardAccts = mean(propSatisBankcardAccts))lcdf %>% group_by(grade) %>% summarise(avgTotalBc = mean(num_bc_tl) ,avgSatisCardAccts = mean(propSatisBankcardAccts))



# Calculate the annual return as a new column, compare to the average interest rates 
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/3)*100

# How do returns vary by grade and sub-grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))
lcdf %>% group_by(grade, sub_grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet)) %>% print(n= Inf)
lcdf %>% group_by(grade, sub_grade) %>% filter(grade =="G" | grade =="F") %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet)) %>% print(n= Inf)

# Generate some new Derived Attributes that could be useful to predicting default - Add as columns
lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0) #


# Clear environment
rm(list = ls()) 

# -------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rpart)
library(caret)
library(ROCR)
library(dplyr)
library(rpart.plot)
library(C50)
library(e1071)
library(ranger)
library(ggplot)

setwd('C:/Users/cjall/OneDrive/Desktop/Grad School/IDS 572/Assignments/Assignment 1')
lcdf <- read_csv('lcData5m.csv')
# Data Cleaning ####

# Delete row with a loan status of current (not relevant to our purposes since it's still active)
lcdf <- subset(lcdf, loan_status != "Current")


#calculate the annualized percentage return
#lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/3)*100

#Some loans are paid back early - find out the actual loan term in months
#  Since last_pymnt_d is a chr variable, we need to covert it to a date var
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)

#Then, considering this actual term, we can calculate the actual annual return 
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0, ((lcdf$total_pymnt - lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm),0)

#Note - character variables can cause a problem with some model packages, so better to convert all of these to factors
lcdf= lcdf %>% mutate_if(is.character, as.factor)

#Derived attribute: proportion of satisfactory bankcard accounts 
lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0)

#For cost-based performance, we want to see the average interest rate, and the average of proportion of loan amount paid back, grouped by loan_status
lcdf%>% group_by(loan_status) %>% summarise(  intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt)  )
# Notice that the totRet on Charged Off loans as -0.366, so, for every dollar invested, there is a loss of .366 cents.   For Fully Paid loans, the totRet seems less than what may be  expected from intRate -- how do you explain this?


#you may like to look at some of these variables
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans)

#data leakage 
#data leakage 
lcdf <- lcdf %>% select(-c(mths_since_recent_bc,num_rev_tl_bal_gt_0,num_accts_ever_120_pd,num_actv_bc_tl,num_actv_rev_tl,pymnt_plan,title,total_pymnt, num_tl_30dpd,num_tl_90g_dpd_24m, num_tl_op_past_12m,mths_since_recent_inq,bc_open_to_buy,emp_length,annRet,application_type,total_pymnt_inv,tot_coll_amt, total_rev_hi_lim,num_tl_120dpd_2m,recoveries,tot_cur_bal,total_rec_int,out_prncp,emp_title,issue_d, num_tl_120dpd_2m,dti,chargeoff_within_12_mths,sub_grade,tot_hi_cred_lim,out_prncp_inv,mo_sin_old_il_acct, mo_sin_old_rev_tl_op,mo_sin_rcnt_rev_tl_op,delinq_2yrs,home_ownership,delinq_amnt,mo_sin_rcnt_tl, tax_liens, hardship_flag, disbursement_method,total_rec_late_fee,last_pymnt_amnt,last_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med,acc_now_delinq,debt_settlement_flag,revol_util,revol_bal,earliest_cr_line, installment, collections_12_mths_ex_med,collection_recovery_fee, total_rec_prncp,total_il_high_credit_limit,total_bal_ex_mort, last_credit_pull_d, total_bc_limit, earliest_cr_line,inq_last_6mths,out_prncp_inv,out_prncp,policy_code,pct_tl_nvr_dlq, addr_state,zip_code,acc_open_past_24mths,mths_since_last_delinq,term ))
lcdf <- lcdf %>% select(-c(actualTerm,actualReturn))
summary(lcdf)

summary(lcdf)


#Drop vars with all empty values
lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))})
dim(lcdf)
#missing value proportions in each column
colMeans(is.na(lcdf))

#remove variables which have more than, for example, 60% missing values
nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm)


colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

summary(lcdf$percent_bc_gt_75)
lcdf<- lcdf %>% replace_na(list(percent_bc_gt_75=47.73))
summary(lcdf$percent_bc_gt_75)

lcdf<- lcdf %>% replace_na(list(mths_since_recent_bc=24.02))
summary(lcdf$mths_since_recent_bc)

summary(lcdf$bc_util)
lcdf<- lcdf %>% replace_na(list( bc_util=62.31))
summary(lcdf$bc_util )

#summary of data in these columns
nm<- names(lcdf)[colSums(is.na(lcdf))>0]
summary(lcdf[, nm])
summary(lcdf)

str(lcdf)

# replace loan status
lcdf$grade <- str_replace_all(lcdf$grade, 'A', '1')
lcdf$grade <- str_replace_all(lcdf$grade, 'B', '2')
lcdf$grade <- str_replace_all(lcdf$grade, 'C', '3')
lcdf$grade <- str_replace_all(lcdf$grade, 'D', '4')
lcdf$grade <- str_replace_all(lcdf$grade, 'E', '5')
lcdf$grade <- str_replace_all(lcdf$grade, 'F', '6')
lcdf$grade <- str_replace_all(lcdf$grade, 'G', '7')
lcdf$grade <- as.numeric(lcdf$grade)
summary(lcdf)


lcdf$loan_status <- factor(lcdf$loan_status, levels=c("Fully Paid", "Charged Off"))

str(lcdf)


#for reproducible results, set a specific value for the random number seed
set.seed(123)

nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]


# tree info
#model
rpmodel1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(minsplit = 30))
rpmodel1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(cp=0.0001, minsplit = 50))

rpmodel1<- prune.rpart(rpmodel1, cp=0.0003)
plotcp(rpmodel1)
#plotcp(rpmodel1)
summary(rpmodel1)
# with different classification
CTHRESH=0.3
predProbTrn=predict(rpmodel1,lcdfTrn, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table(predTrnCT , true=lcdfTrn$loan_status)
#print(rpmodel1)
#plot(rpmodel1, uniform=TRUE,  main="Decision Tree ", minsplit=30)
#text(rpmodel1, use.n=TRUE, all=TRUE, cex=.7)
#rpart.plot::prp(rpmodel1, type=2, extra=1)


predTrn=predict(rpmodel1, lcdfTrn, type='class')
confusionMatrix(predTrn,lcdfTrn$loan_status, positive = "Charged Off")

predTst=predict(rpmodel1, lcdfTst, type='class')
confusionMatrix(predTst,lcdfTst$loan_status, positive = "Charged Off")


score=predict(rpmodel1,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   
#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values


#With a different classsification threshold
#CTHRESH=0.3
#predProbTrn=predict(rpmodel1,lcdfTrn, type='prob')
#predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
#table(predTrnCT , true=lcdfTrn$loan_status)
# Or, to set the predTrnCT values as factors, and then get the confusion matrix
#table(predictions=factor(predTrnCT, levels=c("Fully Paid", "Charged Off")), actuals=lcdfTrn$loan_status)


#Or you can use the confusionMatrix fuction from the caret package



#summary(lcdf)
# model 2
rpmodel2 <- rpart(loan_status ~., data=lcdfTrn,method="class", parms = list(split = "gini"), control = rpart.control(cp=0.0001, minsplit = 20))
#print(rpmodel2)
plot(rpmodel2, uniform=TRUE,  main="Decision Tree ", minsplit=30)


predTrn=predict(rpmodel2, lcdfTrn, type='class')
confusionMatrix(predTrn,lcdfTrn$loan_status, positive = "Charged Off")

predTst=predict(rpmodel2, lcdfTst, type='class')
confusionMatrix(predTst,lcdfTst$loan_status, positive = "Charged Off")

score=predict(rpmodel2,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   
#ROC curve

aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)
# shows value 
#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values

#Lift curve
liftPerf <-performance(pred, "lift", "rpp")
plot(liftPerf)


# decision tree 3
rpmodel3<- C5.0(loan_status ~.,data=lcdfTrn, method="class", control=C5.0Control(CF=0.3,earlyStopping = FALSE))

predTrn=predict(rpmodel3, lcdfTrn, type='class')
confusionMatrix(predTrn,lcdfTrn$loan_status, positive = "Charged Off")
predTst=predict(rpmodel3, lcdfTst, type='class')
confusionMatrix(predTst,lcdfTst$loan_status, positive = "Charged Off")


score=predict(rpmodel3,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   
#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values

# decision tree 4

rpmodel4<- rpart(loan_status ~., data=lcdfTrn,method="class",parms = list(split = "information"),control = rpart.control(cp=0.0001, minsplit = 30))

predTrn=predict(rpmodel4, lcdfTrn, type='class')
confusionMatrix(predTrn,lcdfTrn$loan_status, positive = "Charged Off")
predTst=predict(rpmodel4, lcdfTst, type='class')
confusionMatrix(predTst,lcdfTst$loan_status, positive = "Charged Off")

# graph 
score=predict(rpmodel4,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   
#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values

str(lcdfTrn)
summary(lcdfTrn)
# Random Forests
list_na <- colnames(lcdfTrn)[ apply(lcdfTrn, 2, anyNA) ]
list_na

colnames(lcdfTrn)
colnames(lcdfTst)

nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]
View(lcdfTrn)
View(lcdfTst)
colnames(lcdf)

set.seed(123)
colMeans(is.na(lcdfTrn))
colMeans(is.na(lcdfTst))

?colMeans

library(ranger)
rfModel1 <- ranger(loan_status ~., data=lcdfTrn,
                   num.trees =200, importance='permutation')
importance(rfModel1)
rfModel1

rfModel2 <- ranger(loan_status ~., data=subset(lcdfTrn, select=-c(loan_amnt, int_rate, annual_inc)),
                   num.trees =200, importance='permutation')
importance(rfModel2)
rfModel2

rfModel3 <- ranger(loan_status ~., data=lcdfTst,
                   num.trees =200, importance='permutation')
importance(rfModel3)
rfModel3

rfModel4 <- ranger(loan_status ~., data=subset(lcdfTst, select=-c(loan_amnt, int_rate, annual_inc)),
                   num.trees =200, importance='permutation')
importance(rfModel4)
rfModel4

?confusionMatrix
# Confusion Matrix for RF Model 1
predTrnrf=predict(rfModel1, lcdfTrn, type = 'response')
predTrnrf$predictions
View(predTrnrf)
table(pred=predTrnrf$predictions, true=lcdfTrn$loan_status)
confusionMatrix(predTrnrf,lcdfTrn$loan_status, positive = "Charged Off")

predTstrf=predict(rfModel1, lcdfTst, type='response')
table(pred=predTstrf$predictions, true=lcdfTst$loan_status)
confusionMatrix(predTstrf,lcdfTst$loan_status, positive = "Charged Off")

# Confusion Matrix for RF Model 2
predTrnrf=predict(rfModel2, lcdfTrn, type='response')
table(pred=predTrnrf$predictions, true=lcdfTrn$loan_status)
confusionMatrix(predTrnrf,lcdfTrn$loan_status, positive = "Charged Off")

predTstrf=predict(rfModel2, lcdfTst, type='response')
table(pred=predTstrf$predictions, true=lcdfTst$loan_status)
confusionMatrix(predTstrf,lcdfTst$loan_status, positive = "Charged Off")

# Confusion Matrix for RF Model 3
predTrnrf=predict(rfModel3, lcdfTrn, type='response')
table(pred=predTrnrf$predictions, true=lcdfTrn$loan_status)
confusionMatrix(predTrnrf,lcdfTrn$loan_status, positive = "Charged Off")

predTstrf=predict(rfModel3, lcdfTst, type='response')
table(pred=predTstrf$predictions, true=lcdfTst$loan_status)
confusionMatrix(predTstrf,lcdfTst$loan_status, positive = "Charged Off")

# Confusion Matrix for RF Model 4
predTrnrf=predict(rfModel4, lcdfTrn, type='response')
table(pred=predTrnrf$predictions, true=lcdfTrn$loan_status)
confusionMatrix(predTrnrf,lcdfTrn$loan_status, positive = "Charged Off")

predTstrf=predict(rfModel4, lcdfTst, type='response')
table(pred=predTstrf$predictions, true=lcdfTst$loan_status)
confusionMatrix(predTstrf,lcdfTst$loan_status, positive = "Charged Off")


#6 

PROFITVAL <- 15 # profit (on $100) from accurately identifying Fully_paid loans
COSTVAL <- -25  # loss (on $100) from incorrectly predicting a Charged_Off loan as Full_paid
scoreTst <- predict(rpmodel2,lcdfTst, type="prob")[,"Fully Paid"]  

#Note- we want to identify those loans wth high prob for being FullyPaid
prPerf <- data.frame(scoreTst)
prPerf <- cbind(prPerf, status=lcdfTst$loan_status)
prPerf <- prPerf[order(-scoreTst) ,]  #sort in desc order of  prob(fully_paid)
prPerf$profit <- ifelse(prPerf$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerf$cumProfit <- cumsum(prPerf$profit)
# (Prob of Fully Paid * Prob of Cost)

head(prPerf) 

#to compare against the default approach of investing in CD with 2% int (i.e. $6 profit out of $100 in 3 years)
prPerf$cdRet <- 6
prPerf$cumCDRet <- cumsum(prPerf$cdRet)
plot(prPerf$cumProfit)
lines(prPerf$cumCDRet, col='green')


#the predict function for ranger models returns an object where the 'predictions' slot has the predicted probs for examples
pred_rfModel1=predict(rfModel1,lcdfTst)$predictions

#So we can then obtain the ROC performance as
perfROC_rfRangerTst=performance(prediction(predict(rfModel1,lcdfTst)$predictions[,2], lcdfTst$loan_status), "Fully Paid", "Charged Off")
plot(perfROC_rfRangerTst)

time_stats$infected_t[i] <- sum(infected) Do that last line false


scoreTst <- predict(rfModel1,lcdfTst)
#Note- we want to identify those loans wth high prob for being FullyPaid
prPerf <- data.frame(scoreTst)
prPerf <- cbind(prPerf, status=lcdfTst$loan_status)
prPerf <- prPerf[order(-scoreTst) ,]  #sort in desc order of  prob(fully_paid)
prPerf$profit <- ifelse(prPerf$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerf$cumProfit <- cumsum(prPerf$profit)
# (Prob of Fully Paid * Prob of Cost)

head(prPerf) 

#to compare against the default approach of investing in CD with 2% int (i.e. $6 profit out of $100 in 3 years)
prPerf$cdRet <- 6
prPerf$cumCDRet <- cumsum(prPerf$cdRet)
plot(prPerf$cumProfit)
lines(prPerf$cumCDRet, col='green')
