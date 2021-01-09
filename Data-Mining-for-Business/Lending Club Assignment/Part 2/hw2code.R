rm(list = ls())
## Gradient Boosted Models ####
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
library(gbm)
library(xgboost)

lcdf <- read_csv('C:/Users/cjall/OneDrive/Desktop/Grad School/IDS 572/Assignments/Assignment 2/lcData5m.csv')

# First perform data cleaning / data prep
# Delete row with a loan status of current (not relevant to our purposes since it's still active)
lcdf <- subset(lcdf, loan_status != "Current")
#calculate the annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/3)*100
#Some loans are paid back early - find out the actual loan term in months
#  Since last_pymnt_d is a chr variable, we need to covert it to a date var
#lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")
lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)
#Then, considering this actual term, we can calculate the actual annual return 
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0, ((lcdf$total_pymnt - lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm),0)
#Note - character variables can cause a problem with some model packages, so better to convert all of these to factors
lcdf= lcdf %>% mutate_if(is.character, as.factor)
#data leakage 
lcdf <- lcdf %>% select(-c(total_pymnt,funded_amnt_inv,mths_since_recent_bc,
                           num_rev_tl_bal_gt_0,num_accts_ever_120_pd,num_actv_bc_tl,
                           num_actv_rev_tl,pymnt_plan,title, num_tl_30dpd,num_tl_90g_dpd_24m,
                           num_tl_op_past_12m,mths_since_recent_inq,bc_open_to_buy,emp_length,application_type,
                           total_pymnt_inv,tot_coll_amt, total_rev_hi_lim,num_tl_120dpd_2m,recoveries,tot_cur_bal,
                           total_rec_int,out_prncp,emp_title,issue_d, num_tl_120dpd_2m,dti,chargeoff_within_12_mths,
                           sub_grade,tot_hi_cred_lim,out_prncp_inv,mo_sin_old_il_acct, mo_sin_old_rev_tl_op,mo_sin_rcnt_rev_tl_op,delinq_2yrs,
                           delinq_amnt,mo_sin_rcnt_tl, tax_liens, hardship_flag, disbursement_method,total_rec_late_fee,
                           last_pymnt_amnt,last_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med,acc_now_delinq,debt_settlement_flag,revol_util,
                           revol_bal,earliest_cr_line, installment, collections_12_mths_ex_med,collection_recovery_fee, 
                           total_rec_prncp,total_il_high_credit_limit,total_bal_ex_mort, last_credit_pull_d, total_bc_limit, 
                           earliest_cr_line,inq_last_6mths,out_prncp_inv,out_prncp,policy_code,pct_tl_nvr_dlq, addr_state,zip_code,acc_open_past_24mths,term ))
#lcdf <- lcdf %>% select(-c(actualTerm,actualReturn)) - testing gbm code from class
summary(lcdf)
#Drop vars with all empty values
lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))})
dim(lcdf)# checking dimensions


#missing value proportions in each column
colMeans(is.na(lcdf)) 
#remove variables which have more than, for example, 60% missing values
nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm)
# Replace all NA values with the means of the columns
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
lcdf <- lcdf %>% replace_na(list(mths_since_last_delinq = mean(lcdf$mths_since_last_delinq, na.rm = TRUE), revol_util = mean(lcdf$revol_util, na.rm = TRUE), bc_open_to_buy = mean(lcdf$bc_open_to_buy, na.rm = TRUE), bc_util = mean(lcdf$bc_util, na.rm = TRUE), mo_sin_old_il_acct = mean(lcdf$mo_sin_old_rev_tl_op, na.rm = TRUE), mths_since_recent_bc = mean(lcdf$mths_since_recent_bc, na.rm = TRUE), mths_since_recent_inq = mean(lcdf$mths_since_recent_inq, na.rm = TRUE), num_tl_120dpd_2m = mean(lcdf$num_tl_120dpd_2m, na.rm = TRUE), percent_bc_gt_75 = mean(lcdf$percent_bc_gt_75, na.rm = TRUE)))



#for reproducible results, set a specific value for the random number seed
set.seed(123)
# TRAINING AND TEST SUBSETS
nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(0.75*nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]



# Building a gbm 
head(lcdf$loan_status)
head(unclass(lcdf$loan_status)-1) 


gbm_M1 <- gbm(formula=unclass(loan_status)-1 ~., 
              data=subset(lcdfTrn, select=-c(annRet, actualTerm, actualReturn)),
              distribution = "bernoulli",
              n.trees=1000, weights = ifelse(unclass(lcdfTrn$loan_status) > 0, 2, 1),shrinkage=0.1,
              interaction.depth = 4, bag.fraction=1,cv.folds=5, n.cores=NULL)
summary(gbm_M1)# shows all data
print(gbm_M1)#shows the model with the parameters 


bestIter<-gbm.perf(gbm_M1, method='cv')
bestIter 
scores_gbmM1<- predict(gbm_M1, newdata=lcdfTst, n.tree= bestIter) 
scores_gbmM1

head(scores_gbmM1)# these are the probability for the '1' in the dependent variable
#confusion matrix
predTrn=predict(gbm_M1, lcdfTrn,n.tree= bestIter, type='response')
table(predTrn>0.7,lcdfTrn$loan_status)


#Performance - ROC
pred_gbmM1=prediction(scores_gbmM1, lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid")) 
aucPerf_gbmM1 <-performance(pred_gbmM1, "tpr", "fpr") 
#AUC value
aucPerf_gbmM1=performance(pred_gbmM1, "auc")
aucPerf_gbmM1@y.values

#model 2
gbm_M2 <- gbm(formula=unclass(loan_status)-1 ~., 
              data=subset(lcdfTrn, select=-c(annRet, actualTerm, 
                                             actualReturn)), distribution = "gaussian",
              n.trees=1000, shrinkage=0.01, interaction.depth = 1, bag.fraction=0.5, cv.folds = 4, n.cores=16)

summary(gbm_M2)# shows all data
print(gbm_M2)#shows the model with the parameters 


bestIter<-gbm.perf(gbm_M1, method='cv')
scores_gbmM2<- predict(gbm_M2, newdata=lcdfTst, n.tree= bestIter, type="response") 
head(scores_gbmM2)# these are the probability for the '1' in the dependent variable
#matrix


#Performance - ROC
pred_gbmM2=prediction(scores_gbmM2, lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid")) 
aucPerf_gbmM2 <-performance(pred_gbmM2, "tpr", "fpr") 
#AUC value
aucPerf_gbmM2=performance(pred_gbmM2, "auc")
aucPerf_gbmM2@y.values


summary(gbm_M2)
print(gbm_M2)
plot(aucPerf_gbmM1) 
abline(a=0, b= 1)


# Building an xgb model to eventually predict the returns
#Setting the parameters 
xgbParams <- list(
  booster = "gbtree", # or can be "gblinear"
  objective = "reg:squarederror",
  eta=0.01, #learning rate
  #subsample=1, #subsample fraction for training data
  #for trees
  max_depth=5,
  min_child_weight=1,
  colsample_bytree=1,
  #for linear models
  lambda = 0, #L2 regularization for weights
  alpha = 0, #L1 regularization fo weights
  lambda_bias=0 #L2 reguralizer for bias term
)

#Needs all data to be numeric - another way to sconvert categorical (i.e. factor) variables using one-hot encoding
ohlcdfTrn<- model.matrix(~.+0, data=lcdfTrn %>% select(-c('loan_status'))) #we need to exclude loan_staus in modeling actualReturn
ohlcdfTst<- model.matrix(~.+0, data=lcdfTst %>% select(-c('loan_status')))
xgb_Mrcv <- xgb.cv( data=subset(ohlcdfTrn,select=-c(annRet, actualTerm)), label=ohlcdfTrn[,"actualReturn"],
                    nrounds = 1000, max.depth=4, nfold = 5, eta=0.05, objective=" reg:squarederror " )

#Alternately, obtain the Dmatrix object for the training data first and use this multiple times
dtrain <- xgb.DMatrix(subset(ohlcdfTrn,select=-c(annRet, actualTerm, actualReturn)), label=ohlcdfTrn[,"actualReturn"])
xgb_Mrcv <- xgb.cv( data = dtrain, nrounds = 1000, max.depth=4, nfold = 5, eta=0.05, objective=" reg:squarederror " )
bestIter<-which.min(xgb_Mrcv$evaluation_log$test_rmse_mean)
xgb_Mr<- xgb.train( data = dtrain, nrounds = bestIter, max.depth=4, eta=0.05,
                    objective="reg:squarederror")
xgb_Mr_importance <- xgb.importance(model = xgb_Mr)
xgb_Mr_importance %>% view()

# Creating an xgb Model to predict the loan_status probability

#Needs all data to be numeric -- so we convert categorical (i.e. factor) variables #using one-hot encoding - multiple ways to do this
# use the dummyVars function in the 'caret' package to convert factor variables to # dummy-variables
fdum<-dummyVars(~.,data=lcdf %>% select(-loan_status))
dxlcdf <- predict(fdum, lcdf)
# for loan_status, check levels and convert to dummy vars and drop the second dummy var
#lcdf$loan_status <- factor(lcdf$loan_status, levels=c("Fully Paid", "Charged Off"))
#levels(lcdf$loan_status)
fplcdf <- class2ind(lcdf$loan_status, drop2nd = TRUE)
#Training, test subsets
dxlcdfTrn <- dxlcdf[trnIndex,]
fplcdfTrn <- fplcdf[trnIndex]
dxlcdfTst <- dxlcdf[-trnIndex,]
fplcdfTst <- fplcdf[-trnIndex]
dxTrn <- xgb.DMatrix( subset(dxlcdfTrn, select=-c(annRet, actualTerm, actualReturn)), label=fplcdfTrn)
dxTst <- xgb.DMatrix( subset(dxlcdfTst, select=-c(annRet, actualTerm, actualReturn)), label=fplcdfTst)

xgbWatchlist <- list(train = dxTrn, eval = dxTst)
#we can watch the progress of learning thru performance on these datasets

#can specify which evaluation metrics we want to watch
xgb_lsM1 <- xgb.train( xgbParam, dxTrn, nrounds = 1000,
                       xgbWatchlist, early_stopping_rounds = 10)

#Stop if performance does not improve after xx rounds
xgb_lsM1$best_iteration 

#xgb model predictions
xpredTrg <- predict(xgb_lsM1, dxTrn)
head(xpredTrg)
xpredTrgTst <- predict(xgb_lsM1, dxTst)
head(xpredTrgTst)

#confusion matrix
table(pred=as.numeric(xpredTrg>0.5), act=fplcdfTrn)
#ROC, AUC performance
xpredTst<-predict(xgb_lsM1, dxTst)
pred_xgb_lsM1=prediction(xpredTst, lcdfTst$loan_status, label.ordering =
                           c("Charged Off", "Fully Paid"))
aucPerf_xgb_lsM1=performance(pred_xgb_lsM1, "tpr", "fpr")
plot(aucPerf_xgb_lsM1)
abline(a=0, b= 1)

# Models for actualReturn
xgb_Mr2<- xgboost(data=dtrain, xgbParams, nrounds=962, eta=0.01, subsample=0.7)
xgbr_Mrcv <- xgb.cv( data = dtrain, nrounds = 1000, max.depth=4, nfold = 5,
                     eta=0.05, objective="reg:linear" )

bestIter<-which.min(xgbr_Mrcv$evaluation_log$test_rmse_mean)
xgb.importance(model = xgb_Mr2) %>% view()


#Boosting linear models
xgb_Lin_Rcv <- xgb.cv( data = dtrain, nrounds = 1000, nfold = 5, eta=0.3, subsample=1,
early_stopping_rounds=10, booster="gblinear", alpha=0.0001 )

xgb_Lin_R1 <- xgboost( data = dtrain, nrounds = xgb_Lin_Rcv$best_iteration,
eta=0.3, subsample=1, booster="gblinear", alpha=0.0001 )
xgb.importance(model=xgb_Lin_R1) %>% view()

xgbParamGrid <- expand.grid(
  max_depth = c(2, 5),
  eta = c(0.001, 0.01, 0.1) )

xgbParams <- list (
  booster = "gbtree",
  objective = "reg:linear",
  eta=0.01, #learning rate
  max_depth=5,
  min_child_weight=1,
  colsample_bytree=0.6,
  lambda = 0, #L2 regularization for weights
  alpha = 0, #L1 regularization fo weights
  lambda_bias=0 #L2 reguralizer for bias term
)

for(i in 1:nrow(xgbParamGrid)) {
  xgb_tune<- xgboost(data=dtrain,objective = "reg:squarederror", nrounds=50, eta=xgbParamGrid$eta[i],
                     max_depth=xgbParamGrid$max_depth[i], early_stopping_rounds = 10)
  xgbParamGrid$bestTree[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
  xgbParamGrid$bestPerf[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$train_rmse
}

xgb_Mr<- xgboost( data = dtrain, nrounds = bestIter, max.depth=4, eta=0.05, objective=" reg:squarederror ")

predXgbRet_Trn <- lcdfTrn %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate( predXgbRet=predict(xgb_Mr, subset(ohlcdfTrn,select=-c(annRet, actualTerm, actualReturn))) )
predXgbRet_Trn <- predXgbRet_Trn %>% mutate(tile=ntile(-predXgbRet, 10))
predXgbRet_Trn %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predXgbRet), numDefaults=sum(loan_status=="Charged Off"),
                                                avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), avgTer=mean(actualTerm), totA=sum(grade=="A"),
                                                totB=sum(grade=="B" ), totC=sum(grade=="C"), totD=sum(grade=="D"), totE=sum(grade=="E"), totF=sum(grade=="F") )

# Cross Validation on the Testing Set
predXgbRet_Tst <- lcdfTst %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate( predXgbRet=predict(xgb_Mr2, subset(ohlcdfTst,select=-c(annRet, actualTerm, actualReturn))) )
predXgbRet_Tst <- predXgbRet_Tst %>% mutate(tile=ntile(-predXgbRet, 10))
predXgbRet_Tst %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predXgbRet),
                                                numDefaults=sum(loan_status=="Charged Off"), avgActRet=mean(actualReturn), minRet=min(actualReturn),
                                                maxRet=max(actualReturn), avgTer=mean(actualTerm), totA=sum(grade=="A"), totB=sum(grade=="B" ),
                                                totC=sum(grade=="C"), totD=sum(grade=="D"), totE=sum(grade=="E"), totF=sum(grade=="F") ) 

# Another approach is to calculate expectde return by this equation: 
# Calculate expReturn = (predicted Actual Return)*(prob of Fully Paid) and, sort on this

# Step 1 is considering the top deciles using the probability for out previous models 


pRetSc <- predXgbRet_Tst %>% mutate(poScore=scores_gbmM2)
pRet_d <- pRetSc %>% filter(tile<=d)
pRet_d<- pRet_d %>% mutate(tile2=ntile(-poScore, 20))
pRet_d %>% group_by(tile2) %>% summarise(count=n(), avgPredRet=mean(predXgbRet),
                                         numDefaults=sum(loan_status=="Charged Off"), avgActRet=mean(actualReturn), minRet=min(actualReturn),
                                         maxRet=max(actualReturn), avgTer=mean(actualTerm), totA=sum(grade=="A"), totB=sum(grade=="B" ),
                                         totC=sum(grade=="C"), totD=sum(grade=="D"), totE=sum(grade=="E"), totF=sum(grade=="F") )



# CLEAN UP #################################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

str(lcdf )
