#Assignment 2 #2 Creating our training and testing sets
library(MASS)
library(ISLR)
View(Weekly)
trn = subset(Weekly, Year <= 2008)
tst = subset(Weekly, Year > 2008)

#creating our logistic regression models and confusion matrices
View(Auto)
glmtrn <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = trn)
summary(glmtrn)
predDirection <- predict(glmtrn, trn, type="response")
table(predDirection>0.5,trn$Direction)

predDirection2 <- predict(glmtrn, tst, type="response")
table(predDirection2>0.5,tst$Direction)

#Glms for each Lag variable in our dataset
glm1 <- glm(Direction ~ Lag1, family = binomial, data = trn)
summary(glm1)
pred1 <- predict(glm1, trn, type="response")
table(pred1>0.5,trn$Direction)

pred1 <- predict(glm1, tst, type="response")
table(pred1>0.5,tst$Direction)


glm2 <- glm(Direction ~ Lag2, family = binomial, data = trn)
summary(glm2)
pred2 <- predict(glm2, trn, type="response")
table(pred2>0.5,trn$Direction)

pred2 <- predict(glm2, tst, type="response")
table(pred2>0.5,tst$Direction)

glm3 <- glm(Direction ~ Lag3, family = binomial, data = trn)
summary(glm3)
pred3 <- predict(glm3, trn, type="response")
table(pred3>0.5,trn$Direction)

pred3 <- predict(glm3, tst, type="response")
table(pred3>0.5,tst$Direction)

glm4 <- glm(Direction ~ Lag4, family = binomial, data = trn)
summary(glm4)
pred4 <- predict(glm4, trn, type="response")
table(pred4>0.5,trn$Direction)

pred4 <- predict(glm4, tst, type="response")
table(pred4>0.5,tst$Direction)

glm5 <- glm(Direction ~ Lag5, family = binomial, data = trn)
summary(glm5)
pred5 <- predict(glm5, trn, type="response")
table(pred5>0.5,trn$Direction)

pred5 <- predict(glm5, tst, type="response")
table(pred5>0.5,tst$Direction)

library(ROCR)
#Building an ROC curve and calculating the AUC for each model
# List of predictions
preds_list <- list(predDirection2, pred1, pred2, pred3, pred4, pred5)

# Plot the ROC curves 

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(tst$Direction), m)

# Plot the ROC curves for the Testing data
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Testing Set ROC Curves")
abline(a=0, b=1)
#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values
legend(x = "bottomright", 
       legend = c('All vars 0.5177278', 'Lag1 0.4864659', 'Lag2 0.546321', 'Lag3 0.5242089', 'Lag4 0.5257339', 'Lag5 0.4422417'),
       fill = 1:m)


# Plot the Precision curves for the data
pred <- prediction(preds_list, actuals_list)
perf <- performance(pred,"prec","rec")
## The plot obtained with the standard ROCR functions
## Not run: 
plot(perf, col = as.list(1:m), main = "Testing Set Precision Recall Curves")

## End(Not run)

## Now our Precision/Recall curve avoiding the saw-tooth effect
## Not run: 

PRAUC(y_pred = predDirection2, y_true = tst$Direction)
PRAUC(y_pred = pred1, y_true = tst$Direction)
PRAUC(y_pred = pred2, y_true = tst$Direction)
PRAUC(y_pred = pred3, y_true = tst$Direction)
PRAUC(y_pred = pred4, y_true = tst$Direction)
PRAUC(y_pred = pred5, y_true = tst$Direction)

legend(x = "bottomright", 
  legend = c('All vars 0.5910643', 'Lag1 0.5548618', 'Lag2 0.6255488', 'Lag3 0.6260072', 'Lag4 0.6186099', 'Lag5 0.5816768'),
  fill = 1:m)
