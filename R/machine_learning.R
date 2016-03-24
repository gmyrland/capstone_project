## Machine Learning

library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(VGAM)
library(randomForest)
library(MASS)
library(leaps)
library(glmnet)
set.seed(1234)

f <- reformulate(names(df)[!names(df) %in% c('CaseId', 'fatal')], 'fatal')
#f <- reformulate(c('age', 'height', 'weight', 'factor(sex)', 'factor(role)'), 'fatal')

##########
# BASIC LM
fit <- lm(f, df[sample(nrow(df),10000), ])
summary(fit)$adj.r.squared
coef(fit)
sqrt(sum(summary(fit)$residuals^2)/nrow(df)) # RMSE
qqnorm(summary(fit)$residuals)

###############################
## Basic LM with test/train
n <- nrow(df)
shuffled <- dfcc[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
fit <- lm(fatal ~ age + height + weight + factor(sex) + factor(role), data=train)
pred <- predict(fit, test, type="response")
pred <- as.integer(pred > 0.08)
conf <- table(test$fatal, pred) #confusion matrix
sum(diag(conf)) / sum(conf) #accuracy
summary(fit)
conf[2,2] / (conf[1,2] + conf[2,1]) #precision

###############################
## Binomial logistic regression (not working)
n <- nrow(dfcc)
shuffled <- dfcc[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
fit <- glm(fatal ~ age + height + weight + factor(sex), family = binomial(link="logit"), data=train)
pred <- predict(fit, test, type="link")
anova(fit, test="Chisq")
hist(pred$fit)
conf <- table(test$fatal, pred) #confusion matrix
summary(fit)

####################
## Logistic Regression
n <- nrow(dfcc)
shuffled <- dfcc[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
fit <- vglm(fatal ~ age + height + weight + factor(sex), family=multinomial, data=train)
summary(fit)
probabilities <- predict(fit, test, type="response")
predictions <- apply(probabilities, 1, which.max)
#predictions[which(predictions=="1")] <- levels(iris$Species)[1]
#predictions[which(predictions=="2")] <- levels(iris$Species)[2]
# summarize accuracy
table(predictions, dfcc$fatal)

###########
# NO K FOLD
n <- nrow(dfcc)
shuffled <- dfcc[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
tree <- rpart(fatal ~ age + height + weight + factor(sex) + factor(role), train, method = "class", control = rpart.control(cp=0.001))
pred <- predict(tree, test, type="class")
conf <- table(test$fatal, pred) #confusion matrix
sum(diag(conf)) / sum(conf) #accuracy

#ROC curve
probs <- predict(tree, test, type="prob")[,2]
pred <- prediction(probs, test$fatal)
plot(performance(pred, "tpr", "fpr")) # ROC Curve
performance(pred, "auc", "fpr")@y.values[[1]] #AUC


################
# K FOLD WITH LM
k <- 5
folds <- createFolds(df$fatal, k = k, list = TRUE, returnTrain = FALSE)
rmse <- NULL
adj.r.squared <- NULL
coefs <- NULL
pred25 <- NULL
for (i in 1:k) {
    train <- df[-folds[[i]], ]
    test  <- df[folds[[i]], ]
    fit <- lm(fatal ~ height + weight, data=train)
    prediction <- predict(fit, test)
    rmse[i] <- RMSE(prediction, test$fatal, na.rm=TRUE)
    adj.r.squared[i] <- summary(fit)$adj.r.squared
    coefs[i] <- list(coef(fit))
    pred25[i] <- mean((1 - (test$price-abs(prediction-test$price)) / test$fatal) < 0.25, na.rm=TRUE)
}
data_frame(
    k=1:k, RMSE=rmse, `adj R^2`=adj.r.squared,
    Intercept=sapply(coefs, `[[`, 1), slope=sapply(coefs, `[[`, 2), `Pred(25)`=pred25
) %>%
    round(2) %>% rbind({colMeans(.) %>% {.["k"] <- "Avg"; .}}) %>%
    kable(caption='Results of k-fold iterations.', row.names=FALSE)

################
# DECISION TREE
tree <- rpart(f, data=df, method="class")
predict(tree, df, type="class")

tree <- rpart(fatal ~ age + height + weight + factor(sex) + factor(role), data=dfcc, method="class")
predict(tree, df, type="prob") %>% unique

#performance:
#accuracy = correct / total
#precision = TP/(TP+FP)
#recall = TP/(TP+FN)
#rmse for regression
#Accuracy = (TP + TN) / (TP + FN + FP + TN)
#Precision = TP / (TP + FP)
#Recall = TP / (TP + FN)
#acc_full <- sum(diag(conf_full)) / sum(conf_full)

#confusion matrix
table(titanic$Survived, pred) # confusion matrix

###############
## RandomForest
n <- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
randomForest(fatal ~ age + height + weight, data=train, ntree=5, importance=TRUE)

############
## Stepwise

full <- lm(fatal ~ age + height + weight + factor(sex) + factor(role), data=dfcc)
null <- lm(fatal ~ 1, data=dfcc)
stepAIC(null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)
stepAIC(full, direction="backward", trace=FALSE)

summary(lm(fatal ~ weight, data=dfcc))$adj.r.squared
summary(lm(fatal ~ factor(sex), data=dfcc))$adj.r.squared
summary(lm(fatal ~ age, data=dfcc))$adj.r.squared
summary(lm(fatal ~ factor(role), data=dfcc))$adj.r.squared
summary(lm(fatal ~ height, data=dfcc))$adj.r.squared

##############
## Best Subset
summary(regsubsets(f, data=df, nbest=2))$outmat %>% as.data.frame

################################
## Regularized Linear Regression
cv.fit <- cv.glmnet(as.matrix(dfcc[,-(4:6)]), as.vector(dfcc[,6]), nlambda=1000)
plot(cv.fit)

# Logistic Regression
# Imbalanced classification
## stratified cross validation
## Over or under sampling
## Model using lm and then try different cutoffs for response???
# Would decision tree or randomForest be better?
#

#http://blog.datalifebalance.com/lift-charts-a-data-scientists-secret-weapon/
#http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html

# notes:
# between-class imbalance
# ROC curves, precision-recall, cost curve
# 
# used features which were empirical and knowable before accident in general
# try random oversamping and undersamping (balance the classes, then learn)
# Easy Ensemble - randomly sample from folds of majority class similar in size to minority class
# 
