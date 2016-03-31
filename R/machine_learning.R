## Machine Learning

##########
# BASIC LM
fit <- lm(frm(names(df)), df[sample(nrow(df),10000), ])
summary(fit)$adj.r.squared
coef(fit)
sqrt(sum(summary(fit)$residuals^2)/nrow(df)) # RMSE
qqnorm(summary(fit)$residuals)

###############################
## Basic LM with test/train
ddf <- part_split(df, prop.test=0.3, n=200)
train <- ddf[1]
test <- ddf[2]
fit <- lm(frm(names(df)), data=train)
pred <- predict(fit, test, type="response")
pred <- as.integer(pred > 0.08)
conf <- table(test$fatal, pred) #confusion matrix
sum(diag(conf)) / sum(conf) #accuracy
summary(fit)
conf[2,2] / (conf[1,2] + conf[2,1]) #precision

###############################
## Binomial logistic regression (not working)
n <- nrow(df)
shuffled <- df[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]
fit <- glm(fatal ~ crash_config + eyewear + posture + seatbelt_used + alcohol_test + alcohol_present + compartment_integrity_loss + avoidance_maneuver
, family = binomial(link="logit"), data=train)
pred <- predict(fit, test, type="link")
pred <- predict(fit, test, type="response")
summary(fit)
#confint(fit)
anova(fit, test="Chisq")
hist(pred$fit)
conf <- table(test$fatal, pred > 0.5) #confusion matrix
summary(fit)

fit <- glm(fatal ~ crash_config + eyewear + race + age + airbag_deployment + posture + seatbelt_used + entrapment + event_class + damage_plane + alcohol_test + alcohol_present + roadway_alignment + posted_speed + driver_race + compartment_integrity_loss + avoidance_maneuver + preimpact_location + fire + drive_Wheels, family = binomial(link="logit"), data=train)

#auc
probs <- predict(fit, test, type="response")
pred <- prediction(probs, test$fatal)
plot(performance(pred, "tpr", "fpr")) # ROC Curve
#performance(pred, "auc", "fpr")@y.values[[1]] #AUC
performance(pred, "auc")@y.values[[1]]
table(test$fatal, pred@predictions[[1]] > 0.5)

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
fit <- randomForest(factor(fatal) ~ age + weight + seatbelt_used_police, data=train, ntree=5, importance=TRUE)
fit <- randomForest(f, data=train, ntree=5)
hist(predict(fit, test, type="prob")[,2])

p <- predict(fit, test, type="response")
pr <- prediction(as.integer(p), as.integer(test$fatal))
prf <- performance(pr, measure = "tpr", x.measure="fpr")
plot(prf)

############
## Stepwise

full <- lm(fatal ~ age + height + weight + factor(sex) + factor(role), data=df)
null <- lm(fatal ~ 1, data=df)
stepAIC(null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)
stepAIC(full, direction="backward", trace=FALSE)

summary(lm(fatal ~ weight, data=dfcc))$adj.r.squared
summary(lm(fatal ~ factor(sex), data=dfcc))$adj.r.squared
summary(lm(fatal ~ age, data=dfcc))$adj.r.squared
summary(lm(fatal ~ factor(role), data=dfcc))$adj.r.squared
summary(lm(fatal ~ height, data=dfcc))$adj.r.squared

##############
## Best Subset
dff <- df[sample(nrow(df), 1000), ]
f <- reformulate(names(df)[!names(df) %in% c('CaseId', 'CaseWeight', 'model', 'fatal')], 'fatal')
summary(regsubsets(f, data=dff, nbest=1, nvmax=2, really.big=T))$outmat %>% as.data.frame

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
