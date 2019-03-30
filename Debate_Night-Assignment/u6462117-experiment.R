#install.packages( c("caret", "dplyr"),
#                  dependencies = TRUE) 
# load in packages
library(caret)
library(dplyr)

# load the dataset
data_df <- read.csv("sample_users_100k.csv.bz2", sep="\t", stringsAsFactors = F)

##nrow(data_df)   # num of rows in dataset
##summary(data_df)   # summary of all attributes in dataset

data_df$verified <- as.factor(data_df$verified)
table(data_df$verified)

##############

# make botscore numerical and correct invalid data
data_df$botscore <- as.numeric(data_df$botscore)
toDel <- which(data_df$botscore < 0)
data_df <- data_df[-toDel,]
toDel <- which(is.na(data_df$botscore))
data_df <- data_df[-toDel,]

# construct the $\psi$ measure from the previous tutorial
data_df$psi <- data_df$friendsCount / (data_df$followersCount + 0.01)
str(data_df)

# Deal with missing value and category variables:
features = c(
  'listedCount', # drop na
  'favoritesCount', # drop na
  'friendsCount', # drop na
  'followersCount', # drop na
  'verified', # convert True to 1 and False to 0; na to 0
  'location.objectType', # convert "place" to 1, na to 0
  'mcsize', # convert na to 0
  'influence', # drop na
  'influence_percentile', # drop na
  'tweetsCount', # drop na
  'retweetsCount', # drop na
  'psi',
  'botscore' # drop na
)

# keep only selected features
data_df <- data_df[, features]

# clean verified
data_df$verified[is.na(data_df$verified)] <- F
data_df$verified <- as.numeric(data_df$verified)
# data_df$verified[data_df$verified == T] <- 1
# data_df$verified[data_df$verified == F] <- 0
#@@@ sets everything to 0?? 
data_df$verified <- data_df$verified * 1

# clean location.objectType
data_df$location.objectType[data_df$location.objectType == "place"] <- 1
data_df$location.objectType[is.na(data_df$location.objectType)] <- 0
data_df$location.objectType <- as.numeric(data_df$location.objectType)

# clean mcsize
data_df$mcsize[is.na(data_df$mcsize)] <- 0

# remove non-complete entries
toKeep <- rowSums(is.na(data_df)) == 0
data_df <- data_df[toKeep, ]

# first construct a train and a test sample, each of 1000 users
set.seed(300)
sample2k <- sample_n(tbl = data_df, size = 2000, replace = F)
data_train <- sample2k[1:1000,]
data_test <- sample2k[1001:2000,]

print(sprintf("Our sample has %d rows", nrow(data_train)))
summary(data_train)

###### Caret
plot(formula = botscore ~ psi, data = data_train)


############# DECISION TREE (REGRESSION) ###########
library(rpart)
library(rpart.plot)
#fit <- rpart(survived~., data = data_train, method = 'anova')
#rpart.plot(fit, extra = 106)
#rpart(formula, data=, method='')

############# RANDOM FOREST WITH CARET (ENSEMBLE CLASSIFICATION) ###########
# bagcontrol <- trainControl(sampling="rose",method="repeatedcv", number=5, repeats=3)
# set.seed(300)
# #"rf" method is for training random forest  model
# fit.rf <- train(botscore~., data=data_train, method="rf", metric=metric, trControl=bagcontrol)
# # evaluate results on test set
# test_set$pred <- predict(fit.rf, newdata=test_set)
# confusionMatrix(data = test_set$pred, reference = test_set$left)

############# LINEAR REGRESSION MODEL #################
fitControl <- trainControl(
  # Repeated 5–fold CV
  method = "repeatedcv",
  number = 5,
  # repeated 10 times
  repeats = 10,
  returnResamp = "all")
model_lm_psi <- train(botscore ~ psi, data = data_train,
                      method = "lm", trControl = fitControl)
model_lm_psi

plot(formula = botscore ~ psi, data = data_train)
# retrieve coefficients for the slope and for psi
coef.icept <- coef(model_lm_psi$finalModel)[1]
coef.slope <- coef(model_lm_psi$finalModel)["psi"]
abline(a = coef.icept, b = coef.slope, col = "red", lwd = "3")

# predict the outcome on a test set
model_lm_psi_pred <- predict(model_lm_psi, data_test)
head(data.frame(predicted = model_lm_psi_pred,
                observed = data_test$botscore, row.names = NULL))

# Compute Root Mean Square Error (RMSE), R^2, Mean Absolute Error (MAE)
metrics <- data.frame(matrix(data = NA, nrow = 3, ncol = 0))
metrics <- cbind(metrics, LM_psi = postResample(pred = model_lm_psi_pred,
                                                obs = data_test$botscore) )
metrics

# Compute Absolute Relative Error (ARE) & Mean ARE (MARE) without the influence of outliers
ARE_df <- data.frame(matrix(data = NA, nrow = nrow(data_test), ncol = 0))
ARE_df <- cbind(ARE_df,
                LM_psi = abs(model_lm_psi_pred - data_test$botscore) / data_test$botscore)
ARE_df <- do.call(data.frame, lapply(ARE_df, function(x) replace(x, is.infinite(x), NA)))
metrics <- rbind(metrics, MARE = apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T))
print(metrics, digits = 2)

# Plot boxplot (red diamond = mean)
boxplot(x = ARE_df, outline = F, names = names(ARE_df),
        main = "Prediction ARE", ylab = "ARE")
points(apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T),
       pch = 23, col = "red", bg = "red", cex = 2)
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T) - 0.06,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T)) )
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T) - 0.03,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T)) )

myecdf <- ecdf(sample2k$psi)
data_train$psi_percentile <- myecdf(data_train$psi)
data_test$psi_percentile <- myecdf(data_test$psi)

# Plot scatterplot
plot(formula = botscore ~ psi_percentile, data = data_train)

# Try linear model
model_lm_1 <- train(botscore ~ psi_percentile, data = data_train,
                    method = "lm", trControl = fitControl)
# get 2D line
coef.icept <- coef(model_lm_1$finalModel)[1]
coef.slope <- coef(model_lm_1$finalModel)["psi_percentile"]
# and plot it
plot(formula = botscore ~ psi_percentile, data = data_train)
abline(a = coef.icept, b = coef.slope, col = "red")

# predict the outcome on a test set
model_lm_pred_1 <- predict(model_lm_1, data_test)
ARE_df <- cbind(ARE_df,
                LM_psi_perc = abs(model_lm_pred_1 - data_test$botscore) / data_test$botscore)
ARE_df <- do.call(data.frame, lapply(ARE_df, function(x) replace(x, is.infinite(x), NA)))
# compare predicted outcome and true outcome
metrics <- cbind(metrics,
                 LM_psi_perc = c(postResample(pred = model_lm_pred_1, obs = data_test$botscore),
                                 mean(ARE_df$LM_psi_perc, na.rm = T) ) )
print(metrics, digits = 2)

# Plot boxplot of ARE performance
boxplot(x = ARE_df, outline = F, names = names(ARE_df),
        main = "Prediction ARE", ylab = "ARE")
points(apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T),
       pch = 23, col = "red", bg = "red", cex = 2)
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T) - 0.06,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T)) )
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T) - 0.03,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T)) )

### Problem with applying all features? Predict with all features
model_lm_2 <- train(botscore ~ ., data = data_train,
                    method = "lm", trControl = fitControl)
# predict the outcome on a test set
model_lm_pred_2 <- predict(model_lm_2, data_test)
## compute error metrics
ARE_df <- cbind(ARE_df,
                LM_all = abs(model_lm_pred_2 - data_test$botscore) / data_test$botscore)
ARE_df <- do.call(data.frame, lapply(ARE_df, function(x) replace(x, is.infinite(x), NA)))
# compare predicted outcome and true outcome
metrics <- cbind(metrics,
                 LM_psi_perc = c(postResample(pred = model_lm_pred_2, obs = data_test$botscore),
                                 mean(ARE_df$LM_all, na.rm = T) ) )
boxplot(x = ARE_df, outline = F, names = names(ARE_df),
        main = "Prediction ARE", ylab = "ARE")
points(apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T),
       pch = 23, col = "red", bg = "red", cex = 2)
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T) - 0.06,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T)) )
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T) - 0.03,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T)) )

# Weighted feature display
old <- par(mar = c(5, 9, 4, 2))
barplot(height = sort(coef(model_lm_2$finalModel), decreasing = T),
        horiz = T, las = 1, col = "red", main = "Feature importance")
par(old)

# YeoJohnson transformation (identify and remove variables with near zero variance and perform pca)
data_train_preproc <- preProcess(select(data_train, - botscore),
                                 method = c("center", "scale", "YeoJohnson", "nzv", "pca"))
data_train_preproc

# Identify which variables were ignored, centered, scaled, etc
data_train_preproc$method

# identify the principal components
data_train_preproc$rotation

# construct pre-processed training and testing datasets
train_df <- predict(data_train_preproc, data_train)
test_df <- predict(data_train_preproc, data_test)
head(train_df)

model_lm_3 <- train(botscore ~ ., data = train_df,
                    method = "lm", trControl = fitControl)
model_lm_pred_3 <- predict(model_lm_3, test_df)
ARE_df <- cbind(ARE_df,
                LM_preproc = abs(model_lm_pred_3 - data_test$botscore) / data_test$botscore)
ARE_df <- do.call(data.frame, lapply(ARE_df, function(x) replace(x, is.infinite(x), NA)))
boxplot(x = ARE_df, outline = F, names = names(ARE_df),
        main = "Prediction ARE", ylab = "ARE")
points(apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T),
       pch = 23, col = "red", bg = "red", cex = 2)
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T) - 0.06,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T)) )
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T) - 0.03,
     labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T)) )

# compare predicted outcome and true outcome
metrics <- cbind(metrics,
                 LM_preproc = c(postResample(pred = model_lm_pred_3, obs = test_df$botscore),
                                mean(ARE_df$LM_preproc, na.rm = T) ) )
# and plot them comparatively for each approach:
  par(mfrow=c(1,4))
for (i in 1:4) {
  barplot(height = unlist(metrics[i,]), names.arg = names(metrics),
          main = rownames(metrics)[i], col = cm.colors(4)[1:4])
}
  
par(mfrow=c(1,3))
# scatterplot of psi and botness
plot(formula = botscore ~ psi, data = data_train)
# histogram of psi
hist(data_train$psi, breaks = 1000, main = "Histogram of Psi", xlab = "Psi")
# log-log plot of CCDF of psi
myecdf <- ecdf(x = data_df$psi)
myx <- seq(from = range(data_df$psi)[1], to = range(data_df$psi)[2], length.out = 1000)
plot(x = myx, y = 1 - myecdf(myx),
     type = "l", lwd = 3, col = "blue", log = "xy",
     main = "(log-log) Empirical CCDF of Psi", xlab = "Psi", ylab = "Empirical CCDF")

# Non-linear regression, random forest
model_rf <- train(botscore ~ ., data = data_train,
                  method = "ranger", trControl = fitControl)
# predict the outcome on a test set
model_rf_pred <- predict(model_rf, data_test)
ARE_df <- cbind(ARE_df, RF = abs(model_rf_pred - data_test$botscore) / data_test$botscore)
ARE_df <- do.call(data.frame, lapply(ARE_df, function(x) replace(x, is.infinite(x), NA)))
# compare predicted outcome and true outcome
metrics <- cbind(metrics,
                 RF = c(postResample(pred = model_rf_pred, obs = test_df$botscore),
                        mean(ARE_df$RF, na.rm = T) ) )

boxplot(x = ARE_df, outline = F, names = names(ARE_df),
        main = "Prediction ARE", ylab = "ARE")
points(apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T),
       pch = 23, col = "red", bg = "red", cex = 2)
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T) - 0.06, labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = mean, na.rm = T)) )
text(x = 1:ncol(ARE_df), y = apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T) - 0.03, labels = sprintf("%.3f", apply(X = ARE_df, MARGIN = 2, FUN = median, na.rm = T)))


par(mfrow=c(1,4))
for (i in 1:4) {
  barplot(height = unlist(metrics[i,]), names.arg = names(metrics),
          main = rownames(metrics)[i], col = cm.colors(ncol(metrics))[1:ncol(metrics)])
}

# binarize the botscore
sample2k$is_bot <- F
sample2k$is_bot[sample2k$botscore > 0.5] <- T
sample2k$is_bot <- factor(sample2k$is_bot)
sample2k$botscore <- NULL
summary(sample2k)

data_train <- sample2k[1:1000,]
data_test <- sample2k[1001:2000,]
summary(data_train)

# let's apply a logistic regression with the same sampling setup as the regression task
model_logistic <- train(is_bot ~ ., data = data_train,
                        method = "glm", family="binomial", trControl = fitControl)
model_logistic

model_logistic_pred <- predict(model_logistic, data_test, type="raw")
postResample(pred = model_logistic_pred, obs = data_test$is_bot)

pred_obs <- data.frame(predicted = model_logistic_pred, observed = data_test$is_bot)
confusionMatrix(data = model_logistic_pred, reference = data_test$is_bot,
                dnn = c("Predicted", "Observed"), positive = "TRUE", mode = "everything")

measures <- c(precision = precision(data = model_logistic_pred,
                                    reference = data_test$is_bot,
                                    relevant = "TRUE"),
              recall = recall(data = model_logistic_pred,
                              reference = data_test$is_bot,
                              relevant = "TRUE"),
              fmeasure = F_meas(data = model_logistic_pred,
                                reference = data_test$is_bot,
                                relevant = "TRUE") )
print(measures, digits = 2)
