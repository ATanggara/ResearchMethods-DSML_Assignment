# Load libraries
library(caret)
library(dplyr)

# load the dataset
data_df <- read.csv("sample_users_100k.csv.bz2", sep="\t", stringsAsFactors = F)

##nrow(data_df)   # num of rows in dataset
##summary(data_df)   # summary of all attributes in dataset

data_df$verified <- as.factor(data_df$verified)
table(data_df$verified)

# make botscore numerical and correct invalid data
data_df$botscore <- as.numeric(data_df$botscore)
toDel <- which(data_df$botscore < 0)
data_df <- data_df[-toDel,]
toDel <- which(is.na(data_df$botscore))
data_df <- data_df[-toDel,]

# construct the $\psi$ measure 
data_df$psi <- data_df$friendsCount / (data_df$followersCount + 0.01)
str(data_df)   # show dataset details

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

# # first construct a train and a test sample, each of 1000 users
# set.seed(300)
# sample2k <- sample_n(tbl = data_df, size = 2000, replace = F)
# data_train <- sample2k[1:1000,]
# data_test <- sample2k[1001:2000,]
# 
# print(sprintf("Our sample has %d rows", nrow(data_train)))
# summary(data_train)

#Any missing values?
sum(is.na(data_df))

# first construct a train and a test sample, each of 1000 users
# set.seed(300)
# sample2k <- sample_n(tbl = dataset, size = 1000, replace = F)
# data_train <- sample2k[1:1000,]
# data_test <- sample2k[1001:2000,]

# Section off data for training
set.seed(100)   # setting seed to reproduce results of random sampling (shuffling rows)
trainingRowIndex <- sample(1:nrow(data_df), 0.6*nrow(data_df)) # row indices
data_train <- data_df[trainingRowIndex, ]   # model training data
data_test <- data_df[-trainingRowIndex, ]   # test data

print(sprintf("Our sample has %d rows", nrow(data_train)))
summary(data_train)

#Defining training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 4,
  savePredictions = 'final',
  classProbs = T
)

#Training random forest model
model_rf <- train(botscore ~ ., data = data_train,
      method = "rf", trControl = fitControl, tuneLength=3)
model_rf

plot(formula = botscore ~ psi, data = data_train)
# retrieve coefficients for the slope and for psi
coef.icept <- coef(model_rf$finalModel)[1]
coef.slope <- coef(model_rf$finalModel)["psi"]
abline(a = coef.icept, b = coef.slope, col = "red", lwd = "3")

#Predicting using random forest model
model_rf_pred <- predict(model_rf, data_test)
head(data.frame(predicted = model_rf_pred,
                observed = data_test$botscore, row.names = NULL))
pred_result = data.frame(predicted = model_rf_pred,
                observed = data_test$botscore, row.names = NULL)
pred_result

# Compute Root Mean Square Error (RMSE), R^2, Mean Absolute Error (MAE)
metrics <- data.frame(matrix(data = NA, nrow = 3, ncol = 0))
metrics <- cbind(metrics, RF_psi = postResample(pred = model_rf_pred,
                                                obs = data_test$botscore) )
metrics
