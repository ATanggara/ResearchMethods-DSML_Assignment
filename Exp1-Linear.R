install.packages( c("caret", "dplyr"), dependencies = TRUE)

# load in packages
library(caret)

library(dplyr)

install.packages("corrplot")
library("corrplot")

install.packages(class)
library(class)
# load the dataset

data_df <- read.csv("sample_users_100k.csv.bz2", sep="\t", stringsAsFactors = F)
str(data_df)

# make botscore numerical and correct invalid data
data_df$botscore <- as.numeric(data_df$botscore)
toDel <- which(data_df$botscore < 0)
data_df <- data_df[-toDel,]
toDel <- which(is.na(data_df$botscore))
data_df <- data_df[-toDel,]

str(data_df)

summary(data_df)



#Selecting features
features = c(
  'displayName',
  'statusesCount',
  'summary',
  'listedCount', # drop na
  'favoritesCount', # drop na
  'friendsCount', # drop na
  'followersCount', # drop na
  'verified', # convert True to 1 and False to 0; na to 0
  'location.objectType', # convert "place" to 1, na to 0
  'mcsize', # convert na to 0
  'influence', # drop na
  'influence_percentile', # drop# na
  'tweetsCount', # drop na
  'retweetsCount', # drop na
  'botscore' #drop na
)



#Transform features

# keep only selected features
data_df <- data_df[, features]

# clean verified
data_df$verified[is.na(data_df$verified)] <- F
data_df$verified <- data_df$verified * 1

# clean location.objectType
data_df$location.objectType[data_df$location.objectType == "place"] <- 1
data_df$location.objectType[is.na(data_df$location.objectType)] <- 0
data_df$location.objectType <- as.numeric(data_df$location.objectType)

# clean mcsize
data_df$mcsize[is.na(data_df$mcsize)] <- 0

# clean displayName
data_df$displayName[is.na(data_df$displayName)] <- 0
data_df$displayName [!(is.na(data_df$displayName))] <- 1
data_df$displayName <- as.numeric(data_df$displayName)

# clean summary
data_df$summary[is.na(data_df$summary)] <- 0
data_df$summary [!(is.na(data_df$summary))] <- 1
data_df$summary <- as.numeric(data_df$summary)

# remove non-complete entries
toKeep <- rowSums(is.na(data_df)) == 0
data_df <- data_df[toKeep, ]




#Section off data for training
set.seed(100)  # setting seed to reproduce results of random sampling

trainingRowIndex <- sample(1:nrow(data_df), 0.8*nrow(data_df))  # row indices for training data
trainingData <- data_df[trainingRowIndex, ]  # model training data
testData  <- data_df[-trainingRowIndex, ]   # test data

print(sprintf("Our sample has %d rows", nrow(trainingData)))
summary(trainingData)

#drop displayName and summary
data_df <- subset(data_df, select = -c(displayName, summary))
trainingData <- subset(trainingData, select = -c(displayName, summary))

summary(trainingData)

#Preprocessing Data
data_train_preproc <- preProcess(select(trainingData, - botscore),
                                 method = c("center", "scale", "BoxCox", "YeoJohnson", "nzv", "pca"))
data_train_preproc

data_train_preproc$method

data_train_preproc$rotation

# construct pre-processed training and testing datasets
train_df <- predict(data_train_preproc, trainingData)
test_df <- predict(data_train_preproc, testData)
head(train_df)

model_lm <- train(botscore ~ ., data = train_df,
                    method = "lm")
model_lm_pred <- predict(model_lm, test_df)
summary(model_lm)

pred_result <- data.frame(cbind(actuals=test_df$botscore, 
                                 predicteds=model_lm_pred))
head(pred_result)

# Compute Root Mean Square Error (RMSE), R^2, Mean Absolute Error (MAE)
metrics <- data.frame(matrix(data = NA, nrow = 3, ncol = 0))
metrics <- cbind(metrics, RF_psi = postResample(pred = model_lm_pred,
                                                obs = data_test$botscore) )
metrics

#make actuals_preticteds dataframe
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy

