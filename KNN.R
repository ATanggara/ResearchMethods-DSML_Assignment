install.packages(class)
library(class)
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

# binarize the botscore
data_df$is_bot <- F
data_df$is_bot[data_df$botscore > 0.5] <- T
data_df$is_bot <- factor(data_df$is_bot)
data_df$botscore <- NULL

summary(data_df)

# first construct a train and a test sample, each of 1000 users
# set.seed(300)
# sample2k <- sample_n(tbl = dataset, size = 1000, replace = F)
# data_train <- sample2k[1:1000,]
# data_test <- sample2k[1001:2000,]

# Section off data for training
set.seed(100)   # setting seed to reproduce results of random sampling (shuffling rows)
trainingRowIndex <- sample(1:nrow(data_df), 0.8*nrow(data_df)) # row indices
data_train <- data_df[trainingRowIndex, ]   # model training data
data_test <- data_df[-trainingRowIndex, ]   # test data

print(sprintf("Our sample has %d rows", nrow(data_train)))
summary(data_train)




#Pre-processing
#data_train_preproc <- preProcess(select(data_train, - is_bot),
#                                 method = c("center", "scale", "BoxCox", "YeoJohnson", "nzv", "pca"))
#data_train_preproc

#data_train_preproc$method

#data_train_preproc$rotation

# construct pre-processed training and testing datasets
#train_df <- predict(data_train_preproc, data_train)
#test_df <- predict(data_train_preproc, data_test)
#head(train_df)

# Setting levels for both training and test data
levels(train_df$is_bot) <- make.names(levels(factor(train_df$is_bot)))
levels(test_df$is_bot) <- make.names(levels(factor(test_df$is_bot)))


# Setting up train controls; 3 sepataye 10-fold validations
repeats <- 3
numbers <- 10
tunel <- 10



fitControl <- trainControl(
  method = "repeatedcv", 
  number = numbers, 
  repeats = repeats, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary)

modelknn <- train(is_bot~. , data = train_df, method = "knn",
                  preProcess = c("center", "scale"),
                  trControl = fitControl, tuneLength = tunel)
modelknn
plot(modelknn)

model_knn_pred <- predict(modelknn, test_df, type="raw")
postResample(pred = model_knn_pred, obs = test_df$is_bot)

pred_obs <- data.frame(predicted = model_knn_pred, observed = test_df$is_bot)
head(pred_obs)

confusionMatrix(data = model_knn_pred, reference = test_df$is_bot)
