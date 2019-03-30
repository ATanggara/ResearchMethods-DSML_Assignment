### Regression Tree
library(rpart)
str(dataset)
# rm(predictions)

# Any missing values?
sum(is.na(dataset))

#fit model
fit <- rpart(botscore ~ ., data = dataset, control=rpart.control(minsplit=5))
summary(fit)

# first construct a train and a test sample, each of 1000 users
set.seed(300)
sample2k <- sample_n(tbl = dataset, size = 2000, replace = F)
data_train <- sample2k[1:1000,]
data_test <- sample2k[1001:2000,]

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

model_rf_pred <- predict(fit, data_train)
head(data.frame(predicted = model_rf_pred,
                observed = data_test$botscore, row.names = NULL))

  

