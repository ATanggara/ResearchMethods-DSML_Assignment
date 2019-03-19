## this script file gives the grading function. To use it, simply call the function with the name (and path) of your predictions CSV file.

require(caret)
library(readr)

## you can either use the real ground truth here (if provided) or construct one from the data you have using train-test split.
## columns in "testing_set_labels.csv" are: c("user_id", "botscore_gt", "is_bot_gt")
groundTruth <- read.delim("testing_set_labels.csv.bz2")

## this is the size of the testing dataset (before removing NAs)
nGroundTruth <- nrow(groundTruth)

# make botscore numerical and correct invalid data; remove NA's
groundTruth$botscore <- as.character(groundTruth$botscore)
groundTruth$botscore <- as.numeric(groundTruth$botscore)
groundTruth$botscore[groundTruth$botscore < 0] <- NA
groundTruth$is_bot_gt <- groundTruth$botscore > 0.5
names(groundTruth) <- c("user_id", "botscore_gt", "is_bot_gt")
groundTruth <- groundTruth[!is.na(groundTruth$user_id),]

## this is the grading function. It takes a CSV, tries several loading and correction mechanisms.
## afterwards grades the assignment.
gradeCSVfile <- function(csvfile) {
  # csvfile <- "predictions/predictions_u5937495_u5958754_u6374399.csv"
  ## first load the student CSV
  predictions <- read.delim(csvfile, header = T, sep = "")
  
  ## try different types of loading the CSV
  if (ncol(predictions) < 3) {
    predictionsnew <- read.delim(csvfile, header = T, sep = ",")
    if (ncol(predictionsnew) >= 3)
      predictions <- predictionsnew
  }
  
  if (ncol(predictions) < 3) {
    library(readr)
    predictionsnew <- read_csv(csvfile)
    if (ncol(predictionsnew) >= 3)
      predictions <- predictionsnew
  }
  
  ## Sanity checks: check on names of data.frame
  expectedNames <- c("user_id", "botscore", "is_bot")
  ## maybe there is no name row as the first row in the CSV
  if (ncol(predictions) == 3 && nrow(predictions) ==  (nGroundTruth - 1)) {
    ## if here, it means that there is no header, reload without header
    predictions <- read.delim(csvfile, header = F)
    names(predictions) <- expectedNames
  }
  
  for (name in expectedNames)
    if (!name %in% names(predictions)) {
      warning(sprintf("CSV %s: expected column not existing '%s'", csvfile, name))
    }
  
  for (name in names(predictions))
    if (!name %in% expectedNames) {
      warning(sprintf("CSV %s: unexpected column name '%s'", csvfile, name))
    }
  
  ## let's start working
  ## add ground truth to the prediction data.frame
  predictions <- predictions[!is.na(predictions$user_id),]
  
  # some sanity check for users.
  if (sum(! predictions$user_id %in% groundTruth$user_id) > 0 ) {
    warning(sprintf("File %s: %d users in predictions not present in ground truth!", csvfile, sum(! predictions$user_id %in% groundTruth$user_id)))
  }
  if (sum(! groundTruth$user_id %in% predictions$user_id) > 0 ) {
    warning(sprintf("File %s: %d users in ground truth not present in prediction!", csvfile, sum(! groundTruth$user_id %in% predictions$user_id)))
  }
  
  # View(predictions[! predictions$user_id %in% groundTruth$user_id,])
  predictions <- merge(x = predictions, y = groundTruth, by = "user_id", all = T)
  
  ## bounds for grades
  minmax <- data.frame( classification = c(0.51419, 0.8401), regression = c(0.1218, 0.0897))
  rownames(minmax) <- c("min", "max")
  
  myRegressionGrade <- 0; myRMSE <- NA
  if( "botscore" %in% names(predictions)) {
    myRMSE <- sqrt(mean((predictions$botscore - predictions$botscore_gt)^2, na.rm = T)) 
    myRegressionGrade <- (myRMSE - minmax["min", "regression"]) /
      (minmax["max", "regression"] - minmax["min", "regression"]) * 25
  }
  
  myClassificationGrade <- 0; myaccuracy <- NA
  if ("is_bot" %in% names(predictions)) {
    predictions$is_bot <- as.logical(predictions$is_bot)
    predictions$is_bot <- as.factor(predictions$is_bot)
    predictions$is_bot_gt <- as.factor( predictions$is_bot_gt)
    myaccuracy <- confusionMatrix(data = predictions$is_bot, reference = predictions$is_bot_gt,
                                  dnn = c("Predicted", "Observed"), positive = "TRUE", mode = "everything")$byClass["Balanced Accuracy"]
    
    myClassificationGrade <- (myaccuracy - minmax["min", "classification"]) /
      (minmax["max", "classification"] - minmax["min", "classification"]) * 25
  }
  
  retval <- c(myClassificationGrade, myRegressionGrade, myaccuracy, myRMSE)
  names(retval) <- c("myClassificationGrade", "myRegressionGrade", "myaccuracy", "myRMSE")
  return(retval)
}

## Here is how you use the grading function. Note, you need full (or relative) path to the CSV.
gradeCSVfile(csvfile = "predictions/predictions_u5937495_u5958754_u6374399.csv")
