
# Import libraries
library(caret)
library(dplyr)
library(e1071)
library(BBmisc) #for preprocessing
set.seed(1)

# Import dataset
rawdata <- read.csv("./data/sample_users_100k.csv", sep="\t", stringsAsFactors = F)

# Create copy of dataframe
dataset <- data.frame(rawdata)

# randomly shuffle rows in dataset
dataset <- dataset[sample(nrow(dataset)),]

# count rows with botscore "deleted", "suspended", and "protected"
bs_delt = length(which(dataset$botscore == "deleted"))
bs_susp = length(which(dataset$botscore == "suspended"))
bs_prot = length(which(dataset$botscore == "protected"))
bs_remain = length(dataset$botscore)
print(sprintf("botscore: deleted:%d, suspended:%d, protected:%d, remaining:%d", bs_delt, bs_susp, bs_prot, bs_remain- (bs_delt+bs_susp+bs_prot)))

# Remove rows with botscore of NA
data_feats = c(
  'user_id',
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
  'botscore' # drop na
)

dataset <- dataset[, data_feats]

# clean verified
dataset$verified[is.na(dataset$verified)] <- F
dataset$verified <- dataset$verified * 1
dataset$verified <- as.factor(dataset$verified)

# clean location.objectType
dataset$location.objectType[dataset$location.objectType == "place"] <- 1
dataset$location.objectType[is.na(dataset$location.objectType)] <- 0
dataset$location.objectType <- as.factor(dataset$location.objectType)

# clean mcsize
dataset$mcsize[is.na(dataset$mcsize)] <- 0

# delete rows w/o label (botscore = NA)
toDel <- which(is.na(dataset$botscore))
dataset <- dataset[-toDel,]

#remove rows with NA value
toKeep <- rowSums(is.na(dataset)) == 0
dataset <- dataset[toKeep, ]

#remove rows with "deleted botscore"
delrows <- which(dataset$botscore == "deleted")
dataset <- dataset[-delrows,]

# store user ID
user_id <- dataset$user_id
#delete user ID from dataset
dataset$user_id <- NULL

#remove outliers
dataset <- dataset[-which(dataset$mcsize > 4000),]
dataset <- dataset[-which(dataset$followersCount > 100000),]


