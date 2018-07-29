setwd("C:/Viraj/Bridgeport/FALL-18-Project/Kaggle-competitiom/Predicting-house-prices")

dataset_train <- read.csv("train.csv")

dataset_test <- read.csv("test.csv")

na_count <-sapply(dataset_train, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

# training set shows missing values for the foll variables:
# LotFrontage - 259, Alley - 1369, MasVnrType - 8, BsmtQual - 37,
# BsmtCond - 37, BsmtExposure - 38, BsmtFinType1 - 37, BsmtFinType2 - 38,
# FireplaceQu - 690, GarageType - 81, GarageCond - 81, PoolQC - 1453, 
# Fence - 1179, MiscFeature - 1406.

dataset_train$Alley <- NULL
dataset_train$MiscFeature <- NULL
dataset_train$PoolQC <- NULL
dataset_train$FireplaceQu<- NULL
dataset_train$Fence <- NULL

dataset_train$LotFrontage[is.na(dataset_train$LotFrontage)]<- 0
dataset_train$MasVnrType[is.na(dataset_train$MasVnrType)]<- "None"

# Levels setting - BsmtQual
levels <- levels(dataset_train$BsmtQual)
levels[length(levels) + 1] <- "UN"

dataset_train$BsmtQual <- factor(dataset_train$BsmtQual, levels = levels)

# Levels setting - BsmtCond
levels <- levels(dataset_train$BsmtCond)
levels[length(levels) + 1] <- "UN"

dataset_train$BsmtCond <- factor(dataset_train$BsmtCond, levels = levels)

# Levels setting - BsmtExposure
levels <- levels(dataset_train$BsmtExposure)
levels[length(levels) + 1] <- "UN"

dataset_train$BsmtExposure <- factor(dataset_train$BsmtExposure, levels = levels)


# Levels setting - BsmtFinType1
levels <- levels(dataset_train$BsmtFinType1)
levels[length(levels) + 1] <- "UN"

dataset_train$BsmtFinType1 <- factor(dataset_train$BsmtFinType1, levels = levels)


# Levels setting - BsmtFinType1
levels <- levels(dataset_train$BsmtFinType2)
levels[length(levels) + 1] <- "UN"

dataset_train$BsmtFinType2 <- factor(dataset_train$BsmtFinType2, levels = levels)


dataset_train$BsmtQual[is.na(dataset_train$BsmtQual)] <- "UN"
dataset_train$BsmtCond[is.na(dataset_train$BsmtCond)] <- "UN"
dataset_train$BsmtExposure[is.na(dataset_train$BsmtExposure)] <- "UN"
dataset_train$BsmtFinType1[is.na(dataset_train$BsmtFinType1)] <- "UN"
dataset_train$BsmtFinType2[is.na(dataset_train$BsmtFinType2)] <- "UN"


# Levels setting - GarageType
levels <- levels(dataset_train$GarageType)
levels[length(levels) + 1] <- "UN"

dataset_train$GarageType <- factor(dataset_train$GarageType, levels = levels)



# Levels setting - GarageFinish
levels <- levels(dataset_train$GarageFinish)
levels[length(levels) + 1] <- "UN"

dataset_train$GarageFinish <- factor(dataset_train$GarageFinish, levels = levels)


# Levels setting - GarageQuality
levels <- levels(dataset_train$GarageQual)
levels[length(levels) + 1] <- "UN"

dataset_train$GarageQual <- factor(dataset_train$GarageQual, levels = levels)


# Levels setting - Garagecondition
levels <- levels(dataset_train$GarageCond)
levels[length(levels) + 1] <- "UN"

dataset_train$GarageCond <- factor(dataset_train$GarageCond, levels = levels)



dataset_train$GarageType[is.na(dataset_train$GarageType)] <- "UN"
dataset_train$GarageYrBlt[is.na(dataset_train$GarageYrBlt)] <- 0000
dataset_train$GarageFinish[is.na(dataset_train$GarageFinish)] <- "UN"
dataset_train$GarageQual[is.na(dataset_train$GarageQual)] <- "UN"
dataset_train$GarageCond[is.na(dataset_train$GarageCond)] <- "UN"

colSums(is.na(dataset_train))

colSums(is.na(dataset_test))

####################################################################################

par(mfrow=c(1,2))
hist(dataset_train$SalePrice)
hist(log10(dataset_train$SalePrice))

dataset_train$SalePrice <- log10(dataset_train$SalePrice)

######################################################################################

############################################### TEST ##################################
dataset_test$Alley <- NULL
dataset_test$MiscFeature <- NULL
dataset_test$PoolQC <- NULL
dataset_test$FireplaceQu<- NULL
dataset_test$Fence <- NULL

dataset_test$LotFrontage[is.na(dataset_test$LotFrontage)]<- 0
dataset_test$MasVnrType[is.na(dataset_test$MasVnrType)]<- "None"
dataset_test$MasVnrArea[is.na(dataset_test$MasVnrArea)]<- 0


# Levels setting - BsmtQual
levels <- levels(dataset_test$BsmtQual)
levels[length(levels) + 1] <- "UN"

dataset_test$BsmtQual <- factor(dataset_test$BsmtQual, levels = levels)

# Levels setting - BsmtCond
levels <- levels(dataset_test$BsmtCond)
levels[length(levels) + 1] <- "UN"

dataset_test$BsmtCond <- factor(dataset_test$BsmtCond, levels = levels)

# Levels setting - BsmtExposure
levels <- levels(dataset_test$BsmtExposure)
levels[length(levels) + 1] <- "UN"

dataset_test$BsmtExposure <- factor(dataset_test$BsmtExposure, levels = levels)


# Levels setting - BsmtFinType1
levels <- levels(dataset_test$BsmtFinType1)
levels[length(levels) + 1] <- "UN"

dataset_test$BsmtFinType1 <- factor(dataset_test$BsmtFinType1, levels = levels)


# Levels setting - BsmtFinType1
levels <- levels(dataset_test$BsmtFinType2)
levels[length(levels) + 1] <- "UN"

dataset_test$BsmtFinType2 <- factor(dataset_test$BsmtFinType2, levels = levels)


dataset_test$BsmtQual[is.na(dataset_test$BsmtQual)] <- "UN"
dataset_test$BsmtCond[is.na(dataset_test$BsmtCond)] <- "UN"
dataset_test$BsmtExposure[is.na(dataset_test$BsmtExposure)] <- "UN"
dataset_test$BsmtFinType1[is.na(dataset_test$BsmtFinType1)] <- "UN"
dataset_test$BsmtFinType2[is.na(dataset_test$BsmtFinType2)] <- "UN"


# Levels setting - GarageType
levels <- levels(dataset_test$GarageType)
levels[length(levels) + 1] <- "UN"

dataset_test$GarageType <- factor(dataset_test$GarageType, levels = levels)



# Levels setting - GarageFinish
levels <- levels(dataset_test$GarageFinish)
levels[length(levels) + 1] <- "UN"

dataset_test$GarageFinish <- factor(dataset_test$GarageFinish, levels = levels)


# Levels setting - GarageQuality
levels <- levels(dataset_test$GarageQual)
levels[length(levels) + 1] <- "UN"

dataset_test$GarageQual <- factor(dataset_test$GarageQual, levels = levels)


# Levels setting - Garagecondition
levels <- levels(dataset_test$GarageCond)
levels[length(levels) + 1] <- "UN"

dataset_test$GarageCond <- factor(dataset_test$GarageCond, levels = levels)



dataset_test$GarageType[is.na(dataset_test$GarageType)] <- "UN"
dataset_test$GarageYrBlt[is.na(dataset_test$GarageYrBlt)] <- 0000
dataset_test$GarageFinish[is.na(dataset_test$GarageFinish)] <- "UN"
dataset_test$GarageQual[is.na(dataset_test$GarageQual)] <- "UN"
dataset_test$GarageCond[is.na(dataset_test$GarageCond)] <- "UN"

colSums(is.na(dataset_test))
#######################################################################################

convert.to.dummy <- function(data.set){
  cat.var <-NULL
  temp.data <- data.frame(1:nrow(data.set))
  for(i in 1:ncol(data.set)){
    if(class(data.set[,i]) == "factor"){
      cat.var <- c(cat.var,i)
      factor.levels <- levels(data.set[,i]) # Try to find a way to classify NA's as "NO" otherwise they generate problem downstream
      # First check if there is any 'NA-level'
      if(any(is.na(data.set[,i]))){
        dummy.vector = ifelse(is.na(data.set[,i]),1,0)
        dummy.vector <- data.frame(dummy.vector)
        colnames(dummy.vector)[1] = paste("NO",names((data.set)[i]),sep = ".")
        temp.data <- cbind(temp.data,dummy.vector)
      }
      
      for(j in seq_along(factor.levels)){ # Then deal with normal factor levels
        dummy.vector = ifelse(data.set[,i] == factor.levels[j],1,0)
        
        #Since we already dealt with NAs above
        if(any(is.na(dummy.vector))){dummy.vector[is.na(dummy.vector)] <- 0} 
        
        dummy.vector <- data.frame(dummy.vector)
        colnames(dummy.vector)[1] = paste(names((data.set)[i]),
                                          factor.levels[j],sep = ".")
        temp.data <- cbind(temp.data,dummy.vector)
      }
    }
  }
  #Remove the original categorical variables from data.set
  data.set <- data.set[,-cat.var]     
  #Add the dummy.variable set
  temp.data <- temp.data[,-1] # remove the unnecessary column
  data.set <- cbind(data.set,temp.data)
  
  return(data.set)     
}

##########################PRATICE######################################################
# Process the train set
training.processed <- convert.to.dummy(dataset_train)

# Process the test set
test.processed <- convert.to.dummy(dataset_test)





#################################PRATICE############################################################
View(training.processed)
library(caTools)
set.seed(123)

split = sample.split(training.processed$SalePrice,SplitRatio = 0.7)
new_training  = subset(training.processed,split==TRUE)
new_Valid = subset(training.processed,split == FALSE)

###########################################################################################
library(caTools)
set.seed(123)

split = sample.split(dataset_train$SalePrice,SplitRatio = 0.7)
new_training  = subset(dataset_train,split==TRUE)
new_test = subset(dataset_train,split == FALSE)


new_training <- new_training[,2:76]
new_test <- new_test[,2:76]

########################
training.processed <- convert.to.dummy(new_training)

# Process the test set
test.processed <- convert.to.dummy(new_test)

###########################

# Fitting the model- Multiple Linear Regression
regressor <- lm(formula = new_training$SalePrice ~ .,data=new_training)

summary(regressor)


#predicting the test result

y_pred <- predict(regressor,newdata = new_Valid)
summary(y_pred)

write.csv(y_pred, "House_prices.csv", row.names = TRUE)
#########################################################################################################

########################### Implementing Algorithm ### Multiple Linear regression #######################

library(caTools)
set.seed(123)

split = sample.split(training.processed$SalePrice,SplitRatio = 0.7)
new_training  = subset(training.processed,split==TRUE)
new_valid = subset(training.processed,split == FALSE)

regressor <- lm(formula = new_training$SalePrice ~ .,data=new_training)

y_pred <- predict(regressor,newdata = new_valid)

vaild_dataframe <- data.frame(y_pred)
####################################################

################## Predict for test data ###############################
fitmodel = data.frame(predict(regressor, newdata = test.processed))

View(test.processed)
###########################################################################################################



split = sample.split(dataset_train$SalePrice,SplitRatio = 0.7)
new_training  = subset(dataset_train,split==TRUE)
new_valid = subset(dataset_train,split == FALSE)

regressor <- lm(formula = dataset_train$SalePrice ~ .,data=dataset_train)

y_pred <- predict(regressor,newdata = new_valid)

vaild_dataframe <- data.frame(y_pred)

fitmodel = data.frame(predict(regressor, newdata = dataset_test))

final = data.frame(Id = dataset_test$Id, SalePrice = fitmodel$predict.regressor..newdata...dataset_test.)
write.csv(final, "House_prices.csv", row.names = FALSE)
