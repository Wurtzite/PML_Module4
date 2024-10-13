# PML_Module4
Prediction Assignment, Fitness Devices

---
title: "PML_Mod4_Assignment"
author: "Justinian Wurtzel, MSc"
date: "`r Sys.Date()`"
output: html_document
---
# Introduction and Backgroud 

Final assignment for the Practical Machine Learning certifcate course.

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

Goal
The goal of this projects is to use data collected from various wearable devices on the belt, forearm, arm, and dumbell of six participants. 

Participants were asked to perform barbell lifts correctly and incorrectly in five different ways.

Data Sources
Training, https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
Testing, https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
Project data source, http://groupware.les.inf.puc-rio.br/har

# Libraries and Data Sources
```{r, Setup}
install.packages(c("caret", "rattle", "randomForest", "ggplot2", "corrplot"))
library(caret)
library(rattle)
library(randomForest)
library(ggplot2)
library(corrplot)

Training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
Testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

```

# Data Exploration

```{r, Explore}
summary(Training)
str(Training)
summary(Testing)
str(Testing)

# Identify missing data
missing_values<-colSums(is.na(Training))
missing_values[missing_values>0]

# Drop all columns with more than 99% missing values directly using the missing_values list
Training <- Training[, !names(Training) %in% names(missing_values[missing_values > 0])]
missing_values<-colSums(is.na(Training))
missing_values[missing_values>0]
# Now there are no columns with completely missing data, from 160 variables to 93
summary(Training)

# Remove unnecessary columns, 93 to 86 variables
Training <- Training[, !names(Training) %in% c("X", "user_name", "raw_timestamp_part_1", 
                                               "raw_timestamp_part_2", "cvtd_timestamp", 
                                               "new_window", "num_window")]
```

# Feature Selection and Data Prep
Prepping for Random Forest
```{r, Features and Prep}
library(caret)
class(Training$classe)
Training$class<-as.factor(Training$classe)

preProc <- preProcess(Training[, -ncol(Training)], method = c("center", "scale"))
Training_normalized <- predict(preProc, Training[, -ncol(Training)])
Training_normalized$classe <- Training$classe 

# reproducibility for classmates if running
set.seed(10062024)
trainIndex <- createDataPartition(Training_normalized$classe, p = 0.7, list = FALSE)
trainSet <- Training_normalized[trainIndex, ]
validationSet <- Training_normalized[-trainIndex, ]
summary(trainSet)
summary(validationSet)

```

# Model Fitting

```{r, Model Fitting}
library(caret)
library(randomForest)
library(doParallel)

# Set up cross-validation with 3-fold CV
ctrl <- trainControl(method = "cv", number = 3, verboseIter = TRUE)
# trainSet <- trainSet[sample(1:nrow(trainSet), size = 10000), ]  

#install.packages("doParallel")
#library(doParallel)
cl<-makeCluster(detectCores()-1)
registerDoParallel(cl)

set.seed(10062024)
rf_model <- train(classe ~ ., 
                  data = trainSet, 
                  method = "rf", 
                  trControl = ctrl, 
                  tuneLength = 3,
                  ntree = 100,
                  importance = TRUE)

stopCluster(cl)


# Print the model summary
print(rf_model)

# Plot the model to visualize the tuning results
plot(rf_model)

```


