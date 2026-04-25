

``` r
# =========================
# Practical ML Project Script
# =========================

# Install packages if needed
packages <- c("caret","rpart","rpart.plot","randomForest","corrplot")
installed <- rownames(installed.packages())

for(p in packages){
  if(!(p %in% installed)){
    install.packages(p)
  }
}

library(caret)
```

```
## Warning: package 'caret' was built under R version 4.5.3
```

```
## Loading required package: ggplot2
```

```
## Use suppressPackageStartupMessages() to eliminate package startup messages
```

```
## Loading required package: lattice
```

``` r
library(rpart)
library(rpart.plot)
```

```
## Warning: package 'rpart.plot' was built under R version 4.5.3
```

``` r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 4.5.3
```

```
## randomForest 4.7-1.2
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

``` r
library(corrplot)
```

```
## corrplot 0.95 loaded
```

``` r
set.seed(12345)
```

``` r
# Load Data
```

``` r
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainRaw <- read.csv(trainUrl)
testRaw  <- read.csv(testUrl)
```

``` r
# Data Cleaning
```

``` r
NZV <- nearZeroVar(trainRaw, saveMetrics = TRUE)
trainData <- trainRaw[, !NZV$nzv]
testData  <- testRaw[, !NZV$nzv]

colsToRemove <- grepl("^X|timestamp|user_name", names(trainData))
trainData <- trainData[, !colsToRemove]
testData  <- testData[, !colsToRemove]

trainData <- trainData[, colSums(is.na(trainData)) == 0]
testData  <- testData[, colSums(is.na(testData)) == 0]
```

``` r
# Split Data
```

``` r
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
training <- trainData[inTrain, ]
validation <- trainData[-inTrain, ]
```

``` r
# Decision Tree
```

``` r
modelTree <- rpart(classe ~ ., data = training, method = "class")
predTree <- predict(modelTree, validation, type = "class")

cat("Decision Tree Results:\n")
```

```
## Decision Tree Results:
```

``` r
print(confusionMatrix(predTree, validation$classe))
```

```
## Error:
## ! `data` and `reference` should be factors with the same levels.
```

``` r
# Random Forest
```

``` r
modelRF <- train(
  classe ~ ., 
  data = training,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  ntree = 250
)

predRF <- predict(modelRF, validation)

cat("\nRandom Forest Results:\n")
```

```
## 
## Random Forest Results:
```

``` r
cm <- confusionMatrix(predRF, validation$classe)
```

```
## Error:
## ! `data` and `reference` should be factors with the same levels.
```

``` r
print(cm)
```

```
## function (x) 
## 2.54 * x
## <bytecode: 0x00000213d309a470>
## <environment: namespace:grDevices>
```

``` r
accuracy <- cm$overall["Accuracy"]
```

```
## Error in `cm$overall`:
## ! object of type 'closure' is not subsettable
```

``` r
oos_error <- 1 - accuracy
```

```
## Error:
## ! object 'accuracy' not found
```

``` r
cat("\nAccuracy:", accuracy, "\n")
```

```
## Error:
## ! object 'accuracy' not found
```

``` r
cat("Out-of-sample error:", oos_error, "\n")
```

```
## Error:
## ! object 'oos_error' not found
```

``` r
# Final Prediction
```

``` r
finalPred <- predict(modelRF, testData[, -ncol(testData)])
print(finalPred)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

``` r
# Generate submission files
```


