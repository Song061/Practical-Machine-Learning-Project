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
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

set.seed(12345)

# -------------------------
# Load Data
# -------------------------
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainRaw <- read.csv(trainUrl)
testRaw  <- read.csv(testUrl)

# -------------------------
# Data Cleaning
# -------------------------
NZV <- nearZeroVar(trainRaw, saveMetrics = TRUE)
trainData <- trainRaw[, !NZV$nzv]
testData  <- testRaw[, !NZV$nzv]

colsToRemove <- grepl("^X|timestamp|user_name", names(trainData))
trainData <- trainData[, !colsToRemove]
testData  <- testData[, !colsToRemove]

trainData <- trainData[, colSums(is.na(trainData)) == 0]
testData  <- testData[, colSums(is.na(testData)) == 0]

# -------------------------
# Split Data
# -------------------------
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
training <- trainData[inTrain, ]
validation <- trainData[-inTrain, ]

# -------------------------
# Decision Tree
# -------------------------
modelTree <- rpart(classe ~ ., data = training, method = "class")
predTree <- predict(modelTree, validation, type = "class")

cat("Decision Tree Results:\n")
print(confusionMatrix(predTree, validation$classe))

# -------------------------
# Random Forest
# -------------------------
modelRF <- train(
  classe ~ ., 
  data = training,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  ntree = 250
)

predRF <- predict(modelRF, validation)

cat("\nRandom Forest Results:\n")
cm <- confusionMatrix(predRF, validation$classe)
print(cm)

accuracy <- cm$overall["Accuracy"]
oos_error <- 1 - accuracy

cat("\nAccuracy:", accuracy, "\n")
cat("Out-of-sample error:", oos_error, "\n")

# -------------------------
# Final Prediction
# -------------------------
finalPred <- predict(modelRF, testData[, -ncol(testData)])
print(finalPred)

# -------------------------
# Generate submission files
# -------------------------
pml_write_files <- function(x){
  for(i in 1:length(x)){
    filename <- paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE,
                row.names = FALSE, col.names = FALSE)
  }
}

pml_write_files(finalPred)

cat("\nAll prediction files generated!\n")





install.packages("knitr")
install.packages("markdown")

library(knitr)

knit2html("pml_project.R", output = "report.html")
