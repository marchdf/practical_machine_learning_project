## ------------------------------------------------------------------------
library(ggplot2)
library(caret)

## ------------------------------------------------------------------------
training = read.csv("./pml-training.csv",na.strings=c("NA",""))
testing = read.csv("./pml-testing.csv",na.strings=c("NA",""))

## ------------------------------------------------------------------------
qplot(x=X,y=classe,data=training);
training = subset(training, select = -c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window))
testing = subset(testing, select = -c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window))

## ------------------------------------------------------------------------
n = nrow(testing)
ratio = 0.5
keep = colSums(is.na(training))<ratio*n
training = training[, keep]
testing  = testing [, keep]

## ------------------------------------------------------------------------
k = 5
set.seed(42)

InTrain<-createDataPartition(y=training$classe,p=0.01,list=FALSE)
df<-training[InTrain,]

model = train(classe ~ ., method = 'rf', data = df, trControl=trainControl(method="cv",number=k), allowParallel=TRUE,prox=TRUE)
print(model$finalModel)

## ------------------------------------------------------------------------
predictions = predict(model, newdata = testing)

## ----code=readLines(knitr::purl('./predicting_activities.Rmd', documentation = 1)), eval = FALSE----
## NA

