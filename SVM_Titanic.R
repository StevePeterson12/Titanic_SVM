Titanic <- read.csv("titanic.csv")
CleanData <- Titanic[,-3]

# Encoding var sex to 1,2
CleanData$Sex <- factor(CleanData$Sex, 
                        levels = c("male", "female"), 
                        labels = c(1, 2))

# Splitting the dataset
library(caTools)
set.seed(42)
split = sample.split(CleanData$Survived, SplitRatio = 0.75)
training_set = subset(CleanData, split == TRUE)
test_set = subset(CleanData, split == FALSE)

# Fitting data to SVM
library(e1071)
classifier = svm(formula = Survived ~., data = training_set,
                 type = 'C-classification', kernel = 'linear')

# Pridicting the test set results
y_predict = predict(classifier, newdata = test_set[, -1])

#confusion Matrix
con_matrix = table (test_set[, 1], y_predict)
