library(caTools)
library(e1071)

titanic <- read.csv("titanic.csv")
# Removing name variable
clean.data <- titanic[,-3]

# Encoding variable 'male' and 'female' to 1,2
clean.data$Sex <- factor(clean.data$Sex, 
                        levels = c("male", "female"), 
                        labels = c(1, 2))

# Splitting the dataset
set.seed(42)
split = sample.split(clean.data$Survived, SplitRatio = 0.75)
training_set = subset(clean.data, split == TRUE)
test_set = subset(clean.data, split == FALSE)

# Fitting data to SVM
classifier = svm(formula = Survived ~., data = training_set,
                 type = 'C-classification', kernel = 'linear')

# Pridicting the test set results
y_predict = predict(classifier, newdata = test_set[, -1])

#confusion Matrix
con_matrix = table (test_set[, 1], y_predict)
