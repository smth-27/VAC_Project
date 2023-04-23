
library(e1071)
library(caret)
library(randomForest)

# Load the mtcars dataset
data(mtcars)

#data_visualisation
plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon")
barplot(table(mtcars$cyl), xlab = "Number of Cylinders", ylab = "Count of Cars")
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders", ylab = "Miles per Gallon")
plot(density(mtcars$mpg), main = "Density Plot of Miles per Gallon", xlab = "Miles per Gallon", ylab = "Density")
pairs(mtcars[, c("mpg", "disp", "hp", "wt")], main = "Scatterplot Matrix")

#splitting dataset
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
set.seed(123)
train_index <- sample(nrow(mtcars), floor(0.7 * nrow(mtcars)))
train_data <- mtcars[train_index, ]
test_data <- mtcars[-train_index, ]

#Classifiers:
#svm
svm_model <- svm(am ~ ., data = train_data, kernel = "linear")
svm_preds <- predict(svm_model, newdata = test_data)
confusionMatrix(svm_preds, test_data$am)
#rfc
rfc_model <- randomForest(am ~ ., data = train_data, ntree = 500)
rfc_preds <- predict(rfc_model, newdata = test_data)
confusionMatrix(rfc_preds, test_data$am)
