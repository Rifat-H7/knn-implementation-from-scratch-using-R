
dataset <- read.csv("E:/Sem 11/DM/diabetes_prediction_dataset.csv")

dataset$diabetes <- ifelse(dataset$diabetes == 1, "yes", "no")

library(datasets)
install.packages("ggplot2")
library(ggplot2)

set.seed(123)
train_index <- sample(1:nrow(dataset), 0.8*nrow(dataset))
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2)^2))
}

manhattan_distance <- function(x1, x2) {
  sum(abs(x1 - x2))
}

maxD_distance <- function(x1, x2) {
  max(abs(x1 - x2))
}

knn_predict1 <- function(train_data, test_data, k) {
  predictions <- c()
  for (i in 1:nrow(test_data)) {
    distances <- apply(train_data[, 2:5], 1, euclidean_distance, test_data[i, 2:5])
    nearest_neighbors <- train_data[order(distances)[1:k], 6]
    prediction <- names(which.max(table(nearest_neighbors)))
    predictions <- c(predictions, prediction)
  }
  return(predictions)
}

knn_predict2 <- function(train_data, test_data, k) {
  predictions <- c()
  for (i in 1:nrow(test_data)) {
    distances <- apply(train_data[, 2:5], 1, manhattan_distance, test_data[i, 2:5])
    nearest_neighbors <- train_data[order(distances)[1:k], 6]
    prediction <- names(which.max(table(nearest_neighbors)))
    predictions <- c(predictions, prediction)
  }
  return(predictions)
}

knn_predict3 <- function(train_data, test_data, k) {
  predictions <- c()
  for (i in 1:nrow(test_data)) {
    distances <- apply(train_data[, 2:5], 1, maxD_distance, test_data[i, 2:5])
    nearest_neighbors <- train_data[order(distances)[1:k], 6]
    prediction <- names(which.max(table(nearest_neighbors)))
    predictions <- c(predictions, prediction)
  }
  return(predictions)
}

k_choice <- c(3,5)
acceu<-c()
accman<-c()
accmaxD<-c()

actuals <- test_data[, 6]

for(k in k_choice){
  
  for(i in 1:3){
    if(i==1){
      predictions1 <- knn_predict1(train_data, test_data, k)
      accuracy <- sum(predictions1 == actuals)/length(actuals)
      acceu<-c()<-c(acceu<-c(), round(accuracy*100, 2))
      print(paste0("When k=", k, " Accuracy with euclidean distance is: ", round(accuracy*100, 2), "%"))
    }
    else if(i==2){
      predictions2 <- knn_predict2(train_data, test_data, k)
      accuracy <- sum(predictions2 == actuals)/length(actuals)
      accman<-c(accman, round(accuracy*100, 2))
      print(paste0("When k=", k, " Accuracy with manhattan distance is: ", round(accuracy*100, 2), "%"))
    }
    else if(i==3){
      predictions3 <- knn_predict3(train_data, test_data, k)
      accuracy <- sum(predictions3 == actuals)/length(actuals)
      accmaxD<-c(accmaxD, round(accuracy*100, 2))
      print(paste0("When k=", k, " Accuracy with maximum dimension distance is: ", round(accuracy*100, 2), "%"))
    }
  }
  
}

result_eu <- data.frame(k = k_choice, accuracy = acceu)
result_man <- data.frame(k = k_choice, accuracy = accman)
result_maxD <- data.frame(k = k_choice, accuracy = accmaxD)

install.packages("gridExtra")
library(gridExtra)

plot1<-ggplot(result_eu, aes(x = k, y = accuracy)) +
  geom_line(color = "red") +
  geom_point() +
  labs(x = "k", y = "Accuracy", title = "k-NN Accuracy with euclidean distance") +
  theme_minimal()
plot2<-ggplot(result_man, aes(x = k, y = accuracy)) +
  geom_line(color = "green") +
  geom_point() +
  labs(x = "k", y = "Accuracy", title = "k-NN Accuracy with manhattan distance") +
  theme_minimal()
plot3<-ggplot(result_maxD, aes(x = k, y = accuracy)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(x = "k", y = "Accuracy", title = "k-NN Accuracy with maximum dimension distance") +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, ncol = 2)

