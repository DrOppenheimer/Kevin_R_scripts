# Data scientists use various models to analyze and extract insights from data. 
# Some of the most common models include: 
# 1. Linear Regression: A basic statistical model used to understand the relationship 
# between independent variables and a dependent variable, assuming a linear relationship. 
# 2. Logistic Regression: It’s used for binary classification problems, predicting 
# the probability of a certain event occurring. 
# 3. Decision Trees: These models use a tree-like structure to make decisions based 
# on feature values. Random Forests and Gradient Boosting are extensions of this model, 
# improving accuracy by combining multiple decision trees. 
# 4. Support Vector Machines (SVM): A supervised learning model used for classification 
# and regression analysis. SVMs aim to find the best possible boundary between classes. 
# 5. Neural Networks: Including models like Multi-layer Perceptrons (MLP), 
# Convolutional Neural Networks (CNN), and Recurrent Neural Networks (RNN), 
# these models are part of deep learning methods and are used for various tasks such as 
# image recognition, natural language processing, and time series analysis. 
# 6. Clustering Algorithms: K-means, hierarchical clustering, and DBSCAN are used to group 
# similar data points together based on certain characteristics. 
# 7. Naive Bayes: Based on Bayes’ theorem, this model is particularly useful for text 
# classification and sentiment analysis. 
# 8. Ensemble Methods: Techniques like Bagging (Bootstrap Aggregating), Boosting, and 
# Stacking combine multiple models to improve overall performance and reduce overfitting. 
# 9. Time Series Models: ARIMA (AutoRegressive Integrated Moving Average) and 
# LSTM (Long Short-Term Memory) are commonly used for analyzing and forecasting 
# time-dependent data. The choice of model depends on various factors like the nature of 
# the data, the problem being addressed (classification, regression, clustering, etc.), 
# the size of the dataset, and the desired outcome. Data scientists often experiment with 
# multiple models to find the one that best fits the particular task at hand.


# Load required library 
library(e1071) 
# Load the iris dataset 
data(iris) 
# Split the dataset into training and testing sets 
set.seed(123) 
train_index <- sample(1:nrow(iris), 0.7 * nrow(iris)) 
train_data <- iris[train_index, ] 
test_data <- iris[-train_index, ] 
# Train the Naive Bayes model 
nb_model <- naiveBayes(Species ~ ., data = train_data) 
# Make predictions on the test set 
predictions <- predict(nb_model, test_data) 
# Evaluate the accuracy of the model 
accuracy <- mean(predictions == test_data$Species) 
summary(predictions == test_data$Species)
print(paste("Accuracy of Naive Bayes model:", round(accuracy * 100, 2), "%"))
# 93.33 %

# Explanation: 1. Load Libraries: We use the e1071 library, which includes functions for Naive Bayes classification. 
# 2. Load Data: We load the built-in “iris” dataset, which contains measurements of iris flowers. 
# 3. Split Data: We split the dataset into training and testing sets using a 70-30 split. 
# 4. Train Model: The naiveBayes() function trains the Naive Bayes model using the training data, predicting the species of iris flowers 
# based on other features. 
# 5. Make Predictions: We use the trained model to predict the species of iris flowers in the test set. 
# 6. Evaluate Accuracy: The accuracy of the model is calculated by comparing the predicted species with the actual species in the test set. 
# This example demonstrates a simple implementation of a Naive Bayes classifier using the “iris” dataset in R for species classification.

# Visualizing the fit of a Naive Bayes model can be a bit challenging since Naive Bayes is a probabilistic classifier and doesn’t 
# inherently produce a visual representation like decision boundaries in some other models. However, we can visualize the predicted 
# probabilities or the confusion matrix to understand how well the model performs. Here’s an example of how you can visualize the 
# confusion matrix using
# Confusion Matrix 
conf_matrix <- table(predictions, test_data$Species) 
print("Confusion Matrix:") 
print(conf_matrix) 
# Visualizing Confusion Matrix 
library(ggplot2) 
conf_matrix_df <- as.data.frame(conf_matrix) 
ggplot(data = conf_matrix_df, aes(x = predictions, y = Var2, fill = Freq)) + 
  geom_tile() + 
  geom_text(aes(label = Freq), vjust = 1) + 
  labs(x = "Predicted", y = "Actual", fill = "Frequency") + 
  theme_minimal()
# This code generates a confusion matrix and visualizes it using a heatmap. The confusion matrix helps assess the performance of the model 
# by showing how often the predicted classes match the actual classes. Alternatively, you can also visualize the predicted probabilities for 
# each class using various plot types like histograms, density plots, or boxplots to understand how confident the model is in its predictions 
# for different classes. However, Naive Bayes might not provide direct probability estimates for visualizing due to its assumptions about the 
# conditional independence of features. Keep in mind that the visualization of a Naive Bayes model’s fit might not be as straightforward as for 
# some other models due to its probabilistic nature and assumptions. Visualizations like confusion matrices and probability distributions can still 
# provide insights into the model’s performance.



# Train SVM based model
svm_model <- svm(Species ~ ., data = train_data)
# Predict using the SVM model
predictions <- predict(svm_model, test_data)
# Evaluate the accuracy of the model 
accuracy <- mean(predictions == test_data$Species) 
summary(predictions == test_data$Species)
print(paste("Accuracy of SVM model:", round(accuracy * 100, 2), "%"))
# 91.11 %






