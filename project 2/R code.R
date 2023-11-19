df=read.csv(file.choose(), header = T)

df$species[df$species=='Iris-setosa']<-0
df$species[df$species=='Iris-versicolor']<-1
df$species[df$species=='Iris-virginica']<-2
df$species <- as.numeric(df$species)

#To identify the no. of missing values 
summary(df)

#outlier analysis 
boxplot(df$sepal_length,main="boxplot of sepal length")
boxplot(df$sepal_width,main="boxplot of sepal width")
boxplot(df$petal_length,main="boxplot of petal length")
boxplot(df$petal_width,main="boxplot of petal width")

quant<-quantile(df$sepal_width,probs = c(0.25,0.75),na.rm = T)
quant

outlier<- 1.5*IQR(df$sepal_width)

df[df$sepal_width< quant[1]-outlier,]
df[df$sepal_width> quant[2]+outlier,]

df$sepal_width[df$sepal_width< quant[1]-outlier]<- quant[1]
df$sepal_width[df$sepal_width> quant[2]+outlier]<-quant[2]
df[33:34,]

library(ggplot2)
pl<-ggplot(data=df,aes(x=sepal_width,y=petal_length))
print(pl+geom_point(aes(color=factor(species))))

library(randomForest)
library(caret)
set.seed(123)
train_indices <-createDataPartition(df$species,p = 0.7, list = F)
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Train the Random Forest model
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 500)

# Make predictions on the test set
predictions <- predict(rf_model, test_data)

# Evaluate the model
confusion_matrix <- table(predictions, test_data$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Display the confusion matrix and accuracy
print("Confusion Matrix:")
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
