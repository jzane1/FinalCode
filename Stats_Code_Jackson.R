# library(glmnet)
# library(dplyr)
# library(car)

# Reading the data
trainDat1 <- read.csv("2018Draft.csv")
trainDat2 <- read.csv("2022Draft.csv")

# Filter out undesired positions such as defensive or special teams players
positions <- c("WR", "RB", "QB")
trainDat1 <- filter(trainDat1, Pos %in% positions)
trainDat2 <- filter(trainDat2, Pos %in% positions)

# Selecting relevant columns 
trainDat1 = trainDat1[,c(-1,-3, -4, -5, -7, -8,-9,-12, -13, -14, -15, -16, -18, -19, -23, -25, -26, -27, -28, -29, -30)]
trainDat2 = trainDat2[,c(-1,-3, -4, -5, -7, -8,-9,-12, -13, -14, -15, -16, -18, -19, -23, -25, -26, -27, -28, -29, -30)]


# Removing rows with NA values
trainDat1 <- na.omit(trainDat1)
trainDat2 <- na.omit(trainDat2)

#Training the model on the previous years data
ols_model <- lm(Pick ~ ., data = trainDat1)

#Giving p.vals and importance of each predictor variable
summary(ols_model)


predictions <- predict(ols_model, newdata = trainDat2)
actuals <- trainDat2$Pick

#Testing different plots to use and implement in my poster.
errors <- actuals - predictions
plot(predictions, errors, main = "Prediction Error Plot",
     xlab = "Predicted Values", ylab = "Errors", pch = 19)
abline(h = 0, col = "red") 


#This was a method to discern which variables were collinear with each other
vif_result <- vif(ols_model)
print(vif_result)
