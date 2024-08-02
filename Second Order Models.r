economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Print the first six rows
print("head")
head(economic, 6)

plot(economic$gdp, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and GDP",
     xlab = "GDP", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

plot(economic$inflation, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and Inflation",
     xlab = "Inflation", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

# Create the second order regression model and print the statistics
model1 <- lm(wage_growth ~ gdp + I(gdp^2), data=economic)
summary(model1)

newdata <- data.frame(gdp=1.70)

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Create the second order regression model and print the statistics
model2 <- lm(wage_growth ~ inflation + gdp + inflation:gdp + I(inflation^2) + I(gdp^2) , data=economic)
summary(model2)

newdata <- data.frame(inflation=2.1, gdp=1.70)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Create the second order regression model and print the statistics
model3 <- lm(wage_growth ~ inflation + economy + inflation:economy + I(inflation^2) + I(inflation^2):economy, data=economic)
summary(model3)

newdata <- data.frame(inflation=2.1, economy='recession')

print("prediction interval")
prediction_pred_int <- predict(model3, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model3, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Line that loads economic data set from economic.csv file
economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Line that prints the variables and data set
print("Variables")
sapply(economic, class)

print("dataset")
economic

# Line that shows number of rows in the data set from economic.csv file
print("Number of rows")
nrow(economic)

# Line that shows number of columns in the data set from economic.csv file
print("Number of columns")
ncol(economic)

# 3. Quadratic (Second Order) Model with One Quantitative Variable
# Correlation Analysis 
# Line that creates the scatterplot of wage growth and unemployment
plot(economic$wage_growth, economic$unemplyment,
     main = "Scatterplot of Wage Growth and Unemployment",
     xlab = "Unemployment", ylab = "Wage Growth",
     col="purple", 
     pch = 19)

# Reporting Results 
# Line that creates the quadratic second order regression model for wage growth using unemployment as the independent variable
print("Second order regression model for wage growth and unemployment")
secquad <- lm(wage_growth ~ unemployment + I(unemployment^2), data=economic)
summary(secquad)

# Making Predictions Using Model
# Using predicted wage growth if unemployment is 2.54
wgunemployment1 <- data.frame(unemployment = 2.54)

# Prediction interval for model 1
print("Prediction interval for wage growth")
prediction_pred_int <- predict(secquad, wgunemployment1, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

# Prediction interval for model 1
print("Confidence interval for wage growth")
prediction_conf_int <- predict(secquad, wgunemployment1, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)

# 4. Complete Second Order Model with Two Quantitative Variables
# Reporting Results 
# Line that creates the quadratic second order regression model for wage growth as the response variable, and unemployment and GDP growth as predictor variables
print("Second order regression model wage growth as the response variable, and unemployment and GDP growth as predictor variables")
secquad2 <- lm(wage_growth ~ unemployment + gdp + I(unemployment^2) + I(gdp^2) , data=economic)
summary(secquad2)

# Making Predictions Using Model 
# Line where predictions of wage growth if unemployment is 2.50 and GDP growth is 6.50 
wgunemploymentgdp <- data.frame(unemployment = 2.50, gdp = 6.50)

# Prediction interval for wage growth if unemployment is 2.50 and GDP growth is 6.50 
print("Prediction interval wage growth if unemployment is 2.50 and GDP growth is 6.50")
prediction_pred_int <- predict(secquad2, wgunemploymentgdp, interval = "predict", level = 0.95) 
round(prediction_pred_int, 4)

# Confidence interval for wage growth if unemployment is 2.50 and GDP growth is 6.50 
print("Confidence interval for wage growth if unemployment is 2.50 and GDP growth is 6.50")
prediction_conf_int <- predict(secquad2, wgunemploymentgdp, interval = "confidence", level = 0.95) 
round(prediction_conf_int, 4)

# 5. Complete Second Order Model with One Quantitative and One Qualitative Variable
# Reporting Results
# Line that creates the quadratic second order regression model for wage using using unemployment and economy as predictors.
print("Second order regression model wage growth as the response variable using unemployment and economy as predictor variables")
secquad3 <- lm(wage_growth ~ unemployment + economy + unemployment:economy + I(unemployment^2) + I(unemployment^2):economy , data=economic)
summary(secquad3)

# Making Predictions Using Model 
# Line where predictions of wage growth if unemployment is 2.50 and the economy is not in recession 
wgunemploymenteco <- data.frame(unemployment = 2.50, economy = 'no_recession')

# Prediction interval for wage growth if unemployment is 2.50 and the economy is not in recession 
print("Prediction interval wage growth if unemployment is 2.50 and the economy is not in recession")
prediction_pred_int <- predict(secquad3, wgunemploymenteco, interval = "predict", level = 0.95) 
round(prediction_pred_int, 4)

# Confidence interval for wage growth if unemployment is 2.50 and the economy is not in recession 
print("Confidence interval for wage growth if unemployment is 2.50 and the economy is not in recession")
prediction_conf_int <- predict(secquad3, wgunemploymenteco, interval = "confidence", level = 0.95) 
round(prediction_conf_int, 4)
