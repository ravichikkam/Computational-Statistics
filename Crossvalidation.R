#### Assignment 1
#### part 1 
## loading Deat.csv and building a regression model to predict the outcome

#### Load libraries and dataset ----------------------
library(tidyverse)
library(boot)
library(car)
library(quantreg)
library(class)
source("cv_function.R")

##### create a FUNCTION to reduce repetition during diagnostics


#load dataset
data <- read_csv("HtVol.csv")
head(data)
str(data)

#### standardizing your variables ----------------------

for(i in 4:ncol(data)) {
  #excluding columns 1, 2 and 3(HtVol, Male and CT)
  data[[i]] <- as.numeric(scale(data[[i]]))
}
head(data)

#### Exploratory Data Analysis on Deat.csv -------------------------
#understanding variable HtVol

plot_Htvol <- data %>% 
                    ggplot(mapping = aes(y = HtVol, x = seq_len(nrow(data)))) 
                    + geom_point() 
plot_Htvol
plot_Htvol + geom_smooth()

#plot of all variables against HtVol
plot_male <- data %>% 
  ggplot(mapping = aes(y = logHtVol, x =Male)) + geom_point() 
plot_male

plot_age <- data %>% 
  ggplot(mapping = aes(y = logHtVol, x = Age)) + geom_point() 
plot_age + geom_smooth()

plot_Ht <- data %>% 
  ggplot(mapping = aes(y = logHtVol, x = Ht)) + geom_point() 
plot_Ht + geom_smooth()

plot_Wt <- data %>% 
  ggplot(mapping = aes(y = logHtVol, x = Wt)) + geom_point() 
plot_Wt + geom_smooth()

plot_BMI <- data %>% 
  ggplot(mapping = aes(y = logHtVol, x = BMI)) + geom_point() + geom_smooth()
plot_BMI

plot_BSA <- data %>% 
  ggplot(mapping = aes(y = logHtVol, x = BSA)) + geom_point() 
plot_BSA + geom_smooth()

#### creating an initial model with the complete data -------------------

model0 <- glm(HtVol ~ . , data = data)
summary(model0)
# AIC = 674.91

## residual analysis ---------
hist(model0$residuals)
#appear to be near normal

plot(model0$fitted.values, model0$residuals)
abline(h = 0)
# acute case of hetroscadacity

cv_error <- cv.glm(data, model0, K = 10)$delta[1]

###### Part 1, Variant 1 ------------------------------------------------------

#model2 using the variables mentioned in the assignments

model2 <- glm(HtVol ~ Male + Age + Ht + Wt, data = data)
summary(model2)

#residual analysis
hist(model2$residuals, breaks = 10)
plot(model2$fitted.values, model2$residuals)
abline(h = 0)
# Presence of Hetroscedacity

# Transformation on the dependent variable -----------
data$logHtVol <-  log(data$HtVol)

# Recreating model2 after transforming HtVol
model3 <- glm(logHtVol ~ Male + Age + Ht + Wt, data = data)
summary(model3)

#residual analysis
hist(model3$residuals)
plot(model3$fitted.values, model3$residuals)
abline(h = 0)
# calculate RMSE
RMSE3 <-  sqrt(mean((model3$residuals)^2))
RMSE3
#0.919007
#calculating vif
vif(model3)
# presence of multicolinearity established

#### Step wise regression to avoid multicolinearity

model4 <- step(model3, direction = "forward")
summary(model4)
#model remained the same as moodel3

model5 <- step(model3, direction = "backward")
summary(model5)
#residual analysis
hist(model5$residuals, breaks = 10)
plot(model5$fitted.values, model5$residuals)
abline(h = 0)
# calculate RMSE
RMSE5 <- sqrt(mean((model5$residuals)^2))
RMSE5
#0.12067
#calculating vif
vif(model5)
# multicollinearity is absent hence a more stable model

#### models with interaction terms
model6 <- glm(logHtVol ~ Male + Male*Ht + Male*Wt + Ht + Wt, data = data)
summary(model6)

model7 <- glm(logHtVol ~ Male + Ht + Wt + Ht*Wt, data = data)
summary(model7)
#residual analysis
hist(model7$residuals, breaks = 10)
plot(model7$fitted.values, model7$residuals)
abline(h = 0)
# calculate RMSE
RMSE7 <- sqrt(mean((model7$residuals)^2))
RMSE7
#0.89530
#calculating vif
vif(model7)

# building models including polynomial terms
# based on the plots of predictors against logHtVol, looks like only Wt needs a polynomial term

model8 <- glm(logHtVol ~ Male + Ht + poly(Wt, 2), data = data)
summary(model8)
#residual analysis
hist(model8$residuals, breaks = 10)
plot(model8$fitted.values, model7$residuals)
abline(h = 0)
# calculate RMSE
RMSE8 <- sqrt(mean((model8$residuals)^2))
RMSE8
#0.9178089
#calculating vif
vif(model8)


#### perform CV among selected OLS models -----------
test1 <-  list(model5, model7, model8)
cv_test1 <- vector("numeric", length = length(test1))
for(i in seq_along(test1)) {
  cv_test1[i] <- cv.glm(data = data, test1[[i]], K = 10)$delta[1]
}
cv_test1
# we are choosing model5 as lest estimated test error

## Using LAD to estimate coefficients of model5

model5_LAD <- rq(logHtVol ~ Male + Ht + Wt, data = data)
summary(model5_LAD)
#residual analysis
hist(model5_LAD$residuals, breaks = 10)
plot(model5_LAD$fitted.values, model5_LAD$residuals)
abline(h = 0)
# calculate RMSE
RMSE5_LAD <- sqrt(mean((model5_LAD$residuals)^2))
RMSE5_LAD
#0.927680

##### Part 1, Variant 2 -------------------------------------------------------

# build an initial model against HtVol using variables like Male, Age, BMI, BSA

model9 <- glm(HtVol ~ Male + Age + BMI + BSA, data = data)
summary(model9)
#residual analysis
hist(model9$residuals, breaks = 10)
plot(model9$fitted.values, model9$residuals)
abline(h = 0)
# clear case of hetroscedacity

# building a new model using log tranformed variable of Ht Vol
model10 <- glm(logHtVol ~ Male + Age + BMI + BSA, data = data)
summary(model10)
#residual analysis
hist(model10$residuals, breaks = 10)
plot(model10$fitted.values, model10$residuals)
abline(h = 0)
# calculate RMSE
RMSE10 <- sqrt(mean((model10$residuals)^2))
RMSE10
#0.9638315
#calculating vif
vif(model10)
# vif of BSA is 10, which indicates very high level of multicolinearity

# performing step wise regression to eleiminate multicolinearity
model11 <- step(model10, direction = "forward")
summary(model11)
# no change observed in model11 from model 10

model12 <- step(model10, direction = "backward")
summary(model12)
#residual analysis
hist(model12$residuals, breaks = 10)
plot(model12$fitted.values, model12$residuals)
abline(h = 0)
# calculate RMSE
RMSE12 <- sqrt(mean((model12$residuals)^2))
RMSE12
#0.9764386
#calculating vif
vif(model12)

## building a model with interactive terms
model13 <- glm(logHtVol ~ BMI + BSA + BMI*BSA, data = data)
summary(model13)
#residual analysis
hist(model13$residuals, breaks = 10)
plot(model13$fitted.values, model13$residuals)
abline(h = 0)
# calculate RMSE
RMSE13 <- sqrt(mean((model13$residuals)^2))
RMSE13
#0.1280042
#calculating vif
vif(model13)

## introducing model with polynomial terms
## BMI is higly linearly correlated with logHtVol so we are not introducing any polynomial terms for BMI
## BSA has acute nonlinear relation with logHtVol, so we are introducing only 2nd order polynomial term

model14 <- glm(logHtVol ~ BMI + poly(BSA, 2), data = data)
summary(model14)
#residual analysis
hist(model14$residuals, breaks = 10)
plot(model14$fitted.values, model14$residuals)
abline(h = 0)
# calculate RMSE
RMSE14 <- sqrt(mean((model14$residuals)^2))
RMSE14
#0.9503129
#calculating vif
vif(model14)

#### CV amongst OLS models for part1, variant2 --------------
test2 <-  list(model12, model13, model14)
cv_test2 <- vector("numeric", length = length(test2))
for(i in seq_along(test2)) {
  cv_test2[i] <- cv.glm(data = data, test2[[i]], K = 10)$delta[1]
}
cv_test2
## the most basic model (model12) seems to have the best test error

## Using LAD to estimate coefficients of model12

model12_LAD <- rq(logHtVol ~ BMI + BSA, data = data)
summary(model12_LAD)
#residual analysis
hist(model12_LAD$residuals, breaks = 10)
plot(model12_LAD$fitted.values, model12_LAD$residuals)
abline(h = 0)
# calculate RMSE
RMSE12_LAD <- sqrt(mean((model12_LAD$residuals)^2))
RMSE12_LAD
#0.9933268

#### CV across best 4 models (model5, model5_LAD, model12, model12_LAD)---------

cv_model5 <- get_cv_error_lm(data = data, formula = model5$formula, 
                             dv = "logHtVol", K = 10)
cv_model5_LAD <- get_cv_error_rq(data = data, formula = model5_LAD$formula, 
                             dv = "logHtVol", K = 10)

cv_model12 <- get_cv_error_lm(data = data, formula = model12$formula, 
                             dv = "logHtVol", K = 10)
cv_model12_LAD <- get_cv_error_rq(data = data, formula = model12_LAD$formula, 
                                 dv = "logHtVol", K = 10)

##### Error rate Matrix--------------
error_rmse <- data.frame(Folds = 1:10, model5 = cv_model5$rmse, 
                        model5_LAD = cv_model5_LAD$rmse,
                        model12 = cv_model12$rmse,
                        model12_LAD = cv_model12_LAD$rmse)

error_rmse <- rbind(error_rmse, list("Mean", mean(error_rmse$model5),
                                              mean(error_rmse$model5_LAD),
                                              mean(error_rmse$model12),
                                              mean(error_rmse$model12_LAD)))

error_mae <- data.frame(Folds = 1:10, model5 = cv_model5$mae, 
                         model5_LAD = cv_model5_LAD$mae,
                         model12 = cv_model12$mae,
                         model12_LAD = cv_model12_LAD$mae)

error_mae <- rbind(error_mae, list("Mean", mean(error_mae$model5),
                                           mean(error_mae$model5_LAD),
                                           mean(error_mae$model12),
                                           mean(error_mae$model12_LAD)))

error_mdape <- data.frame(Folds = 1:10, model5 = cv_model5$mdape, 
                         model5_LAD = cv_model5_LAD$mdape,
                         model12 = cv_model12$mdape,
                         model12_LAD = cv_model12_LAD$mdape)

error_mdape <- rbind(error_mdape, list("Mean", mean(error_rmse$model5),
                                     mean(error_rmse$model5_LAD),
                                     mean(error_rmse$model12),
                                     mean(error_rmse$model12_LAD)))

##### Error Charts ------------

ggplot(data = error_rmse[-11, ]) + 
  geom_smooth(mapping = aes(x = 1:10, y = model5, color = "model5_OLS"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model5_LAD, color = "model5_LAD"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model12, color = "model12_OLS"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model12_LAD, color = "model12_LAD"),
              se = FALSE) + scale_x_continuous(breaks = c(1:10)) + 
  labs(x = "folds", y = "RMSE", title = "RMSE across each fold")

ggplot(data = error_mae[-11, ]) + 
  geom_smooth(mapping = aes(x = 1:10, y = model5, color = "model5_OLS"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model5_LAD, color = "model5_LAD"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model12, color = "model12_OLS"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model12_LAD, color = "model12_LAD"),
              se = FALSE) + scale_x_continuous(breaks = c(1:10)) + 
  labs(x = "folds", y = "MAE", title = "MAE across each fold")

ggplot(data = error_mdape[-11, ]) + 
  geom_smooth(mapping = aes(x = 1:10, y = model5, color = "model5_OLS"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model5_LAD, color = "model5_LAD"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model12, color = "model12_OLS"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:10, y = model12_LAD, color = "model12_LAD"),
              se = FALSE) + scale_x_continuous(breaks = c(1:10)) + 
  labs(x = "folds", y = "MDAPE", title = "MDAPE across each fold")

# Plot for mean errors

error_frame <- data.frame(RMSE_total = as.numeric(error_rmse[11, -1]), 
                          MAE_total = as.numeric(error_mae[11, -1]),
                          MDAPE_total = as.numeric(error_mdape[11, -1]))

ggplot(data = error_frame) + 
  geom_smooth(mapping = aes(x = 1:4, y = RMSE_total, color = "RMSE"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:4, y = MAE_total, color = "MAE"), 
              se = FALSE) + 
  geom_smooth(mapping = aes(x = 1:4, y = MDAPE_total, color = "MDAPE"), 
              se = FALSE) + 
  labs(y = "Test Error", x = "Models", title = "Mean error by cost function")

# error_plot_cummulative <- error_rmse %>% 
#   ggplot(mapping = aes(x = 1:4, y = error_rmse[11, -1])) + geom_point() + 
#   geom_line()
# 
# error_plot_cummulative

plot(c(1:4), error_rmse[11, -1])

##### Part 2, CV technique ----------------------------------------------------

death <- read_csv("Deat.csv")
head(death)
str(death)

#look for missing values
na <- vector("numeric", length = ncol(death))
for(i in seq_len(ncol(death))) {
  na[i] <- sum(is.na(death[[i]]))
}
na
# looks like there is a missing value in column 19
which(is.na(death[[19]]))
# row 25 of column 19 is NA. Lets replace it highest frequency of column 19

table(death[[19]])
# highest frequency is 0 
death[25, 19] <- 0

#### Logistic Regression -----------------------------
set.seed(123)
# fit a logistic regression model to predict Death by removing ID variable

class1 <- glm(Death ~ ., data = death[, -1], family = "binomial")
summary(class1)

# cross validation 
get_cv_logistic(data = death, formula = class1$formula, dv = "Death", K = 10)
#0.1293857

# model using step wise regression approach
class2 <- step(class1, direction = "forward")
summary(class2)
# no change observed in the model

class3 <- step(class1, direction = "backward")
summary(class3)
# looks like a better model with lower AIC, less and more significant features

get_cv_logistic(data = death, formula = class3$formula, dv = "Death", K = 10)
#0.1042058

## including the only significant factor
class4 <- glm(Death~ GFRDeterior , data = death[, -1], family = "binomial")
summary(class4)

get_cv_logistic(data = death[, -1], formula = test2$formula, dv = "Death", K = 10)
# 0.09217716


##### KNN --------------------------------------

# Before proceeding with KNN we need to standardize variables as KNN is pretty sensitive to it
death[["Age"]] <- as.numeric(scale(death[["Age"]]))
death[["MaxDiameter"]] <- as.numeric(scale(death[["MaxDiameter"]]))

set.seed(123)
class_knn1 <- knn.cv(death, death$Death, k = 1)
error1 <- mean(class_knn1 != death$Death)
error1
# 0.162037

# performing cv using knn along k from 13 to 17
cv_error <- vector("numeric", length = 17)
train_error <- vector("numeric", length = 17)
for(i in 1:17) {
  class <- knn.cv(death, death$Death, k = i)
  cv_error[i] <- mean(class != death$Death)
  
  class_train <- knn(death, death, death$Death, k = i)
  train_error[i] <- mean(class_train != death$Death)
}
error_df <- data.frame(k = 1:17, cv_error = cv_error, 
                       train_error = train_error)

## plot for train and test error
ggplot(error_df) + 
  geom_smooth(mapping = aes(x = 1:17, y = cv_error, color = "cv_error"), 
              se = FALSE) +
  geom_smooth(mapping = aes(x = 1:17, y = train_error, color = "train_error"), 
              se = FALSE) + scale_x_continuous(breaks = c(1:17)) +
  labs(x = "K Value", y = "Error", title = "Errors across K values")

## Cross validtion error seems to be stabilised at K= 6 which is 0.11111
set.seed(123)
#### Part 2, Validation set approach ------------------------------------------
# Split 1: train = 80 % and test =  20%

rows <- c(1:nrow(death))
select <- sample(rows, round(0.8 * nrow(death)))
death_train <- death[select, ]
death_test <- death[-select, ]

# use class4 logistic regression model built earlier 
validation_model_glm1 <- glm(class4$formula, family = "binomial", 
                            data = death_train)
test_glm1 <- predict(validation_model_glm1, newdata = death_test, 
                    type = "response")
validation_error_glm1 <- mean(death_test$Death != (test_glm1 >= 0.5)) 
validation_error_glm1
#0.06976

# use knn model with k = 6
validation_model_knn1 <- knn(death_train, death_test, death_train$Death, k = 6)
validation_error_knn1 <- mean(death_test$Death != validation_model_knn1)
validation_error_knn1
#0.06976

table(death_test$Death)
#baseline = 0.06976

# Split 2: train = 60 % and test =  40%
set.seed(123)
rows <- c(1:nrow(death))
select <- sample(rows, round(0.6 * nrow(death)))
death_train <- death[select, ]
death_test <- death[-select, ]

# use class3 logistic regression model built earlier 
validation_model_glm2 <- glm(class4$formula, family = "binomial", 
                             data = death_train)
test_glm2 <- predict(validation_model_glm2, newdata = death_test, 
                     type = "response")
validation_error_glm2 <- mean(death_test$Death != (test_glm2 >= 0.5)) 
validation_error_glm2
#0.08139535

# use knn model with k = 6
validation_model_knn2 <- knn(death_train, death_test, death_train$Death, k = 6)
validation_error_knn2 <- mean(death_test$Death != validation_model_knn2)
validation_error_knn2
#0.08139535

table(death_test$Death)
#0.093023


# Split 3: train = 50 % and test =  50%
set.seed(123)
rows <- c(1:nrow(death))
select <- sample(rows, round(0.5 * nrow(death)))
death_train <- death[select, ]
death_test <- death[-select, ]

# use class3 logistic regression model built earlier 
validation_model_glm3 <- glm(class4$formula, family = "binomial", 
                             data = death_train)
test_glm3 <- predict(validation_model_glm3, newdata = death_test, 
                     type = "response")
validation_error_glm3 <- mean(death_test$Death != (test_glm3 >= 0.5)) 
validation_error_glm3
#0.08333333

# use knn model with k = 6
validation_model_knn3 <- knn(death_train, death_test, death_train$Death, k = 6)
validation_error_knn3 <- mean(death_test$Death != validation_model_knn3)
validation_error_knn3
#00.1018519

table(death_test$Death)
#0.1018519


