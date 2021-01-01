# install.packages("shinyjs")
# install.packages("igraph")
# install.packages("shinycssloaders")

library(shinyjs)
library(shiny)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(pls)
library(ggplot2)
#library(tabplot)  # WARNING:  This may not install as CRAN is withdrawing tabplot due to maintenance "issues"
library(visNetwork)
library(leaflet)
library(car)
library(shinycssloaders)
library(skimr)
#library(withSpinner)
library(rpart)
library(rpart.plot)
library(caret)
library(recipes)
library(recipes)
library(tree)
library(MASS)
library(ISLR)
library(randomForest)

a <- seq(1, 10,1)
a

missing_value_placeholders = c("","na","NA",-99,"--")
population_df <- read.csv("Ass2Data.csv", header=T, sep = ",", na.strings = missing_value_placeholders)

skim(population_df)
# Replace missing value placeholders with NA
population_df[population_df %in% missing_value_placeholders] <- NA

population_df %>% 
  write_csv("Ass2Data_missing_replace.csv")

# 0. Visualising the distribution of missing values

data(population_df)
d <- population_df
limit <- 1000
if (nrow(d) > limit) {
  d <- d[sample(1:nrow(d), limit),]
}
d <- d[order(d$COUNTRY, decreasing = FALSE),]
visdat::vis_dat(d)  #use vis_dat insteat of vis_miss

# 1. 
# The missing value in categary variables: GOVERNMENT and HEALTHCARE_BASIS;
# As to "GOVERNMENT", generally speaking, every country have a kind of government is a resonable assumption. The Type of government "LEFT","RIGHT","UNSTABLE","AUTOCRATIC" almost contain the exist govrnment type, according to my personal aview.
# The possiblity that a country doesn't have government is very small, so the missing value of GOVERNMENT is more likely to be "Not Available".
# As to "HEALTHCARE_BASIS", the list of "INSURANCE","PRIVATE", "FREE" almost contain all the type of healthcare system. If there is a country does not have healthcare system, it can be tread as "PRIVATE". 
# So the missing value of "HEALTHCARE_BASIS" is more likely to be "Not Available" rather then "Not Applicable".
# The conclusion is that, in this case, there is no categary variable need to change the missing value into a paticular "none" group.

# Define the data type of categary variables
population_df$HEALTHCARE_BASIS <- as.factor(population_df$HEALTHCARE_BASIS)
population_df$GOVERNMENT <- as.factor(population_df$GOVERNMENT)

# 2.
# I found that some missing value of "HEALTHCARE_COST" are “Not Applicable” because of the "Free" "HEALTHCARE_BASIS" type.
# So we need to replace them with number 0.

population_df$HEALTHCARE_COST <- ifelse(population_df$HEALTHCARE_BASIS == "FREE" & is.na(population_df$HEALTHCARE_COST),0,population_df$HEALTHCARE_COST) #Assign missing to zero

# 3. Excessively missing variables

# Define a function to calculates the ratio of missingness of a vector.
pMiss <- function(x){ sum(is.na(x))/length(x)*100 }

threshold <- 50

cRatio <- apply(X = population_df, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
# apply() function, MARGIN = 1, means the function will be applied over row, MARGIN = , means the function will be applied over column.
cat("Variables to remove:", paste(colnames(population_df)[cRatio >= threshold], collapse = ","))



# The missing ratio of variable "AGEMEDIAN" is exceed 50%, 
# but we need to further think about whether it is a very important variable which should not removed in the model.



# importance and correlations

# creat a subset contains the observations which donnot have missing values.
r_removedNA <- na.exclude(population_df)  # remove observations contain missing values


rf_train_model=randomForest(r_removedNA$DEATHRATE~.,data=r_removedNA, mtry=4, ntree=100, importance=TRUE)

varImpPlot(rf_train_model)   

# According to the random forest model output, "GOVERNMENT" and "POPDENSITY" are very import variable to predict the deathrate, which should not delet. 
# The variable "AGEMEDIAN" should be delect, because it is not the very importent variable and its missingness are expired 50%.

population_df_cr <- population_df[, cRatio<50]#Remove variable

#population_df$AGEMEDIAN <- NULL


# How low does threshold have to fall before it reports observations?---Threshold
# Threshold 1 threshold = 50%
threshold <- 50
rRatio <- apply(X = population_df_cr, MARGIN = 1, FUN = pMiss) 
cat("Observations to remove (First 50):", paste(rownames(population_df_cr)[rRatio >= threshold], collapse = ","))
population_df_r1 <- population_df_cr[rRatio <50,]

# Threshold 2 threshold = 45%
threshold <- 45
rRatio <- apply(X = population_df_cr, MARGIN = 1, FUN = pMiss) 
cat("Observations to remove (First 50):", paste(rownames(population_df_cr)[rRatio >= threshold], collapse = ","))
population_df_r2 <- population_df_cr[rRatio < 45,]

# Threshold 3 threshold = 30%
threshold <- 30
rRatio <- apply(X = population_df_cr, MARGIN = 1, FUN = pMiss) 
cat("Observations to remove (First 50):", paste(rownames(population_df_cr)[rRatio >= threshold], collapse = ","))
population_df_r3 <- population_df_cr[rRatio < 30,]


# vis::miss # From the row: The removed observations are in group. when we sheft the threshold. 
d <- population_df[order(d$COUNTRY, decreasing = FALSE),]

visdat::vis_dat(d)  #use vis_dat insteat of vis_miss

d <- population_df_cr[order(d$COUNTRY, decreasing = FALSE),]
visdat::vis_dat(d)  #use vis_dat insteat of vis_miss

d <- population_df_r1[order(d$COUNTRY, decreasing = FALSE),]
visdat::vis_dat(d)  #use vis_dat insteat of vis_miss

d <- population_df_r2[order(d$COUNTRY, decreasing = FALSE),]
visdat::vis_dat(d)  #use vis_dat insteat of vis_miss

d <- population_df_r3[order(d$COUNTRY, decreasing = FALSE),]

visdat::vis_dat(d)  #use vis_dat insteat of vis_miss



##EDA

## Missingness pattern
#Corrgram
m <- is.na(population_df_cr) + 0 # this is a trick that transforms Logical to Binary
cm <- colMeans(m)
m <- m[, cm > 0 & cm < 1, drop = FALSE] #remove none-missing or all-missing variables

corrgram::corrgram(cor(m), order = "OLO", abs = TRUE)
title(main = "Variable missing value correlation",
      sub = "Notice whether variables are missing in sets")


# ?corrgram::corrgram
# gg_miss
naniar::gg_miss_upset(population_df, nsets=15)
naniar::gg_miss_upset(population_df_r1, nsets=15)

boxplot

data_boxplot <- population_df_r1[,c(3:10, 12:13)]
car::Boxplot(formula=~.,data=data_boxplot, outlier.colour = "red", ylab = NA, las = 2,
             notch = FALSE, varwidth = FALSE,
             horizontal = FALSE,
            col = brewer.pal(n = dim(data_boxplot)[2], name = "RdBu"), 
            id = list(n=Inf, location="avoid"),
             main = "Boxplots of Ass2 Data")


#Using rpart tree to generate a variable imporce chart.
population_df$MISSINGNESS <- apply(X = is.na(population_df), MARGIN = 1, FUN = sum)
tree <- train(MISSINGNESS ~ . -COUNTRY, data = population_df, method = "rpart", na.action = na.rpart)
rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)

population_df_r1$MISSINGNESS <- apply(X = is.na(population_df_r1), MARGIN = 1, FUN = sum)
tree <- train(MISSINGNESS ~ . -COUNTRY, data = population_df_r1, method = "rpart", na.action = na.rpart)
rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)

population_df <- population_df[,-15]
population_df_r1 <- population_df_r1[,-14]
# From the chart we see that the variable "population", There are some parttens in the missingness, it suggests that it should be possible to predict the value that is goint to be missing.
# so the missingness type of the variables are not Missing Completely At Random(MCAR). 
# Accoring to the author, the missingness is not Missing Not At Random(MNAR). So the missingness is Missing At Random(MAR)
# Which means it is suitable to use imputation model to expect future missing data.



#?What's this population pattern about? Why it choose POPULATION.

# 5. Create a test - train split.
set.seed(1)
subIndex <- caret::createDataPartition(y = population_df_r1$DEATHRATE, p = 0.7, list = FALSE)

train <- population_df_r1[subIndex,]

test <- population_df_r1[-subIndex,]

### Develop a recipe-based processing pipeline. 

## Stratage 1: only use KNN without either step_center or step_scale

rec_0 <- recipes::recipe(DEATHRATE ~., data = train) %>%
  step_rm(COUNTRY) %>% #id is not a predictor
  step_naomit(DEATHRATE, skip=TRUE) %>%
  step_knnimpute(all_predictors(), neighbours = 5) %>%
  step_dummy(all_nominal())

## Stratage 2: Use KNN, step_center and step_scale with the order that step_center and step_scale follow KNN

rec <- recipes::recipe(DEATHRATE ~., data = train) %>%
  step_rm(COUNTRY) %>% #id is not a predictor
  step_naomit(DEATHRATE, skip=TRUE) %>%
  step_knnimpute(all_predictors(), neighbours = 5) %>%
  step_center(all_numeric(), -has_role("outcome")) %>%
  step_scale(all_numeric(), -has_role("outcome")) %>%
  step_dummy(all_nominal())


## Stratage 3: Shift the order, KNN comes after step_center and step_scale
rec_1 <- recipes::recipe(DEATHRATE ~., data = train) %>%
  step_rm(COUNTRY) %>% #id is not a predictor
  step_naomit(DEATHRATE, skip=TRUE) %>%
  step_center(all_numeric(), -has_role("outcome")) %>%
  step_scale(all_numeric(), -has_role("outcome")) %>%
  step_knnimpute(all_predictors(), neighbours = 5) %>%
  step_dummy(all_nominal())

## Stratage 3: Shift the order, KNN comes after step_center and step_scale
rec_2 <- recipes::recipe(DEATHRATE ~., data = train) %>%
  step_rm(COUNTRY) %>% #id is not a predictor
  step_naomit(DEATHRATE, skip=TRUE) %>%
  step_center(all_numeric(), -has_role("outcome")) %>%
  step_scale(all_numeric(), -has_role("outcome")) %>%
  step_dummy(all_nominal()) %>%
  step_knnimpute(all_predictors(), neighbours = 5)

## Training model
# Model_0 by using strategy 1
set.seed(10)
model_0 <- caret::train(rec_0, data = train, method = "glmnet")
plot(model_0)

# Model by using strategy 2
set.seed(10)
model <- caret::train(rec, data = train, method = "glmnet")
plot(model)

# Model_1 by using strategy 3
set.seed(10)
model_1 <- caret::train(rec_1, data = train, method = "glmnet")
plot(model_1)

set.seed(10)
model_2 <- caret::train(rec_2, data = train, method = "glmnet")
plot(model_2)
#### Performance of the models

# Using strategy 1 the model_0 performance with tain data
predict_deathrate_0<-predict(model_0, newdata = test)
residuals_0 <- test$DEATHRATE - predict_deathrate_0

rmse_test_0 = caret::RMSE(pred= predict_deathrate_0, obs = test$DEATHRATE)
rmse_test_0

# Using strategy 2 the model performance with tain data

predict_deathrate<-predict(model, newdata = test)
residuals <- test$DEATHRATE - predict_deathrate

rmse_test = caret::RMSE(pred= predict_deathrate, obs = test$DEATHRATE)
rmse_test

## Using strategy 3 the model performance with tain data
predict_deathrate_1<-predict(model_1, newdata = test)
residuals_1 <- test$DEATHRATE - predict_deathrate_1

rmse_test_1 = caret::RMSE(pred= predict_deathrate_1, obs = test$DEATHRATE)

rmse_test_1

## Using strategy 4 the model performance with tain data
predict_deathrate_2<-predict(model_2, newdata = test)
residuals_2 <- test$DEATHRATE - predict_deathrate_2

rmse_test_2 = caret::RMSE(pred= predict_deathrate_2, obs = test$DEATHRATE)

rmse_test_2

## lm residual

# The test data prediction performance by using strategy 1 model_0
test_residual_0 <- data.frame(id = test$COUNTRY, Y = test$DEATHRATE, Yhat = predict_deathrate_0)

test_residual_0$Residual <- test_residual_0$Y - test_residual_0$Yhat

rang <- range(c(test_residual_0$Y, test_residual_0$Yhat))

ggplot(test_residual_0) +
  geom_point(mapping = aes(x = Yhat, y = Y)) +
  geom_abline(slope=1, col = "blue") +
  labs(title = paste("Model with dependent X & Y outliers \n The MRSE of the test dataset is ", format(rmse_test_0,digits=3)), y = "actual", x = "predicted") +
  coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)

# The test data prediction performance by using strategy 2 model
test_residual <- data.frame(id = test$COUNTRY, Y = test$DEATHRATE, Yhat = predict_deathrate)

test_residual$Residual <- test_residual$Y - test_residual$Yhat
rang <- range(c(test_residual$Y, test_residual$Yhat))

ggplot(test_residual) +
  geom_point(mapping = aes(x = Yhat, y = Y)) +
  geom_abline(slope=1, col = "blue") +
  labs(title = paste("Model with dependent X & Y outliers \n The MRSE of the test dataset is ", format(rmse_test,digits=3)), y = "actual", x = "predicted") +
  coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)


# The test data prediction performance by using strategy 3 model_1
test_residual_1 <- data.frame(id = test$COUNTRY, Y = test$DEATHRATE, Yhat = predict_deathrate_1)

test_residual_1$Residual <- test_residual_1$Y - test_residual_1$Yhat
rang <- range(c(test_residual_1$Y, test_residual_1$Yhat))

ggplot(test_residual_1) +
  geom_point(mapping = aes(x = Yhat, y = Y)) +
  geom_abline(slope=1, col = "blue") +
  labs(title = paste("Model with dependent X & Y outliers \n The MRSE of the test dataset is ", format(rmse_test_1,digits=3)), y = "actual", x = "predicted") +
  coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)

# The test data prediction performance by using strategy 4 model_2
test_residual_2 <- data.frame(id = test$COUNTRY, Y = test$DEATHRATE, Yhat = predict_deathrate_2)

test_residual_2$Residual <- test_residual_2$Y - test_residual_2$Yhat
rang <- range(c(test_residual_2$Y, test_residual_2$Yhat))

ggplot(test_residual_2) +
  geom_point(mapping = aes(x = Yhat, y = Y)) +
  geom_abline(slope=1, col = "blue") +
  labs(title = paste("Model with dependent X & Y outliers \n The MRSE of the test dataset is ", format(rmse_test_2,digits=3)), y = "actual", x = "predicted") +
  coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
# ggplot(test_residual) +
#   geom_point(mapping = aes(x = Yhat, y = Y)) +
#   geom_abline(slope=1, col = "blue") +
#   labs(title = "Model with dependent X & Y outliers", y = "actual", x = "predicted") +
#   coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)


## Boxplot of the modelbased outliers
r1_residual = population_df_r1$DEATHRATE - predict(model, newdata = population_df_r1)

coef_0 <- 1.5  ##this is a high value to use.
ggplot(data = population_df_r1, mapping = aes(x = r1_residual)) +
  geom_boxplot(coef = coef_0, outlier.colour = "red") +
  labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef_0), x = "Residual") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())  


coef <- 2  ##this is a high value to use.
ggplot(data = population_df_r1, mapping = aes(x = r1_residual)) +
  geom_boxplot(coef = coef, outlier.colour = "red") +
  labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef), x = "Residual") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())  

coef_1 <- 2.5  ##this is a high value to use.
ggplot(data = population_df_r1, mapping = aes(x = r1_residual)) +
  geom_boxplot(coef = coef_1, outlier.colour = "red") +
  labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef_1), x = "Residual") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())  

coef_2 <- 2.6  ##this is a high value to use.
ggplot(data = population_df_r1, mapping = aes(x = r1_residual)) +
  geom_boxplot(coef = coef_2, outlier.colour = "red") +
  labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef_2), x = "Residual") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())  






