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
pMiss <- function(x){ sum(is.na(x))/length(x)*100 }


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


#Update on user change



