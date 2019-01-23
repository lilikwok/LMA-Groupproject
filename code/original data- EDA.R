
# load library
library(readr)
library(tidyverse)

#load data
data <- read_csv("ss16pca.csv")
summary(data)

#original_data
original_data <- data %>%
  select(WAGP,WKHP,SEMP,SCHL,SEX,AGEP) %>%  # select the variables of interest
  mutate(WAGP = as.integer(WAGP),    # convert data type 
         WKHP = as.integer(WKHP),
         SEMP = as.integer(SEMP),
         SEX = as.factor(SEX),
         SCHL = as.factor(SCHL),
         AGEP = as.integer(AGEP),
         earnings = WAGP + SEMP) %>%
  rename(Wage = WAGP,
         Working_hours = WKHP,
         self_employ_earning = SEMP,
         education = SCHL,
         age = AGEP)
summary(original_data)

quantitative <- select(original_data,Wage,Working_hours,self_employ_earning,age,earnings) 
summary(quantitative)

# generate and examine histograms for quantitative variables of interest
par(mfrow = c(2, 3))
hist(original_data$Wage,main = "Histogram of Wage")
hist(original_data$Working_hours,main = "Histogram of Working Hours")
hist(original_data$self_employ_earning,main = "Histogram of Self Employment Earnings")
hist(original_data$age,main = "Histogram of age")
hist(tidydata$earnings,main = "Histogram of earnings")

# generate and examine bar charts/graphs for qualitative variables of interest

par(mfrow = c(1, 2))
barplot(table(original_data$education),main= "Education attainment Distribution",horiz = TRUE, 
        las = 2,xlab = "Population",ylab = "Eduation attainment catagories")
barplot(table(original_data$SEX),main= "Sex Distribution", xlab = "Gender",
        names.arg = c("Male", "Female"),las = 1)

# generate and examine cross tabulations, scatterplots, and/or correlation coefficients of interest


# How education affect earnings: 
p <- xtabs(original_data$earnings~original_data$education)
p
barplot(p,main= "How education affect earnings?",
        horiz = TRUE, 
        las = 2,
        beside = TRUE,
        adj = 0
        )
#
addmargins(prop.table(xtabs(original_data$earnings~original_data$education)))
mc_data <- quantitative[,1:length(quantitative)]
round(cor(mc_data,use ="complete.obs" ),2)
plot(quantitative$earnings,quantitative$Working_hours)

