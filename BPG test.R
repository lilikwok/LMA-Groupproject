# add marital status 
# BPG test 

#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))


# load library
library(readr)
library(tidyverse)
library(here)
library(lmtest) # for BPG test
library(sandwich) # robust standard

#load data
data <- read_csv(here("ss16pca.csv"))


#tidy data
tidydata <- data %>%
  select(WAGP,WKHP,SEMP,SCHL,SEX,AGEP,MAR) %>%  # select the variables of interest
  mutate(WAGP = as.integer(WAGP),    # convert data type 
         WKHP = as.integer(WKHP),
         SEMP = as.integer(SEMP),
         SEX = as.factor(SEX),
         SCHL = as.factor(SCHL),
         AGEP = as.integer(AGEP),
         MAR = as.factor(MAR),
         earnings = WAGP + SEMP) %>% # calculate earnings per hours 
  filter(AGEP > 16 & AGEP <=64 & WKHP >= 30) %>% # filter the working age
  rename(Wage = WAGP,
         Working_hours = WKHP,
         self_employ_earning = SEMP,
         education = SCHL,
         age = AGEP,
         marital = MAR)

# as the range of earnings is too big, we pick up the range of data from 1% to 99%
tidydata <- subset(tidydata, earnings <= quantile(earnings, prob = 1 - 1/100,na.rm = TRUE)& 
                     earnings > quantile(earnings, prob = 1/100,na.rm = TRUE) )

# descriptive statistics                      
summary(tidydata)

# create dummies
df <- select(tidydata,earnings,education, SEX,marital,age) %>%
  mutate( value_e = 1,
          value_s = 1,
          value_m = 1,
          id=1:n()) %>%
  spread(key = education, value = value_e) %>% # convert education attainment into dummies
  spread(key= SEX, value_s)%>%  # convert gender into dummies
  rename(male = '1',female = '2') %>%
  spread(key = marital, value = value_m) %>%# convert gender into dummies
  rename(married = '1',widowed = '2', divorced = '3', seperated = '4',never.married = '5')
  
df[is.na(df)] <- 0

summary(df)
# group dummies
model_variables <- df %>% 
  mutate(
    Non_school = df$`01`,
    no_HS_diploma = df$`02`+df$`03`+df$`04`+df$`05`+df$`06`+df$`07`+df$`08`+df$`09`+df$`10`+df$`11` + df$`12` +df$`13` + df$`14` + df$`15`,
    HS_diploma = df$`16`+df$`17`,
    No_college_degree = df$`18`+df$`19`,
    associate = df$`20`,
    bachelor = df$`21`,
    master = df$`22`,
    professional = df$`23`,
    doctor = df$`24`
  )%>%
  select(-HS_diploma, -female,-never.married, -c(04:27))
# select HS_diploma, female and never married as absent dummy  
summary(model_variables)

# linear regression 
#In the state of California, how do earnings vary by educational attainment?

# original model 
orignal.Equation <- lm(earnings ~ male + Non_school+no_HS_diploma+ No_college_degree + associate+bachelor+
                      master+professional+doctor,data = model_variables)
summary(orignal.Equation)



# Add Variables (marital, age, age^2)

add.Equation <- lm(earnings ~ male+age + I(age*age) + Non_school+no_HS_diploma
                   + No_college_degree + associate+bachelor+ master+professional
                   +doctor + married+ divorced + seperated +widowed ,data = model_variables)
s <- summary(add.Equation)
s

# BPG test (Heteroskedasticity)

bptest(add.Equation)
bptestequation = lm(residuals(add.Equation)*residuals(add.Equation) ~ male+age + I(age*age) + Non_school+no_HS_diploma+ 
                      No_college_degree + associate+bachelor+ master+professional+doctor + married+ divorced + 
                      seperated +widowed ,data = model_variables)
summary(bptestequation)

  # BP = 7108.1, df = 15, p-value < 2.2e-16, reject the H0
  # So the model has heteroskedasticity 



#Estimate Logarithmic Model with Age in Quadratic Form
LogEarnings.Equation =  lm(log(earnings,base = exp(1)) ~ male+age + I(age*age) + Non_school+no_HS_diploma
                           + No_college_degree + associate+bachelor+ master+professional
                           +doctor + married+ divorced + seperated +widowed ,data = model_variables)

summary(LogEarnings.Equation)

# BPG TEST AGAIN 

bptest(LogEarnings.Equation)
bptest.logequation = lm(residuals(LogEarnings.Equation)*residuals(LogEarnings.Equation) ~ male+age + I(age*age) + Non_school+no_HS_diploma+ 
                      No_college_degree + associate+bachelor+ master+professional+doctor + married+ divorced + 
                      seperated +widowed ,data = model_variables)
summary(bptest.logequation)

# data:  LogEarnings.Equation
#BP = 785.2, df = 15, p-value < 2.2e-16, reject H0

# Robust standard 

#Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(add.Equation, type = "HC") #the diagonal elements are the variances of the parameter estimates

#Generate the Robust standard errors and print them on screen 
sandwich_se <- diag(vcovHC(add.Equation, type = "HC"))^0.5
sandwich_se
# generate the robust standard errors and t - test 
coeftest(add.Equation, vcov = vcovHC(add.Equation))



