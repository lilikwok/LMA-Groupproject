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

# dummy race: 
# RACAIAN,American Indian and Alaska Native recode
# RACASN,Asian recode
# RACBLK,Black or African American recode
# RACNH,Native Hawaiian recode
# RACPI,Other Pacific Islander recode
# RACSOR,some other race
# RACWHT,white alone

#tidy data
tidydata <- data %>%
  select(WAGP,WKHP,SEMP,SCHL,SEX,AGEP,MAR,RACAIAN,RACASN,RACBLK,RACNH,RACPI,RACSOR,RACWHT) %>%  # select the variables of interest
  mutate(WAGP = as.integer(WAGP),    # convert data type 
         WKHP = as.integer(WKHP),
         SEMP = as.integer(SEMP),
         SEX = as.factor(SEX),
         SCHL = as.factor(SCHL),
         AGEP = as.integer(AGEP),
         MAR = as.factor(MAR),
         island = RACNH + RACPI,# combine hawaii and other island as a whole(island)
         island = ifelse(island == 0, 0, 1),
         earnings = WAGP + SEMP) %>% # calculate earnings per hours 
  filter(AGEP > 16 & AGEP <=64 & WKHP >= 30) %>% 
  select (-RACNH, -RACPI) %>%# filter the working age
  rename(Wage = WAGP,
         Working_hours = WKHP,
         self_employ_earning = SEMP,
         education = SCHL,
         age = AGEP,
         marital = MAR,
         Indian.Alaska = RACAIAN,
         Asian = RACASN,
         Black = RACBLK,
         other.race = RACSOR,
         White = RACWHT
         )
summary(tidydata)
# as the range of earnings is too big, we pick up the range of data from 1% to 99%
tidydata <- subset(tidydata, earnings <= quantile(earnings, prob = 1 - 1/100,na.rm = TRUE)& 
                     earnings > quantile(earnings, prob = 1/100,na.rm = TRUE) )

# descriptive statistics                      
summary(tidydata)

# create dummies
df <- select(tidydata,earnings,education, SEX,marital,age,Indian.Alaska,Asian,Black,other.race,White,island) %>%
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
  select(-HS_diploma, -female,-never.married,-other.race,-c(10:33))
# select HS_diploma, female, never married, other race as absent dummy
summary(model_variables)

# linear regression 
#In the state of California, how do earnings vary by educational attainment?

# original model 
orignal.Equation <- lm(earnings ~ male + Non_school+no_HS_diploma+ No_college_degree + associate+bachelor+
                      master+professional+doctor+,data = model_variables)
summary(orignal.Equation)



# Add Variables (marital, age, age^2)

add.Equation <- lm(earnings ~ male+age + I(age*age) + Non_school+no_HS_diploma
                   + No_college_degree + associate+bachelor+ master+professional
                   +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                   + Asian + Black+ White + island,data = model_variables)
summary(add.Equation)

# BPG test (Heteroskedasticity)

bptest(add.Equation)
bptestequation = lm(residuals(add.Equation)*residuals(add.Equation) ~ 
                      male+age + I(age*age) + Non_school+no_HS_diploma
                    + No_college_degree + associate+bachelor+ master+professional
                    +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                    + Asian + Black+ White + island,data = model_variables)
summary(bptestequation)

  # BP = 7279.2, df = 20, p-value < 2.2e-16, reject the H0
  # So the model has heteroskedasticity 



#Estimate Logarithmic Model with Age in Quadratic Form
LogEarnings.Equation =  lm(log(earnings,base = exp(1)) ~ male+age + I(age*age) + Non_school+no_HS_diploma
                           + No_college_degree + associate+bachelor+ master+professional
                           +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                           + Asian + Black+ White + island,data = model_variables)

summary(LogEarnings.Equation)

# BPG TEST AGAIN 

bptest(LogEarnings.Equation)
bptest.logequation = lm(residuals(LogEarnings.Equation)*residuals(LogEarnings.Equation) ~ 
                          male+age + I(age*age) + Non_school+no_HS_diploma
                        + No_college_degree + associate+bachelor+ master+professional
                        +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                        + Asian + Black+ White + island,data = model_variables)
summary(bptest.logequation)

# data:  LogEarnings.Equation
#BP = 870.75, df = 20, p-value < 2.2e-16, reject H0

# Linear regression Robust standard 

#Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(add.Equation, type = "HC") #the diagonal elements are the variances of the parameter estimates

#Generate the Robust standard errors and print them on screen 
sandwich_se <- sqrt(diag(vcovHC(add.Equation, type = "HC")))
sandwich_se
# generate the robust standard errors and t - test 
coeftest(add.Equation, vcov = vcovHC(add.Equation,type = "HC"))

# Logrithm regression Robust standard 

coeftest(LogEarnings.Equation, vcov = vcovHC(LogEarnings.Equation,type = "HC"))


# Seperation Equation 
# Male 
Male <- model_variables %>%
  filter(male == 1)
summary(Male)
male.equation <-  lm(earnings ~ age + I(age*age) + Non_school+no_HS_diploma
                                + No_college_degree + associate+bachelor+ master+professional
                                +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                                + Asian + Black+ White + island,data = Male)
summary(male.equation)
# BPG TEST 
bptest(male.equation)
male.bpgtestequation = lm(residuals(male.equation)*residuals(male.equation) ~ 
                    age + I(age*age) + Non_school+no_HS_diploma
                    + No_college_degree + associate+bachelor+ master+professional
                    +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                    + Asian + Black+ White + island,data = Male)
summary(male.bpgtestequation)
# LOG 
male.logequation = lm(log(earnings, base = exp(1) )~ age + I(age*age) + Non_school+no_HS_diploma
                          + No_college_degree + associate+bachelor+ master+professional
                          +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                          + Asian + Black+ White + island,data = Male)
summary(male.logequation)
# BPG TEST 
bptest(male.logequation)
male.bpgtestlogequation = lm(residuals(male.logequation)*residuals(male.logequation) ~ 
                            age + I(age*age) + Non_school+no_HS_diploma
                          + No_college_degree + associate+bachelor+ master+professional
                          +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                          + Asian + Black+ White + island,data = Male)
summary(male.bpgtestlogequation)
# robust standard error for linear regression for male 
coeftest(male.equation, vcov = vcovHC(male.equation,type = "HC"))
# robust standard error for log regression for male 
coeftest(male.logequation, vcov = vcovHC(male.logequation,type = "HC"))

# Female 
Female <- model_variables %>%
  filter(male == 0)
summary(Female)
female.equation <-  lm(earnings ~ age + I(age*age) + Non_school+no_HS_diploma
                     + No_college_degree + associate+bachelor+ master+professional
                     +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                     + Asian + Black+ White + island,data = Female)
summary(female.equation)
# BPG TEST 
bptest(female.equation)
female.bpgtestequation = lm(residuals(female.equation)*residuals(female.equation) ~ 
                            age + I(age*age) + Non_school+no_HS_diploma
                          + No_college_degree + associate+bachelor+ master+professional
                          +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                          + Asian + Black+ White + island,data = Female)
summary(female.bpgtestequation)
# LOG 
female.logequation = lm(log(earnings, base = exp(1) )~ age + I(age*age) + Non_school+no_HS_diploma
                      + No_college_degree + associate+bachelor+ master+professional
                      +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                      + Asian + Black+ White + island,data = Female)
summary(female.logequation)
# BPG TEST 
bptest(female.logequation)
female.bpgtestlogequation = lm(residuals(female.logequation)*residuals(female.logequation) ~ 
                               age + I(age*age) + Non_school+no_HS_diploma
                             + No_college_degree + associate+bachelor+ master+professional
                             +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                             + Asian + Black+ White + island,data = Female)
summary(female.bpgtestlogequation)
# robust standard error for linear regression for male 
coeftest(female.equation, vcov = vcovHC(male.equation,type = "HC"))
# robust standard error for log regression for male 
coeftest(female.logequation, vcov = vcovHC(male.logequation,type = "HC"))