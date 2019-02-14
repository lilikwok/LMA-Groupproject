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
library(stargazer) # beautiful format

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

##################################################################################

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
         other.race = RACNH + RACPI + RACSOR,# Other race(include island and other race)
         other.race = ifelse(other.race == 0, 0, 1),
         earnings = WAGP + SEMP) %>% # calculate earnings per hours 
  filter(AGEP > 16 & AGEP <=64 & WKHP >= 30) %>% 
  select (-RACNH, -RACPI, -WAGP,-WKHP,-SEMP,-RACSOR) %>%# filter the working age
  rename(
         education = SCHL,
         age = AGEP,
         marital = MAR,
         Indian.Alaska = RACAIAN,
         Asian = RACASN,
         Black = RACBLK,
         White = RACWHT
  ) 
summary(tidydata)
# as the range of earnings is too big, we pick up the range of data from 1% to 99%
tidydata <- subset(tidydata, earnings <= quantile(earnings, prob = 1 - 1/100,na.rm = TRUE)& 
                     earnings > quantile(earnings, prob = 1/100,na.rm = TRUE) )



# create dummies
df <- select(tidydata,earnings,education, SEX,marital,age,Indian.Alaska,Asian,Black,other.race,White) %>%
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
  select(-HS_diploma, -female,-never.married,-other.race,-c(9:32))
# select HS_diploma, female, never married, other race as absent dummy
# descrptive statistics
summary(model_variables)
stargazer(model_variables, type = "text", title="Descriptive statistics", 
          digits=1, out="Descriptive statistics.txt")

#######################################################################
# OLS
#In the state of California, how do earnings vary by educational attainment?


#Estimate Logarithmic Model with only education 
orignal.Equation <- lm(log(earnings,base = exp(1)) ~ male + Non_school+no_HS_diploma+ No_college_degree + associate+bachelor+
                         master+professional+doctor,data = model_variables)
summary(orignal.Equation)
bptest(orignal.Equation)

coeftest(orignal.Equation, vcov = vcovHC(orignal.Equation,type = "HC"))

orignal.se <- sqrt(diag(vcovHC(orignal.Equation,type = "HC")))
#Estimate Logarithmic Model with Age in Quadratic Form
LogEarnings.Equation =  lm(log(earnings,base = exp(1)) ~ male+age + I(age*age) + Non_school+no_HS_diploma
                           + No_college_degree + associate+bachelor+ master+professional
                           +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                           + Asian + Black+ White ,data = model_variables)
summary(LogEarnings.Equation)

# BPG TEST AGAIN 

bptest(LogEarnings.Equation)

# data:  LogEarnings.Equation
#BP = 870.75, df = 20, p-value < 2.2e-16, reject H0


# Logrithm regression Robust standard 

coeftest(LogEarnings.Equation, vcov = vcovHC(LogEarnings.Equation,type = "HC"))
log.se <- sqrt(diag(vcovHC(LogEarnings.Equation,type = "HC")))
#output beautiful format

stargazer(orignal.Equation,LogEarnings.Equation,
          se = list(orignal.se,log.se),
          title = "Table 2 /n Estimated Log (Earnings) Equations",
          type = "text", out = "model3.txt")
##################################################################

# Seperation Equation 
# Male 
Male <- model_variables %>%
  filter(male == 1)
summary(Male)

# LOG 
male.logequation = lm(log(earnings, base = exp(1) )~ age + I(age*age) + Non_school+no_HS_diploma
                      + No_college_degree + associate+bachelor+ master+professional
                      +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                      + Asian + Black+ White ,data = Male)
summary(male.logequation)
# BPG TEST 
bptest(male.logequation)

# robust standard error for log regression for male 
male.model <- coeftest(male.logequation, vcov = vcovHC(male.logequation,type = "HC"))
male.se <- sqrt(diag(vcovHC(male.logequation,type = "HC")))
# Female 
Female <- model_variables %>%
  filter(male == 0)
summary(Female)

# LOG 
female.logequation = lm(log(earnings, base = exp(1) )~ age + I(age*age) + Non_school+no_HS_diploma
                        + No_college_degree + associate+bachelor+ master+professional
                        +doctor + married+ divorced + seperated +widowed+Indian.Alaska
                        + Asian + Black+ White ,data = Female)
summary(female.logequation)
# BPG TEST 
bptest(female.logequation)

# robust standard error for log regression for male 
female.model <- coeftest(female.logequation, vcov = vcovHC(female.logequation,type = "HC"))
female.se <- sqrt(diag(vcovHC(female.logequation,type = "HC")))
# table 

stargazer(male.logequation ,female.logequation,
          se = list(male.se,female.se),
          type = "text",
          title = "Table 3. Estimated Log (Earnings) Equations by Gender",
          out = "gender.txt",
          column.labels = c("Male","Female"),align = TRUE)