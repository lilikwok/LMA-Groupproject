# Multiple Regression 
# load library
library(readr)
library(tidyverse)
library(here)

#load data
data <- read_csv(here("ss16pca.csv"))


#tidy data
tidydata <- data %>%
  select(WAGP,WKHP,SEMP,SCHL,SEX,AGEP) %>%  # select the variables of interest
  mutate(WAGP = as.integer(WAGP),    # convert data type 
         WKHP = as.integer(WKHP),
         SEMP = as.integer(SEMP),
         SEX = as.factor(SEX),
         SCHL = as.factor(SCHL),
         AGEP = as.integer(AGEP),
         earnings = WAGP + SEMP) %>% # calculate earnings per hours 
  filter(AGEP > 16 & AGEP <=64 & WKHP >= 30) %>% # filter the working age
  rename(Wage = WAGP,
         Working_hours = WKHP,
         self_employ_earning = SEMP,
         education = SCHL,
         age = AGEP)

# as the range of earnings is too big, we pick up the range of data from 1% to 99%
tidydata <- subset(tidydata, earnings <= quantile(earnings, prob = 1 - 1/100,na.rm = TRUE)& 
                     earnings > quantile(earnings, prob = 1/100,na.rm = TRUE) )

# descriptive statistics                      
summary(tidydata)

# create dummies
df <- select(tidydata,earnings,education, SEX) %>%
  mutate( value_e = 1,
          value_s = 1,
          id=1:n()) %>%
  spread(key = education, value = value_e) %>% # convert education attainment into dummies
  spread(key= SEX, value_s)%>%  # convert gender into dummies
  rename(male = '1',female = '2') 
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
  select(-HS_diploma, -female, -c(03:26))

summary(model_variables)

# linear regression 
#In the state of California, how do earnings vary by educational attainment?

educ_earnings <- lm(earnings ~ male+Non_school+no_HS_diploma+ No_college_degree + associate+bachelor+
     master+professional+doctor ,data = model_variables)
summary(educ_earnings)

#Does the premium for higher education vary by gender?
# female 

female <- model_variables %>%
  filter(male == 0)
female_earnings <- lm(earnings ~ Non_school+no_HS_diploma+ No_college_degree + associate+bachelor+
                      master+professional+doctor ,data = female)
summary(female_earnings)

#male 
male <-  model_variables %>%
  filter(male == 1)
male_earnings <- lm(earnings ~ Non_school+no_HS_diploma+ No_college_degree + associate+bachelor+
                        master+professional+doctor ,data = male)
summary(male_earnings)
