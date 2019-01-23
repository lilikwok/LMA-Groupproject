# load library
library(readr)
library(tidyverse)

#load data
data <- read_csv("~/ECON 5300/ss16pca.csv")
summary(data)


#tidy data
tidydata <- data %>%
  select(WAGP,WKHP,SEMP,SCHL,SEX,AGEP) %>%  # select the variables of interest
  mutate(WAGP = as.integer(WAGP),    # convert data type 
         WKHP = as.integer(WKHP),
         SEMP = as.integer(SEMP),
         SEX = as.factor(SEX),
         SCHL = as.factor(SCHL),
         AGEP = as.integer(AGEP),
         earnings = WAGP + SEMP,
         unitearnings = earnings / (WKHP*52)) %>% # calculate earnings per hours 
  filter(AGEP > 16 & AGEP <=64) %>% # filter the working age
  rename(Wage = WAGP,
         Working_hours = WKHP,
         self_employ_earning = SEMP,
         education = SCHL,
         age = AGEP)

# as the range of unit earnings is too big, we pick up the range of data from 1% to 99%
tidydata <- subset(tidydata, unitearnings <= quantile(unitearnings, prob = 1 - 1/100,na.rm = TRUE)& 
                     unitearnings > quantile(unitearnings, prob = 1/100,na.rm = TRUE) )

# descriptive statistics                      
summary(tidydata)

# boxplot
boxplot(tidydata$unitearnings)

# examine the first 10 or 20 observations (rows of data) corresponding to variables of interest (columns) and compare the observed values to the data dictionaryLinks to an external site.for person records
head(tidydata,10)

# compute and examine descriptive statistics including the minimum, maximum, mean, and median for quantitative variables of interest
quantitative <- select(tidydata,wage,Working_hours,self_employ_earning,age,unitearnings) 
summary(quantitative)

# generate and examine histograms for quantitative variables of interest
par(mfrow = c(2, 3))
hist(tidydata$Wage,main = "Histogram of Wage")
hist(tidydata$Working_hours,main = "Histogram of Working Hours")
hist(tidydata$self_employ_earning,main = "Histogram of Self Employment Earnings")
hist(tidydata$age,main = "Histogram of age")
hist(tidydata$unitearnings,main = "Histogram of unit earnings")

# generate and examine bar charts/graphs for qualitative variables of interest

par(mfrow = c(1, 2))
barplot(table(tidydata$education),main= "Education attainment Distribution",horiz = TRUE, 
        las = 2,xlab = "Population",ylab = "Eduation attainment catagories")
barplot(table(tidydata$SEX),main= "Sex Distribution", xlab = "Gender",
        names.arg = c("Male", "Female"),ylim =c(0,100000),las = 1)


# pick up the useful column we want 
# convert education attainment into dummy variables. 
interest_Variables <- select(tidydata,unitearnings, education, SEX) %>%
  mutate( value_e = 1,
          value_s = 1,
          id=1:n()) %>%
  spread(key = education, value = value_e) %>% # convert education attainment into dummies
  spread(key= SEX, value_s)%>%  # convert gender into dummies
  rename(m = '1',f = '2') # m represent male, f represent female

summary(interest_Variables)
  
# one concern: should we combine all the group level to this education attainment 




