#First Sample R Program for Labor Market Analysis

#Install psych package
library(psych)

#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location

#Import Data
dat <- read.csv("ACS Data for LMA Project.csv", header=TRUE)  


#Print variable names on the screen
colnames(dat)

##Perform exploratory data analysis

#Look at the first 10 rows of data 
head(dat,10)

#display the internal structure of data
str(dat) 

#Generate min, max, mean, median, first and third quartiles for each variable
summary(dat)

#Generate histogram for select variable(s)
hist(dat$Age)

#Generate boxplot for select variable(s)
boxplot(dat$Age)

#Generate frequency bar graph for select variable(s)
counts <- table(dat$Educational.Attainment)
counts <- cbind(counts, c(4, 5, 8, 2, 6, 1, 7, 3))
barplot(counts[order(counts[ ,2])],main="Educational.Attainment", names.arg=c("NoHSD", "HSD", 
                                                                            "SC", "Assoc", "Bach", "MA", "Prof", "Doct"),srt = 45, cex.names = 0.65)

#Generate crosstabs for select pair of variables
xtabs(~ dat$Race.Ethnicity + dat$Educational.Attainment)

#Generate select correlation coefficients 
cor(dat$Age, dat$Earnings.Past.12.Months)

#Generate scatterplot(s)
plot(dat$Age, dat$Earnings.Past.12.Months)

##Select a subsample that satisfies your selection criteria

#For example, the code below removes all observations with missing values for age or hours
subsample <- na.omit(dat[,c("Age", "Usual.Weekly.Hours")]) #for your program, list all variables you plan to use

#The code below removes employed individuals with zero earnings 
#Note that this subsample is based on the full data set called dat but you may need to exclude observations with missing values 
#as well as observations with values outside of a certain range
employedwithpay <- subset(dat, Earnings.Past.12.Months > 0 & Employed == 1) #note the use of == rather than =

#Look at the first 10 rows of data in the employedwithpay subsample
head(employedwithpay,10)

##Generate and Round Descriptive Statistics Needed for Table 1
#Note: Your table will include all the variables used in your analysis

round( mean(employedwithpay[,"Earnings.Past.12.Months"]), 0)
median(employedwithpay[,"Earnings.Past.12.Months"])
round( sqrt(var(employedwithpay[,"Earnings.Past.12.Months"])), 0)
min(employedwithpay[,"Earnings.Past.12.Months"])
max(employedwithpay[,"Earnings.Past.12.Months"])


#=========#
#== END ==#
#=========#
