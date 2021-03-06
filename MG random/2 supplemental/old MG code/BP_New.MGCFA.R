##libraries
library(lavaan)
library(semPlot)
library(semTools)

##Set working directory 
setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_BoredomProneness-_RR_RN_Paper.csv")
summary(master)

##Rachel Working Directory##
master <- read_csv("~/Desktop/Meaning_Scales_BoredomProneness-_RR_RN_Paper.csv")

##Reverse Code
##in the qualtrics true = 1, false = 2
##recode the false ones so that all 1s get 1 point, 
##all 2s should be coded as zero points
library(car)
master$Q83_1 = recode(master$Q83_1, "1='2'; 2='1'")
master$Q83_7 = recode(master$Q83_7, "1='2'; 2='1'")
master$Q83_8 = recode(master$Q83_8, "1='2'; 2='1'")
master$Q83_11 = recode(master$Q83_11, "1='2'; 2='1'")
master$Q83_13 = recode(master$Q83_13, "1='2'; 2='1'")
master$Q83_15 = recode(master$Q83_15, "1='2'; 2='1'")
master$Q83_18 = recode(master$Q83_18, "1='2'; 2='1'")
master$Q83_22 = recode(master$Q83_22, "1='2'; 2='1'")
master$Q83_23 = recode(master$Q83_23, "1='2'; 2='1'")
master$Q83_24 = recode(master$Q83_24, "1='2'; 2='1'")

##Make everything 0 and 1 to add up correctly
##true is 1, false is 2, so subtract 2 
master[ , 3:30] = 2 - master[ , 3:30]

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:30], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,31)]
dontcolumn = replacepeople[ , c(1,2,31)]

##MICE
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##(By your powers) Combine (I am Captain Planet!)
filledin_none = cbind(dontcolumn, nomiss)
summary(filledin_none)

##Small note: When MICE is run on this project the YEAR column
##is moved from the last column to the third column.
##This trend is not observed with data that does not need
##to be MICE'd. All code exluding ID columns has been corrected
##for this. 
#~Hannah 

##Outliers##
##Mahal

mahal = mahalanobis(filledin_none[ , -c(1:3)], 
                    colMeans(filledin_none[ , -c(1:3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:3)])

##Linearity plot
##Create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##Multivariate normality
hist(standardized, breaks=15)

##Homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
##This scale is true/false. The number of True and False for each question quantified below.
score = rowSums(noout[ , 4:31])
summary(score)

##Factoring source##
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

####overall cfa everyone together##
overallmodel = '
BP =~ Q83_1 + Q83_2 + Q83_3 + Q83_4 + Q83_5 + Q83_6 + Q83_7 + Q83_8 + 
      Q83_9 + Q83_10 + Q83_11 + Q83_12 + Q83_13 + Q83_14 + Q83_15 + 
      Q83_16 + Q83_17 + Q83_18 + Q83_19 + Q83_20 + Q83_21 + Q83_22 + 
      Q83_23 + Q83_24 + Q83_25 + Q83_26 + Q83_27 + Q83_28
'

overall.fit = cfa(overallmodel, 
                  data=nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

####separate group models####
##notrandom
overall.fit.nr = cfa(overallmodel, 
                    data=notrandom, 
                    meanstructure = TRUE)

summary(overall.fit.nr, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

##random
overall.fit.r = cfa(overallmodel, 
                    data=random, 
                    meanstructure = TRUE)

summary(overall.fit.r, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)


####multi group testing####
###measurement invariance
options(scipen = 999)

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

