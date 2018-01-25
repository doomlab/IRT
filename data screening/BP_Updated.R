##Set working directory 
setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_BoredomProneness-_RR_RN_Paper.csv")
summary(master)

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
##is moved from the last column to the thrid column.
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
score = rowSums(noout[ , 4:31])
summary(score)
