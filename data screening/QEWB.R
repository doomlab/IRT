setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_QuestionnaireEudaimonic_RR_RN_Paper.csv")
summary(master)

##No reverse coded items.
library(car)
master$Q106_3 = recode(master$Q106_3, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_7 = recode(master$Q106_7, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_11 = recode(master$Q106_11, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_12 = recode(master$Q106_12, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_16 = recode(master$Q106_16, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_19 = recode(master$Q106_19, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_20 = recode(master$Q106_20, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:23], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,24)]
dontcolumn = replacepeople[ , c(1,2,24)]

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

mahal = mahalanobis(nomiss[ , -c(1:3)], 
                    colMeans(nomiss[ , -c(1:3)], na.rm = TRUE),
                    cov(nomiss[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:3)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=30)

##homogeneity and homoscedasticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
QWEB <- rowSums(noout[, c(1:21)])
QWEB
