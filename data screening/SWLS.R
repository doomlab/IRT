setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_SatisfactionWithLife_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:7], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,8)]
dontcolumn = replacepeople[ , c(1,2,8)]

nomiss = replacepeople

##Small note: When MICE is run on this project the YEAR column
##is moved from the last column to the thrid column.
##This trend is not observed with data that does not need
##to be MICE'd. All code exluding ID columns has been corrected
##for this. 
#~Hannah 

##Outliers##
##Mahal

mahal = mahalanobis(nomiss[ , -c(1,2,8)], 
                    colMeans(nomiss[ , -c(1,2,8)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,8)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,8)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1,2,8)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1,2,8)])

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
SWLS <- rowSums(noout[, c(3:7)])
SWLS

