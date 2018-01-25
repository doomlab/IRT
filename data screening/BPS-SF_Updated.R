##Set working directory 
setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_BoredomPronenessShort-RR_RN_Paper.csv")
summary(master)

##No reverse coded items.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:14], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,15)]
dontcolumn = replacepeople[ , c(1,2,15)]

##MICE not needed here. 
nomiss = replacepeople

##Small note: When MICE is run on this project the YEAR column
##is moved from the last column to the thrid column.
##This trend is not observed with data that does not need
##to be MICE'd. All code exluding ID columns has been corrected
##for this. 
#~Hannah 

##Outliers##
##Mahal

mahal = mahalanobis(nomiss[ , -c(1,2,15)], 
                    colMeans(nomiss[ , -c(1,2,15)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,15)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1,2,15)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:2,15)])

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
#Internal 
internal = rowSums(noout[,c("Q84_1", "Q84_3", "Q84_5", "Q84_6", "Q84_8", "Q84_9")])
internal

#External 
external = rowSums(noout[,c("Q84_2", "Q84_4", "Q84_7", "Q84_10", "Q84_11", "Q84_12")])
external



