setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_MeaningMakingScale_RR_RN_Paper.csv")
summary(master)

##Reverse Code for number 3
library(car)
master$Q21_3 = recode(master$Q21_3, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:9], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,10)]
dontcolumn = replacepeople[ , c(1,2,10)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,10)], 
                    colMeans(nomiss[ , -c(1,2,10)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,10)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,10)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,10)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,10)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
MMS <- rowSums(noout[, c(3:9)])
MMS


