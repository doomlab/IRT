setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_LifeEngagementTest.csv")
summary(master)

##Reverse Code. 
library(car)
master$Q94_1 = recode(master$Q94_1, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q94_3 = recode(master$Q94_3, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q94_5 = recode(master$Q94_5, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:8], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,9)]
dontcolumn = replacepeople[ , c(1,2,9)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,9)], 
                    colMeans(nomiss[ , -c(1,2,9)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,9)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,9)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,9)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,9)])

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
LET <- rowSums(noout[, c(3:8)])
LET


