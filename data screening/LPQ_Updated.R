setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_LifePurposeQuestionnaire_RR_RN_Paper.csv")
summary(master)

##Reverse Code. True = 1, False = 2
##1 Point for True: 3, 4, 6, 7, 10, 13, 17, 18, 20; 
master[ , c(5,6,8,9,12,15,19,20,22)] = 2 - master[ , c(5,6,8,9,12,15,19,20,22)]
##1 Point for False: 1, 2, 5, 8, 9, 11, 12, 14, 15, 16, 19
master[ , c(3,4,7,10,11,13,14,16,17,18,21)] = master[ , c(3,4,7,10,11,13,14,16,17,18,21)] - 1
master$Q3_16[master$Q3_16 > 1 ] = NA 
summary(master)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:22], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,23)]
dontcolumn = replacepeople[ , c(1,2,23)]

##let's mice it!
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##put everything back together
filledin_none = cbind(dontcolumn, nomiss)
summary(filledin_none)

##Outliers##
mahal = mahalanobis(filledin_none[ , -c(1:3)], 
                    colMeans(filledin_none[ , -c(1:3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1:3)], use="pairwise.complete.obs"))
mahal
summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:3)])

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
LPQ <- rowSums(noout[, c(4:23)])
LPQ

