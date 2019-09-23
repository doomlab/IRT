setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_MeaningInLife_Steger_RR_RN_Paper.csv")
summary(master)

##Reverse Code. 9
library(car)
master$Q99_9 = recode(master$Q99_9, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:12], 1, percentmiss) 
table(missing)


##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)

replacecolumn = replacepeople[ , -c(1,2,13)]
dontcolumn = replacepeople[ , c(1,2,13)]

##no missing data.
nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,13)], 
                    colMeans(nomiss[ , -c(1,2,13)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,13)], use="pairwise.complete.obs"))
mahal
summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,13)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,13)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,13)])

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
PM <- rowSums(noout[, c(3,6,7,8,11)])
PM

SM<- rowSums(noout[, c(4,5,9,10,12)])
SM

