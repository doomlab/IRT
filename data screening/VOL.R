setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_VOL_RR_RN_Paper.csv")
summary(master)

##No reverse coded items.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:21], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,22)]
dontcolumn = replacepeople[ , c(1,2,22)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,22)], 
                    colMeans(nomiss[ , -c(1,2,22)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,22)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,22)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,22)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,22)])

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
pos = rowSums(noout[,c("Q25_1", "Q25_2", "Q25_3", "Q25_4", "Q25_5", "Q25_6","Q25_7", "Q25_8", "Q25_9", "Q25_10", "Q25_11", "Q25_12","Q25_13")])
pos

#External 
Neg = rowSums(noout[,c("Q25_14", "Q25_15", "Q25_16", "Q25_17", "Q25_18", "Q25_19")])
Neg
