setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_FulfillmentOfMeaning_RR_RN_Paper.csv")
summary(master)

##fix qualtrics coding that's incorrect 1-5 instead of 0-4
master[ , c(3:14)] = master[ , c(3:14)] - 1

##Reverse Code. 2, 4, 5, 7, 9 , 11
master[ , c(4, 6, 7, 9, 11, 13)] = 4 - master[ , c(4, 6, 7, 9, 11, 13)] 
summary(master)
##I'm on Smoko
####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:14], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,15)]
dontcolumn = replacepeople[ , c(1,2,15)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,15)], 
                    colMeans(nomiss[ , -c(1,2,15)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,15)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,15)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,15)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,15)])

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
FOM <- rowSums(noout[, c(3:14)])
summary(FOM)

library(lavaan)

nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

overallmodel = '
FOM =~ Q102_1 + Q102_2 + Q102_3 + Q102_4 + Q102_5 + Q102_6 + Q102_7 +
Q102_8 + Q102_9 + Q102_10 + Q102_11 + Q102_12
'

##fit for overall (excluding paper)
overall.fit = cfa(model = overallmodel, 
                  data = nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE) ##no heywood cases

##fit for random 
random.fit = cfa(overallmodel, 
                 data = random, 
                 meanstructure = TRUE)
summary(random.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
summary(notrandom.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

library(semTools)

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = 'Source', 
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

##Broke Down on Metric Invariance 

partial = partialInvariance(multisteps, 
                            type = "loadings")
##save only the results for easier viewing
loadinsfree = partial$results

multisteps2 = measurementInvariance(overallmodel, 
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c("FOM=~Q102_2") 
## Allow for question 2 to vary                    