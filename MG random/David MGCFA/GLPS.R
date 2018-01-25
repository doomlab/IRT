setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_GeneralLifePurposeScale_RR_RN_Paper.csv")
summary(master)

##Reverse Code. 5 8 and 13
master[ , c(7, 10, 15)] =  8 - master[ , c(7, 10, 15)]

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:17], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,18)]
dontcolumn = replacepeople[ , c(1,2,18)]

nomiss = replacepeople
##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,18)], 
                    colMeans(nomiss[ , -c(1,2,18)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,18)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,18)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,18)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,18)])

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
GLPS <- rowSums(noout[, c(3:17)])
summary(GLPS)

##CFA
library(lavaan)

nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

overallmodel = '
GLPS =~ Q89_1 + Q89_2 + Q89_3 + Q89_4 + Q89_5 + Q89_6 + Q89_7
+ Q89_8 + Q89_9 + Q89_10 + Q89_11 + Q89_12 + Q89_13 + Q89_14 + Q89_15
'

##fit for overall 
overall.fit = cfa(model = overallmodel, 
                  data = nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE) 

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

##It broke down on the Strict Invariance level

partial = partialInvariance(multisteps, 
                            type = "residuals")

##save only the results for easier viewing
residualsfree = partial$results

##click on the name, sort by FREE CFI
##release the biggest one first

multisteps2 = measurementInvariance(overallmodel, 
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c(Q89_3~~Q89_3))
##Removing question 3 did it. 

