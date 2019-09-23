setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_ExpressionsOfLifeMeaning_RR_RN_Paper.csv")
summary(master)

##Reverse Code. No Reverse Coding.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:42], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,43)]
dontcolumn = replacepeople[ , c(1,2,43)]

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
ELM <- rowSums(noout[, c(4:43)])
summary(ELM)

library(lavaan)
##data sets
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

overallmodel = '
RS =~ Q108_1 + Q108_2 + Q108_3 + Q108_4 + Q108_5 + Q108_6 + Q108_7 + Q108_8 +
Q108_9 + Q108_10 + Q108_11 + Q108_12 + Q108_13 + Q108_14 + Q108_15 + Q108_16 +
Q108_17 + Q108_18 + Q108_19 + Q108_20 + Q108_21 + Q108_22 + Q108_23 + Q108_24 +
Q108_25 + Q108_26 + Q108_27 + Q108_28 + Q108_28 + Q108_29 + Q108_30 + Q108_31 +
Q108_32 + Q108_33 + Q108_34 + Q108_35 + Q108_36 + Q108_37 + Q108_38 + Q108_39 +
Q108_40' 


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

## It broke down at Scalar Invariance 

partial = partialInvariance(multisteps, 
                            type = "intercepts")
##save only the results for easier viewing
interceptsfree = partial$results

##click on the name, sort by FREE CFI 
##release the biggest one first

multisteps2 = measurementInvariance(overallmodel,  
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c(QQ108_19~1 , QQ108_15~1 ,
                                                        Q108_11~1, Q108_23~1 ,
                                                      Q108_9~1, Q108_16~1, Q108_2~1))

## Allow for question 19, 15, 11, 23, 9, 16, and 2 to vary
