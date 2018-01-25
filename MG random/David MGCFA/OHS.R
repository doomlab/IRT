setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_OrientationToHappiness_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:20], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,21)]
dontcolumn = replacepeople[ , c(1,2,21)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,21)], 
                    colMeans(nomiss[ , -c(1,2,21)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,21)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,21)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,21)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,21)])

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

##Scored
Meaning <- rowMeans(noout[, c(4,7,13,14,16,19)])
Meaning

Engage <- rowMeans(noout[, c(5,10,15,17,18,20)])
Engage

Pleasure <- rowMeans(noout[, c(3,6,8,9,11,12)])
Pleasure

##CFA with internal + external + noout datasets / need to drop paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)
overallmodel = '
MN =~ Q81_2 + Q81_5 + Q81_11 + Q81_12 + Q81_14 + Q81_17 
EG =~ Q81_3 + Q81_8 + Q81_13 + Q81_15 + Q81_16 + Q81_18
PL =~ Q81_1 + Q81_4 + Q81_6 + Q81_7 + Q81_9 + Q81_10
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

fitMeasures(random.fit) ##Dr. B, is this line of code necessary?

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
summary(notrandom.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

##invariances 
library(semTools)
multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = 'Source', 
                                   strict = T)

fitmeasures(multisteps)
fitMeasures(multisteps$fit.configural)
fitMeasures(multisteps$fit.loadings)
fitMeasures(multisteps$fit.intercepts)
fitMeasures(multisteps$fit.residuals)
fitMeasures(multisteps$fit.means)

##Broke down at Scalar Level

##partials f/residuals
partial = partialInvariance(multisteps, 
                            type = "intercepts")

interceptssfree = partial$results
multisteps2 = measurementInvariance(overallmodel, 
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c(Q81_9~1))

##Allow for question 9 to vary 
