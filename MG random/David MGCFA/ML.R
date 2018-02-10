setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_MeaningInLife_Warner_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####
##accuracy##
##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:16], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,17)]
dontcolumn = replacepeople[ , c(1,2,17)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,17)], 
                    colMeans(nomiss[ , -c(1,2,17)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,17)], use="pairwise.complete.obs"))
mahal
summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,17)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,17)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,17)])

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

###Scoring
ML <- rowSums(noout[, c(3:16)])
ML

##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)
##data sets

overallmodel = 'ML =~ Q100_1 + Q100_2 + Q100_3 + Q100_4 + Q100_5 + Q100_6 + Q100_7 + Q100_8 +
      Q100_9 + Q100_10 + Q100_11 + Q100_12 + Q100_13 + Q100_14
'
overall.fit = cfa(model = overallmodel, 
                   data=nomissnop, 
                   meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

##random
overall.fit.r = cfa(overallmodel, 
                    data=random, 
                    meanstructure = TRUE)

summary(overall.fit.r, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)
##notrandom
overall.fit.nr = cfa(overallmodel, 
                     data=notrandom, 
                     meanstructure = TRUE)

summary(overall.fit.nr, 
        standardized=TRUE,
        rsquare=TRUE, 
        fit.measure = TRUE)
####Multigroup Testing####
library(semTools)
options(scipen = 999)
multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

## broke down on strict invariance 
partial = partialInvariance(multisteps, 
                            type = "residuals")
redidualsfree = partial$results
multisteps2 = measurementInvariance(overallmodel,  
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c("Q100_8~~Q100_8"))
                                    