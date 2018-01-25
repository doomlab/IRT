setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_SatisfactionWithLife_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:7], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,8)]
dontcolumn = replacepeople[ , c(1,2,8)]

nomiss = replacepeople

##Small note: When MICE is run on this project the YEAR column
##is moved from the last column to the thrid column.
##This trend is not observed with data that does not need
##to be MICE'd. All code exluding ID columns has been corrected
##for this. 
#~Hannah 

##Outliers##
##Mahal

mahal = mahalanobis(nomiss[ , -c(1,2,8)], 
                    colMeans(nomiss[ , -c(1,2,8)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,8)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,8)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1,2,8)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1,2,8)])

##Linearity plot
##Create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##Multivariate normality
hist(standardized, breaks=15)

##Homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
SWLS <- rowSums(noout[, c(3:7)])
SWLS

##exclude paper people using noout dataset 
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

##overallmodel 
names(nomissnop)
library(lavaan)
overallmodel = '
SWLS =~ Q27_1 + Q27_2 + Q27_3 + Q27_4 + Q27_5
'
fit = cfa(overallmodel, 
          data = nomissnop, 
          meanstructure = TRUE)

summary(fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
fitMeasures(fit)
table(nomissnop$Source)

##subset for other two cfa
random = subset(nomissnop, Source == "1")
notrandom = subset(nomissnop, Source =="0")

##fit for random 
random.fit = cfa(overallmodel, 
                 data = random, 
                 meanstructure = TRUE)
summary(random.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
fitMeasures(random.fit)

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
summary(notrandom.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE) 
fitMeasures(notrandom.fit)

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