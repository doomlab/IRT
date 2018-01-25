setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_PILshort_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####
##Reverse Code. No reverse coding. Stopped here for scoring.

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
names(notypos)
missing = apply(notypos[ , 3:6], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2, 7)]
dontcolumn = replacepeople[ , c(1,2, 7)] ##removed the 7 column here

nomiss = replacepeople
summary(nomiss)
summary(replacepeople) ##dont mice bec nothing less than 5

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2, 7)], 
                    colMeans(nomiss[ , -c(1,2,7)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2, 7)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,7)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,7)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)

##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,7)])

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
PIL <- rowSums(noout[, c(3:6)])
summary(PIL)

##exclude paper people using noout dataset 
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

##overall model 
library(lavaan)
overallmodel = '
PIL =~ Q8 + Q9 + Q10 + Q11
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