setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_LifeRegardIndex_RR_RN_Paper.csv")
summary(master)

##Reverse Code. None.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:30], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)

replacecolumn = replacepeople[ , -c(1,2,31)]
dontcolumn = replacepeople[ , c(1,2,31)]

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
correlations = cor(filledin_none[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(filledin_none), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=filledin_none[ , -c(1:3)])

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

##exclude paper people using noout dataset 
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

##Scoring
Framescale<- rowSums(noout[, c(4,6,7,10,11,12,13,14,16,17,19,26,28,31)])
Framescale

Fufillscale<- rowSums(noout[, c(5,8,9,15,18,20,21,22,23,24,25,27,29,30)])
Fufillscale

##overall model 
names(filledin_none)
overallmodel = '
FRAME =~ Q91_1 + Q91_3 + Q91_4 + Q91_7 + Q91_8 + Q91_9 + Q91_10 + Q91_11 + Q91_13 + Q91_14 + Q91_16 + Q91_23 + Q91_25 + Q91_28
FULFILL =~ Q91_2 + Q91_5 + Q91_6 + Q91_12 + Q91_15 + Q91_17 + Q91_18 + Q91_19 + Q91_20 + Q91_21 + Q91_22 + Q91_24 + Q91_26 + Q91_27'

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
