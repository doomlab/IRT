setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_PPM&PMP_RR_RN_Paper.csv")
summary(master)

####Data Screening####
##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:10], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,11)]
dontcolumn = replacepeople[ , c(1,2,11)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,11)], 
                    colMeans(nomiss[ , -c(1,2,11)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,11)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,11)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,11)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,11)])

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
PPMS <- rowSums(noout[, c(3:10)])
summary(PPMS)

####CFA####
##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)

overallmodel = '
PMS =~ Q96_1 + Q96_2 + Q96_3 + Q96_4 + Q96_5 + Q96_6 + Q96_7 + Q96_8'

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



