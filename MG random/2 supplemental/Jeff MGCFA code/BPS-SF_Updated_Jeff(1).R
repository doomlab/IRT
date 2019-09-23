##Set working directory 
setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_BoredomPronenessShort-RR_RN_Paper.csv")
summary(master)

##No reverse coded items.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
names(notypos)
missing = apply(notypos[ , 3:14], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,15)]
dontcolumn = replacepeople[ , c(1,2,15)]

##MICE not needed here. If was we would have to replace and bind
nomiss = replacepeople

##Outliers##
##Mahal
names(nomiss)

mahal = mahalanobis(nomiss[ , -c(1,2,15)], 
                    colMeans(nomiss[ , -c(1,2,15)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,15)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1,2,15)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:2,15)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=30)

##homogeneity and homoscedasticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
#Internal 
internal = rowSums(noout[,c("Q84_1", "Q84_3", "Q84_5", "Q84_6", "Q84_8", "Q84_9")])
summary(internal)

#External 
external = rowSums(noout[,c("Q84_2", "Q84_4", "Q84_7", "Q84_10", "Q84_11", "Q84_12")])
summary(external)

##CFA with internal + external + noout datasets / need to drop paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

library(lavaan)
overallmodel = '
INT =~ Q84_1 + Q84_3 + Q84_5 + Q84_6 + Q84_8 + Q84_9
EXT =~ Q84_2 + Q84_4 + Q84_7 + Q84_10 + Q84_11 + Q84_12
'
fit = cfa(overallmodel, 
          data = nomissnop, 
          meanstructure = TRUE)

summary(fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE) ##no heywood cases 
fitMeasures(fit)

table(nomissnop$Source)
##all significant 
## 0 is not random 
## 1 is random 
## 2 is paper 

##subset 
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
        fit.measure = TRUE) ##this model seems better than random, let's see where it breaks down 
fitMeasures(notrandom.fit)

##invariances 
library(semTools)
multisteps = measurementInvariance(overallmodel, 
                      data = nomissnop, 
                      group = 'Source', 
                      strict = T)

fitMeasures(multisteps$fit.configural)
fitMeasures(multisteps$fit.loadings)
fitMeasures(multisteps$fit.intercepts)
fitMeasures(multisteps$fit.residuals)
fitMeasures(multisteps$fit.means)

##loadings seem to be related between groups 

##partials f/residuals
partial = partialInvariance(multisteps, 
                            type = "residuals")

partial
strictfree = partial$results
group.partial = c("Q84_3~~Q84_3")

##after letting it go 
partialstrict = measurementInvariance(overallmodel, 
                                      data = nomissnop, 
                                      group = 'Source', 
                                      strict = T, 
                                      group.partial = c("Q84_3~~Q84_3"))

fitMeasures(partialstrict$fit.residuals)
