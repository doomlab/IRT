setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_SourcesOfMeaning_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:15], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,16)]
dontcolumn = replacepeople[ , c(1,2,16)]

##no missing data.
nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,16)], 
                    colMeans(nomiss[ , -c(1,2,16)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,16)], use="pairwise.complete.obs"))
mahal
summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,16)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,16)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,16)])

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

##scoring
##looks like each of the 13 are rated on each of the 10 constructs 
##might not be right 
Achievement = rowSums(filledin_none[ , c(3:15)])
Framework = rowSums(filledin_none[ , c(3:15)])
Religion = rowSums(filledin_none[ , c(3:15)])
DeathAcceptance = rowSums(filledin_none[ , c(3:15)])
InterpersonalSatisfaction = rowSums(filledin_none[ , c(3:15)])
ExcitementFulfillment = rowSums(filledin_none[ , c(3:15)])
GivingtoWorld = rowSums(filledin_none[ , c(3:15)])
LackExistentialVacuum = rowSums(filledin_none[ , c(3:15)])
Intimacy = rowSums(filledin_none[ , c(3:15)])
Control = rowSums(filledin_none[ , c(3:15)])