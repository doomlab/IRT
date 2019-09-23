setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_MeaningInLife_Jim_RR_RN_Paper.csv")
summary(master)

##Reverse Coding
library(car)
master$Q110_1 = recode(master$Q110_1, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")

####DATA SCREENING####
##accuracy##
summary(master)

##fix the miscored  is this needed? 
master[ , 18:22] = master[ , 18:22] - 1

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:22], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,23)]
dontcolumn = replacepeople[ , c(1,2,23)]

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

##Harmony
har = colSums(noout[,c("Q110_3", "Q110_2", "Q110_6", "Q110_1")])
har

hartot = sum(har)
hartot

#Life perspective 
lp = colSums(noout[,c("Q109_1", "Q109_3", "Q109_6", "Q109_7", "Q109_9", "Q109_11", "Q109_13")])
lp

lptot = sum(lp)
lptot

##Confusion
con = rowSums(noout[,c("Q109_2", "Q109_5", "Q109_4", "Q109_8", "Q109_10", "Q109_12", "Q109_14")])
con

#Spirit
sp = rowSums(noout[,c("Q110_4", "Q110_5")])
sp

##Factoring source##
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

####overall cfa everyone together##
overallmodel = '
harmony =~ Q110_3 + Q110_2 + Q110_6 + Q110_1
lifeperspective =~ Q109_1 + Q109_3 + Q109_6 + Q109_7 + Q109_9 + Q109_11 + Q109_13
confusion =~ Q109_2 + Q109_5 + Q109_4 + Q109_8 + Q109_10 + Q109_12 + Q109_14
spirit =~ Q110_4 + Q110_5
'

overall.fit = cfa(overallmodel, 
                  data=nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

####separate group models####
##notrandom
overall.fit.nr = cfa(overallmodel, 
                     data=notrandom, 
                     meanstructure = TRUE)

summary(overall.fit.nr, 
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


####multi group testing####
###measurement invariance
options(scipen = 999)

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)



## I have no clue what is going on here so I did not want to delete it.
###reliability 
harmony = c("Q110_3", "Q110_2", "Q110_6", "Q110_1")
life = c("Q109_1", "Q109_3", "Q109_6", "Q109_7", "Q109_9", "Q109_11", "Q109_13")
confusion = c("Q109_2", "Q109_5", "Q109_4", "Q109_8", "Q109_10", "Q109_12", "Q109_14")
spirit = c("Q110_4", "Q110_5")

library(psych)
alpha(noout[, harmony])
alpha(noout[ , life])
alpha(noout[ , confusion])
alpha(noout[ , spirit])

##totals
harmony2 = c("Q111_3", "Q111_2", "Q111_6", "Q111_1")
spirit2 = c("Q111_4", "Q111_5")
library(car)

noout$Q111_1 = recode(noout$Q110_1, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
noout$Q111_2 = recode(noout$Q110_2, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
noout$Q111_3 = recode(noout$Q110_3, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
noout$Q111_4 = recode(noout$Q110_4, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
noout$Q111_5 = recode(noout$Q110_5, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
noout$Q111_6 = recode(noout$Q110_6, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")

noout$haravg = apply(noout[ , harmony2], 1, mean)
noout$lifeavg = apply(noout[ , life], 1, mean)
noout$conavg = apply(noout[ , confusion], 1, mean)
noout$spiritavg = apply(noout[ , spirit2], 1, mean)

