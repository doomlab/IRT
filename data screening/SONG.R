##Set working directory 
setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_SeekingOfNoeticGoals_RR_RN_Paper.csv")
summary(master)

##Reverse Code. 9
library(car)
master$Q107_3 = recode(master$Q107_3, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_4 = recode(master$Q107_4, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_5 = recode(master$Q107_5, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_6 = recode(master$Q107_6, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_9 = recode(master$Q107_9, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_12 = recode(master$Q107_12, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_13 = recode(master$Q107_13, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_15 = recode(master$Q107_15, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_18 = recode(master$Q107_18, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q107_20 = recode(master$Q107_20, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:22], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,23)]
dontcolumn = replacepeople[ , c(1,2,23)]

##MICE
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##(By your powers) Combine (I am Captain Planet!)
filledin_none = cbind(dontcolumn, nomiss)
summary(filledin_none)

##Small note: When MICE is run on this project the YEAR column
##is moved from the last column to the thrid column.
##This trend is not observed with data that does not need
##to be MICE'd. All code exluding ID columns has been corrected
##for this. 
#~Hannah 

##Outliers##
##Mahal

mahal = mahalanobis(filledin_none[ , -c(1:3)], 
                    colMeans(filledin_none[ , -c(1:3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:3)])

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
##Existential vacu
vac = colSums(noout[,c("Q107_3", "Q107_4", "Q107_8", "Q107_9", "Q107_11", "Q107_12", "Q107_13", "Q107_14","Q107_17", "Q107_19")])
vac

vactot = sum(vac)
vactot

#Life perspective 
WM = rowSums(noout[,c("Q107_1", "Q107_6", "Q107_7", "Q107_10", "Q107_15", "Q107_18", "Q107_20")])
WM


