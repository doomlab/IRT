setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_SenseofCoherence_RR_RN_Paper.csv")
summary(master)

##No reverse coded items.
library(car)
master$Q57_1 = recode(master$Q57_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q59_1 = recode(master$Q59_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q61_1 = recode(master$Q61_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q62_1 = recode(master$Q62_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q63_1 = recode(master$Q63_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q64_1 = recode(master$Q64_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q66_1 = recode(master$Q66_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q69_1 = recode(master$Q69_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q70_1 = recode(master$Q70_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q71_1 = recode(master$Q71_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q73_1 = recode(master$Q73_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q74_1 = recode(master$Q74_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q77_1 = recode(master$Q77_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q78_1 = recode(master$Q78_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q79_1 = recode(master$Q79_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:31], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,32)]
dontcolumn = replacepeople[ , c(1,2,32)]

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

mahal = mahalanobis(nomiss[ , -c(1:3)], 
                    colMeans(nomiss[ , -c(1:3)], na.rm = TRUE),
                    cov(nomiss[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:3)])

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

##scoring (only one factor from the PDF)
SCS = rowSums(filledin_none[ , c(3:31)])
