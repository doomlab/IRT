##Set working directory 
setwd("D:/GA/Scales")
master = read.csv("Meaning_Scales_DailyMeaningScale_RN_RR_Done.csv")
summary(master)

##Reverse Code. No reverse coding. Stopped here for scoring.
##fix text columns
master$Q86_8_TEXT = as.numeric(master$Q86_8_TEXT)
master$Q86_11_TEXT = as.numeric(master$Q86_11_TEXT)
master$Q86_13_TEXT = as.numeric(master$Q86_13_TEXT)

####DATA SCREENING####
##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:24], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude 
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,25)]
dontcolumn = replacepeople[ , c(1,2,25)]

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
mahal = mahalanobis(filledin_none[ , -c(1,2,3)], 
                    colMeans(filledin_none[ , -c(1,2,3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1,2,3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1,2,3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,3)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:2,3)])

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
##Daily Meaning average Q1 & Q2 (Col 4&5 in dataset)
daymean <- rowMeans(noout[,c("Q85_1", "Q85_2")])
summary(daymean)

daylife <- noout$Q85_3
summary(daylife)

Eud <- rowSums(noout[,c(7:13)])
summary(Eud)

Hed <- rowSums(noout[,c(14:25)])
summary(Hed)

