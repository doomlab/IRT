library(car)
library(memisc)

##set working directory
setwd("~/Desktop")

##Import all the data
##imported the data we compiled as LPQallcompiled
##imported as firstlpqfile 
##imported as secondlpqfile
##imported as thirdlpqfile
##imported as fourthlpqfile

##dr b's stuff here
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/IRT/NCHC riley/data/jeff LPQ done")
LPQallcompiled = read.csv("LPQ.csv")
firstlpqfile = read.csv("firstlpq.csv")
secondlpqfile = read.csv("secondlpq.csv")
thirdlpqfile = read.csv("thirdlpq.csv")
fourthlpqfile = read.csv("fourthlpq.csv")

##Import the SPSS files - they are all paper datasets
##no longer need to do since we converted to csv 

##moved up the reverse coding, so we only have to do it once
###changing to 0 and 1 - columns are currently 1 true, 2 false, so subtract 2
names(LPQallcompiled)
summary(LPQallcompiled)
##fix the 12 typo
LPQallcompiled$lpq16_1[ LPQallcompiled$lpq16_1 == 12] = NA

allcolumns = c("lpq1_1", "lpq2_1", "lpq3_1", "lpq4_1", "lpq5_1", "lpq6_1", "lpq7_1", "lpq8_1", "lpq9_1", "lpq10_1", "lpq11_1", "lpq12_1", "lpq13_1", "lpq14_1", "lpq15_1", "lpq16_1", "lpq17_1", "lpq18_1", "lpq19_1", "lpq20_1")
LPQallcompiled[ , allcolumns] = 2 - LPQallcompiled[ , allcolumns]

###reverse code items 1, 2, 5, 8, 9, 11, 12, 14, 15, 16, 19
reverse = c("lpq1_1", "lpq2_1", "lpq5_1", "lpq8_1", "lpq9_1", "lpq11_1", "lpq12_1", "lpq14_1", "lpq15_1", "lpq16_1", "lpq19_1")
LPQallcompiled[ , reverse] = 1 - LPQallcompiled[ , reverse]
summary(LPQallcompiled)

##make sure this dataaset doesn't have any decimals or weird numbers
apply(LPQallcompiled[ , allcolumns], 2, table)

##the csv files are a mix of computer and paper, so need to subset
##subset the csv that we have, shouldn't have to do this for others
##note: switched this to the subset function which is more reliable and easier
LPQallcompiledrandom = subset(LPQallcompiled, Source == '1')
LPQallcompilednotrandom = subset(LPQallcompiled, Source == '0')
LPQallcompiledpaper = subset(LPQallcompiled, Source == '2')
LPQallcompiledpaper = LPQallcompiledpaper[, 3:22] ##dropping the extra columns


##subset firstlpqfile so just lpq paper
firstlpqfilepaper = firstlpqfile[ , c(200:219)]
names(firstlpqfile)
firstlpqfilepaper[ firstlpqfilepaper > 0 & firstlpqfilepaper < 1 ] = NA
apply(firstlpqfilepaper,2,table)

##subset secondlpqfile so just lpq paper
secondlpqfilepaper = secondlpqfile[ , c(66:85)]
names(secondlpqfile)
secondlpqfilepaper[ secondlpqfilepaper > 0 & secondlpqfilepaper < 1 ] = NA
apply(secondlpqfilepaper,2,table)

##subset thirdlpqfile so just lpq paper
thirdlpqfilepaper = thirdlpqfile[ , c(135:154)]
names(thirdlpqfile)
thirdlpqfilepaper[ thirdlpqfilepaper > 0 & thirdlpqfilepaper < 1 ] = NA
thirdlpqfilepaper[ thirdlpqfilepaper > 1] = NA
apply(thirdlpqfilepaper,2,table)

##subset fourthlpqfile 
fourthlpqfilepaper = fourthlpqfile[ , c(78:97)]
names(fourthlpqfile)
fourthlpqfilepaper[ fourthlpqfilepaper > 0 & fourthlpqfilepaper < 1 ] = NA
apply(fourthlpqfilepaper,2,table)
##if only I could freaking spell fourth right (ps I fixed this ha)
##set all decimals = to NA. see code above under each dataset
  
##drop all the extra columns, so you just have 1-20
##done. did this for all of the paper sets above. now need to combine

##combine datasets into using rbind
##LPQallcompiledrandom is the dataset for random (just our compiled data)
##LPQallcompilednotrandom is the dataset for not random (just our compiled data)
allpaper = rbind(LPQallcompiledpaper, firstlpqfilepaper, secondlpqfilepaper, thirdlpqfilepaper, fourthlpqfilepaper)

##allpaper data screening
summary(allpaper)

##missing all paper by row
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}
missingallpaper = apply(allpaper, 1, percentmiss)
table(missingallpaper)
replacepeopleallpaper = subset(allpaper, missingallpaper <= 5)

##missing by column
apply(replacepeopleallpaper, 2, percentmiss)

library(mice)
tempnomiss = mice(replacepeopleallpaper)
replacedallpaper = complete(tempnomiss, 1)
summary(replacedallpaper)
##outliers
mahal = mahalanobis(replacedallpaper, 
                    colMeans(replacedallpaper, na.rm = T), 
                    cov(replacedallpaper, use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(replacedallpaper))
cutoff
ncol(replacedallpaper)
summary(mahal < cutoff)
nooutreplacedallpaper = subset(replacedallpaper, mahal < cutoff)

##assumptions 
##additivity
correlreplacedallpaper = cor(nooutreplacedallpaper)
symnum(correlreplacedallpaper)
##set up for assumptions 
random = rchisq(nrow(nooutreplacedallpaper), 7)
fake = lm(random ~ ., data = nooutreplacedallpaper)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)
##normality 
hist(standardized) ##positive skew
##linearity 
qqnorm(standardized)
abline(0, 1) ##guess its okay 
##homog and s
plot(fitted, standardized)
abline(0, 0)
abline(v = 0) ##graph is a bit iffy


##LPQallcompiledrandom 
LPQallcompiledrandom = LPQallcompiledrandom[ , 3:22]
summary(LPQallcompiledrandom)

##missing lpqallcompiled random 
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}
missingallcompiledrandom = apply(LPQallcompiledrandom, 1, percentmiss)
table(missingallcompiledrandom)
replacepeopleallcompiledrandom = subset(LPQallcompiledrandom, missingallcompiledrandom <=5)

##check for columns
apply(replacepeopleallcompiledrandom, 2, percentmiss)

library(mice) ##this one will not mice LOL (because you called your missing thing the wrong thing!) 
tempnomiss = mice(replacepeopleallcompiledrandom)
replacedallcompiledrandom = complete(tempnomiss, 1)
summary(replacedallcompiledrandom)
##outliers
mahal = mahalanobis(replacedallcompiledrandom, 
                    colMeans(replacedallcompiledrandom, na.rm = T), 
                    cov(replacedallcompiledrandom, use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(replacedallcompiledrandom))
cutoff
ncol(replacedallcompiledrandom)
summary(mahal < cutoff)
nooutreplacedallcompiledrandom = subset(replacedallcompiledrandom, mahal < cutoff)
##assumptions 
##additivity
correlreplacedallcompiledrandom = cor(nooutreplacedallcompiledrandom)
symnum(correlreplacedallcompiledrandom) ##good
##set up for assumptions 
random = rchisq(nrow(nooutreplacedallcompiledrandom), 7)
fake = lm(random ~ ., data = nooutreplacedallcompiledrandom)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)
##normality 
hist(standardized) ##positive skew
##linearity 
qqnorm(standardized)
abline(0, 1) ##looks good
##homog and s
plot(fitted, standardized)
abline(0, 0)
abline(v = 0)##yes!

##LPQallcompilednotrandom 
LPQallcompilednotrandom = LPQallcompilednotrandom[ , 3:22]
summary(LPQallcompilednotrandom)

##missing lpqallcompilednotrandom
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}
missingallcompilednotrandom = apply(LPQallcompilednotrandom, 1, percentmiss)
table(missingallcompilednotrandom)
replacepeopleallcompilednotrandom = subset(LPQallcompilednotrandom, missingallcompilednotrandom <= 5)

##columns
apply(replacepeopleallcompilednotrandom,2,percentmiss)

library(mice) ###this one will not mice either I broke it
tempnomiss = mice(replacepeopleallcompilednotrandom)
replacedallcompilednotrandom = complete(tempnomiss, 1)
summary(replacedallcompilednotrandom)
##outliers
mahal = mahalanobis(replacedallcompilednotrandom, 
                    colMeans(replacedallcompilednotrandom, na.rm = T), 
                    cov(replacedallcompilednotrandom, use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(replacedallcompilednotrandom))
cutoff
ncol(replacedallcompilednotrandom)
summary(mahal < cutoff)
nooutreplacedallcompilednotrandom = subset(replacedallcompilednotrandom, mahal < cutoff)
##assumptions 
##additivity
correlreplacedallcompilednotrandom = cor(nooutreplacedallcompilednotrandom)
symnum(correlreplacedallcompilednotrandom) ##yay
##set up for assumptions 
random = rchisq(nrow(nooutreplacedallcompilednotrandom), 7)
fake = lm(random ~ ., data = nooutreplacedallcompilednotrandom)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)
##normality 
hist(standardized) ##positive skew still
##linearity 
qqnorm(standardized)
abline(0, 1) ##guess its okay 
##homog and s
plot(fitted, standardized)
abline(0, 0)
abline(v = 0)##meets both

####covariance testing####
library(monomvn)
###make covariance tables
not_cor = cov(PILnotr, use="pairwise")
rand_cor = cov(PILrand, use="pairwise")
paper_cor = cov(paperdata, use="pairwise")

#mean tables
notrandom = unlist(sapply(PIL[ , 3:22], function(cl) list(means=mean(cl,na.rm=TRUE))))
random = unlist(sapply(PILrand, function(cl) list(means=mean(cl,na.rm=TRUE))))
paper = unlist(sapply(paperdata, function(cl) list(means=mean(cl,na.rm=TRUE))))

##rsme mean, cov, mean, cov
rmse.muS(notrandom, not_cor, paper, paper_cor)
rmse.muS(notrandom, not_cor, random, rand_cor)
##figure out how to do standardized residuals

####t test by item###
notrandom = as.data.frame(notrandom)

t.test(notrandom, random,
       var.equal = T,
       paired = F)

mean(notrandom)
mean(random)
sd(notrandom)
sd(random)
####effect sizes####
d.indt(m1 = 5.60, sd1 = 1.12, n1 = 20,
       m2 = 4.56, sd2 = 1.34, n2 =20,
       a = .05, k = 2)

##if significant and effect size CI does not cross zero 
##run individual t-tests by item 

####total scores####
PILnotr$total = apply(PILnotr, 1, mean)

t.test(PILnotr$total, PILr$total,
       var.equal = T,
       paired = F)

mean(stuff)
####effect sizes####
d.indt(m1 = 5.60, sd1 = 1.12, n1 = 20,
       m2 = 4.56, sd2 = 1.34, n2 =20,
       a = .05, k = 2)
