library(car);library(memisc)

##set working directory for Dr. B
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/IRT/NCHC riley/data/rachel PIL things") 
PIL = read.csv("PIL.csv")
week1.file.04.29.10 = read.delim("week1 file 04.29.10.csv")
meaning.working.file.5.10.10 = read.delim("meaning working file 5-10-10.csv")
SES.Mike.and.Mike.Data = read.delim("SES Mike and Mike Data.csv")
logotherapy_data_factor_analysis_study = read.delim("logotherapy_data_factor_analysis_study.csv")

##Rachel import data
PIL <- read.csv("~/Downloads/PIL.csv")
week1.file.04.29.10 <- read.delim("~/Downloads/week1 file 04.29.10.csv")
meaning.working.file.5.10.10 <- read.delim("~/Downloads/meaning working file 5-10-10.csv")
SES.Mike.and.Mike.Data <- read.delim("~/Downloads/SES Mike and Mike Data.csv")
logotherapy_data_factor_analysis_study <- read.delim("~/Downloads/logotherapy_data_factor_analysis_study.csv")

##reverse coding the PIL
PIL[ , c("PIL2", "PIL5", "PIL7", "PIL10", "PIL14", "PIL15", "PIL17", "PIL18", "PIL19")] = 8 - PIL[ , c("PIL2", "PIL5","PIL7", "PIL10", "PIL14", "PIL15", "PIL17", "PIL18", "PIL19")]

##subsetting the PIL
##Zero = notrandom, One = random, Two = paper
PIL$Source = factor(PIL$Source, levels = c(0,1,2),
                             labels = c("Zero", "One", "Two"))
Zero = subset(PIL, Source == "Zero")
One = subset(PIL, Source == "One")
Two = subset(PIL, Source == "Two")

##removing all columns except PIL questions
notrandomcomputer = Zero[, c(3:22)]
randomcomputer = One[, c(3:22)]
paper = Two[, c(3:22)]

##removing all columns except PIL for other data sets
Paper1dropcolumns = week1.file.04.29.10[, c(8:27)]
colnames(meaning.working.file.5.10.10)
Paper2dropcolumns = meaning.working.file.5.10.10[, c(55:74)]
Paper3dropcolumns = SES.Mike.and.Mike.Data[, c(2:21)]
Paper4dropcolumns = logotherapy_data_factor_analysis_study[, c(1:20)]


###combinging all paper data sets
allpaper = rbind(Paper1dropcolumns, Paper2dropcolumns, Paper3dropcolumns, Paper4dropcolumns, paper)

##data screening
##each dataset separately (and separate not random/random/paper)
summary(randomcomputer)
summary(notrandomcomputer)
summary(allpaper)

apply(randomcomputer, 2, table)
apply(notrandomcomputer, 2, table)
apply(allpaper, 2, table)

##All paper - PIL2 has a decimal,PIL7 has a decimal,PIL10 has a decimal, PIL13 has a decimal,PIL14, PIL15, PIL16, PIL17
allpaper$PIL2 = as.integer(allpaper$PIL2)
allpaper$PIL5 = as.integer(allpaper$PIL5)
allpaper$PIL7 = as.integer(allpaper$PIL7)
allpaper$PIL10 = as.integer(allpaper$PIL10)
allpaper$PIL13 = as.integer(allpaper$PIL13)
allpaper$PIL14 = as.integer(allpaper$PIL14)
allpaper$PIL15 = as.integer(allpaper$PIL15)
allpaper$PIL16 = as.integer(allpaper$PIL16)
allpaper$PIL17 = as.integer(allpaper$PIL17)
allpaper$PIL18 = as.integer(allpaper$PIL18)
allpaper$PIL19 = as.integer(allpaper$PIL19)


######Paper#####
##missing data
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}

##participants (row)
missing = apply(allpaper, 1, percentmiss)
table(missing)
replacepeople = subset(allpaper, missing <= 5)
nopeople  = subset(allpaper, missing > 5)
summary(replacepeople)
summary(nopeople)

##variables (columns)
apply(replacepeople, 2, percentmiss)

library(mice)
tempnomiss = mice(replacepeople)
replacedallpaper = complete(tempnomiss, 1)
summary(replacedallpaper)
##outliers
mahal = mahalanobis(replacedallpaper[ , ],
                    colMeans(replacedallpaper[ , ], na.rm = T),
                    cov(replacedallpaper[ , ], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(replacedallpaper[ , ]))
ncol(replacedallpaper[ , ])
cutoff
summary(mahal < cutoff)
nooutpaper = subset(replacedallpaper, mahal < cutoff)

##assumptions
##additivity
correl = cor(nooutpaper[ , ])
symnum(correl)

finalpaper = nooutpaper

##set up for assumptions
random = rchisq(nrow(finalpaper), 7)
fake = lm(random ~ ., data = finalpaper)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
library(moments)
skewness(finalpaper[ , ], na.rm = T)
kurtosis(finalpaper[ , ], na.rm = T)
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homog + s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

######Random#####
##missing data
##participants (row)
missingrandomcomputer = apply(randomcomputer, 1, percentmiss)
table(missingrandomcomputer)
replacepeoplerandom = subset(randomcomputer, missingrandomcomputer <= 5)
nopeoplerandom  = subset(randomcomputer, missingrandomcomputer > 5)
summary(replacepeoplerandom)
summary(nopeoplerandom)

##variables (column)
apply(replacepeoplerandom, 2, percentmiss)
replacecolumnrandom = replacepeoplerandom[ , -15]
dontcolumnrrandom = replacepeoplerandom[ , 15]

library(mice)
tempnomissrandom = mice(replacecolumnrandom)
replacedrandom_temp = complete(tempnomissrandom, 1)

replacedrandom = cbind(replacedrandom_temp[ , 1:14], 
                       PIL15 = dontcolumnrrandom, 
                       replacedrandom_temp[ , 15:19])
summary(replacedrandom)



##outliers
mahalrandom = mahalanobis(replacedrandom[ , ],
                    colMeans(replacedrandom[ , ], na.rm = T),
                    cov(replacedrandom[ , ], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(replacedrandom[ , ]))
ncol(replacedrandom[ , ])
cutoff
summary(mahalrandom < cutoff)
nooutrandom = subset(replacedrandom, mahalrandom < cutoff)

##assumptions
##additivity
correlrandom = cor(nooutrandom[ , ])
symnum(correlrandom)

finalrandom = nooutrandom

##set up for assumptions
randomrandom = rchisq(nrow(finalrandom), 7)
fakerandom = lm(randomrandom ~ ., data = finalrandom)
standardizedrandom = rstudent(fakerandom)
fittedrandom = scale(fakerandom$fitted.values)

##normality
library(moments)
skewness(finalrandom[ , ], na.rm = T)
kurtosis(finalrandom[ , ], na.rm = T)
hist(standardizedrandom)

##linearity
qqnorm(standardizedrandom)
abline(0,1)

##homog + s
plot(fittedrandom, standardizedrandom)
abline(0,0)
abline(v = 0)

######NOTRandom#####
##missing data
##participants (row)
missingnotrandomcomputer = apply(notrandomcomputer, 1, percentmiss)
table(missingnotrandomcomputer)
replacepeoplenotrandom = subset(notrandomcomputer, missingnotrandomcomputer <= 5)
nopeoplenotrandom  = subset(notrandomcomputer, missingnotrandomcomputer > 5)
summary(replacepeoplenotrandom)
summary(nopeoplenotrandom)

##variables (columns)
apply(replacepeoplenotrandom, 2, percentmiss)

library(mice)
tempnomissnotrandom = mice(replacepeoplenotrandom)
replacednotrandom = complete(tempnomissnotrandom, 1)
summary(replacednotrandom)
##outliers
mahalnotrandom = mahalanobis(replacednotrandom[ , ],
                          colMeans(replacednotrandom[ , ], na.rm = T),
                          cov(replacednotrandom[ , ], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(replacednotrandom[ , ]))
ncol(replacednotrandom[ , ])
cutoff
summary(mahalnotrandom < cutoff)
nooutnotrandom = subset(replacednotrandom, mahalnotrandom < cutoff)

##assumptions
##additivity
correlnotrandom = cor(nooutnotrandom[ , ])
symnum(correlnotrandom)

finalnotrandom = nooutnotrandom

##set up for assumptions
randomnotrandom = rchisq(nrow(finalnotrandom), 7)
fakenotrandom = lm(randomnotrandom ~ ., data = finalnotrandom)
standardizednotrandom = rstudent(fakenotrandom)
fittednotrandom = scale(fakenotrandom$fitted.values)

##normality
library(moments)
skewness(finalnotrandom[ , ], na.rm = T)
kurtosis(finalnotrandom[ , ], na.rm = T)
hist(standardizednotrandom)

##linearity
qqnorm(standardizednotrandom)
abline(0,1)

##homog + s
plot(fittednotrandom, standardizednotrandom)
abline(0,0)
abline(v = 0)




####covariance testing####
library(monomvn)
###make covariance tables
not_cor = cov(finalnotrandom, use="pairwise")
rand_cor = cov(finalrandom, use="pairwise")
paper_cor = cov(finalpaper, use="pairwise")

#mean tables
notrandomm = unlist(sapply(finalnotrandom, function(cl) list(means=mean(cl,na.rm=TRUE))))
randomm = unlist(sapply(finalrandom, function(cl) list(means=mean(cl,na.rm=TRUE))))
paperm = unlist(sapply(finalpaper, function(cl) list(means=mean(cl,na.rm=TRUE))))

##rsme mean, cov, mean, cov
rmse.muS(notrandomm, not_cor, paperm, paper_cor)
rmse.muS(notrandomm, not_cor, randomm, rand_cor)
##figure out how to do standardized residuals

####t test by item###

##i think this should be paired T because the items are paired
library(effsize)
t.test(notrandomm, 
       randomm,
       var.equal = T,
       paired = T)

cohen.d(notrandomm, 
        randomm,
        pooled = T,
        paired = T)

mean(notrandomm)
mean(randomm)
sd(notrandomm)
sd(randomm)

t.test(notrandomm, 
       paperm,
       var.equal = T,
       paired = T)

cohen.d(notrandomm, 
       paperm,
       pooled = T,
       paired = T)

mean(notrandomm)
mean(paperm)
sd(notrandomm)
sd(paperm)

####effect sizes####
##run effsize.R first to get this to work (put in folder)
##not random and random
d.indt(m1 = 4.94, sd1 = 0.83, n1 = 20,
       m2 = 4.85, sd2 = 0.92, n2 =20,
       a = .05, k = 2)

##notrandom and paper
d.indt(m1 = 4.94, sd1 = 0.83, n1 = 20,
       m2 = 5.42, sd2 = 0.42, n2 =20,
       a = .05, k = 2)

##if significant and effect size CI does not cross zero 
##run individual t-tests by item 

cohen.d(finalnotrandom$PIL1, 
       finalpaper$PIL1,
       pooled = T,
       paired = F)

cohen.d(finalnotrandom$PIL2, 
        finalpaper$PIL2,
        pooled = T,
        paired = F)

####total scores####
###No total scores - need to create these
finalpaper$total = apply(finalpaper, 1, sum)
finalnotrandom$total = apply(finalnotrandom, 1, sum)
finalrandom$total = apply(finalrandom, 1, sum)

t.test(finalpaper$total, finalnotrandom$total,
       var.equal = T,
       paired = F)

cohen.d(finalpaper$total, finalnotrandom$total,
       pooled = T,
       paired = F)

t.test(finalnotrandom$total, finalrandom$total,
       var.equal = T,
       paired = F)

cohen.d(finalnotrandom$total, finalrandom$total,
        pooled = T,
        paired = F)

mean(finalnotrandom$total)
mean(finalrandom$total)
mean(finalpaper$total)
sd(finalnotrandom$total)
sd(finalrandom$total)
sd(finalpaper$total)

####effect sizes####
d.indt(m1 = 98.80, sd1 = 17.11, n1 = 20,
       m2 = 96.91, sd2 = 15.25, n2 =20,
       a = .05, k = 2)

d.indt(m1 = 98.80, sd1 = 17.11, n1 = 20,
       m2 = 108.31, sd2 = 14.40, n2 =20,
       a = .05, k = 2)
