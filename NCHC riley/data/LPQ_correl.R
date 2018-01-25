library(car);library(memisc)

##set working directory
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/IRT/NCHC riley/data/computer data")

##Import all the data
##Import the SPSS files - they are all paper datasets
##the csv files are a mix of computer and paper, so need to subset
##drop all the extra columns, so you just have 1-20
PIL <- read.csv("PIL.csv")

##combine datasets into using rbind
PILnotr = NOT RANDOM COMPUTER
PILrand = RANDOM COMPUTER
paperdata = PAPER STUFF

##data screening
##each dataset separately (and separate not random/random/paper)

##screen for decimals
apply(example, 2, table)
example[ example > 0 & example < 1 ] = NA


####covariance testing####
library(monomvm)
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
