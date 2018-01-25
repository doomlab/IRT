##effect size script written by Dr. B##
##noncentral code taken from: http://mars.wiwi.hu-berlin.de/mediawiki/slides/index.php/Comparison_of_noncentral_and_central_distributions##

####noncentral t####
ncpt<-function(x,q,df,confirm=FALSE){
  .f<-function(ncp,x,df,q)abs(q-pt(x,df=df,ncp=ncp))
  .n<-1;
  while	(
    (
      (pt(x,df=df,ncp=-.n) < q+(1-q)/2 )
      |
      (pt(x,df=df,ncp=.n) > q/2)
      )
    &
    (.n < Inf)
    )
    .n <- .n *2 ;
  if (confirm)
    optimize(f=.f,x=x,df=df,q=q,interval=c(-.n,.n))
  else
    optimize(f=.f,x=x,df=df,q=q,interval=c(-.n,.n))$minimum
  }

####noncentral chisq####
ncpchisq<-function(x,q,df,confirm=FALSE){
  .f<-function(ncp,x,df,q)abs(q - pchisq(x,df=df,ncp=ncp))
  if (pchisq(x,df=df)<=q){
    if (confirm) {
      minimum <-0;
      objective <- pchisq(x,df=df)-q;
      data.frame(minimum,objective)
      }else
        0
    }else {
      .n<- 1;
      while	(
        (pchisq(x,df=df,ncp=.n) > q/2)
        &
        (.n < Inf)
        )
        .n <- .n + 1;
      if (confirm)
        optimize(f=.f,x= x,df=df,q=q,interval=c(0,.n))
      else
        optimize(f=.f,x= x,df=df,q=q,interval=c(0,.n))$minimum
      }
  };

####noncentral f####
ncpf<-function(x,q,df1,df2,confirm=FALSE){
  .f<-function(ncp,x,df1,df2,q)abs(q - pf(x,df1=df1,df2=df2,ncp=ncp))
  if (pf(x,df1=df1,df2=df2)<=q){
    if (confirm) {
      minimum <-0;
      objective <- pf(x,df1=df1,df2=df2)-q;
      data.frame(minimum,objective)
      }else
        0
    }else {
      .n<- 1;
      while	(
        (pf(x,df1=df1,df2=df2,ncp=.n) > q/2)
        &
        (.n < Inf)
        )
        .n <- .n +1 ;
      if (confirm)
        optimize(f=.f,x= x,df1=df1,df2=df2,q=q,interval=c(0,.n))
      else
        optimize(f=.f,x= x,df1=df1,df2=df2,q=q,interval=c(0,.n))$minimum
      }
  };

####format for apa style####
apa <- function(x, k) format(round(x, k), nsmall=k)
p.value = function(p, k) {
  if (k <= 2) { 
    if (p < .01){
      pout = "p < .01"
    } else { pout = paste("p = ", apa(p, k))}
    }
  if (k > 2) { 
    if(p < .001){
      pout = "p < .001"
      } else { pout = paste("p = ", apa(p, k))}
  }
  return(pout)
}

####cohen's d single t from means####
d.singlet = function (m = 0, u = 0, sd = 1, n = 10, a = .05, k = 2) {
  se = sd / sqrt(n)
  d = (m - u) / sd
  t = (m - u) / se
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n-1))
  ncphigh = ncpt(x = t, q = ahigh, df = (n-1))
  dlow = ncplow / sqrt(n)
  dhigh = ncphigh / sqrt(n)
  Mlow = m - se*qt(a/2, n-1, lower.tail = FALSE)
  Mhigh = m + se*qt(a/2, n-1, lower.tail = FALSE)
  p = pt(abs(t), n-1, lower.tail = F)*2
  cat("M = ", 
      apa(m, k),
      ", SD = ", 
      apa(sd, k),
      ", SE = ", apa(se, k),
      ", ", (1-a)*100, "%CI[", 
      apa(Mlow, k),
      " - ",
      apa(Mhigh, k),
      "]",
      "\nt(", n-1, ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ", 
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####cohen's d single t from t####
d.singlett = function (t = 0, n = 10, a = .05, k = 2) {
  d = t / sqrt(n)
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n-1))
  ncphigh = ncpt(x = t, q = ahigh, df = (n-1))
  dlow = ncplow / sqrt(n)
  dhigh = ncphigh / sqrt(n)
  p = pt(abs(t), n-1, lower.tail = F)*2
  cat("t(", n-1, ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ", 
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####cohen's d dependent t averages####
d.deptavg = function (m1 = 0, m2 = 0, sd1 = 1, sd2 = 1, n = 10, a = .05, k = 2) {
  d = (m1 - m2) / ((sd1 + sd2)/2)
  se1 = sd1 / sqrt(n)
  se2 = sd2 / sqrt(n)
  t = (m1 - m2) / ((se1 + se2)/2)
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n-1))
  ncphigh = ncpt(x = t, q = ahigh, df = (n-1))
  dlow = ncplow / sqrt(n)
  dhigh = ncphigh / sqrt(n)
  M1low = m1 - se1*qt(a/2, n-1, lower.tail = FALSE)
  M1high = m1 + se1*qt(a/2, n-1, lower.tail = FALSE)
  M2low = m2 - se2*qt(a/2, n-1, lower.tail = FALSE)
  M2high = m2 + se2*qt(a/2, n-1, lower.tail = FALSE)
  p = pt(abs(t), n-1, lower.tail = F)*2
    cat("M1 = ", 
      apa(m1, k),
      ", SD = ", 
      apa(sd1, k),
      ", SE = ", apa(se1, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M1low, k),
      " - ",
      apa(M1high, k),
      "]",
      "\nM2 = ", 
      apa(m2, k),
      ", SD = ", 
      apa(sd2, k),
      ", SE = ", apa(se2, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M2low, k),
      " - ",
      apa(M2high, k),
      "]",
      "\nd = ", 
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "] \nNote: t and p values not reported because they are not correct for hypothesis testing.", sep = "")
}

####cohen's d dependent t differences####
d.deptdiff = function (mdiff = 0, sddiff = 1, n = 10, a = .05, k = 2) {
  d = mdiff / sddiff
  se = sddiff / sqrt(n)
  t = mdiff / se
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n-1))
  ncphigh = ncpt(x = t, q = ahigh, df = (n-1))
  dlow = ncplow / sqrt(n)
  dhigh = ncphigh / sqrt(n)
  Mlow = mdiff - se*qt(a/2, n-1, lower.tail = FALSE)
  Mhigh = mdiff + se*qt(a/2, n-1, lower.tail = FALSE)
  p = pt(abs(t), n-1, lower.tail = F)*2
  cat("M = ", 
      apa(mdiff, k),
      ", SD = ", 
      apa(sddiff, k),
      ", SE = ", apa(se, k),
      ", ", (1-a)*100, "%CI[", 
      apa(Mlow, k),
      " - ",
      apa(Mhigh, k),
      "]",
      "\nt(", n-1, ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
  }

####cohen's d independent t from means####
d.indt = function (m1 = 0, m2 = 0, sd1 = 1, sd2 = 1, n1 = 10, n2 = 10, a = .05, k = 2) {
  spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
  d = (m1 - m2) / spooled
  se1 = sd1 / sqrt(n1)
  se2 = sd2 / sqrt(n2)
  sepooled = sqrt((spooled^2/n1 + spooled^2/n2))
  t = (m1 - m2) / sepooled
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n1+n2-2))
  ncphigh = ncpt(x = t, q = ahigh, df = (n1+n2-2))
  dlow = ncplow / sqrt(((n1*n2) / (n1+n2)))
  dhigh = ncphigh / sqrt(((n1*n2) / (n1+n2)))
  M1low = m1 - se1*qt(a/2, n1-1, lower.tail = FALSE)
  M1high = m1 + se1*qt(a/2, n1-1, lower.tail = FALSE)
  M2low = m2 - se2*qt(a/2, n2-1, lower.tail = FALSE)
  M2high = m2 + se2*qt(a/2, n2-1, lower.tail = FALSE)
  p = pt(abs(t), (n1-1 + n2-1), lower.tail = F)*2
  cat("M1 = ", 
      apa(m1, k),
      ", SD = ", 
      apa(sd1, k),
      ", SE = ", apa(se1, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M1low, k),
      " - ",
      apa(M1high, k),
      "]",
      "\nM2 = ", 
      apa(m2, k),
      ", SD = ", 
      apa(sd2, k),
      ", SE = ", apa(se2, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M2low, k),
      " - ",
      apa(M2high, k),
      "]",
      "\nt(", (n1-1 + n2-1), ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####cohen's d independent t from t####
d.indtt = function (t = 0, n1 = 10, n2 = 10, a = .05, k = 2) {
  d = 2*t/sqrt(n1+n2-2)
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n1+n2-2))
  ncphigh = ncpt(x = t, q = ahigh, df = (n1+n2-2))
  dlow = ncplow / sqrt(((n1*n2) / (n1+n2)))
  dhigh = ncphigh / sqrt(((n1*n2) / (n1+n2)))
  p = pt(abs(t), (n1-1 + n2-1), lower.tail = F)*2
  cat("t(", (n1-1 + n2-1), ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####hedge's g independent t ####
g.indt = function (m1 = 0, m2 = 0, sd1 = 1, sd2 = 1, n1 = 10, n2 = 10, a = .05, k = 2) {
  correction = 1 - (3 / (4*(n1+n2)-9))
  spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
  d = ((m1 - m2) / spooled)*correction
  se1 = sd1 / sqrt(n1)
  se2 = sd2 / sqrt(n2)
  sepooled = sqrt((spooled^2/n1 + spooled^2/n2))
  t = (m1 - m2) / sepooled
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n1+n2-2))
  ncphigh = ncpt(x = t, q = ahigh, df = (n1+n2-2))
  dlow = correction*(ncplow / sqrt(((n1*n2) / (n1+n2))))
  dhigh = correction*(ncphigh / sqrt(((n1*n2) / (n1+n2))))
  M1low = m1 - se1*qt(a/2, n1-1, lower.tail = FALSE)
  M1high = m1 + se1*qt(a/2, n1-1, lower.tail = FALSE)
  M2low = m2 - se2*qt(a/2, n2-1, lower.tail = FALSE)
  M2high = m2 + se2*qt(a/2, n2-1, lower.tail = FALSE)
  p = pt(abs(t), (n1-1 + n2-1), lower.tail = F)*2
    cat("M1 = ", 
      apa(m1, k),
      ", SD = ", 
      apa(sd1, k),
      ", SE = ", apa(se1, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M1low, k),
      " - ",
      apa(M1high, k),
      "]",
      "\nM2 = ", 
      apa(m2, k),
      ", SD = ", 
      apa(sd2, k),
      ", SE = ", apa(se2, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M2low, k),
      " - ",
      apa(M2high, k),
      "]",
      "\nt(", (n1-1 + n2-1), ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####glass delta independent t ####
delta.indt = function (m1 = 0, m2 = 0, sd1 = 1, sd2 = 1, n1 = 10, n2 = 10, a = .05, k = 2) {
  spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
  d = (m1 - m2) / sd1
  se1 = sd1 / sqrt(n1)
  se2 = sd2 / sqrt(n2)
  sepooled = sqrt((spooled^2/n1 + spooled^2/n2))
  t = (m1 - m2) / sepooled
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpt(x = t, q = alow, df = (n1+n2-2))
  ncphigh = ncpt(x = t, q = ahigh, df = (n1+n2-2))
  dlow = ncplow / sqrt(((n1*n2) / (n1+n2)))
  dhigh = ncphigh / sqrt(((n1*n2) / (n1+n2)))
  M1low = m1 - se1*qt(a/2, n1-1, lower.tail = FALSE)
  M1high = m1 + se1*qt(a/2, n1-1, lower.tail = FALSE)
  M2low = m2 - se2*qt(a/2, n2-1, lower.tail = FALSE)
  M2high = m2 + se2*qt(a/2, n2-1, lower.tail = FALSE)
  p = pt(abs(t), (n1-1 + n2-1), lower.tail = F)*2
  cat("M1 = ", 
      apa(m1, k),
      ", SD = ", 
      apa(sd1, k),
      ", SE = ", apa(se1, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M1low, k),
      " - ",
      apa(M1high, k),
      "]",
      "\nM2 = ", 
      apa(m2, k),
      ", SD = ", 
      apa(sd2, k),
      ", SE = ", apa(se2, k),
      ", ", (1-a)*100, "%CI[", 
      apa(M2low, k),
      " - ",
      apa(M2high, k),
      "]",
      "\nt(", (n1-1 + n2-1), ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####phi chi-square####
v.chisq = function (x2 = 10, n = 10, r = 2, c = 2, a = .05, k = 2) {
  dfsmall = min(r-1, c-1)
  v = sqrt(x2 / n*dfsmall)
  ahigh = a / 2
  alow = 1-(a/2)
  dftotal = (r-1)*(c-1)
  ncplow = ncpchisq(x = x2, q = alow, df = dftotal)
  ncphigh = ncpchisq(x = x2, q = ahigh, df = dftotal)
  vlow = sqrt((ncplow + dftotal) / (n*dfsmall))
  vhigh = sqrt((ncphigh + dftotal) / (n*dfsmall)) 
  p = pchisq(x2, dftotal, lower.tail = F)
  if(p < .001){ p = .001}
  cat("X2(", dftotal, ") = ", 
      apa(x2, k),
      ", ", 
      p.value(p, k), 
      ", V = ", 
      apa(v, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(vlow, k), 
      " - ", 
      apa(vhigh, k), "]", sep = "")
}

####odds risk ratios####
odds = function (n11 = 1, n12 = 1, n21 = 1, n22 = 1, a = .05, k = 2) {
  odds = (n11 / n12) / (n21 / n22)
  se = sqrt((1/n11)+(1/n12)+(1/n21)+(1/n22))
  olow = exp(log(odds)) - qnorm(a/2, lower.tail = F)*se
  ohigh = exp(log(odds)) + qnorm(a/2, lower.tail = F)*se
  cat("Odds = ", 
      apa(odds, k),
      ", SE = ", 
      apa(se, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(olow, k), 
      " - ", 
      apa(ohigh, k), "]", sep = "")
}

####eta squared and r####
eta.anova = function (dfm = 1, dfe = 10, f = 10, a = .05, k = 2) {
  eta = (dfm*f) / (dfm*f + dfe)
  ahigh = a / 2
  alow = 1-(a/2)
  ncplow = ncpf(x = f, q = alow, df1 = dfm, df2 = dfe)
  ncphigh = ncpf(x = f, q = ahigh, df1 = dfm, df2 = dfe)
  elow = ncplow / (ncplow + dfm + dfe + 1)
  ehigh = ncphigh / (ncphigh + dfm + dfe + 1)
  p = pf(f, dfm, dfe, lower.tail = F)
  cat("F(", dfm, ", ", dfe, ") = ", 
      apa(f, k),
      ", ", 
      p.value(p, k), 
      ", n2 = ", 
      apa(eta, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(elow, k), 
      " - ", 
      apa(ehigh, k), "] \nNote: use np2 when you have multiple IVs.", sep = "")
}

####proportion test####
d.prop = function (p1 = 0, p2 = 0, n1 = 10, n2 = 10, a = .05, k = 2) {
  ppooled = (p1*n1+p2*n2) / (n1+n2)
  se = sqrt( ppooled * (1-ppooled) * ((1/n1) + (1/n2)))
  z = (p1-p2)/ se
  p = pnorm(abs(z), lower.tail = F)*2

  se1 = sqrt((p1*(1-p1) / n1))
  se2 = sqrt((p2*(1-p2) / n2))
  z1 = p1 / se1
  z2 = p2 / se2
  alow = a / 2
  ahigh = 1-(a/2)
  z1low = z1 - qnorm(alow, lower.tail = F)*se1
  z1high = z1 + qnorm(alow, lower.tail = F)*se1
  z2low = z2 - qnorm(alow, lower.tail = F)*se2
  z2high = z2 + qnorm(alow, lower.tail = F)*se2
  d = z1 - z2

  dlow = d - qnorm(alow, lower.tail = F)*se
  dhigh = d + qnorm(alow, lower.tail = F)*se
  
  cat("Z1 = ", 
      apa(z1, k),
      ", SE = ", apa(se1, k),
      ", ", (1-a)*100, "%CI[", 
      apa(z1low, k),
      " - ",
      apa(z1high, k),
      "]",
      "\nZ2 = ", 
      apa(z2, k),
      ", SE = ", apa(se2, k),
      ", ", (1-a)*100, "%CI[", 
      apa(z2low, k),
      " - ",
      apa(z2high, k),
      "]",
      "\nZ = ", 
      apa(z, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      apa(d, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(dlow, k), 
      " - ", 
      apa(dhigh, k), "]", sep = "")
}

####correlation  r to r2####
r.correl = function (r = .00, n=10, a = .05, k = 2) {
  rsq = r^2
  se = sqrt(4*rsq*((1-rsq)^2)*((n - 3)^2)/((n^2-1)*(3+n)))
  t = r / sqrt((1-rsq)/(n-2))
  Fvalue = t^2
  ahigh = a / 2
  alow = 1-(a/2)
  dfm = 1
  dfe = n-2
  ncplow = ncpf(x = Fvalue, q = alow, df1 = dfm, df2 = dfe)
  ncphigh = ncpf(x = Fvalue, q = ahigh, df1 = dfm, df2 = dfe)
  rsqlow = ncplow / (ncplow + dfm + dfe + 1)
  rsqhigh = ncphigh / (ncphigh + dfm + dfe + 1)
  p = pf(Fvalue, dfm, dfe, lower.tail = F)
  cat("r = ", 
      apa(r, k),
      " t(", (n-2), ") = ", 
      apa(t, k),
      ", ", 
      p.value(p, k), 
      ", r2 = ",
      apa(rsq, k), 
      ", ", (1-a)*100, "%CI[", 
      apa(rsqlow, k), 
      " - ", 
      apa(rsqhigh, k), "]", sep = "")
}

