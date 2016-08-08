#read in test dataset
autism<- read.csv("C:/Users/laujohns/Documents/figures-for-longitudinal-data/autism.csv")
attach(autism)
detach(autism)

### time-varying variables ###
#VSAE=dependent variable (social development score)
#AGE=independent variable (time-varying)

### time-varying variables ###
#CHILDID= identification variable
#SICDEPG=independent variable (children's expressive language score at baseline; fixed effect)

#reformat some variables
autism$age2<-as.factor(autism$age)
autism$sicdegp2<-factor(autism$sicdegp, levels=c(1, 2, 3), labels=c("SICDEGP=1", "SICDEGP=2", "SICDEGP=3"))
attach(autism)

#check to make sure formatting correct
class(autism$age2)
class(autism$sicdegp2)

#run quick summary stats for mean vsae at each age and by each baseline expressive score (SICDE group)
tapply(vsae, age, mean, na.rm=T)
tapply(vsae, sicdegp, mean, na.rm=T)

#load necessary packages
library(mgcv)
library(nlme)
library(stats)
library(ggplot2)

#create datasets
autism.sicde1<-subset(autism,sicdegp==1)
autism.sicde2<-subset(autism,sicdegp==2)
autism.sicde4<-subset(autism,sicdegp==3)

#No missing data, but if there were we would need to create complete datasets
test.males.complete<-na.omit(test.males[,c("id", "age", "tolerance", "sex", "exposure","time")])
test.females.complete<-na.omit(test.females[,c("id", "age", "tolerance", "sex", "exposure","time")])

#Spaghetti plot to visualize individual change in tolerance variable over time by sex

plot.1<-ggplot(data=autism, aes(age, vsae, colours=as.factor(childid)))+ 
                 geom_point(size=0.2)+geom_line(colour=childid, size=0.2)+facet_grid( ~ sicdegp2, labeller= label_value)+
  scale_x_continuous(name="Age (years)", breaks=c(2, 4, 6, 8, 10, 12)) + 
  scale_y_continuous(name="VSAE") +
  theme_bw()+
    ggtitle("Spaghetti Plot Showing Individual Change In\nVSAE Score Across Age and by SICDE Group")+
    theme(plot.title=element_text(face="bold",size=14, family="sans"), 
          axis.text.x = element_text(angle = 0, hjust = 1,vjust=1, size=10,family="sans"), 
          axis.text.y=element_text(vjust=1, size=10,family="sans"),
          text=element_text(size=14,family="sans"), 
          strip.text.x=element_text(size=14, face="bold",family="sans"),
          legend.title=element_blank())
plot.1




















test2<-read.txt("http://www-personal.umich.edu/~bwest/rat_brain.dat")
test2
###now use gam package
library(gam)

gam.ft4<-gam(bwz~ns(ft4_2,df=5)+gat2+age+factor(race_cat_new)+t1bmi+factor(insur2)+factor(parity_cat)+factor(gender1), weights=weight, na.action=na.omit, data=ft4.v2.complete)
summary(gam.ft4)
plot(gam.ft4, ask=T, main="Natural spline with df=5 using 'gam' package")
par(mfrow=c(1,1))
AIC(gam.ft4)

gam2.ft4<-gam(bwz~lo(ft4_2)+gat2+age+factor(race_cat_new)+t1bmi+factor(insur2)+factor(parity_cat)+factor(gender1),weights=weight,na.action=na.omit, data=ft4.v2.complete)
summary(gam2.ft4)
plot(gam2.ft4, ask = T, main="Loess smoothing term using 'gam' package")
AIC(gam2.ft4) 

gam3.ft4<-gam(bwz~s(ft4_2)+gat2+age+factor(race_cat_new)+t1bmi+factor(insur2)+factor(parity_cat)+factor(gender1),weights=weight,na.action=na.omit, data=ft4.v2.complete)
summary(gam3.ft4)
plot(gam3.ft4, ask = T, main="Smoothing term using 'gam' package")
AIC(gam3.ft4)

###use mcgv package
library(mgcv)

rat.brain <- read.table("http://www-personal.umich.edu/~bwest/rat_brain.dat", h = T) 
attach(rat.brain) 

region.f <- region
region.f[region == 1] <- 1
region.f[region == 2] <- 2
region.f[region == 3] <- 0
region.f <- factor(region.f)
treat <- treatment
treat[treatment == 1] <- 0
treat[treatment == 2] <- 1

library(nlme)

# Random intercept model
lme.1<-lme(activate~treatment*region, random=~1|animal, data=rat.brain)
summary(lme.1)

# Random intercept + slope model
lme.2<-lme(activate~treatment*region, random=~1+region|animal, data=rat.brain)
summary(lme.2)
anova(lme.1, lme.2)

gamm.1<-gamm(tolerance~s(time, k=3), random=list(id=~1), data=test) #fewer than k unique covariate values because k=10 by default; change k to 5
AIC(gamm.1)
summary(gamm.1$gam)
plot(gamm.1$gam, page=1)


#Regression spline
regspline.ft4<-gam(bwz~s(log(ft4_2), fx=T, k=5)+gat2+age+factor(race_cat_new)+t1bmi+factor(insur2)+factor(parity_cat)+factor(gender1), weights=weight, na.action=na.omit,data=ft4.v2.complete)
plot(regspline.ft4, residuals = T, main="Regression spline using 'mgcv' package: ln-transformed FT4")
AIC(regspline.ft4)#1029

#RPenalized spline
penspline.ft4<-gam(bwz~s(log(ft4_2), fx=F)+gat2+age+factor(race_cat_new)+t1bmi+factor(insur2)+factor(parity_cat)+factor(gender1), weights=weight, na.action=na.omit,data=ft4.v2.complete)
plot(penspline.ft4, residuals = T, main="Penalized spline using 'mgcv' package: ln-transformed FT4")
AIC(penspline.ft4)#AIC









#Model usng a gamm function with
t3.ptb.gamm1<-gamm(t3~s(gat, by=factor(preterm)), random=list(id=~1+gat), data=t3.gamm.complete)
summary(t3.ptb.gamm1$gam) 
plot(t3.ptb.gamm1$gam, page=1)

###Predicted values
predict.t3.ptb<-predict(t3.ptb.gamm1$gam, type="terms", se.fit=T)
predict.t3.ptb$fit[1:10,] #column 1 is controls (preterm=0), column 2 is cases(preterm=1)

fit.t3.controls<-predict.t3.ptb$fit[,1]+t3.ptb.gamm1$gam$coeff[1]
fit.t3.cases<-predict.t3.ptb$fit[,2]+t3.ptb.gamm1$gam$coeff[1]
summary(fit.t3.cases)

controls.se.plus<-fit.t3.controls+(1.96*predict.t3.ptb$se.fit[,1])
controls.se.minus<-fit.t3.controls-(1.96*predict.t3.ptb$se.fit[,1])
summary(controls.se.plus)

cases.se.plus<-fit.t3.cases+(1.96*predict.t3.ptb$se.fit[,2])
cases.se.minus<-fit.t3.cases-(1.96*predict.t3.ptb$se.fit[,2])
summary(cases.se.minus)

###Order and plot
o<-order(gat)
o.case<-order(gat[preterm==1])
o.control<-order(gat[preterm==0])

par(mar=c(6,5,3,2))
plot(gat[o], t3[o], col="white",xaxt='n',yaxt='n',xlab="", ylab="",ylim=c(1,2))
polygon(c(gat[preterm==1][o.case],
          rev(gat[preterm==1][o.case])),
        c(cases.se.minus[preterm==1][o.case],  
          rev(cases.se.plus[preterm==1][o.case])),
        col='purple4',
        border=NA)
polygon(c(gat[preterm==0][o.control], 
          rev(gat[preterm==0][o.control])),
        c(controls.se.minus[preterm==0][o.control], 
          rev(controls.se.plus[preterm==0][o.control])),
        col='gray64',
        border=NA)
lines(gat[preterm==1][o.case],
      fit.t3.cases[preterm==1][o.case],
      lty=2, col=1, lwd=2)
lines(gat[preterm==1][o.case],
      cases.se.plus[preterm==1][o.case],
      lty=2, col=1, lwd=0.5)
lines(gat[preterm==1][o.case],
      cases.se.minus[preterm==1][o.case],
      lty=2, col=1, lwd=0.5)
lines(gat[preterm==0][o.control],
      fit.t3.controls[preterm==0][o.control],
      lty=1, col=1, lwd=2)
lines(gat[preterm==0][o.control],
      controls.se.plus[preterm==0][o.control],
      lty=1, col=1, lwd=0.5)
lines(gat[preterm==0][o.control],
      controls.se.minus[preterm==0][o.control],
      lty=1, col=1, lwd=0.5)
leg.txt<-c("Cases","Controls")
legend(30,1.2,leg.txt,lty=c(2,1),lwd=c(2,2),cex=1, col=c("purple4", "grey64"))
title(xlab="GA at Sample Collection (Weeks)",cex.lab=1.2, font=2)
title(ylab=expression(paste("T3 (ng/mL)")),cex.lab=1.2, font=2)
axis(1,cex.axis=1.00)
axis(2,cex.axis=1.00)
title(main="Predicted T3 Across Pregnancy: \nBy Case-Control Status")
text(37,1.95,"p(interaction)< 0.0001",adj=1,cex=1)
rug(gat, side=1, quiet = getOption("warn") < 0, lwd=1)

