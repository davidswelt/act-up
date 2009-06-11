h <- read.csv("Challenge/human_data.csv", header=T)
## [1] "Version"      "Subject"      "Time.Step"    "EnvirInFlow"  "EnvirOutFlow"
## [6] "UserInFlow"   "UserOutFlow"  "AmountInTank" "Goal"        


# subject 33 is fucked-up

h <- subset(h, Subject!='t33')
h$UserFlow <- -h$UserOutFlow+h$UserInFlow
quartz()
library(Hmisc)

pfunc <- function(x,y,...) {
  panel.xYplot(x, y, ...)
  panel.abline(h=4.0, col = "grey")
#  panel.xYplot(-d$chosenValveSetting , d$step,  type="l")
  if (Vers != 0) {
    d2 <- subset(d, Version==Vers | Vers==0)
    panel.lines(d2$step,  d2$chosenValveSetting , type="l", xlime=c(0,100), col="black")
    panel.lines(d2$step,  d2$waterLevel , type="l", xlime=c(0,100), col="black")
  }
}
gr <- function(version=0, subj=0) {
# shows empirical data as aggregate w/ confidence intervals
  # and single model prediction on top of that
  d <<- read.table("water.txt", header=T);

  Vers <<- version

  xYplot( AmountInTank + UserFlow ~ Time.Step,  data=subset(h, (Vers==0 | Version==Vers) & (Subject==subj | subj==0)), nx=F, method=smean.cl.boot, type='l',lty.bands=c(2,2), ylim=c(-20,20), panel=pfunc, main=version)
 
}

ha <- function(version=0, subj=0) {
 # shows empirical data and model prediction as aggregate w/ confidence intervals
 Vers <- version ;
 #quartz();
 xYplot(AmountInTank + UserFlow  ~ Time.Step, nx=F, data= subset(h, Vers==0 | Version==Vers), method=smean.cl.boot, type='l',lty.bands=c(2,2), ylim=c(-15,15),
        main=paste("Subjects: ", version)) ;  #        sub="Subjects: water level and chosen in/outflow valve")
}
ma <- function(version=0, subj=0) {
 # shows empirical data and model prediction as aggregate w/ confidence intervals
 d <<- read.table("water.txt", header=T);

  Vers <- version ;

# quartz();
 xYplot( waterLevel + chosenValveSetting  ~ step,  nx=F, data=subset(d, Vers==0 | Version==Vers), method=smean.cl.boot, type='l',lty.bands=c(2,2), ylim=c(-15,15),
        col=c("black", "orange"), main=paste("Model: ", version));
                                        #, sub="Model: water level and chosen in/outflow valve")
}


pl <- function () {
    pl.simple("Sine");
    pl.simple("Sine2");
    gr("Linear increase");
    dev.set(which = dev.next());
    gr("Linear decrease");
    dev.set(which = dev.next());
    gr("Non Linear increase");
    dev.set(which = dev.next());
    gr("Non Linear decrease");
    dev.set(which = dev.next());
}

pl.pdf <-  function () {
pdf("human-l-i.pdf")
gr("Linear increase")
dev.off()
pdf("human-l-d.pdf")
gr("Linear decrease")
dev.off()


pdf("human-n-i.pdf")
gr("Non Linear increase")
dev.off()
pdf("human-n-d.pdf")
gr("Non Linear decrease")
dev.off()

}
 
pl.simple <- function (version=0) {
  d <<- read.table("water.txt", header=T)
  d <- subset(d, Version==version | version==0)
  
  plot(d$waterLevel, type="l", ylim=c(min(d$waterLevel, d$assumedRatio,d$trend,-d$chosenValveSetting),max(d$waterLevel, d$assumedRatio,d$trend,-d$chosenValveSetting)))
  abline(h=4.0, col="grey")
  
  #lines(d$assumedVratio, type="l", col="red")
  
  lines(-d$chosenValveSetting, type="l", col="orange")
  lines(d$trend, type="l", col="red")
  
#  abline(h=1.0, col="orange")
  #abline(a=0.2, b=0.1, col="orange")
}

graphs <- function () {

pdf("li_human.pdf")
ha("Linear increase")
dev.off()


pdf("nd_human.pdf")
ha("Non Linear decrease")
dev.off()

 


pdf("li_model.pdf")
ma("Linear increase")
dev.off()


pdf("nd_model.pdf")
ma("Non Linear decrease")
dev.off()


pdf("sine_model.pdf")
ma("Sine")
dev.off()


pdf("sine2_model.pdf")
ma("Sine2")
dev.off()

}

d <<- read.table("water.txt", header=T)
# envWait calcTimeFac blendTemp subject step Version trend waterLevel chosenValveSetting
d$Subject=d$subject
d$Time.Step=d$step
d$AmountInTank=d$waterLevel

# compare means

optimize <- function () {
xdf <- data.frame()
for (v in levels(as.factor(d$Version)))
{
h.means <- with(subset(h,Version==v), tapply(AmountInTank, list(Time.Step), mean, simplify=TRUE))
for (b in levels(as.factor(d$blendTemp)))
{
  for (e in levels(as.factor(d$envWait)))
    {
      for (c in levels(as.factor(d$calcTimeFac)))
        {
          d2 <-  subset(d, Version==v & blendTemp==b & envWait==e & calcTimeFac==c)
          ll <- tapply(d2$AmountInTank, d2$Time.Step, mean)
          ##ss <- rowbind(ss, data.frame(blendTemp=b, envWait=e, calcTimeFac=c,
          print(c(v,b,e,c))
          mcor=cor(ll, h.means)
          print(mcor)
          xdf <- rbind(xdf, data.frame(version=v,blendTemp=b,envWait=e,calcTimeFac=c,meancor=mcor))
        }
    }
}}
return(xdf)
}


# combine 10 steps per subject and determine variance
# --> 10 means per subject
# then take mean of these variances across all subjects

# then look at correlation

optimize2 <- function () {
  xdf <- data.frame()

for (b in levels(as.factor(d$blendTemp)))
{
  d1 <- subset(d, blendTemp==b)
  for (e in levels(as.factor(d$envWait)))
    {
      d2 <- subset(d1, envWait==e)
      for (c in levels(as.factor(d$calcTimeFac)))
        {
          d3 <- subset(d2, calcTimeFac==c)
          for (v in levels(as.factor(h$Version)))
            {
              d.vars.means = c()
              h.vars.means = c()

              d4 <- subset(d3, Version==v)
              h4 <- subset(h, Version==v)
              for (i in 0:9) # step group
                {
                  d5 <- subset(d4, Time.Step>=i*10 & Time.Step<(i+1)*10)
                  h5 <- subset(h4, Time.Step>=i*10 & Time.Step<(i+1)*10)
                  d.vars = c()
                  for (s in levels(as.factor(d5$Subject)))
                    {
                                        # collection variance for step group and this subject
                      d.vars <- c(d.vars, var(subset(d5,  Subject==s)$AmountInTank))
                    }
                  d.vars.means = c(d.vars.means, mean(d.vars))
                  
                  h.vars = c()
                  for (s in levels(as.factor(h5$Subject)))
                    {
                      dx <- subset(h5,Subject==s)
                                        # collection variance for step group and this subject
                      if (length(dx$Version)>0)
                        {
                          h.vars <- c(h.vars, var(dx$AmountInTank))
                        }
                    }
                  h.vars.means = c(h.vars.means, mean(h.vars))
                }

              vcor <- cor(d.vars.means, h.vars.means)
              print(vcor)
              xdf <- rbind(xdf, data.frame(version=v,blendTemp=b,envWait=e,calcTimeFac=c,varcor=vcor))

            }
        }}} 
return(xdf)
}





d.means <- with(d, tapply(AmountInTank, list(blendTemp,envWait,calcTimeFac,Time.Step), mean, simplify=FALSE))
h.means <- with(h, tapply(AmountInTank, list(Time.Step), mean, simplify=FALSE))

d.var <- with(d, tapply(AmountInTank, list(blendTemp,envWait,calcTimeFac,Time.Step), var))
h.var <- with(h, tapply(AmountInTank, list(Time.Step), var))

# for each factor combination
cor.means <- with(d.means, by(d.means, list(blendTemp,envWait,calcTimeFac), function (a) {cor(a, h.means)}))
cor.var <- by(d.var, list(blendTemp,envWait,calcTimeFac), function (a) {cor(a, h.var)})



