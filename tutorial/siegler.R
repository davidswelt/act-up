# R Code to analyze the Siegler output


s11 = c(0 ,  .05, .86 , 0  ,.02 , 0 , .02 , 0 ,  0 , .06)
s12 = c(0 ,  .04 ,.07 ,.75 ,.04 , 0 , .02 , 0 ,  0 , .09)
s13 = c(0 ,  .02 , 0  ,.10 ,.75 ,.05, .01 ,.03,  0 , .06)
s22 = c(.02 , 0 , .04 ,.05 ,.80 ,.04 , 0 , .05 , 0 ,  0)
s23 = c(0   , 0 , .07 ,.09 ,.25 ,.45 ,.08,.01, .01, .06)
s33 = c(.04 , 0 ,  0  ,.05 ,.21 ,.09 ,.48 , 0 , .02, .11)

pl <- function (problem, actup, actr, siegler, add=FALSE)
{
sx = c(0,1,2,3,4,5,6,7,8,NA)
if (add)
{
  lines(names(table(actup)),table(actup)/length(actup), type="l", col="red", lty=2)
} else
{
  plot(names(table(actup)),table(actup)/length(actup), type="l", col="red", lty=2,ylim=c(0,0.85),xlim=c(0,8),main=problem, ylab="p", xlab="response")
}
lines(sx,siegler, type="l", col="blue")
lines(names(table(actr)),table(actr)/length(actr), lwd=2, col="black", lty=3)
}

quartz()
pl("1+1", d$c11, ds$c11, s11)

quartz()
pl("1+2", d$c12, ds$c12, s12)

quartz()
pl("2+3", d$c23, ds$c23, s23)

quartz()
pl("3+3", d$c33, ds$c33, s33)

quartz()
pl("", d$c12, ds$c12, s12)
pl("", d$c11, ds$c11, s11, TRUE)
pl("", d$c33, ds$c33, s33, TRUE)

# all others:
pl("", d$c23, ds$c23, s23, TRUE)
pl("", d$c22, ds$c22, s22, TRUE)
pl("", d$c12, ds$c12, s12, TRUE)
pl("", d$c22, ds$c22, s22, TRUE)
pl("", d$c13, ds$c13, s13, TRUE)

# legend
pdf("response-legend.pdf", height=4, width=5)
plot(c(0,5),c(0.1,0.1), type="l", col="blue", ylim=c(0,0.85),xlim=c(0,8),main="", ylab="p", xlab="response")
lines(c(0,5),c(0.2,0.2),  type="l", col="red", lty=2)
lines(c(0,5),c(0.3,0.3), lwd=2, col="black", lty=3)
dev.off()
