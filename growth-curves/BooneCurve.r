#BooneData = read.table("c:\\BooneCurveData2.txt", h=F, sep=",",col.names = c("Attacked", "PropKilled"))

#Booneetal2011
0.5309, 0.0553
1.5302, 0.1988
2.2797, 0.154
6.9015, 0.5367
10.7738, 0.5339
23.39, 0.7529
2.2797, 4.6323e-3
2.4046, 0.0674
4.653, 0.4439
10.7738, 0.6444
30.8848, 0.9714
125.09, 0.8553
0.6558, 0.0105
1.0305, 0.4317
2.6543, 0.402
2.9042, 0.3004
0.9056, 0.5243
1.0305, 0.2555
3.0291, 0.6141
23.0153, 0.8035
41.0028, 0.9452
0.6558, 0.3242
3.5288, 0.5066
3.279, 0.2497
17.644, 0.8182
0.9056, 0.378
8.0257, 0.6442
0.7807, 0.5721

BooneData = read.table("c:\\Users\\Barry\\BooneCurveData2.txt", h=F, sep=",",col.names = c("Attacked", "PropKilled"))

str(BooneData)

plot(PropKilled~Attacked, data=BooneData)
plot(PropKilled~log(Attacked), data=BooneData)
BooneData$Site = c(rep("A",6), rep("B",6), rep("D",5), rep("E",4), rep("F",4), rep("G",3))
BooneData$Year = c(2000:2005, 2000:2005, 2001:2005, 2002:2005, 2002:2005, 2003:2005)

logit = function(p){
 log(p/(1-p))
}

plot(logit(PropKilled) ~ (log(Attacked)), data = BooneData)
Yr2000 = subset(BooneData, Year == "2000")
Yr2001 = subset(BooneData, Year == "2001")
Yr2002 = subset(BooneData, Year == "2002")
Yr2003 = subset(BooneData, Year == "2003")
Yr2004 = subset(BooneData, Year == "2004")
Yr2005 = subset(BooneData, Year == "2005")

lm01 = lm(logit(PropKilled)~log(Attacked), data = Yr2001)
summary(lm01)

lm02 = lm(logit(PropKilled)~log(Attacked), data = Yr2002)
summary(lm02)

lm03 = lm(logit(PropKilled)~log(Attacked), data = Yr2003)
summary(lm03)

lm04 = lm(logit(PropKilled)~log(Attacked), data = Yr2004)
summary(lm04)

lm05 = lm(logit(PropKilled)~log(Attacked), data = Yr2005)
summary(lm05)

win.graph(width=10,height=6)
par(mfrow=c(1,2))

#Barry Cooke's logit linearization
plot(logit(PropKilled)~log(Attacked), xlim=c(-1,5),ylim=c(-6,5), pch=19, col = "black", data = Yr2001,main="Cooke's logit function",xlab="log (trees per ha per year attacked)")
points(logit(PropKilled)~log(Attacked), pch=19, col = "yellow", data = Yr2002)
points(logit(PropKilled)~log(Attacked), pch=19, col = "blue", data = Yr2003)
points(logit(PropKilled)~log(Attacked), pch=19, col = "green", data = Yr2004)
points(logit(PropKilled)~log(Attacked), pch=19, col = "red", data = Yr2005)

#lines(log(Yr2001$Attacked),fitted(lm01), col = "black")
#lines(log(Yr2002$Attacked),fitted(lm02), col = "yellow")
#lines(log(Yr2003$Attacked),fitted(lm03), col = "blue")
#lines(log(Yr2004$Attacked),fitted(lm04), col = "green")
#lines(log(Yr2005$Attacked),fitted(lm05), col = "red")

curve(-3.374+ 1.764*x,-1,5,add=T,col="black") #lm01
curve(-0.821+ 0.050*x,-1,5,add=T,col="yellow") #lm02
curve(-0.445+ 0.406*x,-1,5,add=T,col="blue") #lm03
curve(-2.941+ 1.608*x,-1,5,add=T,col="green") #lm04
curve(+0.3068+ 0.4072*x,-1,5,add=T,col="red") #lm05

legend(3,-2.5,c(2001:2005), col = c("black","yellow","blue","green","red"), lty = 1)

#Devin's Goodsman's Hill function

hill = function(a,b,x){
 exp(a)*x^b/(1+exp(a)*x^b)
}

#sigmoidal hill function plot
plot(PropKilled~log(Attacked), xlim=c(-1,5),ylim=c(0,1), pch=19, col = "black", data = Yr2001,main="Goodsman's Hill function",xlab="log (trees per ha per year attacked)")
points(PropKilled~log(Attacked), pch=19, col = "yellow", data = Yr2002)
points(PropKilled~log(Attacked), pch=19, col = "blue", data = Yr2003)
points(PropKilled~log(Attacked), pch=19, col = "green", data = Yr2004)
points(PropKilled~log(Attacked), pch=19, col = "red", data = Yr2005)

curve(hill(-3.374,1.764,exp(x)),-1,5,add=T,col="black") #lm01
curve(hill(-0.821,0.050,exp(x)),-1,5,add=T,col="yellow") #lm02
curve(hill(-0.445,0.406,exp(x)),-1,5,add=T,col="blue") #lm03
curve(hill(-2.941,1.608,exp(x)),-1,5,add=T,col="green") #lm04
curve(hill(+0.3068,0.4072,exp(x)),-1,5,add=T,col="red") #lm05

win.graph(width=10,height=6)
par(mfrow=c(1,2))
fudge<-1.2  #0.9
fudge2<-.9
par(las=1)
#curve(log(hill(-0.821,0.050,exp(x))),-2,5,col="red",lwd=3,ylim=c(-4,2),main="Allee effect relaxed",xlab="log attack density(trees/ha/yr)",ylab="log component survival")
#curve(fudge-0.03*exp(x),-2,5,add=T,col="blue",lwd=2)
#curve(fudge2-0.03*exp(x),-2,5,add=T,col="black",lwd=2,lty=2)
#curve(log(hill(-0.821,0.050,exp(x)))+(fudge-0.03*exp(x)),-2,5,add=T,col="purple",lwd=3)
#abline(h=0)
#abline(v=2)

curve(log(hill(-2.941,1.608,exp(x))),-2,5,col="red",lwd=3,ylim=c(-4,2),main="Allee effect of host defense strong",xlab="log attack density(trees/ha/yr)",ylab="log component recruitment")
curve(fudge-0.03*exp(x),-2,5,add=T,col="blue",lwd=3,lty=2)
curve(fudge2-0.03*exp(x),-2,5,add=T,col="black",lwd=2,lty=1)
curve(log(hill(-2.941,1.608,exp(x)))+(fudge-0.03*exp(x)),-2,5,add=T,col="purple",lwd=3,lty=2)
abline(h=0)
abline(v=2)
abline(v=1.4,lwd=2,lty=1,col="purple")

curve(log(hill(-0.445,0.406,exp(x))),-2,5,col="red",lwd=3,ylim=c(-4,2),main="Allee effect of host defense weakened",xlab="log attack density(trees/ha/yr)",ylab="log component recruitment")
curve(fudge-0.03*exp(x),-2,5,add=T,col="blue",lwd=3,lty=2)
curve(fudge2-0.03*exp(x),-2,5,add=T,col="black",lwd=2,lty=1)
curve(log(hill(-0.445,0.406,exp(x)))+(fudge-0.03*exp(x)),-2,5,add=T,col="purple",lwd=3,lty=2)
abline(h=0)
abline(v=2)
abline(v=-.95,lwd=2,lty=1,col="purple")

BooneAttackData = read.table("c:\\Users\\Barry\\manuscripts\\Booneattacksuccess.txt", header=T, sep=" ")
attach(BooneAttackData)
win.graph()
#plot(FemPerHa,PropResist)
plot(log10(FemPerHa),log10(PropResist/(1-PropResist)))
attack.lm<-summary(lm(log10(PropResist/(1-PropResist))~log10(FemPerHa)))

#Grouped for Janice
stressed = rbind(Yr2002,Yr2003,Yr2005)
unstressed = rbind(Yr2000,Yr2001,Yr2004)
lm.s= lm(logit(PropKilled)~log(Attacked), data = stressed)
summary(lm.s)
lm.u= lm(logit(PropKilled)~log(Attacked), data = unstressed)
summary(lm.u)

pdf("C:\\Users\\Barry\\BooneCurveData_StressedUnstressed.pdf",width=6,height=6)
#win.graph(width=6,height=6)
plot(PropKilled~log(Attacked), xlim=c(-1,5),ylim=c(0,1), pch=19,
	col = "blue", data = unstressed,main="MPB Recruitment Curves",
	xlab="log (trees per ha per year attacked)",
	ylab="proportion of attacked treees killed")
points(PropKilled~log(Attacked), pch=19, col = "red", data = stressed)
curve(hill(-3.3556,1.6621,exp(x)),-1,5,add=T,col="blue",lwd=2) #lm.u
curve(hill(-0.5587,0.5661,exp(x)),-1,5,add=T,col="red",lwd=2) #lm.s
legend(-0.85,0.95,c("unstressed, r2 = 0.72","stressed, r2 = 0.63"), col = c("blue","red"),lty = 1,lwd=2)
dev.off()
