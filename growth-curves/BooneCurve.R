library(amc)

## Boone et al. 2011 (Fig 2)
BooneData <- read.csv("growth-curves/BooneCurveData2.csv")

str(BooneData)

plot(PropKilled ~ Attacked, data = BooneData)
plot(PropKilled ~ log(Attacked), data = BooneData)
BooneData$Site <- c(rep("A", 6), rep("B", 6), rep("D", 5),
                    rep("E", 4), rep("F", 4), rep("G", 3))
BooneData$Year <- c(2000:2005, 2000:2005, 2001:2005,
                    2002:2005, 2002:2005, 2003:2005)

plot(logit(PropKilled) ~ (log(Attacked)), data = BooneData)
Yr2000 <- subset(BooneData, Year == "2000")
Yr2001 <- subset(BooneData, Year == "2001")
Yr2002 <- subset(BooneData, Year == "2002")
Yr2003 <- subset(BooneData, Year == "2003")
Yr2004 <- subset(BooneData, Year == "2004")
Yr2005 <- subset(BooneData, Year == "2005")

lm01 <- lm(logit(PropKilled) ~ log(Attacked), data = Yr2001)
summary(lm01)

lm02 <- lm(logit(PropKilled) ~ log(Attacked), data = Yr2002)
summary(lm02)

lm03 <- lm(logit(PropKilled) ~ log(Attacked), data = Yr2003)
summary(lm03)

lm04 <- lm(logit(PropKilled) ~ log(Attacked), data = Yr2004)
summary(lm04)

lm05 <- lm(logit(PropKilled) ~ log(Attacked), data = Yr2005)
summary(lm05)

dev.new(width = 10, height = 6)
par(mfrow = c(1, 2))

# Barry Cooke's logit linearization
plot(logit(PropKilled) ~ log(Attacked), xlim = c(-1, 5), ylim = c(-6, 5), pch = 19,
     col = "black", data = Yr2001, main = "Cooke's logit function",
     xlab = "log (trees per ha per year attacked)")
points(logit(PropKilled) ~ log(Attacked), pch = 19, col = "yellow", data = Yr2002)
points(logit(PropKilled) ~ log(Attacked), pch = 19, col = "blue", data = Yr2003)
points(logit(PropKilled) ~ log(Attacked), pch = 19, col = "green", data = Yr2004)
points(logit(PropKilled) ~ log(Attacked), pch = 19, col = "red", data = Yr2005)

curve(lm01$coefficients[[1]] + lm01$coefficients[[2]] * x, -1, 5, add = TRUE, col = "black")
curve(lm02$coefficients[[1]] + lm02$coefficients[[2]] * x, -1, 5, add = TRUE, col = "yellow")
curve(lm03$coefficients[[1]] + lm03$coefficients[[2]] * x, -1, 5, add = TRUE, col = "blue")
curve(lm04$coefficients[[1]] + lm04$coefficients[[2]] * x, -1, 5, add = TRUE, col = "green")
curve(lm05$coefficients[[1]] + lm05$coefficients[[2]] * x, -1, 5, add = TRUE, col = "red")

legend(3, -2.5, c(2001:2005), col = c("black", "yellow", "blue", "green", "red"), lty = 1)

## Devin Goodsman's Hill function
## (this ends up being equivalent as Cooke's approach)
hill <- function(a, b, x) {
 exp(a) * x^b / (1 + exp(a) * x^b)
}

# sigmoidal hill function plot
plot(PropKilled ~ log(Attacked), xlim = c(-1, 5), ylim = c(0, 1), pch = 19,
     col = "black", data = Yr2001, main = "Goodsman's hill function",
     xlab = "log (trees per ha per year attacked)")
points(PropKilled ~ log(Attacked), pch = 19, col = "yellow", data = Yr2002)
points(PropKilled ~ log(Attacked), pch = 19, col = "blue", data = Yr2003)
points(PropKilled ~ log(Attacked), pch = 19, col = "green", data = Yr2004)
points(PropKilled ~ log(Attacked), pch = 19, col = "red", data = Yr2005)

curve(hill(lm01$coefficients[[1]], lm01$coefficients[[2]], exp(x)), -1, 5, add = TRUE, col = "black")  ## lm01
curve(hill(lm02$coefficients[[1]], lm02$coefficients[[2]], exp(x)), -1, 5, add = TRUE, col = "yellow") ## lm02
curve(hill(lm03$coefficients[[1]], lm03$coefficients[[2]], exp(x)), -1, 5, add = TRUE, col = "blue")   ## lm03
curve(hill(lm04$coefficients[[1]], lm04$coefficients[[2]], exp(x)), -1, 5, add = TRUE, col = "green")  ## lm04
curve(hill(lm05$coefficients[[1]], lm05$coefficients[[2]], exp(x)), -1, 5, add = TRUE, col = "red")    ## lm05

dev.new(width = 10, height = 6)
par(mfrow = c(1, 2))
yint2 <- 0.9          ## from MacQuarrie 2011 (Fig 3d)
yint1 <- yint2 + 0.3 ## somewhat arbitrary;
                       ## chosen so that the resulting curve passes 1 when flexed

par(las = 1)
xmin <- -2
xmax <- 6

# unweakened host defences (2004 fit)
a <- 0.9
curve(log(hill(lm04$coefficients[[1]], lm04$coefficients[[2]], exp(a * x))), xmin, xmax,
      col = "red", lwd = 3, ylim = c(-4, 2),
      main = "Allee effect of host defense strong",
      xlab = "log attack density(trees/ha/yr)",
      ylab = "log component recruitment")
curve(yint1 - 0.03 * exp(1 * x), xmin, xmax, add = TRUE, col = "darkgreen", lwd = 3, lty = 1)
curve(yint1 - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 1)

## mortality from emigration/dispersal
# r: relative stocking value (0,1)
# d: slope parameter [1,Inf)
# s: scaling parameter (0,1)
m_e <- function(r, d, s) {
  s * exp(1 - d * r)
}

s <- 0.9
d <- 3
r <- seq(0.2, 1, 0.2)
curve(yint1 - m_e(r[1], d, s) - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2)
curve(yint1 - m_e(r[2], d, s) - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2)
curve(yint1 - m_e(r[3], d, s) - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2)
curve(yint1 - m_e(r[4], d, s) - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2)
curve(yint1 - m_e(r[5], d, s) - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2)

## intensified competition via host removal (thinning)
r <- seq(0.2, 1, 0.2)
curve(yint1 - 0.03 * exp(a * (x + r[1])), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 2)
curve(yint1 - 0.03 * exp(a * (x + r[2])), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 2)
curve(yint1 - 0.03 * exp(a * (x + r[3])), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 2)
curve(yint1 - 0.03 * exp(a * (x + r[4])), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 2)
curve(yint1 - 0.03 * exp(a * (x + r[5])), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 2)

curve(yint2 - 0.03 * exp(a * x), xmin, xmax, add = TRUE, col = "black", lwd = 2, lty = 1)
curve(log(hill(lm04$coefficients[[1]], lm04$coefficients[[2]], exp(a * x))) +
        (yint1 - 0.03 * exp(a * x)), xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2)
abline(h = 0)
abline(v = 2)
abline(v = 1.4, lwd = 2, lty = 1, col = "purple") ## needs to actually be the intersect

# weakened host defenses (2003 fit)
curve(log(hill(lm03$coefficients[[1]], lm03$coefficients[[2]], exp(x))), xmin, xmax,
      col = "red", lwd = 3, ylim = c(-4,2),
      main = "Allee effect of host defense weakened",
      xlab = "log attack density(trees/ha/yr)",
      ylab = "log component recruitment")
curve(yint1 - 0.03 * exp(x), xmin, xmax, add = TRUE, col = "blue", lwd = 3, lty = 2)
curve(yint2 - 0.03 * exp(x), xmin, xmax, add = TRUE, col = "black", lwd = 2, lty = 1)
curve(log(hill(lm03$coefficients[[1]], lm03$coefficients[[2]], exp(x))) +
        (yint1 - 0.03 * exp(x)), xmin, xmax, add = TRUE,
      col = "purple", lwd = 3, lty = 2)
abline(h = 0)
abline(v = 2)
abline(v = -0.95, lwd = 2, lty = 1, col = "purple")

# grouped
stressed <- rbind(Yr2002, Yr2003, Yr2005)
unstressed <- rbind(Yr2000, Yr2001, Yr2004)
lm.s <- lm(logit(PropKilled) ~ log(Attacked), data = stressed)
summary(lm.s)
lm.u <- lm(logit(PropKilled) ~ log(Attacked), data = unstressed)
summary(lm.u)

pdf("figures/BooneCurveData_StressedUnstressed.pdf", width = 6, height = 6)
#dev.new(width = 6, height = 6)
plot(PropKilled ~ log(Attacked), xlim = c(-1, 5), ylim = c(0, 1), pch = 19,
     col = "blue", data = unstressed, main = "MPB Recruitment Curves",
     xlab = "log (trees per ha per year attacked)",
     ylab = "proportion of attacked treees killed")
points(PropKilled ~ log(Attacked), pch = 19, col = "red", data = stressed)
curve(hill(lm.u$coefficients[[1]], lm.u$coefficients[[2]], exp(x)), -1,5, add = TRUE,
      col = "blue", lwd = 2)
curve(hill(lm.s$coefficients[[1]], lm.s$coefficients[[2]], exp(x)), -1,5, add = TRUE,
      col = "red", lwd = 2)
legend(-0.85, 0.95, c("unstressed, r2 = 0.72", "stressed, r2 = 0.63"),
       col = c("blue", "red"), lty = 1, lwd = 2)
dev.off()
