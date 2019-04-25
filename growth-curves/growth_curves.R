#install_github("achubaty/amc@development")
library(amc)

## Boone et al. 2011 (Fig 2)
BooneData <- read.csv("growth-curves/BooneCurveData2.csv")
BooneData$Year <- c(2000:2005, 2000:2005, 2001:2005,
                    2002:2005, 2002:2005, 2003:2005)

## use 2004 data; why data from this year only?
## - had to do with weather/stand conditions?
## - it is the only statistically significant fit.
Yr2004 <- subset(BooneData, Year == "2004")
lm04 <- lm(amc::logit(PropKilled) ~ log(Attacked), data = Yr2004)
summary(lm04)

a <- 0.85              ## slope? - how quickly the curve drops off
b <- 0.03              ##
yint2 <- 0.9           ## y-intercept/assymptote; from MacQuarrie 2011 (Fig 3d)
yint1 <- yint2 + 0.3   ## somewhat arbitrary; chosen so that the resulting curve passes 1 when flexed

xmin <- -2
xmax <- 6

## cooperation curve (red)
coop <- function(x, a, b0, b1) {
  log(amc::hill(b0, b1, exp(a * x)))
}

## mortality from emigration/dispersal
# r: relative stocking value (0,1) -- i.e., r = 1 - proportion_depletion
# d: slope parameter [1,Inf)
# s: scaling parameter (0,1)
m_e <- function(r, d, s) {
  s * exp(1 - d * r)
}

## competition curve (blue)
comp <- function(x, a, b, d, s, r, yint) {
  yint - m_e(r, d, s) - b * exp(a * x)
}

################################################################################
s <- 0.5
d <- 3
r <- seq(0.0, 1, 0.2)

## plot the curves
png("growth-curves/cooperation-competition-curves.png", width = 1200, height = 800)
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]), xmin, xmax,
      col = "red", lwd = 3, ylim = c(-4, 2), cex = 3, cex.axis = 1.5, cex.lab = 1.5,
      #main = "Allee effect of host defense strong",
      xlab = "log attack density(trees/ha/yr)",
      ylab = "log component recruitment")
abline(h = 0, cex = 2)
abline(v = 0, cex = 2)
abline(v = 2, lty = 3, cex = 2)
curve(comp(x, a = a, b = b, d = d, s = s, r = r[1], yint = yint1),
      xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2, cex = 3) ## 100% depletion
curve(comp(x, a = a, b = b, d = d, s = s, r = r[2], yint = yint1),
      xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2, cex = 3) ## 80% depletion
curve(comp(x, a = a, b = b, d = d, s = s, r = r[3], yint = yint1),
      xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2, cex = 3) ## 60% depletion
curve(comp(x, a = a, b = b, d = d, s = s, r = r[4], yint = yint1),
      xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2, cex = 3) ## 40% depletion
curve(comp(x, a = a, b = b, d = d, s = s, r = r[5], yint = yint1),
      xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 2, cex = 3) ## 20% depletion
curve(comp(x, a = a, b = b, d = d, s = s, r = r[6], yint = yint1),
      xmin, xmax, add = TRUE, col = "blue", lwd = 2, lty = 1, cex = 3) ## 0% depletion
text(c(-1.25, -1.25), c(1.3, -0.4), c("0% stand depletion", "100% stand depletion"), cex = 2)
dev.off()

png("growth-curves/composite-curves.png", width = 1200, height = 800)
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]), xmin, xmax,
      col = "white", lwd = 3, ylim = c(-4, 2), cex = 3, cex.axis = 1.5, cex.lab = 1.5,
      #main = "Allee effect of host defense strong",
      xlab = "log attack density(trees/ha/yr)",
      ylab = "log component recruitment")
abline(h = 0, cex = 2)
abline(v = 0, cex = 2)
abline(v = 2, lty = 3, cex = 2)
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]) +
        comp(x, a = a, b = b, d = d, s = s, r = r[1], yint = yint1),
      xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2, cex = 3) ## 100% depletion
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]) +
        comp(x, a = a, b = b, d = d, s = s, r = r[2], yint = yint1),
      xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2, cex = 3) ## 80% depletion
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]) +
        comp(x, a = a, b = b, d = d, s = s, r = r[3], yint = yint1),
      xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2, cex = 3) ## 60% depletion
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]) +
        comp(x, a = a, b = b, d = d, s = s, r = r[4], yint = yint1),
      xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2, cex = 3) ## 40% depletion
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]) +
        comp(x, a = a, b = b, d = d, s = s, r = r[5], yint = yint1),
      xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2, cex = 3) ## 20% depletion
curve(coop(x, a = a, b0 = lm04$coefficients[[1]], b1 = lm04$coefficients[[2]]) +
        comp(x, a = a, b = b, d = d, s = s, r = r[6], yint = yint1),
      xmin, xmax, add = TRUE, col = "purple", lwd = 3, lty = 2, cex = 3) ## 0% depletion
text(c(-1.25, 0.90), c(-2.25, -3.65), c("0% stand depletion", "100% stand depletion"), cex = 2)
dev.off()
