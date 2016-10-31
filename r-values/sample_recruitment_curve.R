########################################################################
# Recruitment curve modeling: a nonlinear example                      #
########################################################################

library(magrittr)
library(phaseR)
library(plot3D)
library(RColorBrewer)
library(rgl)
library(scatterplot3d)

# control parameters govern strength of positive and negative first and second-order feedback
a1 = 0.80
a2 = -0.89

# create empty data structures for X[t] and R[t]
X = c(1, 0.5, rep(NA, 98))
R = c(rep(NA, 100))

# Run simulation, using a standard nonlinear recruitment function
for (i in 2:100) {
  R[i] <- 1 - exp(-a1*X[i] - a2*X[i-1])
  X[i+1] <- X[i] + R[i]
}

# Plot the nonlinear time series
dev.new(noRStudioGD = TRUE)
#par(las=1)
plot(X, type="l", xlab=c("time"), ylab=c("X[t]"))
points(X, pch=19, col="black")

# Try to recover the model parameters used in generating the simulation
R <- X[2:100]-X[1:99]
traj2 <- data.frame(Xt = X[2:99], Xt_1 = X[1:98], Rt = R[2:99])

# Plot the trajectory in the phase plane, with fitted equation overplotted as surface
dev.new(noRStudioGD = TRUE)
#par(las=1)
traj2.s3d <- scatterplot3d(traj2, scale.y = 1, color = "black", pch = 19,
                           zlim = c(-0.75, 0.75),
                           xlab = "X[t]", ylab = "X[t-1]", zlab = "R[t]")

start0 <- c(a = 1, b = 0.5, c = -0.5)  # starting value
traj2.nlm <- nls(Rt ~ a + exp(b*Xt + c*Xt_1), data = traj2, start = start0)
summary(traj2.nlm)

traj2.s3d$plane3d(traj2.nlm, lty.box="solid")

n.pred <- 51
Xt.pred <- seq(-6, 4, length.out = n.pred)
Xt_1.pred <- seq(-6, 4, length.out = n.pred)
XtXt_1 <- expand.grid(Xt = Xt.pred, Xt_1 = Xt_1.pred)

Rt.pred <- matrix(nrow = n.pred, ncol = n.pred,
                  data = predict(traj2.nlm, newdata = data.frame(XtXt_1), 
                  interval = "prediction"))

# predicted z-values, fitted points for droplines to surface
fitpoints <- predict(traj2.nlm)

# some sample colour palettes to use for plotting
colours.alt.rnbw <- c("#781c81", "#3f4ea1", "#4683c1", "#57a3ad", "#6db388", "#b1be4e", "#dfa53a", "#e7742f", "#d92120")
colours.blue.red <- c(rev(brewer.pal(9, "Blues")[3:8]), brewer.pal(9, "Reds")[3:8])
colours.spectral <- brewer.pal(11, "Spectral")


dev.new(noRStudioGD = TRUE)
scatter3D(x = traj2$Xt, y = traj2$Xt_1, z = traj2$Rt, theta = 20, phi = 20,
          xlab = "X[t]", ylab = "X[t-1]", zlab = "R[t]", clab = "R[t]",
          pch = 18, cex = 2, lwd = 1.5, bty = "g", ticktype = "detailed",
          xlim = c(-6, 4), ylim = c(-6, 4), zlim = c(-1, 1),
          surf = list(x = Xt.pred, y = Xt_1.pred, z = Rt.pred, facets = NA),
          col = colours.blue.red,
          colkey = list(length = 0.8, width = 0.4),
          main = "nonlinear reproduction surface")

dev.new(noRStudioGD = TRUE)
bg3d("white")
plot3d(x = traj2$Xt, y = traj2$Xt_1, z = traj2$Rt, type="p",
      xlab = "X[t]", ylab = "X[t-1]", zlab = "R[t]",
      pch = 18, cex = 2, lwd = 1.5, site=5,
      xlim = c(-6, 4), ylim = c(-6, 4), zlim = c(-1, 1),
      main = "nonlinear reproduction surface")
persp3d(Xt.pred, Xt_1.pred, Rt.pred,
        xlim = c(-6, 4), ylim = c(-6, 4), zlim = c(-1, 1),
        col = "grey", alpha = 0.2)
