#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Supplement to: MacQuarrie CJK and BJ Cooke. Density-dependant population dynamics of 
# mountain pine beetle in natural and thinned forests. 
#
# This file contains a series of helper functions used in the analysis of mountain pine beetle
# reproduction rate data. For more information please see the header to Supplement 1.R
#
# If you find our functions useful, please feel free to use them but include a reference to our paper.
#
# This script was written at the Northern Forestry Centre of Natural Resources Canada Canadian Forest Service
# in Edmonton, Alberta, Canada. 
#
# File produced using Notepad++ ver. 4.8.2 http://notepad-plus.sourceforge.net) with syntax highlighting  
# via NppToR (http://sourceforge.net/projects/npptor/) 
# 
# Chris J K MacQuarrie July 2009.
# cjkmacquarrie@gmail.com                                 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Function text.box
#####################################

#writes the value of label.text into a box, used to create headers and axis labels in matrix layout plots

text.box <- function (label.text = "", ...)
			{

			plot( 0, 0, xlim = c( 0, 1), ylim = c( 0, 1), ann = F, xaxt = "n", 
										 yaxt = "n", bty = "n", pch = NA)

			polygon( x = c( -2, -2, 2, 2), y = c( -2, 2, 2, -2) )

			text( x = 0.5, y = 0.5, adj = c( 0.5, 0.5), labels = label.text, ... )

			}

#Function plot.box
######################################

# similar to text.box, plots the raw data and fitted model (along with its confidence intervals) 
# along a specified sequence, will also write other information into the corners of the plot if specified, for  
# instance, as coded it asks for the model equation, an id label for the plot and the plot name. 

# Currently hard coded to accept only lnxt as a predictor, but this could be modified...

plot.box <- function ( pred.seq = seq( -2, 6.5, 0.25 ), model = "", x.data = "", y.data = "", 
					   name.plot = "", equation.text = "", IDletter.plot = "", 
					   cex = 1.5, col.lines = "black", ...)
			{

			new <- data.frame( lnxt = pred.seq )

			conflimits <- predict( model, new, interval = "confidence")

			matplot( 
					 new$lnxt, cbind( conflimits ), ylim = c( -6, 4), xlim = c( -2, 6),
					 lty = c( 1,2,2), type = "l", xaxt = "n", ann = F,  col = col.lines,  
					 lwd = 1, bty = "n", ... 
                    )
	
					axis( side = 1, pos = 0 ) 
					
					abline( h = 0 )

			points( x.data, y.data ,pch = 19, cex = 1.0, col = "black"  )

			legend( x = "topright", legend = name.plot, bty = "n", ... )

			legend( x = "bottomleft", bty = "n", legend = equation.text, ... ) 

			legend( x = "topleft", legend = IDletter.plot, bty = "n", ...)

			}

# Function multipleX2
######################################

# computes a chi-square goodness-of-fit test for each of the four Rt vs Nt models, along with a shifted version of 
# the 'limit' model, then writes the test statistics from each to a common output file.

multipleX2 <- function( x, obs)
				{
				obs <- exp(obs) # back transform the input lnxt values

				# control forest linear model
				lin1 <- chisq.test(x = obs, p =  exp( 2.19 - 0.63 * x ) / 
												 sum( exp( 2.19 - 0.63 * x ) ) 
								  )

				# monitored forest linear model
				lin2 <- chisq.test(x = obs, p = exp( 1.01 - 0.40 * x ) / 
												sum( exp( 1.01 - 0.40 * x ) ) 
								   )

				# limit forest exponential model
				nlin1 <- chisq.test(x = obs, p = exp( 0.39 -0.04*exp(x) ) / 
												 sum( exp( 0.39 -0.04 * exp(x) ) ) 
									) 

				# basal forest exponential model
				nlin2 <- chisq.test(x = obs, p = exp( 0.90 -0.03*exp(x) ) / 
												 sum( exp( 0.90 - 0.03 * exp(x) ) ) 
									) 

				# limit model shifted up by 2
				nlin3 <- chisq.test(x = obs, p = exp( 2.39 -0.04*exp(x) )/ 
												 sum( exp( 2.39 - 0.04 * exp(x) ) ) 
									) 
				
				models <- list( lin1, lin2, nlin1, nlin2, nlin3 )

				out <- data.frame(
								X2 = sapply( models, function(x) x$statistic),

								degfree = sapply( models, function(x) x$parameter),

								pvalue = sapply( models, function(x) x$p.value),

								row.names = c("control", "monitor", "limit", "basal", "limit+2")
								 )
				}		


# a workhorse function to plot the panels in Figure 3
panelformat.2 = function(x,y, ...)
			{

			panel.xyplot(x,y, pch = 20, col = "black", fill = "white", ...)

			panel.abline(h = 0)
			# the exponetial curve
			panel.curve( expr = 0.62 -0.03*exp(x), from = 1, to = 5, col = c("black") ) 

			# the linear curve
			panel.curve( expr = 2.19 - 0.63 * x, from = 1, to = 5, col = c("black") ) 

			# the exponetial curve shifted upwards
			panel.curve( expr = 2+0.62 -0.03*exp(x), from = 1, to = 5, col = c("black"),
						 lty = "46" ) 
			
			}


