#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Supplement to: MacQuarrie CJK and BJ Cooke. Density-dependant population dynamics of 
# mountain pine beetle in natural and thinned forests. 
#
# This file contains the code necessary to reproduce our analysis and construct figures 2 and 3 (note the 
# publication version of Figures 2 and 3 were touched up a bit in Inscape prior to publication. Currently, 
# the lattice package does not make it easy to add supplemental text to the panels, so I added the 
# figure identification labels (a,b,c, etc...) and the asterisks (*) to Figure 3 by hand.
#                                  
# The script was built and tested in R version 2.8.0 and requires additional packages available from CRAN.
# Please note the script attempts to load these packages at start-up and R will produce warnings if the packages  
# are not installed on your system. The script will also attempt to load an accessory file of utility functions 
# Again ,if these are not found running the script will produce errors. 
#
# If you find our functions useful, please feel free to use them but include a reference to our paper.
#  
# This script will attempt to load the two datasets that accompanied this file, if you neglected to download these
# files, please do so now. Permission is granted to use these data in other applications as long as the data are 
# attributed to the original authors/collectors. See the meta-data for the two datasets for this information.
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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!! ATTENTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#                                              
# please set the location of the helperfunctions.R and data files before continuing
#
cwd <- #e.g. "c:\\mystuff\\my_R\\cooldata\\"
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Prologue - Establish the analysis environment

infunctions <-  paste( cwd, "helperfunctions.R", sep = "") 

indatafile1 <-  paste( cwd, "StudyData.csv", sep = "") 

indatafile2 <-  paste( cwd, "USDAFSData.csv", sep = "") 

# load the neccesary packages and functions
library( nlme ); library( lattice ); library( grid )

source( infunctions ) 

#read in the data extracted from the 8 pine beetle tree mortality studies  

treemort <- read.table( indatafile1, header = TRUE, sep = ",", dec=".", fill = TRUE)

#read in the data extracted from the US Forest Service pine mortality surveys  

usforests <- read.table( indatafile2, header = TRUE, sep = ",", dec=".", fill = TRUE )


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End Prologue !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



##########
#   R    #
##########

# Part 1 - Model the natural and thinned forest data from the 8 pine beetle tree mortality studies


#declare a useful vector

treats<-c("control", "monitor", "limit", "basal")


#Calculate the reproduction rate (r) for all records in the treemort data

attach(treemort)

reprod.rate<-numeric()

for (i in 1:length(yearabs))
	{
	# if two adjacent rows in the data frame are from the same study and are from consequtive years, calculate r
	# if not, set r = 0 
	x <-ifelse(Study[i+1]==Study[i] & yearabs[i+1]==(yearabs[i]+1), # if true..
							per_ha_dead[i+1]/per_ha_dead[i], 		# set x = r
							0 										# if not set x = 0
			   )

	reprod.rate<-append(reprod.rate, x, i)

	}

detach(treemort)

# add the vector of r values to the treemort data frame and clean up
treemort <- cbind( treemort, reprod.rate ) 				
rm( reprod.rate, x, i) 									

# calculate the natural log of the reproduction rate (R) and population size (X)
treemort$lnr <- log( treemort$reprod.rate ) 				
treemort$lnxt <- log( treemort$per_ha_dead ) 				

#remove any 0 values
treemort.no.r0 <- treemort[ which( treemort$reprod.rate > 0 & treemort$per_ha_dead > 0 ), ] 

#partition the data by treatment into four datasets.

cntrl <- treemort.no.r0[ which( treemort.no.r0$Treat == "control" ), ]
limit <- treemort.no.r0[ which ( treemort.no.r0$Treat == "limit" ), ]
basal <- treemort.no.r0[ which( treemort.no.r0$Treat == "basal" ), ]
monitor <- treemort.no.r0[ which( treemort.no.r0$Treat == "monitor" ), ]

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#fit the three parameter model to each data set. extract the parameter values and show them in the console


fm1control.nls <- nls ( lnr ~ a - b * exp( c * lnxt), data = cntrl, 
							start = c( a = 1, b = 1, c = 1 ) )

fm1limit.nls <- nls( lnr ~ a - b * exp( c * lnxt ), data = limit, 
							start = c( a = 1, b = 1, c = 1 ) )

fm1basal.nls <- nls( lnr ~ a - b * exp( c * lnxt ), data = basal, 
							start = c( a = 0.5, b = 0.5, c = 0.5 ) )

fm1monitor.nls <- nls( lnr ~ a - b * exp(c * lnxt), data = monitor, 
								start = c( a = 1, b = 1, c = 1) )

#extract the three parameter values from each fitted model and show in console

avalues<-rbind( summary(fm1control.nls)$coef[1,1:2], summary(fm1monitor.nls)$coef[1,1:2], 
				summary(fm1limit.nls)$coef[1,1:2], summary(fm1basal.nls)$coef[1,1:2] )

bvalues<-rbind( summary(fm1control.nls)$coef[2,1:2], summary(fm1monitor.nls)$coef[2,1:2], 
				summary(fm1limit.nls)$coef[2,1:2], summary(fm1basal.nls)$coef[2,1:2] )

cvalues<-rbind( summary(fm1control.nls)$coef[3,1:2], summary(fm1monitor.nls)$coef[3,1:2], 
				summary(fm1limit.nls)$coef[3,1:2], summary(fm1basal.nls)$coef[3,1:2] )

nlssummary.table<-as.data.frame( cbind( avalues, bvalues, cvalues), row.names = treats)

dimnames( nlssummary.table )[[2]]<-c("a", "a_std", "b", "b_std", "c", "c_std")

rm( avalues, bvalues, cvalues )

nlssummary.table


#Model the control data
########################

attach( cntrl )

#fit a linear regression model 

fm1control.lm <- lm ( lnr ~ lnxt )
summary ( fm1control.lm )				#check the summary
plot( fm1control.lm )					#look at the plots 

#is the linear model significant
anova( fm1control.lm ) #yes

detach( cntrl )


#model the monitoring data
##########################

attach( monitor )

#fit a linear regression 

fm1monitor.lm <- lm( lnr ~ lnxt )
summary( fm1monitor.lm) 				#check the summary
plot( fm1monitor.lm )

#is the model sig?
anova( fm1monitor.lm ) #Yes (marginal)

detach( monitor )



#Model the limit data
#######################

attach( limit )

#fit a linear  model
fm1limit.lm <- lm( lnr ~ lnxt )

summary( fm1limit.lm )				#check the summary

plot( fm1limit.lm)

#fit the exponential model. 
fm2limit.lm <- lm( lnr ~ exp( lnxt ) )

summary( fm2limit.lm )

plot( fm2limit.lm )				

#is the exponential model significant
anova( fm2limit.lm ) #Yes

detach(limit)


#Model the basal area data
########################

attach( basal )

#fit a linear  model 
fm1basal.lm <- lm( lnr ~ lnxt ) 

summary( fm1basal.lm )				

plot( fm1basal.lm )

#fit the exponential model 
fm2basal.lm<-lm( lnr ~ exp( lnxt ) )

summary( fm2basal.lm )

plot( fm2basal.lm )

#is the exponential model sig?
anova( fm2basal.lm ) #Yes

detach( basal )


#Compare the four linear models
###############################

#extract and plot the slope and intercepts of the linear model with standard errors, show in console

intercepts<-rbind( summary(fm1control.lm)$coef[1,1:2], summary(fm1monitor.lm)$coef[1,1:2], 
				summary(fm1limit.lm)$coef[1,1:2], summary(fm1basal.lm)$coef[1,1:2] )

slopes<-rbind( summary(fm1control.lm)$coef[2,1:2], summary(fm1monitor.lm)$coef[2,1:2], 
				summary(fm1limit.lm)$coef[2,1:2], summary(fm1basal.lm)$coef[2,1:2] )

lmsummary.table<-as.data.frame( cbind( intercepts, slopes), row.names = treats)

dimnames( lmsummary.table )[[2]]<-c("i", "i_std", "slope", "slope_std")

rm( intercepts, slopes )

lmsummary.table


#Compare the four exponential decline models
############################################

#fit the exponential model to the control and limit data
fm2control.lm <- lm ( lnr ~ exp(lnxt), data = cntrl )
fm2monitor.lm <- lm( lnr ~ exp(lnxt), data = monitor )


#extract and plot the slope and intercepts of the linear model with standard errors
#display in console


intercepts<-rbind( summary(fm2control.lm)$coef[1,1:2], summary(fm2monitor.lm)$coef[1,1:2], 
				summary(fm2limit.lm)$coef[1,1:2], summary(fm2basal.lm)$coef[1,1:2] )

slopes<-rbind( summary(fm2control.lm)$coef[2,1:2], summary(fm2monitor.lm)$coef[2,1:2], 
				summary(fm2limit.lm)$coef[2,1:2], summary(fm2basal.lm)$coef[2,1:2] )

lmexpsummary.table<-as.data.frame( cbind( intercepts, slopes), row.names = treats)

dimnames( lmexpsummary.table )[[2]]<-c("i", "i_std", "slope", "slope_std")

rm( intercepts, slopes )

lmexpsummary.table

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#############################################################
#Plot Figure 2
#############################################################


#a 2x2 layout with marginal y and x axis, subtitles above panels a+b and c+d;

x11( height = 6.5, width = 9)

nf <- layout( matrix( 
						c( 0, 1, 1, 2, 3, 4, 2, 5, 5, 2, 6, 7, 0, 8, 8), 
						nrow = 5, ncol = 3, byrow = T 
					), 
				widths = c( 1, 4, 4 ), 

				heights = c( 1, 8, 1, 8, 2 ) 
			)

par( mar = c( 0, 0, 0, 0 ) )

# plot a+b subtitle
text.box(label.text = expression( bold( "natural forests") ), cex = 2.0 ) 

#y-axis label
text.box(label.text = expression( 
								bold( 
									paste( log[e], " (", bolditalic( x[t+1]/x[t] ) , " )" ) 
									) 
								) , 
								srt = 90, cex = 2.0
		)

par( mar = c(1,2,1,1) )

#monitoring data
plot.box( pred.seq = seq( -1,6,0.25), model = fm1monitor.lm, # determine the predicted values from  
															 # fm1monitor.lm for the values given in the 
															 # pred.seq vector 

		  x.data = monitor$lnxt, y.data = monitor$lnr,          # the raw data

		  IDletter.plot = "a", name.plot = "monitor",        # label the plot

		  equation.text = expression( paste( 	italic(y), " = 1.01 - 0.40", italic(x), "; ", 
												italic(r)^2, " = 0.20; ", 
												italic(F)["1,92"], " = 24.92; ", 
												italic(p), " < 0.01" 
											) 
									), 
		  cex = 1.5
		)

#control data
plot.box( pred.seq = seq( 1,6,0.25), model = fm1control.lm, x.data = cntrl$lnxt, 
		  y.data = cntrl$lnr, cex = 1.5, IDletter.plot = "b", name.plot = "control", 
		equation.text = expression( paste( 	italic(y), " = 2.19 - 0.63", italic(x), "; ", 
											italic(r)^2, " = 0.35; ", 
											italic(F)["1,29"], " = 15.63; ", 
											italic(p), " < 0.01" ) 
								  )  
		)

par( mar = c( 0, 0, 0, 0 ) )

# plot c+d subtitle
text.box(label.text = expression( bold( "thinned forests") ), cex = 2.0 ) 

par( mar = c(1,2,1,1) )

#limit data
plot.box( pred.seq = seq( -2,6,0.25), model = fm2limit.lm, x.data = limit$lnxt, y.data = limit$lnr, 
		cex = 1.5, IDletter.plot = "c", name.plot = "limit", 
		equation.text = expression( paste( 	italic(y), " = 0.39 - 0.04e"^italic(x), "; ", 
											italic(r)^2, " = 0.50; ", 
											italic(F)["1,31"], " = 33.31; ", 
											italic(p), " < 0.01" ) 
								 ) 
		)


#basal data
plot.box( pred.seq = seq( -2,6,0.25), model = fm2basal.lm, x.data = basal$lnxt, 
		  y.data = basal$lnr, cex = 1.5, IDletter.plot = "d", name.plot = "basal", 
		equation.text = expression( paste( 	italic(y), " = 0.90 - 0.03e"^italic(x), "; ", 
											italic(r)^2, " = 0.48; ", 
											italic(F)["1,26"], " = 26.68; ", 
											italic(p), " < 0.01" ) 
								  ) 
		)

#x-axis

par( mar = c( 0, 0, 0, 0 ) )

text.box(label.text = expression( bold( paste( log[e], bolditalic( " n"[t] ) ) ) ), cex = 2.0 )

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End Part 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Part 2 - Test reproduction rates predicted from the four models against observed tree mortality from 27 
# locations in the United States

# NOTES
# 1) This analysis implements a chi-square goodness-of-fit test via a custom function, multipleX2, that calls 
# chisq.test internally. MultipleX2 computes values of the Goodness-of-fit chi-square statistic comparing observed 
# r values to r values predicted from Xt using the 2 linear and 2 exponential models developed in part 1. Within 
# the function data are transformed from Rt to r and Xt to xt (using exp). This procedure was necessary to meet 
# the assumption of no negative values inherent to a chi-square test and because applying chisq.test to negative 
# values produces a critical error (chisq.test will fail to run). 

# 2) Chi-square tests for < 5 observations are typically not advised and therefore applying multipleX2 for forests 
# with < 5 observations will cause R to throw warnings even though it will calculate the test statistics. A 
# different test may be more appropriate here (e.g., Fisher's exact test, or a G test), for our purposes the 
# chi-square approach was sufficient. 

# declare 2 useful vectors. Note that districts are administrative subunits of forests, see Appendix B.

forests <- c( "BH", "DL", "FL", "GL", "KO", "LO", "NZ")


districts <- c("BLA", "JEF", "GVW", "HHR", "SPB", "SWL", "TAL", "FLA", "CAB",
			"FIR", "FOR", "LIB", "REX", "YAK", "NIM", "PLA", "SUP", "THF", "ELK", 
			"RED")


# Pre-analysis Data manipulation
################################

#calculate some values
usforests$r <- usforests$xtplusone / usforests$xt
usforests$lnr <- log( usforests$r )
usforests$lnxt <- log( usforests$xt )

#limit the analysis to data points where xt != 0
usforests <- usforests[ which( usforests$xt > 0 & usforests$r > 0 ), ]

#loose all records for "AVG" districts = Forest averages, are legacy values in the dataset
usforests<-usforests[ which( usforests$district != "AVG"), ]



#test forests 
#######################

attach(usforests)

#Beaverhead NF
BH <- multipleX2( x = lnxt[ which( forest == "BH") ], obs <- lnr[ which( forest == "BH") ] ) 

#Deerlodge NF
DL <- multipleX2( x = lnxt[ which( forest == "DL") ], obs <- lnr[ which( forest == "DL") ] ) 

#Flathead NF
FL <- multipleX2( x = lnxt[ which( forest == "FL") ], obs <- lnr[ which( forest == "FL") ] ) 

#Gallatin NF
GL <- multipleX2( x = lnxt[ which( forest == "GL") ], obs <- lnr[ which( forest == "GL") ] ) 

#Kootenai NF
KO <- multipleX2( x = lnxt[ which( forest == "KO") ], obs <- lnr[ which( forest == "KO") ] ) 

#Lolo NF
LO <- multipleX2( x = lnxt[ which( forest == "LO") ], obs <- lnr[ which( forest == "LO") ] ) 

#Nez Perce NF
NZ <- multipleX2( x = lnxt[ which( forest == "NZ") ], obs <- lnr[ which( forest == "NZ") ] ) 

#test districts
####################


#Blackfoot IR
BLA <- multipleX2( x = lnxt[ which( district == "BLA") ], obs <- lnr[ which( district == "BLA") ] ) 
#Jefferson RD
JEF <- multipleX2( x = lnxt[ which( district == "JEF") ], obs <- lnr[ which( district == "JEF") ] ) 
#Flathead IR
FLA <- multipleX2( x = lnxt[ which( district == "FLA") ], obs <- lnr[ which( district == "FLA") ] ) 
#Glacier view RD
GVW <- multipleX2( x = lnxt[ which( district == "GVW") ], obs <- lnr[ which( district == "GVW") ] ) 
#Hungry Horse RD
HHR <- multipleX2( x = lnxt[ which( district == "HHR") ], obs <- lnr[ which( district == "HHR") ] ) 
#Spotted Bear RD
SPB <- multipleX2( x = lnxt[ which( district == "SPB") ], obs <- lnr[ which( district == "SPB") ] ) 
#Swan Lake
SWL <- multipleX2( x = lnxt[ which( district == "SWL") ], obs <- lnr[ which( district == "SWL") ] ) 
#Tally Lake
TAL <- multipleX2( x = lnxt[ which( district == "TAL") ], obs <- lnr[ which( district == "TAL") ] ) 
#Cabinet RD
CAB <- multipleX2( x = lnxt[ which( district == "CAB") ], obs <- lnr[ which( district == "CAB") ] ) 
#Fisher River RD
FIR <- multipleX2( x = lnxt[ which( district == "FIR") ], obs <- lnr[ which( district == "FIR") ] ) 
#Fortine RD
FOR <- multipleX2( x = lnxt[ which( district == "FOR") ], obs <- lnr[ which( district == "FOR") ] ) 
#Libby RD
LIB <- multipleX2( x = lnxt[ which( district == "LIB") ], obs <- lnr[ which( district == "LIB") ] ) 
#Rexford RD
REX <- multipleX2( x = lnxt[ which( district == "REX") ], obs <- lnr[ which( district == "REX") ] ) 
#Yaak RD
YAK <- multipleX2( x = lnxt[ which( district == "YAK") ], obs <- lnr[ which( district == "YAK") ] ) 
#Ninemile RD
NIM <- multipleX2( x = lnxt[ which( district == "NIM") ], obs <- lnr[ which( district == "NIM") ] ) 
#Plains RD
PLA <- multipleX2( x = lnxt[ which( district == "PLA") ], obs <- lnr[ which( district == "PLA") ] ) 
#Superior RD
SUP <- multipleX2( x = lnxt[ which( district == "SUP") ], obs <- lnr[ which( district == "SUP") ] ) 
#Thompson Falls RD
THF <- multipleX2( x = lnxt[ which( district == "THF") ], obs <- lnr[ which( district == "THF") ] ) 
#Elk RD
ELK <- multipleX2( x = lnxt[ which( district == "ELK") ], obs <- lnr[ which( district == "ELK") ] ) 
#Red River RD
RED <- multipleX2( x = lnxt[ which( district == "RED") ], obs <- lnr[ which( district == "RED") ] ) 

detach(usforests)

# combine the test statistics from the chi-square tests for all the forests into one data frame, do the same for 
# the statistics from the tests of the individual districts 

# forests

summary.forests <- NULL

cntrl <- NULL ; monitor <- NULL ; limit <- NULL ; basal <- NULL ; limit2 <- NULL

for (i in 1:length(forests) ) 
				{ 
				
				cntrl <- rbind( cntrl, as.list( get( forests[i])[1,] ) ) 

				monitor <- rbind( monitor, as.list( get( forests[i])[2,] ) ) 

				limit <- rbind( limit, as.list( get( forests[i])[3,] ) ) 

				basal <- rbind( basal, as.list( get( forests[i])[4,] ) ) 

				limit2 <- rbind( limit2, as.list( get( forests[i])[5,] ) ) 

				summary.forests <- cbind(cntrl, monitor, limit, basal, limit2)

				}
summary.forests <- cbind(forests, summary.forests) 


# districts

summary.districts <- NULL

cntrl <- NULL; monitor <- NULL; limit <- NULL; basal <- NULL; limit2 <- NULL

for (i in 1:length(districts) ) 
				{ 
				
				cntrl <- rbind(cntrl, as.list(get(districts[i])[1,] ) ) 

				monitor <- rbind(monitor, as.list(get(districts[i])[2,] ) ) 

				limit <- rbind(limit, as.list(get(districts[i])[3,] ) ) 

				basal <- rbind(basal, as.list(get(districts[i])[4,] ) ) 

				limit2 <- rbind(limit2, as.list(get(districts[i])[5,] ) ) 

				summary.districts <- cbind(cntrl, monitor, limit, basal, limit2)

				}
summary.districts <- cbind(districts, summary.districts) 

#write the summaries to the console

# columns 2-4 are for tests of the 'monitor' model, 5-7 are tests of the 'control' model, 8-10 are tests of the 
# 'limit' model, columns 11-13 are tests of the 'basal' model, columns 14-16 are tests of the 'limit+2' model

summary.forests

summary.districts


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#############################################################
#Plot Figure 3
#############################################################

# make two datasets, one containing the data for the forests that were tested, the other containing the districts
# that were tested

surv_for <- subset(usforests, forest%in%forests)
surv_dist <- subset(usforests, district%in%districts) 
surv_dist$district <- ordered(surv_dist$district, levels = districts)

# bind the two datasets together and add a column of ID numbers, ID values are set so that plots are layed out
# in alphabetical order in the final plot. This is not the most elegant way of doing this.

surveyareas <- rbind( cbind( surv_for, ID = rep( c(1:7), 
						times = as.vector( unlist( lapply( 
									  split( surv_for$forest, f=surv_for$forest, drop = T ), length) 
											) ) ) ),
 
					  cbind( surv_dist, ID = rep( c(8, 16, 14, 15, 22, 24, 25, 9, 10, 12, 13,
												 17, 21, 27, 18, 19, 23, 26, 11, 20), 
							times = as.vector( unlist( lapply( 
									 split( surv_dist$district, f=surv_dist$district, drop = T ), 
										    length) 
											) ) ) )
                    )

surveyareas$ID <- ordered(surveyareas$ID)


strip.titles <- c("Beaverhead N.F.", "Deerlodge N.F.", "Flathead N.F.", "Gallatin N.F.", 
				  "Kootenai N.F.", "Lolo N.F.", "Nez Perce N.F.", "Blackfeet I.R.", "Flathead I.R.", 
				  "Cabinet R.D.", "Elk City R.D.", "Fisher River R.D.", "Fortine R.D.",
				  "Glacier View R.D.", "Hungry Horse R.D.", "Jefferson R.D.", "Libby R.D.", 
				  "Nine Mile R.D.","Plains R.D.", "Red River R.D.", "Rexford R.D.", 
				  "Spotted Bear R.D.", "Swan lake R.D.", "Tally Lake R.D.",  "Superior R.D.", 
				  "Thompson Falls R.D.", "Yaak R.D." )

all_areas_alt <- xyplot( lnr ~ lnxt | ID,  data = surveyareas, panel = panelformat.2, as.table = T, 
						layout = c(5, 6), skip = c( rep(F, 7), T, rep(F, 22) ),

                          strip = strip.custom(factor.levels = strip.titles, bg = "white", 
									par.strip.text = list(col = "black", cex = 0.50) ),

 	                      ylab = list( label = expression( 
								  bold( paste( log[e], " (", bolditalic(n[t+1]/n[t] ), " )" ) ) ) ), 

						  xlab = list( label = expression( 
								  bold( paste( log[e], bolditalic( " n"[t] ) ) ) ) ) 	
						)

x11( height = 9, width = 7.5)
print(all_areas_alt)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End Part 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!