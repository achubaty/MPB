# mountainPineBeetleRedTopSpread.R
# Barry Cooke August 12, 2015

# The goal of this module is to simulate the spread of mountain pine beetle outbreaks as a function of
# the growth and dispersal of beetle-killed trees, called "red tops". Recruitment is an annual process,
# with the inter-annual recruitment function taking a variety of possible forms, depending who one takes as an authority
# on the question. Berryman (1979) was an early pioneer who inspired much work in the area, and there are several
# possible interpretation of the data he used from Glacier and Yellowtone National Parks in MO and WY the 1960s.
# Consequently, the module offers the option of using any of four possible interpretations. Two addiitonal options are
# forthcoming at the time of writing, Aug 12 2015. The first of these is a high-order polynomial fit to Berryaman's
# hand-drawn curve.  The second is the linearized multi-niche model of Berryman (1999), which, oddly & inexplicably,
# held endemic-niche dynamics as a fast process (Rt~Xt-1), and epidemic-niche dynamics as a lagged process(Rt~Xt-2).

# Calculating the initial density of red tops and the effect of growth in their number requires read and write access
# to a forest landscape of some sort.

#Litereature Cited
#"Berryman 1979","Berryman 1999","Powell & Bentz 2009","Trczinski & Reid 2009","MacQuarrie & Cooke 2011","Cooke et al. 2016","Cooke et al. 2016"

# Dispersal is currently a stub.


defineModule(sim, list(
  name="mountainPineBeetleRedTopSpread",
  description="Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords=c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors=c(person(c("Barry", "J"), "Cooke", email="Barry.Cooke@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre"))),
   childModules=character(),
  version=numeric_version("0.0.1"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list(),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter("spreadInterval", "numeric", 1, NA, NA, "This describes the interval time between growth events")),
    defineParameter("recruitmentCurveForm", "character", 1, NA, NA, "This describes the source and form of the growth curve to be used")),
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
  inputObjects=data.frame(objectName="landscape",
                           objectClass="RasterStack",
                           other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=NA_character_, stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.mountainPineBeetleRedTopSpread = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    print("This is the the start of your MPB red top model run")
    
    # do stuff for this event
    sim<-sim$mountainPineBeetleRedTopSpreadInit(sim)

    # schedule future event(s)
 	sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "growth")
    sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "dispersal")
    sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "impact")
    sim<-scheduleEvent(sim, params(sim)$mountainPineBeetleRedTopSpread$.plotInitialTime, "mountainPineBeetleRedTopSpread", "plot")
    sim<-scheduleEvent(sim, params(sim)$mountainPineBeetleRedTopSpread$.saveInitialTime, "mountainPineBeetleRedTopSpread", "save")
   
  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
	sim$mountainPineBeetleRedTopSpreadPlot()
	
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)
    sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "plot")

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "mountainPineBeetleRedTopSpread", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
 } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
	sim$mountainPineBeetleRedTopSpreadSave()
	
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)
    sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$.saveInterval, "mountainPineBeetleRedTopSpread", "save")

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "mountainPineBeetleRedTopSpread", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="growth") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
	sim<-sim$mountainPineBeetleRedTopSpreadGrowth(sim)
	
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "mountainPineBeetleRedTopSpread", "templateEvent")
	sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "growth")

	
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="dispersal") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
	
	#NULL dispersal "stub"

	sim<-sim$mountainPineBeetleRedTopSpreadDispersal(sim)
	
	#Use number of living trees in landscape to remove sim$totalNumberMassAttacks and move around landscape 

    # schedule future event(s)

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "mountainPineBeetleRedTopSpread", "templateEvent")
	sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "dispersal")
	
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="impact") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
	
	#NULL impact "stub"
	#Use sim$totalNumberMassAttacks to remove former living trees from landscape

    # schedule future event(s)

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "mountainPineBeetleRedTopSpread", "templateEvent")
	sim<-scheduleEvent(sim, time(sim)+params(sim)$mountainPineBeetleRedTopSpread$spreadInterval, "mountainPineBeetleRedTopSpread", "impact")
	
    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
    }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
mountainPineBeetleRedTopSpreadInit = function(sim) {

  landscape = sim$eval(params(sim)$randomLandscapes$stackname)

  # # ! ----- EDIT BELOW ----- ! #
	sim$totalNumberMassAttacks <- landscape$habitatQuality*10+1 #Initialize the distribution of mass-attacking colonies
	sim$annualRedTopRecruitmentRate <- landscape$habitatQuality*0 #Initialize the distribution of growth rates to 0
	
	# What we really want to do here is read in realistic landscapes sampled from sources,
	# such as Yemshanov et al. (2011) and Beaudoin et al. (2014), two clear cases where model inter-comparison is desirable.

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
mountainPineBeetleRedTopSpreadSave = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
mountainPineBeetleRedTopSpreadPlot = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  
	Plot(sim$annualRedTopRecruitmentRate)
	Plot(sim$totalNumberMassAttacks)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
mountainPineBeetleRedTopSpreadGrowth = function(sim) {
 
#workflow
	
	#This is the Berryman (1979) set of options, where:
	# 1. the endemic niche dynamics are implicit (options 1-3) or explicit (option 4)
	# 2. the epidemic niche dynamics are nonlinear eruptive
	# 3. the study area is a combination of Glacier National Park, Montana 1954-67 (Tunnock 1970) +
	#	 Yellowstone National Park, Wyoming 1966-72 (Parker 1973).
	#    This basically defines the spatial and temporal scope of model usage.
	#    Extrapolations beyond this area and time should trigger user warnings, as host forest landscape and micro-climatic conditions may be very different.
	
	#This branch of the workflow results in four "Berrymanesque" model function choices appropriate to Glacier-Yellowston ca. 1960s:
	#1. poly3.B79.fit:          Single-niche Order (3) polynomial fit to Berryman's (1979) data.
	#2. poly3.B79.forced:       Single-niche Order (3) polynomial force-fit to Berryman's (1979) hand-drawn curve.
	#3. poly3.C16B79.fit:       Single-niche Order (3) polynomial fit to the Tunnock (1970) & Parker (1973) data behind Berryman (1979).
	#4. poly2.C16B79.split.fit: Multi-niche  Order (2) polynomial endemic/epidemic spit fit to the Tunnock (1970) & Parker (1973) data behind Berryman (1979).

	#recruitmentCurveForm is the module parameter governing recruitment curve choice
	#if 		sim$recruitmentCurveForm == B79fit 
	#else if 	sim$recruitmentCurveForm == B79forced
	#else if 	sim$recruitmentCurveForm == B79reanalysis
	#else if 	sim$recruitmentCurveForm == B79split

#Data as portrayed in Berryman (1979).
#These are a mysterious hybrid of Tunnock (1970) & Parker (1973) data, to be accepted in this function "as is".
Berryman1979.yearRedTopsObserved <- c(1:15)                                 
Berryman1979.laggedlog10redTopsObserved <- c(-3.1,-2.75,-2.7,-2.4,-2.3,-1.2,-1,0.2,0.9,0.65,1.05,.95,1.1,1.5,1.85)
Berryman1979.log10changeInRedTopsObserved <- c(0.35,0.4,0.1,-0.4,-0.65,0.3,1,0.75,1.2,-0.7,-0.4,0.2,0.45,0.3,-0.78)	

#Here we compare an Order (3) polynomial fit versus an Order (3) approximation to Berryman's hand-drawn curve. 
win.graph()
plot(Berryman1979.laggedlog10redTopsObserved,Berryman1979.log10changeInRedTopsObserved,
	xlim=c(-3.2,2),ylim=c(-1.5,1.5),xlab="Xt-1",ylab="Rt",main="Berryman (1979)")
	abline(h=c(0))
points(Berryman1979.laggedlog10redTopsObserved[10:15],Berryman1979.log10changeInRedTopsObserved[10:15],pch=19,col="black")

B79.poly3.lm<-lm(Berryman1979.log10changeInRedTopsObserved~poly(Berryman1979.laggedlog10redTopsObserved,3,raw=T))
summary(B79.poly3.lm)

xx<-seq(-3.5,2.2,length.out=250)
lines(xx,predict(B79.poly3.lm,data.frame(Berryman1979.laggedlog10redTopsObserved=xx)),col="green")

######################
# function option #1 #
######################

poly3.B79.fit<- function(x) (B79.poly3.lm$coefficient[4]*x^3+B79.poly3.lm$coefficient[3]*x^2+B79.poly3.lm$coefficient[2]*x+B79.poly3.lm$coefficient[1])

curve(poly3.B79.fit,xx,col="red",add=T)

poly3.params<-c(1.1,-0.2,-0.9,-0.24)

######################
# function option #2 #
######################

poly3.B79.forced<- function(x) (poly3.params[4]*x^3+poly3.params[3]*x^2+poly3.params[2]*x+poly3.params[1])

curve(poly3.B79.forced,xx,col="blue",add=T)

#The lack of O(3) conformity to Berryman's hand-drawn curve suggests Berryman's hand-drawn curve may be O(5) or more.
B79.poly5.lm<-lm(Berryman1979.log10changeInRedTopsObserved~poly(Berryman1979.laggedlog10redTopsObserved,5,raw=T))
summary(B79.poly5.lm)
lines(xx,predict(B79.poly5.lm,data.frame(Berryman1979.laggedlog10redTopsObserved=xx)),col="green")

legend(-1.6,-.6,c("O(3) fit","O(3) forced","O(5) fit"),col=c("red","blue","green"),lty=1,bg="white")

#These parameters do not yet match Berryman's freehand curve
poly5.params<-c(1.1,-0.2,-0.1,-0.12,0.06,-.03)

#These parameters do not yet match Berryman's freehand curve
poly5.forced<- function(x) (poly5.params[6]*x^5+poly5.params[5]*x^4+poly5.params[4]*x^3+poly5.params[3]*x^2+poly5.params[2]*x+poly5.params[1])

#This could become a function option once Berryman's freehand curve has been digitized for formalized curve fitting

curve(poly5.forced,xx,col="black",add=T)

legend(-1.6,-.6,c("O(3) fit","O(3) forced","O(5) fit","O(5) forced"),col=c("red","blue","green","black"),lty=1,bg="white")

#Source data behind Berryman (1979), to be used in cubic single-niche versus quadratic multi-niche model comparison.
#These are the actual data in Tunnock (1970) & Parker (1973).
#Re-analysis to be published in Cooke et al. (2016). 

Tunnock1970.yearRedTopsObserved <- c(1954:1967)
Parker1973.yearRedTopsObserved <- c(1966:1972)
Tunnock1970.redTopsObserved <- c(6,12,6,13,6,3,1,2,3,2,2,1,2,3)
Parker1973.redTopsObserved <- c(9,14,34,70,10,4,1)

#Time-series plot of independent datasets for Tunnock & Parker
win.graph()
plot(Tunnock1970.yearRedTopsObserved,Tunnock1970.redTopsObserved,
	xlim=c(1954,1973),ylim=c(0,80),type="l",
	xlab="year",ylab="xt (trees/ha/yr killed)")
points(Tunnock1970.yearRedTopsObserved,Tunnock1970.redTopsObserved)
lines(Parker1973.yearRedTopsObserved,Parker1973.redTopsObserved)
points(Parker1973.yearRedTopsObserved,Parker1973.redTopsObserved,pch=19,col="black")
legend(1955,75,c("Tunnock (1970)","Parker(1973)"),lty=1,pch=c(21,19),col="black")

#testing phase plot (recruitment curve) diagram: Rt (=logrt) vs Xt-1 (=logxt-1)
win.graph()
plot(log10(Tunnock1970.redTopsObserved[1:length(Tunnock1970.redTopsObserved)-1]),diff(log10(Tunnock1970.redTopsObserved)),
	xlim=c(0,2),ylim=c(-1.5,1),xlab="Xt-1 (log10 trees/yr)",ylab="Rt = log10 xt/xt-1",main="Cooke et al. (2016) re-analysis of Berryman (1979)")
points(log10(Parker1973.redTopsObserved[1:length(Parker1973.redTopsObserved)-1]),diff(log10(Parker1973.redTopsObserved))
	,pch=19,col="black")
abline(h=0)

#O(3) polynomial - endemic niche dynamics implicit
x<-c(log10(Tunnock1970.redTopsObserved[1:length(Tunnock1970.redTopsObserved)-1]),log10(Parker1973.redTopsObserved[1:length(Parker1973.redTopsObserved)-1]))
y<-c(diff(log10(Tunnock1970.redTopsObserved)),diff(log10(Parker1973.redTopsObserved)))

win.graph()
plot(x,y,
	xlim=c(0,2),ylim=c(-1.5,1),xlab="Xt-1 (log10 trees/yr)",ylab="Rt = log10 xt/xt-1",main="Cooke et al. (2016) re-analysis of Berryman (1979)")
points(x[14:19],y[14:19],pch=19,col="black")
abline(h=0)

C16B79.poly3.lm<-lm(y~poly(x,3,raw=T))
summary(C16B79.poly3.lm)

xx<-seq(-3.5,2.2,length.out=250)
lines(xx,predict(C16B79.poly3.lm,data.frame(x=xx)),col="red")

legend(.6,-1,c("O(3) fit"),col=c("red"),lty=1,bg="white")

######################
# function option #3 #
######################

poly3.C16B79.fit<- function(x) (C16B79.poly3.lm$coefficient[4]*x^3+C16B79.poly3.lm$coefficient[3]*x^2+C16B79.poly3.lm$coefficient[2]*x+C16B79.poly3.lm$coefficient[1])

#endemic niche dynamics implicit; epidemic dynamics nonlinear 
x.en<-x[1:13] #en=endemic
y.en<-y[1:13]
x.ep<-x[14:19] #ep=epidemic
y.ep<-y[14:19]

C16B79.endemic.lm<-lm(y.en~poly(x.en,2,raw=T))
summary(C16B79.endemic.lm)
C16B79.epidemic.lm<-lm(y.ep~poly(x.ep,2,raw=T))
summary(C16B79.epidemic.lm)

xx<-seq(-3.5,1.1,length.out=250)
lines(xx,predict(C16B79.endemic.lm,data.frame(x.en=xx)),col="blue",lty=2)

xx<-seq(0,2.2,length.out=250)
lines(xx,predict(C16B79.epidemic.lm,data.frame(x.ep=xx)),col="blue")

legend(.6,-1,c("O(3) single-niche fit","two-niche fit"),col=c("red","blue"),lty=c(1),bg="white")

######################
# function option #4 #
######################

poly2.C16B79.split.fit<- function(x) {

	#For x < split return C16B79.endemic.lm
	#For x >= split return C16B79.epidemic.lm
	#So split vector x at the point where the endemic and epidemic curves intersect, and return the appended segments of the corresponding left and right branches.
	#Use "uniroot" to find unique roots of f(x)-g(x) where f and g are quadratics for endemic and epidemic niche recruitment curves.

	f_x <- function (x) C16B79.endemic.lm$coefficient[3]*x^2+C16B79.endemic.lm$coefficient[2]*x+C16B79.endemic.lm$coefficient[1]
	g_x <- function (x) C16B79.epidemic.lm$coefficient[3]*x^2+C16B79.epidemic.lm$coefficient[2]*x+C16B79.epidemic.lm$coefficient[1]

	recruitment.curve.intersection <- function(x) f_x(x)-g_x(x)

	split<-uniroot(recruitment.curve.intersection,lower=min(x),upper=1.6)

	split.recruitment.fn<-c(f_x(x[x<split$root]),g_x(x[x>=split$root]))

	return(split.recruitment.fn)
}

	x<-seq(-3.5,2.2,length.out=250)
	lines(x,poly2.C16B79.split.fit(x),col="black",lwd=2)


if (sim$recruitmentCurveForm=="B79fit") then {
		sim$annualRedTopRecruitmentRate <- 10^poly3.B79.fit(sim$totalNumberMassAttacks) 
	}
else if (sim$recruitmentCurveForm=="B79forced") then {
		sim$annualRedTopRecruitmentRate <- 10^poly3.B79.forced(sim$totalNumberMassAttacks) 
	}
else if (sim$recruitmentCurveForm=="B79reanalysis") then {
		sim$annualRedTopRecruitmentRate <- 10^poly3.C16B79.forced(sim$totalNumberMassAttacks) 
	}
else if (sim$recruitmentCurveForm=="B79split") then {
		sim$annualRedTopRecruitmentRate <- 10^poly2.C16B79.split.fit(sim$totalNumberMassAttacks) 
	}
else {
	sim$annualRedTopRecruitmentRate <- NA
}
	
	sim$totalNumberMassAttacks <- sim$totalNumberMassAttacks * sim$annualRedTopRecruitmentRate
	
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
mountainPineBeetleRedTopSpreadDispersal = function(sim) {
  # ! ----- EDIT BELOW ----- ! #

	#NULL dispersal "stub"
	sim$totalNumberMassAttacks <- sim$totalNumberMassAttacks


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
