#mountainPineBeetleRedTopSpread - Barry Cooke August 12 2015
					   
#Program starts
library(SpaDES)
						   
mySim <- simInit(
  times=list(start=0.0, stop=10.0, timeunit="year"),
  params=list(.globals=list(stackName="landscape", burnStats="nPixelsBurned"),
				randomLandscapes=list(nx=100,ny=100)),
				mountainPineBeetleRedTopSpread=list(recruitmentCurveForm="B79fit"), #recruitmentCurveForm options = B79fit, B79forced, B79reanalysis, B79split
  modules=list("randomLandscapes","mountainPineBeetleRedTopSpread"),
  paths=list(modulePath="~/modules")
)
mySimOut <- spades(mySim,debug=T)
