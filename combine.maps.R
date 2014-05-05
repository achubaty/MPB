### combine bcab points and poly rasters
wh.pnts = na.omit(match(names(bcab.poly.boreal.raster.stack), names(bcab.pnts.boreal.raster.stack)))
wh.poly = na.omit(match(names(bcab.pnts.boreal.raster.stack), names(bcab.poly.boreal.raster.stack)))

all = list()
inner.count = 0
for (i in 1:nlayers(bcab.poly.boreal.raster.stack)) {
  if (any(names(bcab.poly.boreal.raster.stack)[i] == names(bcab.pnts.boreal.raster.stack))) {
    inner.count = inner.count + 1
    if (any(is.finite(cellStats(bcab.poly.boreal.raster.stack[[i]], "range")))) {
      all[[i]] = merge(bcab.pnts.boreal.raster.stack[[wh.pnts[inner.count]]],
                       bcab.poly.boreal.raster.stack[[wh.poly[inner.count]]])
    } else {
      all[[i]] = bcab.pnts.boreal.raster.stack[[wh.pnts[inner.count]]]
    }
  } else {
    all[[i]] = bcab.poly.boreal.raster.stack[[i]]
  }
}

all = lapply(all, function(x) { x[is.na(x)] <- 0; return(x) })
all = lapply(all, function(x) { x[x>0] <- log(x[x>0])+10; return(x) })
names(all) = substr(names(bcab.poly.boreal.raster.stack), 2, 5)

bcab.all.boreal.raster.brick = brick(all)
bcab.all.boreal.raster.stack = stack(all)

names(bcab.all.boreal.raster.brick) = substr(names(bcab.poly.boreal.raster.stack), 2, 5)
names(bcab.all.boreal.raster.stack) = substr(names(bcab.poly.boreal.raster.stack), 2, 5)

saveObjects(c("bcab.all.boreal.raster.brick", "bcab.all.boreal.raster.stack"), rdata.path)

rm(all, bcab.all.boreal.raster.brick, bcab.all.boreal.raster.stack, wh.pnts, wh.poly)
