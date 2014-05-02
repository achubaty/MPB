## combine pontis and poly
wh.poly.r = na.omit(match(substr(names(ab.poly),1,4), substr(names(poly.r),2,5)))
wh.bcab.r = na.omit(pmatch(substr(names(ab.poly),1,4), names(bcab)))

all = list()
inner.count = 0
for (i in 1:nlayers(bc.r)) {
  if (any(substr(names(bc.r),2,5)[i] == substr(names(poly.r),2,5))) {
    inner.count = inner.count + 1
    if (any(is.finite(cellStats(poly.r[[i]], "range")))) {
      all[[i]] = bc.r[[wh.bcab.r[inner.count]]] + poly.r[[wh.poly.r[inner.count]]]
    } else {
      all[[i]] = bc.r[[wh.bcab.r[inner.count]]]
    }
  } else {
    all[[i]] = bc.r[[i]]
  }
}

all = lapply(all, function(x) { x[is.na(x)] <- 0; return(x) })
all = lapply(all, function(x) { x[x>0] <- log(x[x>0])+10; return(x) })


stk = stack(all)
brk = brick(all)
