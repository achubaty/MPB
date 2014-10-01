# Mountain pine beetle outbreak maps

**Alex M. Chubaty** and **Eliot Mcintire**

Canadian Forest Service  
Pacific Forestry Centre  
506 Burnside Road W  
Victoria, BC V8Z 1M5

Université Laval  
Faculté de foresterie, de géographie et de géomatique  
Département des sciences du bois et de la forêt  

*email:* [Alexander.Chubaty@nrcan.gc.ca][1]  
*phone:* +1.250.298.2347

[1]: <mailto:Alexander.Chubaty@nrcan.gc.ca>

------------------------------------------------------------------

## Overview of MPB map data

Descriptions of:

- data sources
- data collected
- etc.




## Processing the MPB map data

### Importing map data

#### MPB `SpatialPoints` data




#### MPB `SpatialPolygons` data




#### Canadian boreal forest maps




Boreal forest map data uses the `crs.boreal` projection below. We reproject the Canadian administrative boundaries maps to use this projection:


```
## CRS arguments:
##  +proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113 +x_0=0
## +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
```


### Reproject MPB maps to `crs.boreal`

#### MPB `SpatialPoints` data




#### MPB `SpatialPolygons` data




### Rasterize MPB maps

#### Canadian boreal forest maps




#### MPB `SpatialPoints` data




#### MPB `SpatialPolygons` data




### Combine all MPB maps




## Plotting maps

### Boreal forest maps

**The Canadian boreal forest:**

![plot of chunk plot.canada.boreal](figure/plot_canada_boreal.png) 


**The western Canadian boreal forest:**

![plot of chunk plot.western.boreal](figure/plot_western_boreal.png) 


### MPB in western Canada

#### MPB `SpatialPoints` data




#### MPB `SpatialPolygons` data




#### MPB `RasterStack`

![plot of chunk plot.mpb.western.boreal.raster.stack](figure/plot_mpb_western_boreal_raster_stack.png) 


#### MPB `RasterBrick` timeseries


```
## Warning: cannot open file
## 'C:\Temp\R_raster_achubaty\raster_tmp_2014-05-26_162343_4640_90793.gri':
## No such file or directory
```

```
## Error: cannot open the connection
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'brick': Error in nlayers(brk.ll) : 
##   error in evaluating the argument 'x' in selecting a method for function 'nlayers': Error: object 'brk.ll' not found
## Calls: lapply -> nlayers
```

```
## Error: object 'brk.ll' not found
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'writeRaster': Error: object 'brk.ll' not found
```

```
## Error: object 'brk.ll' not found
```

```
## Error: object 'brk.ll' not found
```

```
## Error: error in evaluating the argument 'obj' in selecting a method for function 'plotKML': Error: object 'ts.kml' not found
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'rts': Error: object 'brk.ll' not found
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'plot': Error: object 'ts.rts' not found
```

```
## Error: object 'brk.ll' not found
```

```
## Warning: object 'bcab.all.boreal.raster.stack' not found
```

