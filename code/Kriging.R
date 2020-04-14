setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")
library( gstat )
library(deldir)
library(sp)
library(tmap)

data( fulmar )
#### Make sure voronoipolygons.R is in working directory
source( "voronoipolygons.R" )  

fulmar.spdf = SpatialPointsDataFrame( cbind(fulmar$x,fulmar$y) , fulmar )
fulmar.spdf = fulmar.spdf[fulmar.spdf$year==1999,]
evgm = variogram( fulmar~1 , fulmar.spdf , boundaries=seq(0,250000,length=51) )
fvgm = fit.variogram( evgm , vgm("Mat") )
proj4string(fulmar.spdf) = CRS("+init=epsg:32631")
windows( 9 , 7 );  plot( evgm , pch=19 )
windows( 9 , 7 );  plot( evgm , model=fvgm )

fulmar.voro <- voronoipolygons(fulmar.spdf)
s.grid <- spsample(fulmar.voro,type='regular',n=6000)
krig.est = krige( fulmar~1 , fulmar.spdf , newdata=s.grid , model=fvgm )
krig.grid = SpatialPixelsDataFrame( krig.est , krig.est@data )

levs = c(0,2,4,6,8,Inf)
krig.map.est = tm_shape( krig.grid ) +
  tm_raster( col="var1.pred", breaks=levs , title="Fulmar Density" ,
             palette="Blues" ) +
  tm_layout( legend.bg.color="white" , legend.frame=TRUE ,
             legend.position=c("right","bottom") )

var.levels = c(0,3,6,9,12,Inf)
krig.map.var = tm_shape( krig.grid ) + 
  tm_raster( col="var1.var" , breaks=var.levels , 
             title="Estimated Variance" , palette="Blues" ) +
  tm_layout( legend.bg.color="white" , legend.frame=TRUE ,
             legend.position=c("right","bottom") )

windows( 12 , 9 )
tmap_arrange( krig.map.est , krig.map.var )

par( mfrow=c(1,1) )  ##  Reset to 1x1 plot

ux = unique(coordinates(s.grid)[,1])
uy = unique(coordinates(s.grid)[,2])
predmat3 = matrix( krig.est$var1.pred , length(ux) , length(uy) )
persp( predmat3 , box=FALSE )
contour( predmat3 )
filled.contour( predmat3 )
