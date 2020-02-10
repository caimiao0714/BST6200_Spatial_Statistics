setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

library( sf )
library( GISTools )
library( rgdal )
library( tmap )
library( grid )
library( RColorBrewer )
library( tidyverse )

##
## 3.5.4 Mapping Points and Attributes

data( newhaven )

## Convert  sp  objects to  sf  objects
blocks_sf = st_as_sf( blocks )
breach_sf = st_as_sf( breach )
tracts_sf = st_as_sf( tracts )
windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "white" ) +
  tm_shape( breach_sf ) + 
  tm_dots( size=0.5 , shape=19 , col="red" , alpha=1 )

windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "white" ) +
  tm_shape( breach_sf ) + 
  tm_dots( size=0.5 , shape=19 , col="red" , alpha=0.5 )  

##
## Data on earthquakes near Fiji
##%% START
data( quakes )
head( quakes )  
coords.tmp = cbind( quakes$long , quakes$lat )
quakes.sp = SpatialPointsDataFrame( coords.tmp , 
                                    data=data.frame(quakes) ,
                                    proj4string=CRS("+proj=longlat ") )

quakes_sf = st_as_sf(quakes.sp)
print( quakes_sf )

quakes_df = data.frame(quakes_sf)
x = quakes_df$long
y = quakes_df$lat
windows( 9 , 9 )
plot( x , y )

windows( 9 , 9 )
tm_shape( quakes_sf ) +
  tm_dots(size = 0.5, alpha = 0.3) +
  tm_layout( frame=FALSE )
## Compare with p. 91 in book

windows( 9 , 9 )
tm_shape( quakes_sf ) +
  tm_dots(size = 0.5, alpha = 0.3) +
  tm_layout( inner.margins=c(0,0,0,0) , frame=FALSE )

windows( 9 , 9 )
tm_shape( quakes_sf ) +
  tm_dots(size = 0.5, alpha = 0.3) +
  tm_layout( inner.margins=c(0.05,0.05,0.05,0.36) , frame=TRUE )

tmap_mode("plot")
p1 = tm_shape( quakes_sf ) +
  tm_bubbles( "depth" , scale=1 , shape=19 , alpha=0.3 , title.size="Quake Depths") +
  tm_layout( inner.margins=c(0.05,0.05,0.05,0.36) , frame=TRUE )
p2 = tm_shape( quakes_sf ) +
  tm_dots( "depth" , shape=19 , alpha=1 , size=0.5 ,    ## Be careful with transparency
           palette="Greens" , title="Quake Depths" ) +
  tm_layout( inner.margins=c(0.05,0.05,0.05,0.36) , frame=TRUE )

windows( 9 , 9 )
grid.newpage()
pushViewport( viewport(layout=grid.layout(1,2)) )
print( p1 , viewport( layout.pos.col=1 , height=5 ) )
print( p2 , viewport( layout.pos.col=2 , height=5 ) )

## Plot earthquakes of magnitude greater than 5.5
index = ( quakes_sf$mag > 5.5 )
print( index )
tmp = quakes_sf[ index , ]
tm_shape( tmp ) +
  tm_dots( col=brewer.pal(5,"Greens")[4] , shape=19 ,
           alpha=0.5 , size=1 ) +
  tm_layout( title="Quakes > 5.5" ,
             title.position=c("center","top") ,
             inner.margins=c(0.05,0.05,0.05,0.36) )
##%% STOP
##%%%%%%%%%%%%%%

##  Using Google Maps
##  Book's code doesn't seem to work
##  Chris Brundson 2/5/2020 suggested the following

quakes_sf <- tibble(lon= quakes$long,lat=quakes$lat) %>% st_as_sf(coords=1:2,crs=4326) 
## 
## The crs bit specifies long/lat coordinates in the sf object
## https://www.rdocumentation.org/packages/tmap/versions/2.3-1/topics/tmap_mode
## tmap_mode("view") asks tmap to use an openstreetmap zoomable backdrop
tmap_mode("view") 
tm_shape( quakes_sf ) +
  tm_dots()
## Notes: Several features of tm_dots are not available in tmap_mode("view")
##        Plot is created in RStudio's viewer pane

############################################################################
## 3.5.5 Mapping Lines and Attributes
## SKIP - Book doesn't show how the plots in Figure 3.19 were created

data( newhaven )
proj4string( roads ) = proj4string( blocks )
xmin = bbox(roads)[1,1]
ymin = bbox(roads)[2,1]
xmax = xmin + diff( bbox(roads)[1,] ) / 2
ymax = ymin + diff( bbox(roads)[2,] ) / 2

xx = as.vector( c(xmin,xmin,xmax,xmax,xmin) )
yy = as.vector( c(ymin,ymax,ymax,ymin,ymin) )
windows( 9 , 9 )
plot( 1:2 , 1:2 , type="n" , xlim=c(xmin,xmax) , ylim=c(ymin,ymax) )
  polygon( xx , yy , col="gray")
  
crds = cbind( xx , yy )
Pl = Polygon( crds )
class( Pl )
str( Pl )
ID = "clip"
Pls = Polygons( list(Pl) , ID=ID )
SPls = SpatialPolygons( list( Pls ) )
class( Pls )
str( Pls )
print( Pls )
class( Pls )
df = data.frame( value=1 , row.names=ID )
clip.bb = SpatialPolygonsDataFrame( SPls , df )
proj4string( clip.bb ) = proj4string( blocks )

clip_sf = st_as_sf( clip.bb )
roads_sf = st_as_sf( roads )

roads_tmp = st_intersection( st_cast(clip_sf) , st_geometry(roads_sf) )
st_intersection( st_geometry(st_cast(clip_sf)) , st_geometry(roads_sf))

############################################################################

## 3.5.6 Mapping Raster Attributes
##
##%% START
##%%%%%%%%%%%%%%%%%%%%%%%
library( raster )
data( meuse.grid )
class( meuse.grid )
summary( meuse.grid )
windows( 9 , 7 )
plot( meuse.grid$x , meuse.grid$y , asp=1 )

meuse.sp = SpatialPixelsDataFrame( points=meuse.grid[c("x","y")] ,
                                   data=meuse.grid , 
                                   proj4string=CRS("+init=epsg:28992") )
## See  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
EPSG <- make_EPSG()  ## to see all possible Coordinate Reference Systems

meuse.r = as( meuse.sp , "RasterStack" )
windows( 9 , 7 )
plot( meuse.r )
plot( meuse.sp )
windows( 9 , 7 )
plot( meuse.sp[,5] )
windows( 9 , 7 )
spplot( meuse.sp , c( "part.a" , "part.b" , "soil" , "ffreq" ) ,
        col.regions=topo.colors(20) )

windows( 9 , 7 )
tmap_mode( "plot" )
tm_shape( meuse.r ) +
  tm_raster( col=c("dist","ffreq") , title=c("Distance","Flood Freq.") ,
             palette="Reds" , style=c("kmeans","cat") )

tmap_mode( "view" )
tm_shape( meuse.r ) +
  tm_raster( col="dist" , title="Distance" , breaks=c(seq(0,1,0.2)) ) +
  tm_layout( legend.format=list(digits=1) )

tmap_mode( "view" )
tm_shape( meuse.r ) +
  tm_raster( col="ffreq" , title="Flood Freq" , breaks=seq(0.5,3.5,1) ) +
  tm_layout( legend.format=list(digits=1) )

############################################################################

## 3.6 Descriptive Statistics 
##          

library( tidyverse )
library( GISTools )
library( reshape2 )    ## install.packages("reshape2",dep=TRUE) if nec.

data( newhaven )
hist( blocks$P_VACANT , breaks=40 , col="cyan" ,
      border="salmon" , main="Distribution of Vacant Property %" ,
      xlab="% Vacant" )

windows( 10 , 7 )
ggplot( blocks@data , aes(P_VACANT) ) +
  geom_histogram( col="salmon" , fill="cyan" , bins=40 ) +
  xlab( "% Vacant" ) +
  ylab( "Frequency") +
  labs( title="The Distribution of Vacant Property (%)")

indx = (blocks$P_VACANT > 10)
blocks$vac = indx + 1
blocks$vac = factor( blocks$vac , labels=c("Low","High") )

###  Exercise --> Look up what melt does in the reshape2 package

blocks_data = blocks@data
str( blocks_data )
head( blocks_data , 10 )

## Book's code ...
melted = melt( blocks_data[,c("P_OWNEROCC","P_WHITE","P_BLACK","vac")]) 
head( melted , 20 )
str( blocks@data )
str( melted )

## My code to help understand melt command
  
blocks_data_sub = blocks_data[,c("P_OWNEROCC","P_WHITE","P_BLACK","vac")]
blocks_data_sub
melted = melt( blocks_data_sub , id.vars = "vac" ,
               variable.name = "Variable_Name", 
               value.name = "Observed_Value" )
melted

windows( 12 , 9 )
ggplot( melted , aes( Variable_Name , Observed_Value ) ) +
  geom_boxplot( color="salmon" , fill="orange" , alpha=0.8 ) +
  facet_wrap( ~vac ) +
  xlab( "" ) + 
  ylab( "Percentage" ) +
  ## theme_dark() +
  ggtitle( "Boxplot of High and Low Property Vacancies")

## Scatter Plots and Regressions
windows( 7 , 9 )
par( mfrow=c(2,1) )
plot( blocks_data$P_VACANT/100 , blocks_data$P_WHITE/100 ,
      xlab="% Vacant" , ylab="% White" )
plot( blocks_data$P_VACANT/100 , blocks_data$P_BLACK/100 ,
      xlab="% Vacant" , ylab="% Black" )

p.vac = blocks_data$P_VACANT / 100
p.w   = blocks_data$P_WHITE / 100
p.b   = blocks_data$P_BLACK / 100

df = data.frame( p.vac , p.w , p.b )
mod.1 = lm( p.vac ~ p.w , data=df )
mod.2 = lm( p.vac ~ p.b , data=df )

summary( mod.1 )
summary( mod.2 )

p1 = ggplot( df , aes( p.vac , p.w ) ) +
       geom_smooth( method="lm" ) +
       geom_point() +
       xlab( "Proportion of Vacant Properties" ) +
       ylab( "Proportion White" ) +
       labs( title="Regression of Vacant Properties vs Prop White")
p1       
p2 = ggplot( df , aes( p.vac , p.b ) ) +
  geom_smooth( method="lm" ) +
  geom_point() +
  xlab( "Proportion of Vacant Properties" ) +
  ylab( "Proportion Black" ) +
  labs( title="Regression of Vacant Properties vs Prop Black")
p2

windows( 9 , 9 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(2,1)) )
print( p1 , vp = viewport( layout.pos.row = 1 , height = 5 ) )
print( p2 , vp = viewport( layout.pos.row = 2 , height = 5 ) )

##%% STOP
##%%%%%%%%%%%%%%%%%%%%%%%%%%

## 3.6.3 Mosaic Plots
##
## install.packages( "ggmosaic" , dep=TRUE )
library( ggmosaic )

## pops is the NUMBER of people in each category
pops = data.frame( blocks[,14:18] ) * data.frame( blocks )[,11] 
pops = as.matrix( pops/100 )
colnames( pops ) = c( "White" , "Black" , "Ameri" , "Asian" , "Other" )
pops = round( pops , 0 )   

vac.10 = ( blocks$P_VACANT > 10 )
mat.tab = xtabs( pops ~ vac.10 )

df = melt( mat.tab )
colnames( df ) = c("vac.10" , "Race" , "Number" )

ggplot( data=df ) +
  stat_mosaic( aes( weight=Number , x = product(Race) ,
                    fill=factor( vac.10 )) , na.rm=TRUE ) +
  theme( axis.text.x = element_text(angle=-90,hjust=0.1)) +
  labs( y="Propostion of Vacant Properties" , x="Race" , 
        title="Mosaic Plot of Vacant Properties by Race" ) +
  guides( fill=guide_legend( title="> 10 percent" , reverse=TRUE ))
  