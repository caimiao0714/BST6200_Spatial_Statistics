####  Code from Brunsdon and Comber
####
####  Chapter 3
####


setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

####  Section 3.1.2 Packages
####
installed.packages()
is.element( "sf" , installed.packages() )

#### If outcome is FALSE, then install ...
# install.packages( "sf" , depend=TRUE )    ### Do this only once.
library( sf )

help( sf )

####
####  Section 3.2.1  sp  package
library( GISTools )

data(newhaven)
ls()
class( breach )
unclass( breach )  ## See what breach looks like when we don't use
                   ## the print on a SpatialPoints object
print( breach )
print.SpatialPoints( breach )
plot( breach )
getAnywhere(print.SpatialPoints)

methods( print )   ## Note the asterisk after print.SpatialPoints*
## This indicates the function print.SpatialPoints
## is not available in the default namesapce
## Tell R to find print.SpatialPoints anywhere it can
print.SpatialPoints( breach )
print( breach )

class( blocks )
str( blocks )

head( data.frame( blocks ) )
head( blocks@data )

par( mar=c(0,0,0,0) )
windows( 9 , 9 )
plot( roads , col="red" )
plot( blocks , add=TRUE )
plot( blocks )

####
####  Section 3.2.2  sf  package
library( sf )
help( sf )
vignette( package="sf" )   ## Google "r sf package vignette"
                           ## Take a look at vignette in browser

data( georgia )            ## Part of the  sp  package
georgia_sf = st_as_sf( georgia )   ## Convert georgia from  sp  to  sf

georgia_sf
georgia

georgia$Name
georgia[[13]]

georgia$MedInc
georgia[[14]]

georgia[[15]]   ## The geometry is not part of the data frame

windows( 9 , 9 )
plot( georgia )
plot( georgia_sf )  ## Look how the generic function plot acts on objects of class
                    ## sf as opposed to objects of class sp
plot( georgia_sf[,6] )
plot( georgia_sf[,4:5])

head( data.frame(georgia) )
head( data.frame(georgia_sf) )  ## Note geometry attributes at the end

g_sf_df = data.frame( georgia_sf )
str( g_sf_df )
georgia_geometry = g_sf_df[[15]]
str( georgia_geometry )
county1 = georgia_geometry[[1]][1] 
county2 =  georgia_geometry[[1]][2] 
windows( 8 , 8 )
plot( county1[[1]][[1]][,1] , county1[[1]][[1]][,2] , type="l" , asp=1 )
## View county1 in Global Enviromnent
## or do str( county1 )

g2 = as( georgia_sf , "Spatial" )
class( g2 )
class( georgia_sf )

roads_sf = st_as_sf(roads)
class(roads_sf)
r2 = as(roads_sf, "Spatial")
class(r2)

####
#### 3.3 Reading and Writing Spatial Data

#### 3.3.1 Reading from and Writing to  sp

library( rgdal )

writeOGR( obj=georgia , dsn="." , layer="georgia" , 
          driver="ESRI Shapefile" , overwrite_layer=TRUE )
new.georgia = readOGR( "georgia.shp" )

####
#### 3.3.2 Reading from and Writing to  sf

g2.new = st_read( "georgia.shp" )
st_write( g2.new , "georgia.shp" , delete_layer=TRUE )

####
#### 3.4  tmap
####
#### 3.4.2  Quick tmap using  qtm

library( tmap )
data( georgia )
georgia_sf = st_as_sf( georgia )
windows( 9 , 9 )
qtm( georgia , fill="chocolate2" , style="natural")

windows( 9 , 9 )
qtm( georgia_sf , fill="MedInc" , text="Name" , text.size=0.7 ,
     format="World_wide" , style="classic" , 
     text.root=5 , fill.title="Median Income" )

## What does text.root=5 do?  Try leaving it out.  

windows( 9 , 9 )
qtm( georgia_sf , fill="MedInc" , text="Name" , text.size=0.7 ,
     format="World_wide" , fill.title="Median Income" )

#### 3.4.2  Full  tmap  

plot( georgia_sf )

g = st_union( georgia_sf )   ## Outline of the state of Georgia
plot( g )

windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "aquamarine4" ) +
  tm_borders( lty="dashed" , col="gold" ) +
  tm_style( "natural" , bg.color="gray90" ) +
  tm_shape( g ) +    ## Note you can use tm_shape within another tm_shape
  tm_borders( lwd=2 ) +
  tm_layout( title="The State of Georgia" ,
             title.size=2 ,
             title.position=c(0.54,"top") )
  
windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "aquamarine4" ) +
  tm_borders( lty="dashed" , col="gold" ) +
  tm_style( "natural" , bg.color="gray90" ) +
#  tm_shape( g ) + 
#  tm_borders( lwd=2 ) +
  tm_layout( title="The State of Georgia" ,
             title.size=2 ,
             title.position=c(0.54,"top") )

windows( 9 , 9 )
t1 = tm_shape( georgia_sf ) +
          tm_fill( "coral" ) + 
          tm_borders() +
          tm_layout( bg.color="gray85" ,
                     title="Georgia in sf Format" , 
                     title.size=1.5 ,
                     title.position=c(0.54,"top") )
t1
windows( 9 , 9 )
t2 = tm_shape( georgia ) +
          tm_fill("darkmagenta") +
          tm_borders() +
          tm_layout( bg.color="gray85" , asp=0.86 ) +
          tm_layout( title="Georgia in sp Format" ,
                     title.size=1.5 ,
                     title.position=c(0.54,"top"))
t2

library( grid )   ## for combining plots (par(mfrow=..)) doesn't work
windows( 12 , 7 )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )
print( t1 , vp=viewport( layout.pos.col=1 , height=5 ) )
print( t2 , vp=viewport( layout.pos.col=2 , height=5 ) )
## Note, plot differs from p. 71 in book

head( data.frame( georgia_sf) )
data.frame( georgia_sf )$Name
data.frame( georgia_sf )[,13]

windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "white" ) +
  tm_borders() +
  tm_text( "Name" , size=0.5 ) +
  tm_layout( frame=FALSE )

index = c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17 )
georgia_sf.sub = georgia_sf[index,]
print( georgia_sf.sub )
str( georgia_sf.sub )
class( georgia_sf.sub )

## Plot this subset of counties
windows( 9 , 9 )
tm_shape( georgia_sf.sub ) + 
  tm_fill( "gold1" ) +
  tm_borders( "gray" ) +
  tm_text( "Name" , size=1 ) +
  tm_shape( g ) +
  ## Note tm_shape can be used to add to a tm_shape !
  tm_borders( lwd=2 ) +
  tm_layout( frame=FALSE , title="A Subset of Georgia" ,
             title.size=1.6 , title.position=c(0,"bottom") )

## What if we add a noncontiguous county?  
windows( 8 , 9 )
index1 = c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17, 1)
georgia_sf.sub1 = georgia_sf[index1,]
tm_shape( georgia_sf.sub1 ) + 
  tm_fill( "gold1" ) +
  tm_borders( "gray" ) +
  tm_text( "Name" , size=1 ) +
  tm_borders( lwd=2 ) +
  tm_layout( frame=FALSE , title="A Noncontiguous Subset of Georgia" ,
             title.size=1.15 , title.position=c(0,"bottom") )

## Show these counties within the context of the entire state of Georgia
## Layer 1 (bottom)
tm_shape( georgia_sf ) +
  tm_fill( "white" ) +
  tm_borders( "gray" , lwd=0.5 ) +
  ## Now add the second layer
  tm_shape( g ) +
  tm_borders( lwd=2 ) +
  ## Now add the third layer
  tm_shape( georgia_sf.sub ) +
  tm_fill( "skyblue" ) +
  tm_borders() +
  ## Now add layout parameters
  tm_layout( frame=TRUE , title="Georgia with a subset of counties" ,
             title.size=1 , title.position=c(0.02,"bottom") )
  
##  install.packages( "OpenStreetMap" , dep=TRUE )  
library( OpenStreetMap )
## May have to use 32-bit version of R. 
##   Tools > GlobalOptions  then  
##   General    R Version   Change
##   Use your machine's default version of R (32 bit)
## Define upper left and lower right corners
georgia.sub = georgia[index,]

bbox(georgia.sub)
bbox(georgia.sub)[2,2]
bbox(georgia.sub)[1,1]
ul = as.vector( cbind( bbox(georgia.sub)[2,2] , bbox(georgia.sub)[1,1]) ) 
lr = as.vector( cbind( bbox(georgia.sub)[2,1] , bbox(georgia.sub)[1,2]) ) 
## Download the map from OpenMaps
MyMap = openmap( ul , lr )

## OpenStreetMap doesn't seem to work on Windows

##
## 3.4.5  Saving Your Map
library( GISTools )  ## you may have done this already
data( newhaven )
proj4string( roads ) = proj4string( blocks )

getwd()   ## Always check this first to see where R will write file

pdf( file="newhaven2.pdf" )   ## Opens file
#windows( 9 , 7 )
tm_shape( blocks ) +
  tm_borders() +
  tm_shape( roads ) +
  tm_lines( col="red" ) +
  tm_scale_bar( width=0.22 ) +
  tm_compass( position = c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" , title.size=1.5 ,
             title.position=c(0.55,"top"),legend.outside=TRUE )
dev.off()   ## Closes file

pts_sf = st_centroid(georgia_sf)
# open the file
png(filename = "Figure3.png", w = 5, h = 7, units = "in", res = 150)
# make the map
# windows( 9 , 7 )
tm_shape(georgia_sf) +
  tm_fill("olivedrab4") +
  tm_borders("grey", lwd = 1) +
  # the points layer
  tm_shape(pts_sf) +
  tm_bubbles("PctBlack", title.size = "% Black", col = "gold") +
  tm_format("NLD")
# close the png file
dev.off()

####
####  3.5 Mapping Spatial Data Attributes

rm( list=ls() )
data( newhaven )
ls()

## Convert  sp  objects to  sf  objects
blocks_sf = st_as_sf( blocks )
breach_sf = st_as_sf( breach )
tracts_sf = st_as_sf( tracts )

summary( blocks_sf )
class( blocks_sf )
str( blocks_sf )
windows( 9 , 7 )
plot( blocks_sf )

summary( breach_sf )
class( breach_sf )
str( breach_sf )
windows( 9 , 7 )
plot( breach_sf )

summary( tracts_sf )
class( tracts_sf )
str( tracts_sf )
windows( 9 , 7 )
plot( tracts_sf )

blocks_df = data.frame( blocks_sf )
head( blocks_df )
colnames( blocks_df )  ## Get column names in data frame
names( blocks_sf )     ## Get names of colulmns in blocks_sf's data frame

data.frame( blocks_sf$P_VACANT )
blocks_df$P_VACANT
data.frame( blocks_df$P_VACANT )

attach( blocks_df )  ## NOT RECOMMENDED -- This takes all of the 
                     ## columns in blocks_df and creates individual
                     ## variables for them in the Global Environment
                     ## Using  attach  clutters up your Global Environ.

windows( 9 , 7 )
hist( P_VACANT , col="red" , breaks=seq(0,max(P_VACANT)+2,2) )
detach( blocks_df )

breach.dens = st_as_sf( kde.points(breach,lims=tracts) )
                     ## kde  stands for kernel density estimate
                     ## We'll talk more about these later.
                     ## Note that  breach  is an  sp  object, not
                     ## an  sf  object
str( breach.dens )
summary( breach.dens )
windows( 9 , 7 )
plot( breach.dens )

head( blocks_sf )
blocks_sf$RandVar = rnorm( nrow(blocks_sf) )  ## Creates a new column in
                                              ## the blocks_sf data frame
## 3.5.3 Mapping Polygons and Attributes
windows( 9 , 9 )
tmap_mode( "plot" )
tm_shape( blocks_sf ) +
  tm_polygons( "P_OWNEROCC")

windows( 9 , 9 )
## ... works with  sp  objects too.
tm_shape( blocks ) +
  tm_polygons( "P_OWNEROCC")

windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "P_OWNEROCC" , breaks=seq(0,100,10) ) +
  tm_layout( legend.title.size = 1 ,
             legend.text.size = 1 ,
             legend.position = c(0.5,0.5) )  ## Bad position. Try c(0.1,0.1)

library( RColorBrewer )
display.brewer.all()  ## ??

brewer.pal( 5 , "Greens" )

windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "P_OWNEROCC" , title="Owner Occupied" , palette="Blues" ) +
  tm_layout( legend.title.size=1 )

windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_fill( "P_OWNEROCC" , title="Owner Occupied" , palette="Blues" ) +
  tm_layout( legend.title.size=1 )

windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_fill( "P_OWNEROCC" , title="Owner Occupied" , palette="Blues" ) +
  tm_borders( "gray" , lwd=1.2 ) +
  tm_layout( legend.title.size=1 )

## There are several choices for breaks=

p1 = tm_shape( blocks_sf ) +
       tm_polygons( "P_OWNEROCC" , title="Owner Occupied" , palette="Blues") +
       tm_layout( legend.title.size=0.7 )
p2 = tm_shape( blocks_sf ) +
       tm_polygons( "P_OWNEROCC" , title="Owner Occupied" , palette="Oranges" ,
                    style="kmeans" ) +
       tm_layout( legend.title.size=0.7 )
p3 = tm_shape( blocks_sf ) +
       tm_polygons( "P_OWNEROCC" , title="Owner Occupied" , palette="Greens" ,
                    breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1)) ) +
       tm_layout( legend.title.size=0.7 )
library( grid )
grid.newpage()
windows( 12 , 6 )
pushViewport( viewport( layout=grid.layout(1,3) ) )
print( p1 , vp=viewport( layout.pos.col=1 , h=5 ) )
print( p2 , vp=viewport( layout.pos.col=2 , h=5 ) )
print( p3 , vp=viewport( layout.pos.col=3 , h=5 ) )

windows( 9 , 9 )
tm_shape( blocks_sf ) +
  tm_polygons( "P_OWNEROCC" , title="Owner Occupied" , palette="-GnBu" ,
               breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1)) ,
               legend.hist=TRUE ) +
  tm_scale_bar( width=0.22 ) + 
  tm_compass( position=c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" ,
             title.size=2 , title.position=c(0.55,"top") ,
             legend.hist.size=0.5 )

## If you prefer miles over km ...
windows( 9 , 9 )
tm_shape( blocks_sf , unit="miles" ) +
  tm_polygons( "P_OWNEROCC" , title="Owner Occupied" , palette="-GnBu" ,
               breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1)) ,
               legend.hist=TRUE ) +
  tm_scale_bar( width=0.22 ) + 
  tm_compass( position=c(0.8,0.07) ) +
  tm_layout( frame=FALSE , title="New Haven" ,
             title.size=2 , title.position=c(0.55,"top") ,
             legend.hist.size=0.5 )

## Projections

proj4string( tracts ) = proj4string( blocks )
tracts_sf = st_as_sf( tracts )
tracts_sf = st_transform( tracts_sf , "+proj=longlat +ellps=WGS84")

tracts_sf
str( tracts_sf )   ## There are 29 tracts
str( blocks_sf )   ## There are 129 blocks
windows( 9 , 9 )
plot( tracts_sf$P_OWNEROCC )
plot( blocks_sf$P_OWNEROCC )

tm_shape( blocks_sf ) +
  tm_fill( col="POP1990" , convert2density=TRUE ,
           style="kmeans" , title=expression("Population (per " * km^2 * ")" ) ,
           legend.hist=FALSE , id="name" ) +
  tm_borders( "gray25" , alpha=0.5 ) +
  tm_shape( tracts_sf ) +
  tm_borders( "black" , lwd=2.5 )
  
  

  
  
  
  
  
