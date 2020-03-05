setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

library( sf )
library( GISTools )
library( tmap )

## Section 5.1 Using R as a GIS - Introduction
##
data( georgia )
class( georgia )
class( georgia2 )
## Look in Global Environment to see different projections 
## in georgia and georgia2

georgia_sf = st_as_sf( georgia )
class( georgia_sf )

georgia_v2 = as( georgia_sf , "Spatial" )
class( georgia_v2 )

## Section 5.2 Spatial Intersection and Clip Operations
##

data( tornados )   ## from GISTools
torn_sf = st_as_sf( torn ) 
us_states_sf = st_as_sf( us_states )

myProj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

windows( 9 , 7 )   ## without projection ...
tm_shape( us_states_sf ) +
  tm_polygons( "gray90" ) +
  tm_shape( torn_sf ) +
  tm_dots( col="#FB6A4A" ) +
  tm_shape( us_states_sf ) +
  tm_borders( col="black" ) +
  tm_layout( frame=FALSE )

windows( 9 , 7 )
tm_shape( us_states_sf , projection=myProj ) +
  tm_polygons( "gray90" ) +
  tm_shape( torn_sf ) +
  tm_dots( col="#FB6A4A" ) +
  tm_shape( us_states_sf ) +
  tm_borders( col="black" ) +
  tm_layout( frame=FALSE )

windows( 9 , 7 )  
plot( us_states , col="gray90" )
plot( torn , add=TRUE , pch=1 , col="#FB6A4A4C" , cex=0.4 )

## Using base R plotting with sp objects
windows( 9 , 7 )
plot( us_states , col="gray90" )
plot( torn , add=TRUE , pch=1 , col="#FB6A4A4C" , cex=0.4 )
plot( us_states , add=TRUE )   ## very subtle!!

summary( torn )
summary( us_states )
summary( torn_sf )
RedOrng = "#FB6A4A"

## Suppose we want to study only Texas, New Mexico, Oklahoma and Arkansas
## AoI is "Area of Interest"
indx = us_states$STATE_NAME == "Texas" |
       us_states$STATE_NAME == "New Mexico" |
       us_states$STATE_NAME == "Oklahoma" |
       us_states$STATE_NAME == "Arkansas" 
AoI = us_states[ indx , ]
AoI_sf = us_states_sf[ indx , ]

windows( 9 , 7 )
tm_shape( AoI_sf , projection=myProj ) +
  tm_borders( col="black") +
  tm_layout( frame=FALSE ) +
  tm_shape( torn_sf ) +
  tm_dots( col=RedOrng , size=0.2 , shape=1 , alpha=0.5 ) 
  
windows( 9 , 7 )
torn_clip_sf = torn_sf[ AoI_sf , ]
tm_shape( torn_clip_sf , projection=myProj ) + 
  tm_dots(col=RedOrng , size=0.2 , shape=1 , alpha=0.5 ) +
  tm_shape( AoI_sf )+
  tm_borders()

windows( 9 , 7 )
AoI_torn_sf = st_intersection( AoI_sf , torn_sf )
str( AoI_sf )
str( torn_sf )
tm_shape( AoI_sf , projection=myProj ) +
  tm_borders( col="black" ) +
  tm_layout( frame=FALSE ) +
  tm_shape( AoI_torn_sf ) +
  tm_dots( col=RedOrng , size=0.2 , shape=1 , alpha=0.5 )

## Section 5.3 Buffers
##
AoI = us_states2[ us_states2$STATE_NAME == "Texas" |
                  us_states$STATE_NAME == "New Mexico" |
                  us_states$STATE_NAME == "Oklahoma" |
                  us_states$STATE_NAME == "Arkansas", ] 
AoI.buf = gBuffer( AoI , width=25000 )
windows( 9 , 7 )
par( mar=c(0,0,0,0) )
plot( AoI.buf )
plot( AoI , add=TRUE , border="red" )

## in sf

us_states2_sf = st_as_sf( us_states2 )
AoI_sf = st_as_sf( us_states2_sf[us_states2_sf$STATE_NAME == "Texas" |
                                 us_states$STATE_NAME == "New Mexico" |
                                 us_states$STATE_NAME == "Oklahoma" |
                                 us_states$STATE_NAME == "Arkansas", ] )
AoI_buf_sf = st_buffer( AoI_sf , dist=25000 )

## Get plot of tornados in the buffered area
AoI_buf_sf1 = st_transform( AoI_buf_sf, myProj )
torn_sf     = st_transform( torn_sf   , myProj )

AoI_torn_sf1 = st_intersection( st_union(AoI_buf_sf1) , torn_sf )
str( AoI_buf_sf )
str( torn_sf )

windows( 9 , 7 )
tm_shape( AoI_torn_sf1 , projection=myProj ) +
  tm_dots( col=RedOrng ) +
  tm_shape( AoI_sf ) +
  tm_borders( "red" ) +
  tm_layout( frame=FALSE ) 

## Look at Georgia data on p. 155
## Won't go through it here.

## Section 5.4 Merging Spatial Feature
us_states_sf = st_as_sf( us_states )
AoI.merge_sf = st_sf( st_union( us_states_sf) )
windows( 9 , 7 )
tm_shape( us_states_sf , projection="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") +
  tm_borders( col="green" , lty=3 ) +
  tm_shape( AoI.merge_sf ) +
  tm_borders( lwd=2 , col="black" ) +
  tm_layout( frame=FALSE )

## Section 5.5 Point-in-Polygon and Area Calculations
##

poly.counts

torn.count = poly.counts( torn , us_states )
torn.count
names( torn.count ) = us_states$STATE_NAME
torn.count

proj4string( us_states2 )
st_crs( us_states2_sf )

windows( 9 , 7 )
plot( us_states2 )

poly.areas
gArea

## areas are in square meters
##
## in sp
poly.areas( us_states2 )
poly.areas( us_states2 ) / (1000*1000)
  
## in sf
st_area( us_states2_sf )
st_area( us_states2_sf ) / (1000*1000)

data( newhaven )
blocks$densities = poly.counts( breach,blocks) / 
                   ft2miles( ft2miles( poly.areas(blocks) ) )
windows( 9 , 7 )
plot( blocks$P_OWNEROCC , blocks$densities )
cor( blocks$P_OWNEROCC , blocks$densities )

library( ggplot2 )
library( RColorBrewer )
windows( 9 , 7 )
ggplot( blocks@data , aes(P_OWNEROCC,densities ) ) +
  geom_point() +
  geom_smooth( method="lm" ) +
  ylab( "Density of Breaches")
cor( blocks$P_OWNEROCC , blocks$densities )

data( newhaven )
attach( data.frame(blocks) )
n.breaches = poly.counts( breach , blocks )
n.breaches
area = ft2miles( ft2miles( poly.areas(blocks) ) )
area

blocks_sf = st_as_sf( blocks )
st_area( blocks_sf )            ## st_area is the sf version of poly.areas

model1 = glm( n.breaches ~ P_OWNEROCC , offset=log(area) , family=poisson)
summary( model1 )

s.resids = rstandard( model1 )
resid.shades = shading( c(-2,2) , brewer.pal( 3 , "Greens" ) )

windows( 8 , 8 )
par( mar=c(0,0,0,0) )
choropleth( blocks , s.resids , resid.shades )

model2 = glm( n.breaches ~ P_OWNEROCC + P_VACANT + P_RENTROCC ,
              offset=log(area) , family=poisson )
summary( model2 )
s.resids.2 = rstandard( model2 )

windows( 8 , 8 )
par( mar=c(0,0,0,0) )
choropleth( blocks , s.resids.2 , resid.shades )

detach( data.frame(blocks) )

##
## Section 5.6 Creating Distance Attributes

x = matrix( rnorm(100) , nrow=5 )
colnames(x) = paste0( "Var" , 1:20 )
x
dist(x)
as.matrix( dist(x) )

as.matrix( dist( coordinates(blocks) ) )
as.matrix( dist( coordinates(georgia2) ) )

class( georgia )
class( georgia2 )
proj4string( georgia )
proj4string( georgia2 )

poly.areas( georgia )
poly.areas( georgia2 )

st_area( st_as_sf( georgia2 ) )

appling.area = poly.areas( georgia2 )[1]
appling.area.wikip = 512.11   ## square miles. Obtained from wikipedia
appling.area * 0.62137119^2 / (1000^2 )


## str( georgia )
## str( georgia2 )
gDistance( georgia[1,] , georgia[2,] )   ## fails
gDistance( georgia2[1,] , georgia2[2,] ) ## works
gDistance( georgia2[81,] , georgia2[82,] ) 
gDistance( georgia2[81,] , georgia2[83,] ) ## adjacent counties
gDistance( georgia2[82,] , georgia2[83,] )

georgia@data$Name[ c(81,83) ]
georgia@data$Latitude[ c(81,83) ]
georgia@data$Longitud[ c(81,83) ]

d12.m  = gDistance( georgia2[1,] , georgia2[2,] )
d12.km = gDistance( georgia2[1,] , georgia2[2,] ) / 1000

windows( 8 , 9 )
g = st_union( georgia_sf ) 
index1 = c(1,2,81,82,83)
georgia_sf.sub1 = georgia_sf[index1,]
georgia2@data$Name
windows( 9 , 9 )
tm_shape( georgia_sf ) +
  tm_fill( "white" ) +
  tm_borders( "gray" , lwd=0.5 ) +
  ## Now add the second layer
  tm_shape( g ) +
  tm_borders( lwd=2 ) +
  ## Now add the third layer
  tm_shape( georgia_sf.sub1 ) +
  tm_fill( "skyblue" ) +
  tm_text( "Name" , size=1 ) +  
  tm_borders() +
  tm_scale_bar() +
  ## Now add layout parameters
  tm_layout( frame=TRUE , title="Georgia with a subset of counties" ,
             title.size=1 , title.position=c(0.02,"bottom") )

## Convert to sf
georgia_sf  = st_as_sf( georgia )
georgia2_sf = st_as_sf( georgia2 )
st_distance( georgia_sf[1,] , georgia_sf[2,] )
st_distance( georgia_sf[81,] , georgia_sf[82,] )
st_distance( georgia_sf[81,] , georgia_sf[83,] )  ## adjacent counties

index = c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17 )  #contiguous counties
st_distance( georgia2_sf[index,])

## New Haven Data
data( newhaven )
proj4string(places) = CRS( proj4string(blocks) )
cents = SpatialPoints( coordinates(blocks) , 
                       proj4string = CRS(proj4string(blocks)) )
cents@coords
places@data
distances = ft2miles( gDistance(places,cents,byid=TRUE))
head( round(distances,3) )
distances1 = gWithinDistance( places , cents , byid=TRUE , dist=miles2ft(1.2) )

##  from textbook ...
##  The use of distance measures in conjunction with census data is 
##  particularly useful for analyuzing access to the supply of some
##  facility or service for different social groups. The code below
##  replicates the analysis developed by Comber et al. (2008), examining
##  access to green spaces for different social groups.  In this
##  exercise a hypothetical example is used: we wish to examine the 
##  equity of access to the locations recorded in the places variable
##  (supply) for different ethnic groups as recorded in the blocks
##  data set (demand), on the basis tha we would expect everyone to 
##  be within 1 mile of a facility.

distances = ft2miles( gDistance( places , cents , byid = TRUE ) )
min.dist = as.vector( apply( distances , 1 , min ) )  # how close is each
                                                      # block to the 
                                                      # nearest "place"?
blocks$access = ( min.dist < 1 )   # logical variable
qtm( blocks , "access" )
ethnicity = as.matrix( data.frame( blocks[,14:18] )/100 )
ethnicity = apply( ethnicity , 2 , function(x) (x*blocks$POP1990) )
ethnicity = matrix( as.integer(ethnicity) , ncol=5 )
colnames( ethnicity ) = c("White" , "Black" , "Native American" ,
                          "Asian" , "Other" )
mat.access.tab = xtabs( ethnicity ~ blocks$access )

data.set = as.data.frame( mat.access.tab )
colnames(data.set) = c("Access","Ethnicity","Freq")

modelethnic = glm( Freq ~ Access*Ethnicity , data=data.set , family=poisson )
## Hierarchical model speficication: Y ~ x1*X2 assumes Y ~ X1 + X2 + X1*X2
summary( modelethnic )

modelethnic2 = glm( Freq ~ Access + Ethnicity , 
                    data=data.set , family=poisson )
summary( modelethnic2 )

mod.coefs2 = summary( modelethnic2 )$coef


tab = 100 * ( exp( mod.coefs2[,1] ) - 1 )
tab = tab[ 3:6 ]
names(tab) = colnames(ethnicity)[2:5]


chisq = chisq.test( mat.access.tab )
chisq

library("gplots")
windows( 7 , 9 )
balloonplot( mat.access.tab , main ="Access", xlab ="", ylab="",
             label = FALSE, show.margins = TRUE )

windows( 9 , 7 )
mosaicplot( t(mat.access.tab) , xlab="" , ylab="Access to Supply" ,
            main="Mosaic Plot of Access" , shade=TRUE , las=1 , cex=0.8)

## Section 5.7 Combining Spatial Datasets and their Attributes
##

library(GISTools)
library(sf)
library(tmap)
data(newhaven)
bb = bbox(tracts)

##  using sp
grd = GridTopology( cellcentre.offset=c(bb[1,1]-200,bb[2,1]-200) ,
                    cellsize=c(10000,10000) , cells.dim=c(5,5) ) 
int.layer = SpatialPolygonsDataFrame( as.SpatialPolygons.GridTopology(grd) ,
                                      data=data.frame(c(1:25)) , 
                                      match.ID=FALSE )
ct = proj4string( blocks )
proj4string(int.layer) = ct
proj4string(tracts) = ct
names(int.layer) = "ID"
plot(int.layer)

##  using sf
int.layer_sf = st_as_sf( int.layer )
tracts_sf    = st_as_sf( tracts )
int.res_sf   = st_intersection( int.layer_sf , tracts_sf )

p1 = tm_shape( int.layer_sf ) +
       tm_layout( frame=FALSE ) +
       tm_text( "ID" , size=0.7 ) +
       tm_shape( tracts_sf ) +
       tm_borders( col="red" , lwd=2 ) 
p2 = tm_shape( int.layer_sf ) +
       tm_borders( col="white" ) +
       tm_shape( int.res_sf ) +
       tm_polygons( "HSE_UNITS" , palette=blues9 ) +
       tm_layout( frame=FALSE , legend.show=FALSE )
library( grid )
grid.newpage()
pushViewport( viewport( layout=grid.layout(1,2) ) )
print( p1 , vp=viewport( layout.pos.col=1 ) )
print( p2 , vp=viewport( layout.pos.col=2 ) )
#############################################################
####  Scratch 

us_states
proj4string=CRS("+init=epsg:28992")
us_states_sf = st_transform( us_states_sf , "+proj=gall")


data(World)
w1 <- qtm(World, projection = "+proj=eck4", title="Eckert IV")
w2 <- qtm(World, projection = 3857, title="Mercator")
w3 <- qtm(World, projection = "+proj=gall", title="Gall stereographic")
w4 <- qtm(World, projection = "+proj=robin", title="Robinsin")

windows( 9 , 7 )
print( w3 )
