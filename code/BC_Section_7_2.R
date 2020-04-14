setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

library( tmap )
library( tmaptools )
library( SpatialEpi )
library( tidyverse )
library( GISTools )
library( sp )
library( rgeos )

data( pennLC )
str( pennLC )
str( pennLC$geo )
str( pennLC$data )
str( pennLC$smoking )
str( pennLC$spatial.polygon )
pennLC.data = pennLC$data
penn.state.latlong <- pennLC$spatial.polygon

plot( penn.state.latlong )   ###  Note "flat" projection
print( penn.state.latlong )

plot( pennLC$geo )
print( pennLC$geo )

plot( pennLC$data )
print( pennLC$data )

plot( pennLC$smoking )
print( pennLC$smoking )

#### Convert to UTM zone 17N
penn.state.utm <- set_projection(penn.state.latlong, 3724)
print( penn.state.utm )
class( penn.state.utm )
if ("sf" %in% class(penn.state.utm)) 
  penn.state.utm <- as(penn.state.utm,"Spatial")

#### Obtain the smoking rates
penn.state.utm$smk <- pennLC$smoking$smoking * 100

#### Draw a choropleth map of the smoking rates
windows( 10 , 6 )
tm_shape(penn.state.utm) + 
  tm_polygons( col='smk' , title="Smoking %" , border.col="black" )
