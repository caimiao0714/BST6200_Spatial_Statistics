setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")
####  install.packages( "tigris" )
library( tigris )
library( sp )
library( ggplot2 )
library( ggthemes )
library( tmap )
library( dplyr )

####  Read in the CT shape file using the tigris package
CT = counties("Connecticut" , cb=TRUE )

####  Read in separately the CIVID19 data
COVID19 = read.csv("COVID-19-CT-2020-04-12.csv")

####  See what the CT data looks like (excludes shape info)
print( CT@data )
####  Notice, counties are NOT in alphabetical order

####  Put the names in alphabetical order.

####  First, pull out the CT@data (the data part of the CT file)
####  Second, find the order that the counties are in.
####  Third, redefine the pulled out CT@data, to make counties 
####  appear in alphabetical order
####  Fourth, reassign the CT@data to be the data from with 
####  counties in alphabetical order

CTdata = CT@data
CTdata = arrange( CTdata , NAME )
CT@data = CTdata

print( CT@data )   ####  Check to be sure the counties are now in order.

####  Add the COVID19 information to the CT file
CT$county.tigris = COVID19$County
CT$COVIDcases = COVID19$COVIDcases
CT$POP2019 = COVID19$POP2019
CT$COVIDrate = COVID19$COVIDcases / COVID19$POP2019

CT.sf = st_as_sf( CT )

windows( 9 , 7 )
tm_shape( CT.sf ) +
  tm_fill( col="COVIDrate" , style="fixed" , breaks=seq(0,0.006,0.001) ) +
  tm_borders( col="black" )

MO = counties("Missouri", cb = TRUE)
str( MO@data )

MO.sf = st_as_sf( MO )
MO.sf$propWater = as.numeric( MO.sf$AWATER ) /
                  ( as.numeric( MO.sf$AWATER) + as.numeric( MO.sf$ALAND) )

max( MO.sf$propWater )
brks = seq( 0 , 0.10 , 0.01 )
windows( 9 , 7 )
tm_shape( MO.sf ) +
  tm_fill( col="propWater" , style="fixed" , breaks=brks) +
  tm_borders( col="black" )


