library( sp )
library( spatstat )
library( maptools )    ## contains  as.SpatialPoints.ppp
library( tmaptools )
library( MASS )
## library( kerdiest )

data( bramblecanes )
str( bramblecanes )
x = bramblecanes$x
y = bramblecanes$y

##  install.packages( "kerdiest" )

windows( 9 , 9 )
bramblecanes.kde = kde2d( x , y , 0.3 )
filled.contour( bramblecanes.kde , xlim=c(0,1) , ylim=c(0,1) , asp=1 )



