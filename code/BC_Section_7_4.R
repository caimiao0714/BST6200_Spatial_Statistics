setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

library( shinyjs )   ## Needed to explore color palettes in tmaptools
library( spdep )
library( SpatialEpi )
library( tmap )
library( tmaptools )
library( tmap )
library( tidyverse )
library( GISTools )
library( sp )
library( rgeos )

data( pennLC )

###################################################################
####
####  This assumes that all of BC_Section_7_3.R has been executed.
####
###################################################################

str( penn.state.lw )

pennLC.moran.test = moran.test( penn.state.utm$smk , penn.state.lw )
print( pennLC.moran.test )
str( pennLC.moran.test )

I = pennLC.moran.test$estimate[[1]]
expI = pennLC.moran.test$estimate[[2]]
varI = pennLC.moran.test$estimate[[3]]
z.stat = ( I - expI ) / sqrt(varI)
####        outcome variable    graph list & wghts
####  Note, arguments in reverse order from lag.listw()

moran.range = function(lw) {
  wmat = listw2mat(lw)
  return( range( eigen( (wmat + t(wmat))/2 )$values) )
}

moran.range( penn.state.lw )

moran.var = function(w) {
  n = nrow(w) 
  S0 = sum( w )
  S1 = 0.5 * sum( (w+t(w))^2 )
  S2 = sum( (apply( w , 2 , sum ) + apply( w , 1 , sum ) )^2 )
  varI = ( n^2 * S1  -  n * S2  + 3 * S0^2 ) /
         ( (n-1)*(n+1)*S0^2 ) - (1/(n-1))^2
  return( varI )
}
varI1 = moran.var(w)
####  
####  Small example of 5 regions for illustration
####
w1 = matrix( c ( 0   , 1/4 , 1/4 , 1/4 , 1/4 ,
                 1/2 , 0   , 0   , 0   , 1/2 ,
                 1/2 , 0   , 0   , 1/2 , 0   ,
                 1/3 , 0   , 1/3 , 0   , 1/3 ,
                 1/3 , 1/3 , 0   , 1/3 , 0 ) , nrow=5 , ncol=5 , byrow=TRUE )
z1 = c( 23 , 25 , 18 , 18 , 20 )
nb1 = mat2listw(w1)
moran.test( z1 , nb1 )

str( nb1 )
class( nb1 )
moran.var(w1)

####
####  Back to Pennsylvania Smoking Data
####

pennLC.moran.test = moran.test( penn.state.utm$smk , penn.state.lw )
str( pennLC.moran.test )
str(penn.state.nb2)
w = listw2mat( penn.state.lw )  ## Note, mostly zeros

I = pennLC.moran.test$estimate[[1]]
varI = moran.var(w)
n = nrow(w)
expI = -1/(n-1)
z.statistic = ( I - expI ) / sqrt( varI )
p.value = pnorm( z.statistic , lower.tail=FALSE )
#### If the alternative is two-sided
p.value2 = 2 * ( min( pnorm(z.statistic),pnorm(-z.statistic) ) )

####
####  Using Simulation to Test Significance of Moran's I
####
pennLC.moran.sim = moran.mc( penn.state.utm$smk , penn.state.lw , 99999 )
str( pennLC.moran.sim )
simulated.I = pennLC.moran.sim$res
windows( 9 , 6 )
hist( simulated.I , xlim=c(-0.5,0.5) , breaks=seq(-0.5,0.5,0.01) , col="red" )
abline( v=pennLC.moran.sim$statistic )
