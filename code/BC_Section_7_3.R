setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

library( shinyjs )   ## Needed to explore color palettes in tmaptools
library( spdep )
library( SpatialEpi )
library( tmap )
library( tmaptools )

data( pennLC )

###################################################################
####
####  This assumes that all of BC_Section_7_2.R has been executed.
####
###################################################################

set.seed(4676)

penn.state.utm$smk1 = sample( penn.state.utm$smk )
penn.state.utm$smk2 = sample( penn.state.utm$smk )
penn.state.utm$smk3 = sample( penn.state.utm$smk )
penn.state.utm$smk4 = sample( penn.state.utm$smk )
penn.state.utm$smk5 = sample( penn.state.utm$smk )

vars = sample( c("smk","smk1","smk2","smk3","smk4","smk5") )
real.data.i = which( vars == "smk" )  ## Which one is the REAL data

windows( 9 , 8 )
tm_shape( penn.state.utm ) +
  tm_polygons( col=vars , legend.show=FALSE , palette="Reds" , n=8 ) +
  tm_layout( title=1:6 , title.position=c("right","top") )

tmaptools::palette_explorer()

penn.state.nb = poly2nb( penn.state.utm )
print( penn.state.nb )
str( penn.state.nb )
penn.state.nb[[1]]
penn.state.nb[[21]]
penn.state.net = nb2lines( penn.state.nb , coords=coordinates(penn.state.utm) )
penn.state.net = set_projection( penn.state.net , current.projection=3724 )

####  Queen adjacency vs. rook adjacency
####  Think of 5 regions as shown below:
####
####    XXXXXXOOOOOOOOOOO
####    XXXXXXOOOOOOOOOOO
####    EEEEEEVVVVVVVVVIIIIII
####    EEEEEEVVVVVVVVVIIIIII
####
####  Are  OOO  and  III  adjacent?  It depends!

penn.state.nb2 = poly2nb( penn.state.utm , queen=FALSE )
penn.state.net2 = nb2lines( penn.state.nb2 , coords=coordinates(penn.state.utm) )
penn.state.net2 = set_projection( penn.state.net2 , current.projection=3724 )

windows( 9 , 6 )
tm_shape( penn.state.utm ) +
  tm_borders( col="gray" ) +
  tm_shape( penn.state.net2 ) +
  tm_lines( col="orange" , lwd=2 )

windows( 9 , 6 )
tm_shape( penn.state.utm ) +
  tm_borders( col="gray" ) +
  tm_shape( penn.state.net ) +
  tm_lines( col="blue" , lwd=2 ) +
  tm_shape( penn.state.net2 ) +
  tm_lines( col="orange" , lwd=2 )

penn.state.lw = nb2listw( penn.state.nb2 )
print( penn.state.lw )

penn.state.utm$smk.lagged.means = lag.listw( penn.state.lw , penn.state.utm$smk )
####                                      graph list % wghts  outcome variable

windows( 9 , 7 )
tm_shape( penn.state.utm ) +
  tm_polygons( col="smk.lagged.means" , title="Lagged Means\nfor % Smoking" ) +
  tm_layout( legend.bg.color="white" )

penn.smk = data.frame( penn.state.utm )
windows( 12 , 7 )
par( mfrow=c(1,2) , mar=c(3,3,1,1) )
with( penn.smk , {
  plot( smk , smk.lagged.means , asp=1 , xlim=range(smk) , ylim=range(smk) )
  abline( a=0 , b=1 )
})

with( penn.smk , {
  plot( smk , smk.lagged.means , asp=1 , xlim=range(smk) , ylim=range(smk) )
  abline( v=mean(smk) , lty=2 )
  abline( h=mean(smk.lagged.means) , lty=2 )
})


