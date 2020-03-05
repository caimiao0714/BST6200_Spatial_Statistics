pacman::p_load(GISTools, tmap, tmaptools, spatstat, sf, fMultivar)

data( bramblecanes )
bramblecanes$marks = as.character(bramblecanes$marks)
group0 = as(bramblecanes[bramblecanes$marks == "0"], "SpatialPoints")
proj4string(group0) <- CRS('+init=epsg:26978')

#not sure why error occur
group0_dens = smooth_map( group0)
tm_shape( group0_dens$raster ) +
  tm_raster()




trees = read.csv("homework/trees.csv")
trees_ppp = ppp(trees$x,trees$y,xrange=c(0,200),yrange=c(0,200))
trees_sp = as.SpatialPoints.ppp(trees_ppp)
proj4string(trees_sp) <- CRS('+init=epsg:26978')
#plot will not change no matter how contours change
sp_dens = smooth_map( trees_sp ,
                      breaks= seq( 0 , 50000 , by = 5000 ) , 
                      style="fixed", 
                      bandwidth = c(0.01,0.01))
tm_shape( sp_dens$raster ) +
  tm_raster()

sp_dens = smooth_map( trees_sp ,
                      breaks= seq( 0 , 50000 , by = 10000 ) , 
                      style="fixed", 
                      bandwidth = c(0.015,0.015)) # you also need to adjust the bandwidth
tm_shape( sp_dens$raster ) +
  tm_raster()
