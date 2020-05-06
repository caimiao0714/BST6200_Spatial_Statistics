pacman::p_load(maps, sp, spdep, spatial, maptools, classInt, RColorBrewer, 
               rgdal, GISTools, dplyr, tmap, methods, nimble)

nc.sids = readOGR(system.file("shapes/sids.shp", package="spData")[1])
proj4string(nc.sids) = CRS("+proj=longlat +ellps=clrk66")
row.names(nc.sids) = as.character(nc.sids$FIPS)

nc.data = nc.sids@data %>% 
  dplyr::select(NAME, FIPSNO, BIR74, SID74, NWBIR74,
                BIR79, SID79, NWBIR79, x, y, lon, lat) %>% 
  arrange(NAME)  %>% 
  mutate(SIDrate74 = SID74/BIR74,
         SIDrate79 = SID79/BIR79)

nc.sids@data = nc.data

tm_shape(nc.sids) +
  tm_polygons(col = "SIDrate79", border.col = "black") +
  tm_text("NAME", "lon", "lat", size = 0.8, col = "black") +
  tm_layout("SIDS Raw Rates", title.size = 2)



###################################################
####  Hierarchical Model
####  SPATIAL
###################################################
class( nc.sids )
nc.sids.nb = poly2nb( nc.sids )
nc.sids.net = nb2lines( nc.sids.nb , coords=coordinates(nc.sids) )

tm_shape( nc.sids ) +
  tm_borders( col="darkgray" ) +
  tm_shape( nc.sids.net ) +
  tm_lines( col="darkgreen" , lwd=2 )

nc.sids.lw = nb2listw( nc.sids.nb )

num = rep(0,k)
for (i in 1:k) num[i] = length( nc.sids.lw$neighbours[[i]] )
adj = c()
for (i in 1:k) adj = c(adj,nc.sids.lw$neighbours[[i]] )
L = length(adj)

code = nimbleCode({
  mu ~ dflat()
  tau ~ dgamma( 1 , 0.001 )
  for (i in 1:L)
    weights[i]  <-  1
  s[1:k] ~ dcar_normal(adj[1:L],weights[1:L],num[1:k],tau,zero_mean=1)
  for (i in 1:k) {
    log(theta[i])  <-  mu + s[i]
    y[i] ~ dpois( BIR79[i]*theta[i] )
  }  
})

constants = list( k=100 , L=490 , num=num , adj=adj , BIR79=nc.sids$BIR79 )
data = list( y=nc.sids$SID79 )
inits = list( mu=0 , tau=1 , s=rep(0,k) )

nc.sids.model = nimbleModel( code=code , 
                             constants=constants , 
                             data=data ,
                             inits=inits )

compile.nc.sids.model = compileNimble( nc.sids.model )

nc.sids.model.Conf = configureMCMC( nc.sids.model , print = TRUE )

nc.sids.model.Conf$addMonitors(c("mu","tau","theta"))

nc.sids.model.MCMC = buildMCMC( nc.sids.model.Conf )

compile.nc.sids.MCMC = compileNimble( nc.sids.model.MCMC )

niter = 8000
nburn =  2000
set.seed(1)

start.time = proc.time()
samples = runMCMC( compile.nc.sids.MCMC, niter = niter, nburnin = nburn,
                   inits = inits, nchains = 1, samplesAsCodaMCMC = TRUE )
stop.time = proc.time()
time.elapsed = stop.time - start.time
print( time.elapsed )


samples1 = samples
par(mfrow = c(2, 2), mai = c(.6, .5, .4, .1), mgp = c(1.8, 0.7, 0))
ts.plot(samples1[ , 'mu'], xlab = 'iteration', col="red" , lwd=1.5 ,
        ylab = expression(mu), main = expression(mu))
ts.plot(samples1[ , 'tau'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(tau), main = expression(tau))
ts.plot(samples1[ , 'theta[1]'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(theta[1]), main = expression(theta[1]))
ts.plot(samples1[ , 'theta[2]'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(theta[2]), main = expression(theta[2]))

eta.HB.spatial = exp( apply( samples[,3:(k+2)] , 2 , mean ) )
nc.sids$HB.spatial.est.rate = eta 


tm_shape( nc.sids ) +
  tm_polygons( col="HB.spatial.est.rate" , border.col="black" , 
               breaks=seq(0,0.007,0.001)) +
  tm_text( "NAME" , "lon" , "lat" , size=0.8 , col="black" )  +
  tm_layout("SIDS Rates from \nCAR Model", title.size=1.8)


plot( nc.sids$HB.spatial.est.rate , nc.sids$HB.est.rate , xlim=c(0,0.0062) , 
      ylim=c(0,0.005) , cex=0.1+nc.sids$BIR79/20000 , pch=19 , col="red" ,
      asp=1 , xlab="Raw Rates" , ylab="Hierarchical Model")
abline( a=0 , b=1 ,col="gray")

