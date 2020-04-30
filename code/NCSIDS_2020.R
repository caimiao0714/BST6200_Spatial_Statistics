
setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")
## setwd("C:/Users/Steve/Dropbox/MyCourses/BST6200_Spring_2020")
library( maps )
library( sp )
library( spatial )
library( maptools )
library( spdep )
library( classInt )
library( RColorBrewer )
library( rgdal )
library( GISTools )
library( dplyr )
library( tmap )

## Source:
## https://www.rdocumentation.org/packages/spData/versions/0.3.5/topics/nc.sids
## type ?nc.sids to see explanations of the variable names

example( nc.sids )
nc.sids = readOGR(system.file("shapes/sids.shp", package="spData")[1])
proj4string(nc.sids) = CRS("+proj=longlat +ellps=clrk66")
row.names(nc.sids) = as.character(nc.sids$FIPS)

nc.data = select( nc.sids@data , NAME , FIPSNO , BIR74 , SID74 , NWBIR74 , 
                  BIR79 , SID79 , NWBIR79 , x , y , lon , lat )
nc.data = arrange( nc.data , NAME )                  

nc.data$SIDrate74 = nc.data$SID74/nc.data$BIR74
nc.data$SIDrate79 = nc.data$SID79/nc.data$BIR79
nc.sids@data = nc.data    ## This replaces the data with the smaller set.

windows( 14 , 8 )
tm_shape( nc.sids ) +
  tm_polygons( col="SIDrate79" , border.col="black" ) +
  tm_text( "NAME" , "lon" , "lat" , size=0.8 , col="black" ) +
  tm_layout("SIDS Raw Rates", title.size=2)

data.frame( nc.data$NAME , nc.data$SIDrate79 )

###################################################
####  Hierarchical Model
####  non spatial
###################################################
library(methods)  
library(nimble)

nc.sids.Code = nimbleCode(
{
  for (i in 1:k) {
     theta[i] ~ dnorm( mu , tau )      
     y[i] ~ dpois( n[i]*exp(theta[i]) )
  }
  # prior for hyperparameters
  mu ~  dnorm( -6 , 0.001 )
  tau ~ dgamma( 1, .001)
}
)

k = 100      ## There are 100 counties
n  = nc.sids$BIR79
y  = nc.sids$SID79

nc.sids.Consts = list( k = k , n = n )
nc.sids.Data = list( y = y )
nc.sids.Inits = list( mu = -2 , tau = 12 , theta=rep(0.002,k) )

nc.sids.Model = nimbleModel( nc.sids.Code, 
                             data = nc.sids.Data, 
                             constants = nc.sids.Consts, 
                             inits = nc.sids.Inits )

compile.nc.sids.Model = compileNimble(nc.sids.Model)
str( compile.nc.sids.Model )

nc.sids.Conf = configureMCMC(nc.sids.Model, print = TRUE)
nc.sids.Conf$addMonitors(c("mu","tau","theta"))

nc.sids.MCMC = buildMCMC( nc.sids.Conf )
compile.nc.sids.MCMC = compileNimble(nc.sids.MCMC, project = nc.sids.Model)

niter = 55000
nburn =  5000
set.seed(1)

inits = list( mu=-2 , tau=12 , theta=rep(0.002,k) )
start.time = proc.time()
samples = runMCMC( compile.nc.sids.MCMC, niter = niter, nburnin = nburn,
                   inits = inits, nchains = 1, samplesAsCodaMCMC = TRUE )
stop.time = proc.time()
time.elapsed = stop.time - start.time
print( time.elapsed )

samples[1:100,1:6]

windows( 12 , 9 )
samples1 = samples
par(mfrow = c(2, 2), mai = c(.6, .5, .4, .1), mgp = c(1.8, 0.7, 0))
ts.plot(samples1[ , 'mu'], xlab = 'iteration', col="red" , lwd=1.5 ,
        ylab = expression(mu), main = expression(mu))
ts.plot(samples1[ , 'tau'], xlab = 'iteration', col="red" , lwd=1.5 ,
        ylab = expression(tau), main = expression(tau))
ts.plot(samples1[ , 'theta[1]'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(theta[1]), main = expression(theta[1]))
ts.plot(samples1[ , 'theta[2]'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(theta[2]), main = expression(theta[2]))

eta = exp( apply( samples[,3:102] , 2 , mean ) )

nc.sids$HB.est.rate = eta 

windows( 14 , 8 )
tm_shape( nc.sids ) +
  tm_polygons( col="SIDrate79" , border.col="black" , 
               breaks=seq(0,0.007,0.001) ) +
  tm_text( "NAME" , "lon" , "lat" , size=0.8 , col="black" )

windows( 14 , 8 )
tm_shape( nc.sids ) +
  tm_polygons( col="HB.est.rate" , border.col="black" , 
               breaks=seq(0,0.007,0.001)) +
  tm_text( "NAME" , "lon" , "lat" , size=0.8 , col="black" )  +
  tm_layout("SIDS Rates from \nHierarchical Model", title.size=1.8)

data.frame( nc.sids$NAME , nc.sids$SIDrate79 , nc.sids$HB.est.rate )

windows( 9 , 9 )
plot( nc.sids$SIDrate79 , nc.sids$HB.est.rate , xlim=c(0,0.0062) , 
      ylim=c(0,0.005) , cex=0.2+nc.sids$BIR79/20000 , pch=19 , col="red" ,
      asp=1 , xlab="Raw Rates" , ylab="Hierarchical Model")
abline( a=0 , b=1 ,col="gray")

###################################################
####  Hierarchical Model
####  SPATIAL
###################################################
class( nc.sids )
nc.sids.nb = poly2nb( nc.sids )
nc.sids.net = nb2lines( nc.sids.nb , coords=coordinates(nc.sids) )

windows( 14 , 8 )
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

niter = 55000
nburn =  5000
set.seed(1)

start.time = proc.time()
samples = runMCMC( compile.nc.sids.MCMC, niter = niter, nburnin = nburn,
                   inits = inits, nchains = 1, samplesAsCodaMCMC = TRUE )
stop.time = proc.time()
time.elapsed = stop.time - start.time
print( time.elapsed )

windows( 12 , 9 )
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

windows( 14 , 8 )
tm_shape( nc.sids ) +
  tm_polygons( col="HB.spatial.est.rate" , border.col="black" , 
               breaks=seq(0,0.007,0.001)) +
  tm_text( "NAME" , "lon" , "lat" , size=0.8 , col="black" )  +
  tm_layout("SIDS Rates from \nCAR Model", title.size=1.8)

windows( 9 , 9 )
plot( nc.sids$HB.spatial.est.rate , nc.sids$HB.est.rate , xlim=c(0,0.0062) , 
      ylim=c(0,0.005) , cex=0.1+nc.sids$BIR79/20000 , pch=19 , col="red" ,
      asp=1 , xlab="Raw Rates" , ylab="Hierarchical Model")
abline( a=0 , b=1 ,col="gray")

############################################################
############################################################
####  Small problem from p. 107 in nimble's user manual
####  Nimble uses "sparse" neighbors.
############################################################
############################################################

code = nimbleCode({
  alpha ~ dflat()
  beta ~ dnorm( 0 , 0.0001 )
  tau ~ dgamma( 0.001 , 0.001 )
  for (k in 1:L)
    weights[k]  <-  1
  s[1:N] ~ dcar_normal(adj[1:L],weights[1:L],num[1:N],tau,zero_mean=1)
  for (i in 1:N) {
    log(lambda[i])  <-  alpha + beta*x[i] + s[i]
    y[i] ~ dpois( lambda[i] )
  }  
})
constants = list( N=4 , L=8 , num=c(3,2,2,1) ,
                  adj=c(2,3,4,1,3,1,2,1) , x=c(0,2,2,8) )
data = list( y=c(6,9,7,12) )
inits = list( alpha=0 , beta=0 , tau=1 , s=c(0,0,0,0) )

Rmodel = nimbleModel( code=code , 
                      constants=constants , 
                      data=data ,
                      inits=inits )
compile.Rmodel = compileNimble( Rmodel )

Rmodel.Conf = configureMCMC( Rmodel , print = TRUE )
Rmodel.Conf$addMonitors(c("alpha","beta","tau","lambda"))

Rmodel.MCMC = buildMCMC( Rmodel.Conf )
compile.Rmodel.MCMC = compileNimble( Rmodel.MCMC )

niter = 11000
nburn =  1000
set.seed(1)

start.time = proc.time()
samples = runMCMC( compile.Rmodel.MCMC, niter = niter, nburnin = nburn,
                   inits = inits, nchains = 1, samplesAsCodaMCMC = TRUE )
stop.time = proc.time()
time.elapsed = stop.time - start.time
print( time.elapsed )

samples[1:10,1:7]

windows( 12 , 9 )
samples1 = samples
par(mfrow = c(2, 2), mai = c(.6, .5, .4, .1), mgp = c(1.8, 0.7, 0))
ts.plot(samples1[ , 'alpha'], xlab = 'iteration', col="red" , lwd=1.5 ,
        ylab = expression(alpha), main = expression(alpha))
ts.plot(samples1[ , 'beta'], xlab = 'iteration', col="red" , lwd=1.5 ,
        ylab = expression(beta), main = expression(beta))
ts.plot(samples1[ , 'tau'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(tau), main = expression(tau))
ts.plot(samples1[ , 'lambda[1]'], xlab = 'iteration', col="blue" , lwd=1.5 ,
        ylab = expression(lambda[1]), main = expression(lambda[1]))


