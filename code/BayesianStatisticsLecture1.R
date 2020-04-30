n = 10
x = 7
thetaPrior = rep(1/101,101)
theta = seq(0,1,0.01)
likelihood = function(theta,x)  dbinom(x,10,theta)

j = 51   ## gives theta0 = 0.5
theta0 = theta[j]
evidence = sum( thetaPrior*likelihood(theta,x) )
post = thetaPrior[j]*likelihood(theta[j],x) / evidence
post
## for an arbitrary theta:

post = thetaPrior*likelihood(theta,x) / evidence
windows( 9 , 7 )
plot( theta , post , pch=19 , col="blue" )

####  Continuous Uniform Prior
n = 10 
x = 7
theta = seq( 0 , 1 , 0.001 )
post = dbeta( theta , x+1 , n-x+1 )
windows( 9 , 6 )
plot( theta , post , type="l")

lcl = qbeta( 0.025 , x+1 , n-x+1 )
ucl = qbeta( 0.975 , x+1 , n-x+1 )

iLower = which.min( abs( theta - lcl ) )
iUpper = which.min( abs( theta - ucl ) )

windows( 9 , 6 )
plot( theta , post , type="l")
abline( h=0 )
polygon( c(theta[iLower],theta[iLower:iUpper],theta[iUpper],theta[iLower] ),
         c(0,            post[iLower:iUpper], 0            , 0 ) , col="gray" )
         