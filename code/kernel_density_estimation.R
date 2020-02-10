x = c(53,94,77,83,84,72,89,68,73,82)
n = length(x)
bw = 3
#windows( 9 , 5 )
plot( x , 1:n , type="n" , ylim=c(0,1) )
  points( x , rep(0,n) , pch=19 )
  x0 = seq(40,110,0.25)
  for( i in 1:n ) lines( x0 , dnorm(x0,x[i],bw) )
  y = kde( x0 , x , bw ) 
  lines( x0 , y , lwd=4 , col="blue")
  
#windows( 9 , 5 )
plot( x , 1:n , type="n" , ylim=c(0,1) )
  points( x , rep(0,n) , pch=19 )
  x0 = seq(40,110,0.25)
  for( i in 1:n ) lines( x0 , dunif(x0,x[i]-bw,x[i]+bw) )
  y = kde.uniform( x0 , x , bw ) 
  lines( x0 , y , lwd=2 , col="red")
  
kde = function( xvec , x , bw )
{
  m = length( xvec )
  n = length( x )
  y = matrix( 0 , nrow=n , ncol=m )
  for ( i in 1:n ) y[i,] = dnorm( xvec , x[i] , bw )
  z = apply( y , 2 , sum ) 
  return( z )
}
  
kde.uniform = function( xvec , x , bw )
{
  m = length( xvec )
  n = length( x )
  y = matrix( 0 , nrow=n , ncol=m )
  for ( i in 1:n ) y[i,] = dunif( xvec , min=x[i]-bw , max=x[i]+bw )
  z = apply( y , 2 , sum ) 
  return( z )
}
  