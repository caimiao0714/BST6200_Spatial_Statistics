####  Code from Brunsdon and Comber
####
####  Chapter 2
####
####  Section 2.2 Basic Ingredients of R
####

setwd("C:/Users/srigd/Dropbox/MyCourses/BST6200_Spring_2020")

#### p. 14
x = 3
y = 12
z = x + y
print( z )
c(x,y,z)
print( c(x,y,z) )
sqrt(z)

#### pp. 15-17
tree.heights = c(4.3,7.1,6.3,5.2,3.2,2.1)
tree.heights
sum( tree.heights )
mean( tree.heights )

max.height = max( tree.heights )
tree.heights[1]
tree.heights[1:3]
tree.heights[c(1:3,5)]
tree.heights[c(5,3,2)] 

sqrt( tree.heights )

cities = c("Leicester","Newcastle","London","Leeds","Exeter")
northern = c(FALSE, TRUE, FALSE, TRUE, FALSE)
cities[ northern ]

####
####  Section 2.3 Data Types and Data Classes
####
#### pp. 18-19
character(8)
as.character("8")
as.character(8)
is.character(8)
is.character("8")

numeric(8)
as.numeric( c("1980","-8","Geography") )
as.numeric( c(TRUE , FALSE) )
is.numeric( c( 2 , -3 , FALSE ) )
is.numeric( c( 2 , -3 , "8" ) )

#### p. 19 Logical or Boolean
logical(7)
as.logical( c(7,4,0,-4,5,1) )
as.logical( c(7,4,0,-4,5,1) ) * 1

myData = c( 2 , 5 , -3 , 20 , 14 , 23 , 9 )
myData >= 10
myData[ myData >= 10 ]

indx = myData >= 10
myData[ indx ]

#### Naming Arguments (not in book)
seq( 1 , 11 , 2 )
seq( 1 , 2 , 11 )
seq( from=1 , to=11 , by=2 )
seq( to=11 , by=2 , from=1 )
pnorm( 1.96 , 0 , 2 )
?pnorm
pnorm( mean=0 , sd=2 , q=1.96 )

#### p. 19 Vectors
x = vector( mode="numeric" , length=8 )
x
str(x)
1:5
n = 20
0:n
0:n+1
0:(n+1)

tmp = data.frame( 10:15 , 15:20 )
tmp
tmp1 = data.frame( a=10:15 , b=15:20 )
tmp1
a = tmp1$a
b = tmp1$b
a + b
a*b

#### p. 20 Matrices
A = matrix( 0 , nrow=2 , ncol=3 )
A
B = matrix( c(1,2,3,4,5,6) , nrow=2 , ncol=3 )
B
C = matrix( c(1,2,3,4,5,6) , nrow=2 , ncol=3 , byrow=TRUE )
C
B + C
B * C       ####  * is componentwise multiplication
B %*% C     ####  %*% is matrix multiplication
Ct = t(C)   ####  t is the transpose function
B %*% Ct
t(B) %*% C

flow = matrix(c(2000, 1243, 543, 1243, 212, 545, 
                 654, 168, 109), c(3,3), byrow=TRUE) 
## Rows and columns can have names, not just 1,2,3,...
colnames(flow) <- c("Leeds", "Maynooth", "Elsewhere" )
rownames(flow) <- c("Leeds", "Maynooth", "Elsewhere" )
## examine the matrix
flow 
str( flow )

?sum
help(sum)
# Create a variable to pass to other summary functions
x = matrix(c(3,6,8,8,6,1,-1,6,7),c(3,3),byrow=TRUE)
# Sum over rows
rowSums(x) 
# Sum over columns
colSums(x)  
# Calculate column means
colMeans(x)   
# Apply function over rows (1) or columns (2) of x
apply(x,1,max)
# Logical operations to select matrix elements
x[,c(TRUE,FALSE,TRUE)]  
# Add up all of the elements in x
sum(x)    
# Pick out the leading diagonal
diag(x)
# Matrix inverse
xinv = solve(x)
x %*% xinv
# Tool to handle rounding
zapsmall(x %*% solve(x))   

#### pp. 22-23 Factors
house.type <- c("Bungalow", "Flat", "Flat", 
                "Detached", "Flat", "Terrace", "Terrace")
str( house.type )
# a factor assignment
house.type1 = factor(c("Bungalow", "Flat", 
                       "Flat", "Detached", "Flat", "Terrace", "Terrace"), 
                     levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type1
str( house.type1 )

house.type2 = factor(c("People Carrier", "Flat", 
                       "Flat", "Hatchback", "Flat", "Terrace", "Terrace"), 
                     levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type2

#### p. 23 Ordering
income = factor(c("High", "High", "Low", "Low", 
                  "Low", "Medium", "Low", "Medium"), 
                levels=c("Low", "Medium", "High"))
income > "Low"
income1 = factor( ordered( c("High", "High", "Low", "Low", 
                            "Low", "Medium", "Low", "Medium") ), 
                levels=c("Low", "Medium", "High"))
income1 > "Low"

sort( income )   ### Be carefule with sort; it only sorts a vector

#### p. 24 Lists
tmp.list <- list("Lex Comber",c(2015, 2018), 
                 "Lecturer", matrix(c(6,3,1,2), c(2,2)))
tmp.list
tmp.list[[4]]
tmp.list[[1]]
str( tmp.list )
tmp.list1 = append( tmp.list , list( c(7,6,9,1) ) )
tmp.list1
lapply( tmp.list[[2]] , is.numeric )
lngth = lapply( tmp.list , length )
str( lngth )

#### Functions (not in book)

f = function(x)
{
  z = x^2
  return(z)
}

f(3)
f( c(1,2,3,4) )

g = function( x , y )
{
  z = sqrt( x^2 + y^2 )
  return( z )
}
g(2,3)

x0 = 1
y0 = 1
distance = function( x , y )
{
  z = sqrt( (x-x0)^2 + (y-y0)^2 )
  return( z )
}
distance( 2 , 3 )

#### p. 25 Defining Your Own Classes
employee = list( name="Lex Comber" , start.year=2015 , position="Professor" )
print( employee )
class( employee ) = "staff"

print.staff = function(x)
{
  cat( "Name: " , x$name , "\n" )
  cat( "Start Year:" , x$start.year , "\n" )
  cat( "Job Title:" , x$position , "\n" )
}
print( employee )
print( unclass(employee) )

new.staff = function( name , year , post )
{
  result = list( name=name , start.year=year , position=post )
  class( result ) = "staff"
  return( result )
}


leeds.uni = vector( mode="list" , 3 )
leeds.uni[[1]] = new.staff( "Heppenstall, Allison" , 2017 , "Professor" )
leeds.uni[[2]] = new.staff( "Comber, Lex" , 2016 , "Professor" )
leeds.uni[[3]] = new.staff( "Langlands, Alan" , 2014 , "VC" )

leeds.uni

#### p. 25 data.frame vs. tibble
df = data.frame( dist=seq(0,400,100) , 
                 city=c("Leeds","Nottingham","Leicester","Durham","Newcastle") )
str( df )
df
df$city    #### Note result is factor, not string

df1 = data.frame( dist=seq(0,400,100) , 
                  city=c("Leeds","Nottingham","Leicester","Durham","Newcastle") ,
                  stringsAsFactors = FALSE )

str( df1 )

install.packages( "dplyr" , depend=TRUE )
library( dplyr )   #### Don't forget this.  
tb <- tibble(dist = seq(0,400, 100),  
             city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"))
tb
str( tb )

df$city
df$cit             #### wierd "feature"
df$c

df[,2]
tb[,2]
str(df[,2])
str(tb[,2])

str(df[,1])
str(tb[,1])

names( df )
names( tb )

colnames( df )
colnames( tb )
rownames( df )
rownames( tb )
length( df )
length( tb )
ncol( df )
ncol( tb )
nrow( df )
nrow( tb )

population = c(700,250,230,150,1200)
cbind( df , Pop=population )
cbind( tb , Pop=population )

####
####  Section 2.4 Plots
####
#### p. 35
?rnorm

set.seed( 123 )
x1 = rnorm(100,0,1)
x2 = rnorm(100,0,1)
plot( x1 , x2 )
plot( x1 , x2 , pch=19 , col="red" )
plot( x1 , x2 , pch=12 , col="darkgreen" , cex=3 )

x2 = seq( from=0 , to=2*pi , length.out=100 )    #### len works too
y2 = sin( x2 )
plot( x2 , y2 )
plot( x2 , y2 , type="l" , col="aquamarine4" , lwd=20 )
#### See http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#### for available colors

y2r = y2 + rnorm( 100 , 0 , 0.1 )
plot( x2 , y2 , type="l" , col="aquamarine4" , lwd=3 )
  points( x2 , y2r , col="chocolate1" )

?par
par( mfrow=c(1,2) )
plot( x2 , y2 , type="l" , lwd=3 , col="aquamarine4")
plot( x2 , y2 , type="h" , col="darkgreen" , ylim=c(-1.5,1.5) )

windows( 12 , 8 )
par( mfrow=c(1,2) )
plot( x2 , y2 , type="l" , lwd=3 , col="aquamarine4")
plot( x2 , y2 , type="h" , col="darkgreen" , ylim=c(-1.5,1.5) )
par( mfrow=c(1,1) )   #### resets to a single plot
dev.off()             #### closes graphics window

x2 = seq( 0 , 2*pi , len=100 )
y2 = sin( x2 )
y4 = cos( x2 )

windows( 12 , 8 )
par( mfrow=c(1,2) )
plot( y2 , y4 , type="l")
plot( y2 , y4 , asp=1 )
par( mfrow=c(1,1) )   
dev.off()             

windows( 12 , 8 )
par( mfrow=c(1,2) )
plot( y2 , y4 , type="l")
  polygon( y2 , y4 , col="green" )
plot( y2 , y4 , asp=1 , pch=1 , type="n" )
  polygon( y2 , y4 , col="chocolate3")
par( mfrow=c(1,1) )   
dev.off()         

install.packages( "GISTools" , depend=TRUE )   #### Only do this once
library( GISTools )

data( georgia )
georgia$Longitud
plot( georgia$Longitud , georgia$Latitude , pch=19 , col="red" , asp=1 )

georgia2
str( georgia2[[13]] )    #### Trial and error to see where county names are

georgia.polys
appling = georgia.polys[[1]]
plot( appling , asp=1 , type="n" , xlab="Easting", ylab="Northing")
  polygon(appling, density=14, angle=135) 

#### pp. 40-41
colors()
colours()
windows( 9 , 7 )
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
  points( x=runif(500,126,132)*10000 , y=runif(500,103,108)*10000 , pch=16 , col="red" )  
  polygon(appling, col=rgb(0,0.5,0.7,0.7))  #### 4th arg is transparency
  
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
  polygon( appling , col="#B3B333" ) 
  text( 1287000,1053000 , "Appling County" , cex=1.5 ) 
  text( 1287000,1049000 , "Georgia" , col='darkred' )
  
locator()  #### Then click on plot and press Esc


## skip...
plot( c(-1.5,1.5) , c(-1.5,1.5) , asp=1 , type='n' ) 
  rect( -0.5 , -0.5 , 0.5 , 0.5 , border=NA , col=rgb(0,0.5,0.5,0.7) ) 
  rect( 0 , 0 , 1 , 1 , col=rgb(1,0.5,0.5,0.7) )
  
#### pp. 42-43
##  Meuse River Data Set
##  This data set gives locations and topsoil heavy metal concentrations, 
##  along with a number of soil and landscape variables at the observation 
##  locations, collected in a flood plain of the river Meuse, near the village 
##  of Stein (NL). Heavy metal concentrations are from composite samples of 
##  an area of approximately 15 m x 15 m.
##
##  "dist" is   distance to the Meuse; obtained from the nearest cell 
##  in meuse.grid, which in turn was derived by a spread (spatial distance) 
##  GIS operation, horizontal precision 20 metres; then normalized to [0,1]
  
data( meuse.grid )
# define a SpatialPixelsDataFrame from the data
mat = SpatialPixelsDataFrame( points = meuse.grid[c("x", "y")] , 
                              data = meuse.grid )

str( mat[[5]] )
str( mat$dist )
plot (meuse.grid$x , meuse.grid$y , asp=1 )

windows( 9 , 9 )
image( mat , "dist" )

# set some plot parameters (1 row, 2 columns)
par( mfrow = c(1,2) )
# set the plot margins
par( mar = c(0,0,0,0) )
# plot the points using the default shading


# load the package
library(RColorBrewer)
# select and examine a colour palette with 7 classes
greenpal <- brewer.pal(7,'Greens') 
# and now use this to plot the data
image( mat , "dist" )
image( mat , "dist" , col=greenpal )
  
####
####  Section 2.5 ggplot
####
#### pp. 43-44

## install.packages( "tidyverse" , dep=TRUE )  ## only if necessary
## install.packages( "ggplot2" , dep=TRUE )    ## only if necessary
library( tidyverse )
library( ggplot2 )

## ... from much earlier

x2 = seq( from=0 , to=2*pi , length.out=100 )    #### len works too
y2 = sin( x2 )
y2r = y2 + rnorm( 100 , 0 , 0.1 )

qplot( x2 , y2r , col=I("darkred") , ylim=c(-1.2,1.2) ) +
  geom_line( aes(x2,y2) , col=I("darkgreen") , size=I(1.5) ) +
  theme( axis.text = element_text(size=20) ,
         axis.title = element_text(size=20,face="bold") )

qplot( x2 , y2r , col="darkred" , ylim=c(-1.2,1.2) ) +     #### Remove I()
  geom_line( aes(x2,y2) , col=I("darkgreen") , size=I(1.5) ) +
  theme( axis.text = element_text(size=20) ,
         axis.title = element_text(size=20,face="bold") )

appling = data.frame(appling)
colnames(appling) = c("X","Y")

plot( appling )
plot( appling , type="l" )
plot( appling , type="l" , asp=1 )

# create the first plot with qplot
p1  =  qplot(X, Y, data = appling, geom = "polygon", asp = 1, 
             colour = I("black"),
             fill=I(rgb(0,0.5,0.7,0.4))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20))

# create a data.frame to hold the points 
df  =  data.frame(x = runif(500,126,132)*10000, 
                  y = runif(500,103,108)*10000)
# now use ggplot to contruct the layers of the plot
p2  =  ggplot(appling, aes(x = X, y= Y)) +
  geom_polygon(fill = I(rgb(0,0.5,0.7,0.4))) +
  geom_point(data = df, aes(x, y),col=I('red')) +
  coord_fixed() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20))
# finally combine these in a single plot 
# using the grid.arrange function
# NB you may have to install the gridExtra package
install.packages( "gridExtra" , dep=TRUE )
library(gridExtra)
windows( 12 , 7 )
grid.arrange(p1, p2, ncol = 2)

df = data.frame( georgia )
tb = as.tibble( df )

tb$rural = as.factor( (tb$PctRural > 50) )
tb$rural = as.factor( (tb$PctRural > 50) + 0 )  ## Note the effect of "+0"
levels( tb$rural ) = list( "Non-Rural" = 0 , "Rural" = 1 )

tb$IncClass = rep("Average" , nrow(tb) )
tb$IncClass[ tb$MedInc >= 41204 ] = "Rich"
tb$IncClass[ tb$MedInc <= 29773 ] = "Poor" 

table( tb$IncClass )

ggplot( data=tb , mapping=aes( x=PctBach , y = PctEld ) ) +
  geom_point()

ggplot( data=tb , mapping=aes( x=PctBach , y=PctEld , color=rural) ) +
  geom_point()

ggplot( data=tb , mapping=aes( x=PctBach , y=PctEld ) ) +
  geom_point() +
  geom_smooth( method="lm")

ggplot( data=tb , mapping=aes( x=PctBach , y=PctEld , color=rural) ) +
  geom_point() +
  geom_smooth( method="lm")
  
ggplot( data=tb , mapping=aes( x=PctBach , y=PctEld ) ) +
  geom_point() +
  geom_smooth( method="lm" , col="red" , fill="lightsalmon" ) +
  theme_bw() +
  xlab( "% of population with bachelor degree" ) +
  ylab( "% of population who are elderly" )

ggplot( tb , aes( x=MedInc) ) +
  geom_histogram( , binwidth=5000 , color="red" , fill="gray" ) 

ggplot( tb , aes( x=MedInc) ) +
  geom_histogram( , binwidth=5000 , color="red" , fill="gray" ) +
  geom_density( alpha=0.4 , fill="darksalmon" ) +
  geom_vline( aes( xintercept=median( MedInc , na.rm=TRUE )) ,
              color="orangered1" , linetype="dashed" , size=1 )
  
ggplot(tb, aes( x=MedInc )) + 
  geom_histogram( aes(y=..density..) , binwidth=5000 , colour="white")  +
  geom_density( alpha=.4 , fill="darksalmon" ) +
  geom_vline( aes( xintercept=median(MedInc, na.rm=T) ),
              color="orangered1" , linetype="dashed" , size=1)

which.max( tb$MedInc )
tb$Name[58]

ggplot(tb, aes(x=PctBach, fill=IncClass)) +
  geom_histogram(color="grey30", binwidth = 1) +
  scale_fill_manual("Income Class", 
                    values = c("orange", "palegoldenrod","firebrick3")) +
  facet_grid(IncClass~.) +
  xlab("% Bachelor degrees") +
  ggtitle("Bachelors degree % in different income classes")

ggplot(tb, aes(x = "",PctBach)) +   ## Note error in bookdown file
  geom_boxplot() 

ggplot(tb, aes(IncClass, PctBach, fill = factor(rural))) + 
  geom_boxplot() +
  scale_fill_manual(name = "Rural",
                    values = c("orange", "firebrick3"),
                    labels = c("Non-Rural"="Not Rural","Rural"="Rural")) +
  xlab("Income Class") +
  ylab("% Bachelors")

####
####  Section 2.6 Reading and Writing Data
####
#### p. 50

str( appling )
dim( appling )
head( appling , 20 )

## colnames( appling ) = c("X","Y")   ## This was done earlier
getwd()
write.csv( appling , file="test.csv" )
write.csv( appling , file="test1.csv" , row.names=FALSE )

tmp.appling = read.csv( "test1.csv" )

save( list=ls() , file="MyData.RData" )
save( list="appling" , file="MyData1.RData" )
save( list=c("appling","georgia.polys") , file="MyData2.RData" )

load( "MyData.RData" )  ## Be sure the correct working director is selected

#### 
#### p. 52

install.packages( "rgdal" , dep=TRUE )
library( rgdal )
writeOGR( obj=georgia , dsn="." , layer="georgia" , driver="ESRI Shapefile" ,
          overwrite_layer=T )

new.georgia = readOGR( "georgia.shp" )

install.packages( "sf" , dep=TRUE )
library( sf )

g2 = st_read( "georgia.shp" )
str( g2 )
