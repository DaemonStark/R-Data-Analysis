########################################################################
# This script assumes that a .XDF file "RUM" containg the data from
# the movielens million row data set (www.movielens.org) exists in the
# local directory.
#
# The beginnings of a recommendation system might start with a
# naive attempt to find the best movie
# JBR
# April 15, 2012
########################################################################
cube.3 <-as.data.frame(rxCube(Rating ~ fTitle,data="RUM"))
head(cube.3)
cube.3 <- cube.3[order(-cube.3$Rating),]
head(cube.3[which(cube.3$Counts>50),])

# Now control for user
form <- formula(Rating ~ UserID + fTitle)
system.time(mod.1 <- rxLinMod(form,data="RUM",cube=TRUE))
#Computation time: 51.515 seconds.
   #user  system elapsed 
 #101.70    5.44   52.82 

length(mod.1$coefficients)
#[1] 9746
# Make a data frame of movies with coeffs and coef stds
movie.ratings <- data.frame(
	movieCoef = mod.1$coefficients[grepl(x=row.names(mod.1$coefficients),
					pattern="fTitle")],
	movieCoefSD = mod.1$coef.std.error[grepl(x=row.names(mod.1$coefficients),
					pattern="fTitle")],
	movieNames=grep(x=row.names(mod.1$coefficients),
					pattern="fTitle",value=TRUE))
#
# Sort by coefficient to find the top movie
top.ratings <- movie.ratings[order(-movie.ratings$movieCoef),]
head(top.ratings,n=30)

