# 06.10 Question 1 separate model fit
tic()

baseYear = 1990
data = database
inputGroupsN = length(inputGroups)

I am here again!
  fast commit

loopRes = foreach( groupN = 1:inputGroupsN ) %do% {
require(plyr)
  dGroup = match_df( data, inputGroups[[groupN]]$class, on = c( "Event", "TaxonomicName" ) )
  un = unique( dGroup[,c( "Event", "TaxonomicName" )] )
  un$Index = c( 1:nrow(un) )
  dGroup = merge( dGroup, un )
  un = unique( dGroup[,c( "Index", "Locality" )] )
  un$Ind = c( 1:nrow(un) )
  dGroup = merge( dGroup, un )
  dGroup$Res = NA
  dGroup$Year = dGroup$Year - baseYear
  un$Day = NA
  un$Slope = NA
  un$NYears = NA
  for( i in 1:nrow(un) ){
    ind = which(dGroup$Ind == i)
    tData = dGroup[ind,]
    fm1 = lm( Day ~ Year, data = tData )
    dGroup$Res[ind] = resid(fm1)
    un[i, c("Day", "Slope")] = coef(fm1)
    un$NYears[i] = length( unique(tData$Year) )
  }
  c( list(un), list(dGroup) )
}

toc()

hi!



