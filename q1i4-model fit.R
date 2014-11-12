# 06.10 Question 1 model fit
tic()

hi! a

baseYear = 1990
data = database
inputGroupsN = length(inputGroups)
groupNames = rep(NA, inputGroupsN )
for( i in 1:inputGroupsN ){
  groupNames[i] = inputGroups[[i]]$name;
}

# N = nrow(database)
# remInd = sample( c(1:N), round(N/10) )
# data = data[-remInd, ]

infoTable = data.frame( Group=groupNames, Parks=NA, ParkSpecies=NA, ParkSpeciesSd=NA, ParkSpeciesYears=NA, ParkSpeciesYearsSd=NA )
for( groupN in 1:inputGroupsN ){
  dGroup = match_df( data, inputGroups[[groupN]]$class, on = c( "Event", "TaxonomicName" ) )
  p = table( factor( dGroup$Locality ) )
  infoTable$Parks[groupN] = length(p)
  ps = table( factor( dGroup$Locality ), factor( dGroup$TaxonomicName ) )
  ps = apply( ps>0, 1, sum )
  infoTable$ParkSpecies[groupN] = mean( ps )
  infoTable$ParkSpeciesSd[groupN] = sd( ps )
  un = unique( dGroup[,c( "Locality", "Event", "TaxonomicName" )] )
  un$Index = c( 1:nrow(un) )
  dGroup = merge( dGroup, un )
  psy = table( unique( dGroup[, c("Index", "Year")])$Index )
  infoTable$ParkSpeciesYears[groupN] = mean( psy )
  infoTable$ParkSpeciesYearsSd[groupN] = sd( psy )
}

cluster = makeCluster(8)
registerDoSNOW(cluster)
loopRes = foreach( groupN = 1:inputGroupsN ) %dopar% {
  require(plyr)
  require(lme4)
  dGroup = match_df( data, inputGroups[[groupN]]$class, on = c( "Event", "TaxonomicName" ) )
  un = unique( dGroup[,c( "Event", "TaxonomicName" )] )
  un$Index = factor(c( 1:nrow(un) ))
  dGroup = merge( dGroup, un )
  dGroup$SI = factor( dGroup$Locality:dGroup$Index )
  
  tData = dGroup
  tData$Year = tData$Year - baseYear
  nY = sd(tData$Year)
  tData$Year = tData$Year / nY
  fm1 = lmer( Day ~ Year + (1+Year|Locality) + (1+Year|Index) + (1+Year|SI), data = tData,
              control=lmerControl( optCtrl=list(maxfun=100000), optimizer = "bobyqa", check.nlev.gtr.1="ignore" ) )
  dGroup$Res = resid(fm1)
  c(list(fm1), list(dGroup), list(nY) )  
}
stopCluster(cluster)

models = list()
groups = list()
yearCoef = rep(NA, inputGroupsN)
for( groupN in 1:inputGroupsN ){
  models = c( models, list( loopRes[[groupN]][[1]] ) )
  groups = c( groups, list( loopRes[[groupN]][[2]] ) )
  yearCoef[groupN] = loopRes[[groupN]][[3]]
}

fEffects = data.frame( MDay=rep( NA, inputGroupsN ), Year=NA )
rEffects = list()
for( groupN in 1:inputGroupsN ){
  nY = yearCoef[groupN]
  fEffects[groupN, ] = fixef(models[[groupN]]) / c( 1, nY )
  ref = ranef(models[[groupN]])
  ref$Locality = t( t( ref$Locality ) / c( 1, nY ) )
  ref$Index = t( t( ref$Index ) / c( 1, nY ) )
  ref$SI = t( t( ref$SI ) / c( 1, nY ) )
  rEffects = c( rEffects, list(ref) )
} 

toc()

