require('rmongodb')
source('getMaxSustainedMeasure.R')

summarizeWorkout<- function( mongo, workout.oid )
{
  buf<- mongo.bson.buffer.create(); mongo.bson.buffer.append( buf, "workout_id", workout.oid ); q.bson<- mongo.bson.from.buffer( buf )
  workout.samples<- mongo.find.all( mongo, 'quantathlete.samples', q.bson )
  
  cat( mongo.oid.to.string( workout.oid ) )
  if ( length( workout.samples ) <= 0 )
  {
    print( " - no workouts for user")
    return()
  }
  print( " - processing samples")
  
  names.use<- names( workout.samples[[1]] )
  names.use=names.use[(names.use!='latlng')] 
  names.use<- c( 'latitude', 'longitude', names.use )
  
  s<- workout.samples[[1]]
  
  #
  # build data frame from source sample
  #
  sample.df<- data.frame( matrix( NA, nrow= length( s$time), ncol = length( names.use )))
  names(sample.df)<- names.use
  
  # fix latlng
  if ( !is.null( s[['latlng']]) )
  {
    latlng.matrix<- sapply( s[['latlng']], cbind )
    s[['latitude']]<- latlng.matrix[1,]
    s[['longitude']]<- latlng.matrix[2,]
  }
  
  # suppress silly 0's
  s[['heartrate']][which( s[['heartrate']] == 'NULL' )] <- NA
  s[['heartrate']][which(s[['heartrate']]==0)]<- NA
  s[['watts']][which( s[['watts']] == 'NULL' )] <- NA
  s[['watts']][which(s[['watts']]==0)]<- NA
  
  sample.df[,names.use]<- s[names.use]  # copy by names in use    
  
  time.windows<- c( 6, 12, 3*60, 6*60, 20*60, 60*60 )
  
  # compute pace in m/s
  if ( !is.null( s[['distance']] ) )
  {
    time0<- c( 0, s[['time']][1:(length(s[['time']]) - 1)])
    dur<- s[['time']] - time0
    
    dist0<- c( 0, s[['distance']][1:length(s[['distance']]) - 1])
    length<- s[['distance']] - dist0
    
    speed.mpS<- length/dur
    
    #        cat( paste0( "ready to call max with ", length(sample.df$time), " and ", length( speed.mpS ) ) )
    speed.windows<- sapply( time.windows, function(t) getMaxSustainedMeasure( sample.df$time, speed.mpS, t ) )
  }
  
  heartrate.windows<- sapply( time.windows, function(t) getMaxSustainedMeasure( sample.df$time, unlist(sample.df$heartrate), t ) )
  watts.windows<- sapply( time.windows, function(t) getMaxSustainedMeasure( sample.df$time, unlist(sample.df$watts), t) )      
  
  ## TODO: check if adding NA summaries causes downstream problems
 # if ( !all( is.na( heartrate.windows ) ) )
  {
    ## TODO:  This won't get executed for VERY short workouts...  need to catch those and delete from worklist...
    
    #  db.workoutSummaries.update( { 'workout_id': ObjectId( '547a47839cb06a51a04196c1' ) }, {$set: { 'timeWindows': [6,12,180,360,1200,3600], 'heartrateWindows': [177, 176, 169, 168, 165, 146] }},{'upsert':1} )
    buf<- mongo.bson.buffer.create()
    mongo.bson.buffer.append( buf, "workout_id", mongo.oid.from.string(s$workout_id) )
    criteria.bson<- mongo.bson.from.buffer( buf )
    
    doc.list<- list( "workout_id"=mongo.oid.from.string(s$workout_id), 
                     "timeWindows"=time.windows,  
                     "heartrateWindows"=heartrate.windows,
                     "wattsWindows"=watts.windows )
    if ( !is.null( s[['distance']]) )
    {
      doc.list$paceWindows<- speed.windows
    }
    
    mongo.update( mongo, 'quantathlete.workoutSummaries', 
                  criteria.bson,                  
                  mongo.bson.from.list( doc.list ), 
                  mongo.update.upsert )
  }
}

summarizeWorkoutList<- function( mongo, workouts.ids )
{ 
  i<-1
  for ( w in workouts.ids )
  {
    # w<- workouts.ids[[1]]
    cat( paste0(i, ".", mongo.oid.to.string(w)) )
    
    summarizeWorkout( mongo, w )
    
  
    # remove this id from the worklist
    
    mongo.remove( mongo, 'quantathlete.workoutSummariesOrphans', q.bson )
  
    i<- i+1
  } 
}
