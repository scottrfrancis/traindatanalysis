require('rmongodb')
source('getMaxSustainedMeasure.R')

summarizeWorkoutList<- function( mongo, workouts.ids )
{ 
  for ( w in workouts.ids )
  {
    # w<- workouts.ids[[1]]
    
    buf<- mongo.bson.buffer.create(); mongo.bson.buffer.append( buf, "workout_id", w ); q.bson<- mongo.bson.from.buffer( buf )
    workout.samples<- mongo.find.all( mongo, 'quantathlete.samples', q.bson )
    
    if ( length( workout.samples ) <= 0 )
    {
      print( "no workouts for user")
      return()
    }
    print( "processing samples")
        
    names.use<- names( workout.samples[[1]] )
    names.use=names.use[(names.use!='latlng')] 
    names.use<- c( 'latitude', 'longitude', names.use )
    
    i<-1
    for( s in workout.samples )
    {
      cat( paste0(i, "."))
      #  s<- workout.samples[[1]]
      #
      # build data frame from source sample
      #
      
      # idea is blank a dataframe and fill it in...
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
      s[['heartrate']][which(s[['heartrate']]==0)]<- NA
      s[['power']][which(s[['power']]==0)]<- NA
      
      sample.df[,names.use]<- s[names.use]  # copy by names in use    
      
      time.windows<- c( 6, 12, 3*60, 6*60, 20*60, 60*60 )
      heartrate.windows<- sapply( time.windows, function(t) getMaxSustainedMeasure( sample.df$time, sample.df$heartrate, t ) )
      
      if ( !all( is.na( heartrate.windows ) ) )
      {
        #  db.workoutSummaries.update( { 'workout_id': ObjectId( '547a47839cb06a51a04196c1' ) }, {$set: { 'timeWindows': [6,12,180,360,1200,3600], 'heartrateWindows': [177, 176, 169, 168, 165, 146] }},{'upsert':1} )
        buf<- mongo.bson.buffer.create()
        mongo.bson.buffer.append( buf, "workout_id", mongo.oid.from.string(s$workout_id) )
        criteria.bson<- mongo.bson.from.buffer( buf )
        
        mongo.update( mongo, 'quantathlete.workoutSummaries', 
                      criteria.bson,                  
                      mongo.bson.from.list( list( "workout_id"=mongo.oid.from.string(s$workout_id), "timeWindows"=time.windows,  "heartrateWindows"=heartrate.windows ) ), 
                      mongo.update.upsert )
      }
      i<- i+1
    }
  }  
}
