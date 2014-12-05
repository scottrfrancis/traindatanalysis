source('getMaxSustainedMeasure.R')

summarizeActivitySamplesForUser<- function( user_id, activity_type, from_date=NULL, before_date=NULL )
{
  # find all that users workouts
  # workouts.all<- mongo.find.all( mongo, 'quantathlete.workouts', 
  #                   list(user_id=mongo.oid.from.string(uid)) )
  
  # query: {$and: [{user_id: ObjectId("5467eca89a40e7c8909db618")}, {type: "Run"}]}, {_id:1}  
  buf<- mongo.bson.buffer.create()
  mongo.bson.buffer.start.array( buf, "$and" )
  bufIdx<- 0
  
  mongo.bson.buffer.start.object( buf, "0" ); bufIdx<- bufIdx + 1
  mongo.bson.buffer.append( buf, "user_id", mongo.oid.from.string(user_id) )
  mongo.bson.buffer.finish.object( buf ) # index 0
  
  mongo.bson.buffer.start.object( buf, as.character(bufIdx) ); bufIdx<- bufIdx +1
  mongo.bson.buffer.append( buf, "type", activity_type )
  mongo.bson.buffer.finish.object( buf )  # index 1
  
  if( !is.null(from_date) )
  {
    mongo.bson.buffer.start.object( buf, as.character(bufIdx) ); bufIdx<- bufIdx + 1
    mongo.bson.buffer.start.object( buf, "start_date_local" )
    mongo.bson.buffer.append.string( buf, "$gte", from_date )
    mongo.bson.buffer.finish.object( buf )
    mongo.bson.buffer.finish.object( buf )
  }
  
  if ( !is.null(before_date) )
  {
    mongo.bson.buffer.start.object( buf, as.character(bufIdx) ); bufIdx<- bufIdx + 1
    mongo.bson.buffer.start.object( buf, "start_date_local" )
    mongo.bson.buffer.append.string( buf, "$lt", before_date )
    mongo.bson.buffer.finish.object( buf )
    mongo.bson.buffer.finish.object( buf )
  }
  
  mongo.bson.buffer.finish.object( buf ) # $and array
  q.bson<- mongo.bson.from.buffer( buf )
  
  #   db.workouts.find( {$and: [{'type':'Ride'},{'start_date_local': {$gte: '2014-11-01'}},{'start_date_local':{$lt: '2014-12-01'}}]}, {'name':1} )
#   q.json<- paste0('{"$and":[{"user_id":ObjectId("', mongo.oid.from.string(user_id), '")},',
#                       '{"type":"', activity_type, '"}]}' )
  
  workouts.activity<- mongo.find.all( mongo, 'quantathlete.workouts', q.bson, 
                                  fields=mongo.bson.from.list( c(list(`_id`=1L,'type'=1L,'start_date_local','name'))) )
  
  
  # find the samples for the workout
  workouts.ids<- lapply( workouts.activity, function(w) mongo.oid.from.string(w$`_id` ) )
  
  buf<- mongo.bson.buffer.create()
  mongo.bson.buffer.start.object( buf, "workout_id")
  mongo.bson.buffer.append(buf, "$in", workouts.ids )
  mongo.bson.buffer.finish.object( buf )
  q.bson<- mongo.bson.from.buffer(buf)
  
  workout.samples<- mongo.find.all( mongo, 'quantathlete.samples', q.bson )
  
  if ( length( workout.samples ) <= 0 )
  {
    print( "no workouts for user")
    return()
  }
  print( "processing samples")

  sample.names<- names( workout.samples[[1]] )
  
  #names.good<- sapply( sample.names, function(n) !is.null( unlist(s[n])))
  #names.use=sample.names[names.good]
  names.use=sample.names
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

