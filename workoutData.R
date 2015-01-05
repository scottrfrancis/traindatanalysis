## workoutData.R
#
# fetching/updatin workouts and associated data form the DB
#


# loads the workout from the database along with all sample data
# returns a list with the workout and a dataframe with the samples in the list 
# element 'datastreams'
loadWorkoutData<- function( mongo, workout.oid )
{
  # workout.oid<- mongo.oid.from.string( "548b63bf47514c62e3320993")
  
  # load the workout list
  q.bson<- queryForId( workout.oid )
  workout.data<- mongo.bson.to.list( mongo.find.one( mongo, dbCollection(workouts.collection), q.bson ) )
  
  # load the samples for the workout
  q.bson<- queryForId( workout.oid, "workout_id" )
  workout.samples<- mongo.find.all( mongo, dbCollection(samples.collection), q.bson )
  
#  cat( mongo.oid.to.string( workout.oid ) )
  if ( length( workout.samples ) <= 0 )
  {
#    print( " - no workouts for user")
    return( workout.data )
  }
#  print( " - processing samples")
  
  names.use<- names( workout.samples[[1]] )
  names.use=names.use[(names.use!='latlng')] 
  names.use<- c( 'latitude', 'longitude', names.use )
    
  #
  # build data frame from source sample
  #
  sample.df<- data.frame( matrix( NA, nrow= length( workout.samples[[1]]$time), ncol = length( names.use )))
  names(sample.df)<- names.use
  
  # bust latlng apart
  if ( !is.null( workout.samples[[1]][['latlng']]) )
  {
    latlng.matrix<- sapply( workout.samples[[1]][['latlng']], cbind )
    workout.samples[[1]][['latitude']]<- latlng.matrix[1,]
    workout.samples[[1]][['longitude']]<- latlng.matrix[2,]
  }
  
  sample.df[,names.use]<- workout.samples[[1]][names.use]  # copy by names in use  

  # remap nulls and 0's to NAs and make sure all cols are vectors not lists
  workout.data$samples<- cleanColumnNulls( sample.df )

  workout.data$ftp<- ftpForUser(mongo, workout.data$user_id, workout.data$type, workout.data$start_date )

  workout.data
}

nullListToVector<- function( l, zeros.to.na=TRUE )
{
#   if (class( l ) != 'list')
#   {
#     return( l )
#   }
  
  l[which(l == 'NULL')]<- NA
  if (zeros.to.na)
  {
    l[which(l == 0)]<- NA  
  }
  
  unlist( l )
}

cleanColumnNulls<- function( metrics.df )
{
  colnames<- names( metrics.df )
  lapply( colnames, function(n) metrics.df[n]<<- nullListToVector( (metrics.df[n])[,1], (n != 'time') ) )
  
  metrics.df
}

# updates the workout's data streams in the db to add the stream with stream.name
# assumes timebase is the same
# adds stream as a new columen to the dataframe and returns it
addStream<- function( mongo, workout.data, stream, stream.name )
{
  q.bson<- queryForId( workout.data$'_id', 'workout_id' )
  
  buf<- mongo.bson.buffer.create()
  mongo.bson.buffer.start.object( buf, "$set" )
  mongo.bson.buffer.append( buf, stream.name, stream )
  mongo.bson.buffer.finish.object( buf )
  update.bson<- mongo.bson.from.buffer( buf )
  
  mongo.update( mongo, dbCollection(samples.collection), q.bson, update.bson )
  
  workout.data$samples[stream.name]<- stream
  
  workout.data
}

addFields<- function( mongo, workout.data, fields.list, collection= workouts.collection )
{
  q.bson<- queryForId( workout.data$'_id' )
  
  buf<- mongo.bson.buffer.create()
  mongo.bson.buffer.append.list( buf, "$set", fields.list )
  update.bson<- mongo.bson.from.buffer( buf )
  
  mongo.update( mongo, dbCollection(collection), q.bson, update.bson )
  
  append( workout.data, fields.list )
}

