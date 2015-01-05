source( 'config.R' )

source( 'dbUtils.R' )
source('summarizeWorkoutList.R')

mongo<- initDB( host=dbHost, db=dbName )


while( TRUE )
{
  workout.bson<- mongo.find.one( mongo, dbCollection(orphans.collection), fields=mongo.bson.from.list( c(list('workout_id'=1L))) ) 
  if ( is.null( workout.bson ) )
  {
    break
  }
  workout.oid<- mongo.bson.to.list( workout.bson )$workout_id

  # remove from list right away so no oneelse gets it.  if we fail, the bath job will re-enter it...
  buf<- mongo.bson.buffer.create(); mongo.bson.buffer.append( buf, "workout_id", workout.oid ); q.bson<- mongo.bson.from.buffer( buf )
  mongo.remove( mongo, dbCollection(orphans.collection), q.bson )
  

  summarizeWorkout( mongo, workout.oid )
}

