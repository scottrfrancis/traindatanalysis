source( 'config.R' )

source( 'dbUtils.R' )
source('summarizeWorkout.R')

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
  q.bson<- queryForId( workout.oid, "workout_id" )
  mongo.remove( mongo, dbCollection(orphans.collection), q.bson )
  
  cat( 'summarizing workout ', mongo.oid.to.string(workout.oid), "\n" )
  summarizeWorkout( mongo, workout.oid )
}

