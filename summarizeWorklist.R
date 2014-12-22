source( 'config.R' )

source( 'initMongo.R' )
library(rmongodb)
source('summarizeWorkoutList.R')

mongo<- initMongo( host=dbHost, db=dbName )


#workouts.list<- mongo.find.all( mongo, 'quantathlete.workoutSummariesOrphans', fields=mongo.bson.from.list( c(list('workout_id'=1L))) )

orphans.collection<- paste( dbName, "workoutSummariesOrphans", sep="." )

while( true )
{
  workout.bson<- mongo.find.one( mongo, orphans.collection, fields=mongo.bson.from.list( c(list('workout_id'=1L))) ) 
  if ( is.null( workout.bson ) )
  {
    break
  }
  workout.oid<- mongo.bson.to.list( workout.bson )$workout_id

  # remove from list right away so no oneelse gets it.  if we fail, the bath job will re-enter it...
  buf<- mongo.bson.buffer.create(); mongo.bson.buffer.append( buf, "workout_id", workout.oid ); q.bson<- mongo.bson.from.buffer( buf )
  mongo.remove( mongo, 'quantathlete.workoutSummariesOrphans', q.bson )
  

  summarizeWorkout( mongo, workout.oid )
}


#workouts.list<- mongo.find.all( mongo, 'quantathlete.workouts', fields=mongo.bson.from.list( c( list( '_id'=1L))))
# workouts.ids<- lapply( workouts.list, function(w) mongo.oid.from.string(w$'_id' ) )
#workouts.ids<- lapply( workouts.list, function(w) mongo.oid.from.string(w$'workout_id' ) )

#summarizeWorkoutList( mongo, workouts.ids )
