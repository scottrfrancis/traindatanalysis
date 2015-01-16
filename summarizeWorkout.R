require('rmongodb')
source('getMaxSustainedMeasure.R')
source('streamData.R')
source('timeSeries.R')
source('userProfile.R')
source('workoutData.R')


summarizeWorkout<- function( mongo, workout.oid, details.print=FALSE )
{
  # workout.oid<- mongo.oid.from.string( "548b63bf47514c62e3320993")
  workout.data<- loadWorkoutData( mongo, workout.oid ) 
  if (details.print)
    print( workout.data )
   
  streams.list<- computeStreams( workout.data )
  lapply( seq_along( streams.list ), function(i) { workout.data<<- addStream( mongo, workout.data, streams.list[[i]], names(streams.list)[[i]] ) } )

  fields.list<- computeSummaries( workout.data )
  if (details.print)
    print( fields.list )
  
  q.bson<- queryForId( workout.data$'_id', "workout_id" )  
  fields.list$workout_id <- workout.oid
  fields.list$timeWindows <- time.windows
  mongo.update( mongo, dbCollection(summaries.collection), 
                  q.bson,                  
                  mongo.bson.from.list( fields.list ), 
                  mongo.update.upsert )
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
