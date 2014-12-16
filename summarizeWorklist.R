# require('rjson')
source( 'initMongo.R' )
library(rmongodb)
source('summarizeWorkoutList.R')

mongo<- initMongo( host='quantathlete.com', db='quantathlete')
#mongo<- initMongo( host='localhost', db='quantathlete')


# read IDs for workouts that need summaries
# list.filename<- 'worklist.json'

# args <- commandArgs(trailingOnly = TRUE)
# list.filename<- args[0]

# list.json<- fromJSON( file= list.filename)

workouts.list<- mongo.find.all( mongo, 'quantathlete.workoutSummariesOrphans', fields=mongo.bson.from.list( c(list('workout_id'=1L))) )
workouts.ids<- lapply( workouts.list, function(w) mongo.oid.from.string(w$'workout_id' ) )

summarizeWorkoutList( mongo, workouts.ids )
