## workoutSummary.R
#

source('config.R')
source('initMongo.R')
source('summarizeWorkout.R')



args <- commandArgs(trailingOnly = TRUE)
# print( args )
workout.id.string<- args[1]

mongo<- initMongo( dbHost, dbName )

summarizeWorkout( mongo, mongo.oid.from.string( workout.id.string ), details.print=TRUE )

print( 'NB:  no changes have been made to orphans table' )
