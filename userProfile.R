## userProfile.R
# 
#   utilities for working with user data and mongo
#
source('dbUtils.R')


userForWorkout<- function( mongo, workout.oid )
{
  q.bson<- queryForId( workout.oid )
  
  user<- mongo.find.all( mongo, 'quantathlete.workouts', q.bson, fields=c(list( 'user_id'=1 )) )
  
  user.oid<- mongo.oid.from.string( user[[1]]$user_id )
}

dateOfWorkout<- function( mongo, workout.oid )
{
  q.bson<- queryForId( workout.oid )
  
  date<- mongo.find.all( mongo, 'quantathlete.workouts', q.bson, fields=c(list( 'start_date'=1 )) )
  
  date.workout<- date[[1]]$start_date
}

ftpForUser<- function( mongo, user.oid, actType, date )
{
  buf<- mongo.bson.buffer.create()

  mongo.bson.buffer.append( buf, "user_id", user.oid )
  mongo.bson.buffer.append( buf, "type", actType )
  
  mongo.bson.buffer.start.object( buf, "endDate" )
  mongo.bson.buffer.append( buf, "$lte", date )
  mongo.bson.buffer.finish.object( buf )

  q.bson<- mongo.bson.from.buffer( buf )
  
  thresholds.curs<- mongo.find( mongo, dbCollection( thresholds.collection ), q.bson, sort='{"endDate":-1}', limit=1 )
  mongo.cursor.next( thresholds.curs )
  thresholds<- mongo.bson.to.list( mongo.cursor.value( thresholds.curs ) )
  
  idx<- which( thresholds$timeWindows == 20*60 )
  if ( actType == 'Ride' ) {
    ftp<- thresholds$wattsWindows[idx]
  } else {
    ftp<- thresholds$paceWindows[idx]
  }
  
  ftp
}

intensityFactor<- function( watts.normalized, ftp )
{
 intensity<- watts.normalized/ftp 
}

powerTSS<- function( time_S, watts.normalized, intensity.factor, ftp )
{
  tss<- (time_S*watts.normalized*intensity.factor)/(ftp*3600)*100
}
  