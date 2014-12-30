## estimateFTP.R
#
# regress the 20 min thresholds for activity type and return an estimated FTP
#
source( 'config.R' )

source( 'initMongo.R' )
library(rmongodb)

mongo<- initMongo( host=dbHost, db=dbName )

getTheshold<- function( t, window_S )
{
  list( t$endDate, t$paceWindows[ match( window_S, t$timeWindows ) ] )
}

pushUserTimedDataStream<- function( mongo, collection='quantathlete.userDataStreams', user.oid, streamName, datum, timestamp = Sys.Date()  )
{
  if ( !is.na( datum ) )
  {
    # update the "FTP-dot" named data stream fro the user
    buf<- mongo.bson.buffer.create()
    mongo.bson.buffer.start.array( buf, "$and" )
    bufIdx<- 0
    
    mongo.bson.buffer.start.object( buf, "0" ); bufIdx<- bufIdx + 1
    mongo.bson.buffer.append( buf, "user_id", user.oid )
    mongo.bson.buffer.finish.object( buf ) # index 0
    
    mongo.bson.buffer.start.object( buf, as.character(bufIdx) ); bufIdx<- bufIdx +1
    mongo.bson.buffer.append( buf, "name", streamName )
    mongo.bson.buffer.finish.object( buf )  # index 1
    
    mongo.bson.buffer.finish.object( buf ) # $and array
    criteria.bson<- mongo.bson.from.buffer( buf )
    
    dataStream.bson<- mongo.find.one( mongo, collection,
                                      criteria.bson, fields=c(list( 'timeSeries_id'=1 )) )
    if ( is.null( dataStream.bson ) )
    {
      # create it
      ds.oid<- mongo.oid.create()
      buf<- mongo.bson.buffer.create()
      mongo.bson.buffer.append( buf, "_id", ds.oid )
      doc.bson<- mongo.bson.from.buffer( buf )

      mongo.insert( mongo, collection, doc.bson )
      
      buf<- mongo.bson.buffer.create()
      mongo.bson.buffer.start.object( buf, "$set" )
      mongo.bson.buffer.append( buf, 'user_id', user.oid )
      mongo.bson.buffer.finish.object( buf )
      update.bson<- mongo.bson.from.buffer( buf )
      
      mongo.update( mongo, collection, doc.bson, update.bson )
      
      buf<- mongo.bson.buffer.create()
      mongo.bson.buffer.start.object( buf, "$set" )
      mongo.bson.buffer.append( buf, 'name', streamName )
      mongo.bson.buffer.finish.object( buf )
      update.bson<- mongo.bson.from.buffer( buf )

      mongo.update( mongo, collection, doc.bson, update.bson )
    }
    
    # $push the new datum to the end of the data element 
    update.bson<- mongo.bson.from.JSON( paste0( '{"$push":{"data":', datum, '}}' ) )
    
    mongo.update( mongo, collection, criteria.bson, update.bson )
    
    # find (and create?) and update the time series
    dataStream<- mongo.bson.to.list( mongo.find.one( mongo, 'quantathlete.userDataStreams',
                                                     criteria.bson, fields=c(list( 'timeSeries_id'=1 )) ) )
    
    if ( is.null( dataStream$timeSeries_id ) )
    {
      # create a new timeSeries and link to it
      ts.oid<- mongo.oid.create()
      buf<- mongo.bson.buffer.create()
      mongo.bson.buffer.append( buf, "_id", ts.oid )
      newDoc.bson<- mongo.bson.from.buffer( buf )
      
      mongo.insert( mongo, 'quantathlete.timeSeries', newDoc.bson )
      
      # update the dataStream to use this timeseries
      buf<- mongo.bson.buffer.create()
      mongo.bson.buffer.start.object( buf, "$set" )
      mongo.bson.buffer.append( buf, "timeSeries_id", ts.oid )
      mongo.bson.buffer.finish.object( buf )
      update.bson<- mongo.bson.from.buffer( buf )
      
      mongo.update( mongo, collection, criteria.bson, update.bson )
      
      dataStream$timeSeries_id<- ts.oid
    }
    
    # timeSeries exists.. push the cur date to the end
    buf<- mongo.bson.buffer.create()
    mongo.bson.buffer.append( buf, "_id", dataStream$timeSeries_id )
    criteria.bson<- mongo.bson.from.buffer( buf )
    
    update.bson<- mongo.bson.from.JSON( paste0( '{"$push":{"timestamps":"', timestamp, '"}}' ) )
    
    mongo.update( mongo, 'quantathlete.timeSeries', criteria.bson, update.bson )
  }
}

estimateFTP<- function( mongo, user.oid, type, windowName, date = Sys.Date() )
{
  # build vector of 20 min thresholds for user and type from db
  # db.userThresholds.find({user_id: ObjectId("546115a3bc5f4d0676fb39bb"), type:"Run"}, {endDate:1, timeWindows:1, paceWindows:1})
  buf<- mongo.bson.buffer.create()
  mongo.bson.buffer.start.array( buf, "$and" )
  bufIdx<- 0
  
  mongo.bson.buffer.start.object( buf, "0" ); bufIdx<- bufIdx + 1
  mongo.bson.buffer.append( buf, "user_id", user.oid )
  mongo.bson.buffer.finish.object( buf ) # index 0
  
  mongo.bson.buffer.start.object( buf, as.character(bufIdx) ); bufIdx<- bufIdx +1
  mongo.bson.buffer.append( buf, "type", type )
  mongo.bson.buffer.finish.object( buf )  # index 1
  
  mongo.bson.buffer.finish.object( buf ) # $and array
  q.bson<- mongo.bson.from.buffer( buf )
  
  thresholds<- mongo.find.all( mongo, 'quantathlete.userThresholds', q.bson, 
                    fields=mongo.bson.from.list( c(list('endDate'=1L,'timeWindows'=1,'heartrateWindows'=1, 'paceWindows'=1,'wattsWindows'=1))) )
    
  ftp.df<- data.frame( matrix( NA, nrow= length(thresholds), ncol =2 )) 
  names( ftp.df)<- c( 'date', 'ftp' )
  
#   windowName = 'paceWindows'
#   if ( type == "Ride")
#     windowName = 'wattsWindows'
  
  dp.df<- as.data.frame( t( sapply( thresholds, function(t) rbind( t$endDate, t[[windowName]][match( 20*60, t$timeWindows )] ) ) ) )
  
  ftp.df$date<- as.Date(unlist( dp.df[,1]))
  ftp.df$ftp[which(dp.df[,2] != 'NULL')]<- as.numeric(unlist( dp.df[,2]))
  
  ftp<- NA
  if ( sum( complete.cases( ftp.df ) ) > 0 )
  {
    # run lm against it
    model<- lm( ftp ~ date, data=ftp.df )
    coeffs<- coefficients( model )
  
    # predict FTP for date
    ftp<- coeffs[1] + coeffs[2]*as.numeric( date )
    names(ftp)<- NULL
  }
  
  ftp
}


users.list<- mongo.find.all( mongo, 'quantathlete.users', fields=mongo.bson.from.list( c( list( '_id'=1L))))

users.ride.ftps<- lapply( users.list, function(u) estimateFTP( mongo, mongo.oid.from.string(u$'_id' ), "Ride", 'wattsWindows' ))
users.run.ftps<- lapply( users.list, function(u) estimateFTP( mongo, mongo.oid.from.string(u$'_id' ), "Run", 'paceWindows' ))

users.ride.hrs<- lapply( users.list, function(u) estimateFTP( mongo, mongo.oid.from.string(u$'_id' ), "Ride", 'heartrateWindows' ))
users.run.hrs<- lapply( users.list, function(u) estimateFTP( mongo, mongo.oid.from.string(u$'_id' ), "Run", 'heartrateWindows' ))



# update database
# db.users.update( {_id:ObjectId("546115a3bc5f4d0676fb39bb")}, {$push:{ foo:2}} )
for ( i in 1:length(users.list) )
{ 
  pushUserTimedDataStream(  mongo, 'quantathlete.userDataStreams', mongo.oid.from.string( users.list[[i]]$'_id' ), "Ride-FTP-dot", users.ride.ftps[[i]] )
  pushUserTimedDataStream(  mongo, 'quantathlete.userDataStreams', mongo.oid.from.string( users.list[[i]]$'_id' ), "Run-FTP-dot", users.run.ftps[[i]] )   

  pushUserTimedDataStream(  mongo, 'quantathlete.userDataStreams', mongo.oid.from.string( users.list[[i]]$'_id' ), "Ride-FTHR-dot", users.ride.hrs[[i]] )
  pushUserTimedDataStream(  mongo, 'quantathlete.userDataStreams', mongo.oid.from.string( users.list[[i]]$'_id' ), "Run-FTHR-dot", users.run.hrs[[i]] )   
}


# user.oid<- users.ids[[1]]
# type='Ride'

