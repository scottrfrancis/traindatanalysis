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


estimateFTP<- function( mongo, user.oid, type, date = Sys.Date() )
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
                    fields=mongo.bson.from.list( c(list('endDate'=1L,'timeWindows'=1,'paceWindows'=1))) )
    
  datePaces<- sapply( thresholds, function(t) rbind( t$endDate, t$paceWindows[ match( 20*60, t$timeWindows ) ] ) )
  ftp.df<- data.frame( t(datePaces) )
  names( ftp.df )<- c( 'date', 'ftp' )
  ftp.df$ftp[which( ftp.df$ftp == 'NULL' )] <- NA
  
  # run lm against it
  dates.use<- as.numeric( as.Date(unlist( ftp.df$date[!is.na( ftp.df$ftp ) ] )))
  ftp.use<- unlist( ftp.df$ftp[!is.na( ftp.df$ftp ) ] )
  model.df<- as.data.frame( cbind( dates.use, ftp.use ) )
  
  model<- lm( ftp.use ~ dates.use )
  coeffs<- coefficients( model )

  # predict FTP for date
  ftp<- coeffs[1] + coeffs[2]*as.numeric( date )
}


users.list<- mongo.find.all( mongo, 'quantathlete.users', fields=mongo.bson.from.list( c( list( '_id'=1L))))
users.ids<- lapply( users.list, function(u) mongo.oid.from.string(u$'_id' ) )

# user.oid<- users.ids[[1]]
# type='Ride'

