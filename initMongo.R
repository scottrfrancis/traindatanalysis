require( 'plyr' )


library(rmongodb)

# your connection details will likely differ from these defaults
host <- "quantathlete.com:27017"
host<- "localhost:27017"
username <- ""
password <- ""
db <- "quantathlete"

#connect to mongo
#the create function has the following signature
#mongo.create(host="127.0.0.1", name="", username="", password="", db="admin", timeout=0L)

mongo <- mongo.create(host=host , db=db, username=username, password=password)


# build a query to find all Users


users<- mongo.find.all( mongo, 'quantathlete.users', fields=mongo.bson.from.list( c(list(`_id`=1L))) )
uid<- users[[1]]$`_id`

# find all that users workouts
# workouts.all<- mongo.find.all( mongo, 'quantathlete.workouts', 
#                   list(user_id=mongo.oid.from.string(uid)) )

# query: {$and: [{user_id: ObjectId("5467eca89a40e7c8909db618")}, {type: "Run"}]}, {_id:1}
buf<- mongo.bson.buffer.create()
  mongo.bson.buffer.start.array( buf, "$and" )
    mongo.bson.buffer.start.object( buf, "0" )
      mongo.bson.buffer.append( buf, "user_id", mongo.oid.from.string(uid) )
    mongo.bson.buffer.finish.object( buf ) # index 0

    mongo.bson.buffer.start.object( buf, "1" )
      mongo.bson.buffer.append( buf, "type", "Ride" )
    mongo.bson.buffer.finish.object( buf )  # index 1
  mongo.bson.buffer.finish.object( buf ) # $and array
q.bson<- mongo.bson.from.buffer( buf )
workouts.bike<- mongo.find.all( mongo, 'quantathlete.workouts', q.bson, fields=mongo.bson.from.list( c(list(`_id`=1L))) )


# find the samples for the workout
workouts.ids<- lapply( workouts.bike, function(w) mongo.oid.from.string(w$`_id` ) )

buf<- mongo.bson.buffer.create()
mongo.bson.buffer.start.object( buf, "workout_id")
mongo.bson.buffer.append(buf, "$in", workouts.ids )
mongo.bson.buffer.finish.object( buf )
q.bson<- mongo.bson.from.buffer(buf)

workout.samples<- mongo.find.all( mongo, 'quantathlete.samples', q.bson )
sample.names<- names( workout.samples[[1]] )

names.good<- sapply( sample.names, function(n) !is.null( unlist(s[n])))
names.use=sample.names[names.good]
names.use=names.use[(names.use!='latlng')] # & names.use!='altitude')]

samples<- data.frame()

### TODO:  loop over all workouts
s<- workout.samples[[1]]

sample.df<- data.frame( matrix( NA, nrow= length( s$time), ncol = length( sample.names )))
names(sample.df)<- sample.names

sample.df[,names.use]<- s[names.use]



unlist( lapply( s$heartrate_6, function(x) ifelse(is.null(x), NA, x) ))


addSampleSet<- function(s)
{
  sample.df<- data.frame( matrix( NA, nrow= length( s$time), ncol = length( sample.names )))
  names(sample.df)<- sample.names
  
  sample.df[,names.use]<- s[names.use]
  
  samples<<- rbind.fill( samples, sample.df)
}

lapply( workout.samples, addSampleSet )


