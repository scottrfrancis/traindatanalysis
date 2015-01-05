## dbutils.R
#
#   bits and pieces of common code for working with the Database
#
library(rmongodb)


# some collection names
orphans.collection<- "workoutSummariesOrphans"
samples.collection<- "samples"
summaries.collection<- "workoutSummaries"
thresholds.collection<- "userThresholds"
workouts.collection<- "workouts"



dbCollection<- function( collection.name )
{
  paste( dbName, collection.name, sep="." )
}


initDB<- function( host = "localhost:27017",
                      username = "",
                      password = "",
                      db = "test" ) 
{
  # your connection details will likely differ from these defaults
  
  #connect to mongo
  #the create function has the following signature
  #mongo.create(host="127.0.0.1", name="", username="", password="", db="admin", timeout=0L)
  
  mongo <- mongo.create(host=host , db=db, username=username, password=password)
  
  return( mongo )
}


queryForId<- function( oid, fieldName = "_id" )
{
  buf<- mongo.bson.buffer.create(); 
  mongo.bson.buffer.append( buf, fieldName, oid ); 
  q.bson<- mongo.bson.from.buffer( buf )
}