library(rmongodb)
#source('summarizeActivitySamplesForUser.R')

initMongo<- function( host = "localhost:27017",
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
