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

samples.user.act<- getActivitySamplesForUser( uid, 'Swim' )

