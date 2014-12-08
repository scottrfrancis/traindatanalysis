source( 'initMongo.R' )
library(rmongodb)
source('summarizeActivitySamplesForUser.R')

mongo = initMongo( host='quantathlete.com', db='quantathlete')

# build a query to find all Users

users<- mongo.find.all( mongo, 'quantathlete.users', fields=mongo.bson.from.list( c(list(`_id`=1L))) )
uid<- users[[1]]$`_id`

for ( u in users )
{
  print( u$`_id` )
  sapply( c('Swim', 'Ride', 'Run'), function(a) summarizeActivitySamplesForUser(u$`_id`, a) )
}
