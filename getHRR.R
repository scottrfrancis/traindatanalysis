source( 'initMongo.R' )
library(rmongodb)

#mongo<- initMongo( host='quantathlete.com', db='quantathlete')
mongo<- initMongo( host='localhost', db='quantathlete')

# walk backwards in time in 6*10 day cycles

# for each cycle, build pace in mph for x axis and HR for y axis
