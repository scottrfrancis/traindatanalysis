# pace4HR.R
#
# Builds a chart of pace as function of Heart Rate
#
require( 'ggplot2' )

# f<- repo.files[420]
# 
# 
# segs<- read.csv( f )
# 

segs<- data.frame()
for (f in repo.files)
{
  s<- read.csv( f )
  
  segs<- rbind( segs, s )
}

segs.run<- segs[segs$sport=='run',]
segs.run$pace.mph<- (segs.run$distance.m/1609.34)/(segs.run$duration.s/(60*60))
segs.run.good<- segs.run[ (segs.run$pace.mph > 0) & (segs.run$pace.mph < 15) 
                          & (segs.run$hr.bpm > 40) 
                          & (segs.run$hr.bpm < 190)
                          & !is.na( segs.run$hr.bpm) & !is.na( segs.run$pace.mph), ]

# plot( x=segs.run.good$pace.mph, y=segs.run.good$hr.bpm,
#       pch='.')

qplot( x=segs.run.good$pace.mph, y=segs.run.good$hr.bpm,
       )