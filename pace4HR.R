# pace4HR.R
#
# Builds a chart of pace as function of Heart Rate
#
require( 'ggplot2' )
require( 'plyr' )

# f<- repo.files[420]
# 
# 
# segs<- read.csv( f )
# 

# segs<- data.frame()
# for (f in repo.files)
# {
#   s<- read.csv( f )
#   
#   segs<- rbind( segs, s )
# }


segs.run<- segs[segs$sport=='run',]
segs.run$pace.mph<- (segs.run$distance.m/1609.34)/(segs.run$duration.s/(60*60))
segs.run.good<- segs.run[ (segs.run$pace.mph > 0) & (segs.run$pace.mph < 15) 
                          & (segs.run$hr.bpm > 40) 
                          & (segs.run$hr.bpm < 190)
                          & !is.na( segs.run$hr.bpm) & !is.na( segs.run$pace.mph), ]

segs.run.good$datemonth<- as.factor( format( as.Date( segs.run.good$time ), format="%Y%m" ) )

segs.run.good$daysAgo<- as.integer( Sys.Date() - as.Date( segs.run.good$time ) )
segs.run.good$cyclesAgo<- as.integer( segs.run.good$daysAgo/(12*7) )


# plot( x=segs.run.good$pace.mph, y=segs.run.good$hr.bpm,
#       pch='.')

#  qplot( x=segs.run.good$pace.mph, y=segs.run.good$hr.bpm,
#        colour=factor( datemonth ) )

cycles<- as.numeric( levels( as.factor( segs.run.good$cyclesAgo )))

makeLM<- function( S )
{

#  numSamples= length( S )  
#   m<- lm( hr.bpm ~ pace.mph, data=S )
#   coef( m )  
#   m.co<- coef( m )
#   S$B<- m.co[1]
#   S$m<- m.co[2]  
}

perf.hist<- ddply( segs.run.good, .(cyclesAgo), summarize, 
                   makeLM )
,
                   numSamples= length( hr.bpm ), meanHR= mean( hr.bpm ),
                   )



s1<- segs.run.good[ segs.run.good$cyclesAgo == cycles[1], ]
sn<- segs.run.good[ segs.run.good$cyclesAgo == cycles[length(cycles)], ]

m1<- lm( hr.bpm ~ pace.mph, data=s1 )
mn<- lm( hr.bpm ~ pace.mph, data=sn )

perf<- data.frame()
perf<- rbind( perf, coef(m1) ) 
perf<- rbind( perf, coef(mn) )
colnames( perf )<- c( "B", "m" )


model<- lm( hr.bpm ~ pace.mph + factor( cyclesAgo ), data=segs.run.good )
grid<- with( segs.run.good, expand.grid( 
    pace.mph = seq( min( pace.mph ), max( pace.mph ), length= 20 ),
    cyclesAgo= levels( factor( cyclesAgo )) ) )
grid$hr.bpm<- predict( model, newdata=grid )

#qplot( pace.mph, hr.bpm, data=segs.run.good, color=factor( datemonth ) ) + geom_line( data=grid )
qplot( pace.mph, hr.bpm, data=grid, geom="line", color=factor(cyclesAgo) )


# p<- ggplot( segs.run.good, aes( pace.mph, hr.bpm ) )





