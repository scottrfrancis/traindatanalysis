## timeSeries.R
# 
#   Utilities for working with variable time series
#
require('xts')




movingAvgForTimeWindow<- function( metric, time_S.elapsed, window_S )
{
  ma.metric<- vector()
  ## !! TODO:  THIS IS WRONG!!!!  something is wrong with the roll apply and time calcs !!!
  #
  if ( length( time_S.elapsed ) > window_S ) {
    m<- metric; #[!is.na(metric)]
    t<- time_S.elapsed #[!is.na(metric)]
    z<- .xts( m, t )
    zz<- na.locf(z)
    ep<- endpoints(zz, 'seconds', 1) 
    
    means<- period.apply(zz, ep, mean )
    
  #  means<- means[which( t > window_S )]
    ma.metric<- rollapply( coredata(means)[,1], window_S, mean )
  }
  
  padding.na<- vector( 'numeric', (length(metric) - length(ma.metric)) )
  padding.na[1:(length(metric) - length(ma.metric))]<- NA
  ma.metric<- c( padding.na, ma.metric ) 
}
