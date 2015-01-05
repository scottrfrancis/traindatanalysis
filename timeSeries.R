## timeSeries.R
# 
#   Utilities for working with variable time series
#
require('xts')




movingAvgForTimeWindow<- function( metric, time_S.elapsed, window_S )
{
  z<- .xts( metric, time_S.elapsed )
  zz<- na.locf(z)
  ep<- endpoints(zz, 'seconds', 1) 
  
  means<- period.apply(zz, ep, mean )
  
  ma.metric<- rollapply( coredata(means)[,1], window_S, mean )
  
  padding.na<- vector( 'numeric', (length(metric) - length(ma.metric)) )
  padding.na[1:(length(metric) - length(ma.metric))]<- NA
  ma.metric<- c( padding.na, ma.metric ) 
}
