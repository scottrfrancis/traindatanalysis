require('xts')

getMaxSustainedMeasure<- function( time_S.elapsed, metric, window_S )
{
  z<- .xts( metric, time_S.elapsed )
  zz<- na.locf(z)
  ep<- endpoints(zz, 'seconds', window_S/4 ) 
#  mins<- period.apply(zz, ep, min )
  means<- period.apply(zz, ep, mean )
  
  ## rebucket the mins in groups of 4 -- See Nyquist and Landau for Sampling theory of regular and irregular intervals
  # that is, take the mins of a SLIDING WINDOW of 4 mins
  # the MAX of these rewindowed mins is the max sustained for window_S
  
  if (nrow(means) < 4)
  {
    return( NA )
  }
  
#  max.metric<- max( rollapply( coredata(mins)[,1], 4, min ) )
  max.metric<- max( rollapply( coredata(means)[,1], 4, min ), na.rm=TRUE )
  
  return( max.metric )
}
