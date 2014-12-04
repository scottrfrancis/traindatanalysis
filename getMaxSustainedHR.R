require('xts')

getMaxSustainedHR<- function( time_S.elapsed, heartrate_bpm, window_S )
{
  
  z<- .xts( heartrate_bpm, time_S.elapsed )
  zz<- na.locf(z)
  ep<- endpoints(zz, 'seconds', window_S/2 )  ## this isn't quite right yet...
  period.apply(zz, ep, min )
}