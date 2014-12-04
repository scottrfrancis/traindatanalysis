require('xts')

getMaxSustainedHR<- function( time_S.elapsed, heartrate_bpm, window_S )
{
  
  # compute delta time and delta dist
  time.0<- c( 0, time_S.elapsed[1:(length(time_S.elapsed)-1)])
  duration_S<- time_S.elapsed - time.0
  
  z<- .xts( heartrate_bpm, time_S.elapsed )
  zz<- na.locf(z)
  ep<- endpoints(zz, 'seconds', window_S )
  period.apply(zz, ep, min )
}