## streamData.R
#
# computations on workout streams

time.windows<- c( 6, 12, 3*60, 6*60, 20*60, 60*60 )



segmentDuration<- function( time )
{
  time0<- c( 0, time[1:(length(time) - 1)])
  dur<- time - time0
}

segmentLength<- function( distance )
{
  dist0<- c( 0, distance[1:length(distance) - 1])
  length<- distance - dist0 
}

computeCommonStreams<- function( workout.data, streams )
{
  cStreams<- list()
  if (!(is.null(workout.data$samples$time))) {
    cStreams<- append( cStreams, list( 'duration'= segmentDuration( workout.data$samples$time ) ) )
  }
  if (!(is.null(workout.data$samples$distance))) {
    cStreams<- append( cStreams, list( 'length'= segmentLength( workout.data$samples$distance )) )
  }
  
  cStreams
}

segmentSpeed.mps<- function( length, duration )
{
 speed_mps = length/duration
 
 speed_mps[which(speed_mps == Inf)]<- NA
 speed_mps[which(speed_mps == -Inf)]<- NA
 
 speed_mps
}

computeSpeedStreams<- function( workout.data, streams )
{
  cStreams<- list()
  if (!(is.null(streams$duration)) & !(is.null(streams$length))) {
    cStreams<- append( cStreams, list( 'speed_mps'= segmentSpeed.mps( streams$length, streams$duration ) ) )
  }
  if ( max(workout.data$samples$time) > 30 ) {
    cStreams<- append( cStreams, list( 'speed_mps_30'= movingAvgForTimeWindow( cStreams$speed_mps, workout.data$samples$time, 30 )))
  }
  
  cStreams
}

# computeHRStreams<- function( workout.data, streams )
# {
#  
# }

computePowerStreams<- function( workout.data, streams )
{
  cStreams<- list()
  cStreams<- append( cStreams, list( 'watts_30'= movingAvgForTimeWindow( workout.data$samples$watts, workout.data$samples$time, 30 ) ) )
  
  cStreams
}

runFactories<- function( factories, workout.data )
{
  results<- list()
  lapply( factories, function(f) {
    if (!(is.null(workout.data$samples[[f$depends]]))) {
      results<<- append( results, f$factory( workout.data, results ) )
    }
  })
  
  results
}

stream.factories<- list( '*'= list( 'depends'= 'time', 'factory'= computeCommonStreams),
                          'distance'= list( 'depends'= 'distance', 'factory'= computeSpeedStreams),
#                           'heartrate'= list( 'depends'= 'heartrate', 'factory' = computeHRStreams),
                          'watts'= list( 'depends'= 'watts', 'factory'= computePowerStreams ) )


computeStreams<- function( workout.data )
{
  streams<- runFactories( stream.factories, workout.data )
    
  streams
}

windowStream<- function( time, stream )
{
  windows<- sapply( time.windows, function(t) getMaxSustainedMeasure( time, stream, t ) )
}

windowHeartrate<- function( workout.data, summaries )
{
  heartrate.windows<- windowStream( workout.data$samples$time, workout.data$samples$heartrate )
  summary<- list( 'heartrateWindows'= heartrate.windows )
  
  summary
}

windowPace<- function( workout.data, summaries )
{
  pace.windows<- windowStream( workout.data$samples$time, workout.data$samples$speed_mps )
  summary<- list( 'paceWindows'= pace.windows )
  
  summary
}

windowPower<- function( workout.data, summaries )
{
  power.windows<- windowStream( workout.data$samples$time, workout.data$samples$watts )
  summary<- list( 'powerWindows'= power.windows )
  
  summary
}

normalizedPower<- function( watts )
{
  watts.use<- watts[!is.na(watts)]
  
  np<- (sum(watts.use^4)/length(watts.use))^(1/4)
}

variabilityIndex<- function( np, watts )
{
  np/(mean( watts, na.rm=TRUE ))
}

summarizePower<- function( workout.data, summaries )
{
  summary<- list()
  summary<- append( summary, list( 'power_normalized'= normalizedPower( workout.data$samples$watts_30 ) ) )
  summary<- append( summary, list( 'variability_index'= variabilityIndex( summary$power_normalized, workout.data$samples$watts )))
  summary<- append( summary, list( 'intensity_factor'= intensityFactor( summary$power_normalized, workout.data$ftp )))
  
  summary
}

instensityFactor<- function( np, ftp )
{
  intensity.factor<- NA
  
  if (!(is.null(ftp)) & !(is.na(ftp))) {
    intensity.factor<- np/ftp
  }
  
  intensity.factor
}

tssForPower<- function( workout.data, summaries )
{
  tss<- list( 'tss'= powerTSS( max( workout.data$samples$time ), summaries$power_normalized, summaries$intensity_factor, workout.data$ftp ) )
}

tssForPace<- function( workout.data, summaries )
{
  tss<- summaries$tss
  
  if (workout.data$type == 'Run' & !is.null( summaries$intensity_factor ) ) {
    tss<- (max( workout.data$samples$time )/(60*60))*(summaries$intensity_factor^2)*100
  } 
  
  tss
}

summary.factories<- list( 'heartrate'= list( 'depends'='heartrate', 'factory'= windowHeartrate ),
                          'pace'= list( 'depends'= 'speed_mps', 'factory'= windowPace ),
                          'np'= list( 'depends'= 'watts_30', 'factory'= summarizePower ),
                          'watts'= list( 'depends'= 'watts', 'factory'= windowPower ),
                          'tss'= list( 'depends'='watts_30', 'factory'= tssForPower ),
                          'tss'= list( 'depends'='speed_mps', 'factory'= tssForPace ) )

computeSummaries<- function( workout.data )
{
  summaries<- runFactories( summary.factories, workout.data )
}


