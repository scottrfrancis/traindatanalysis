### ReadTCX.R
#
## reads a TCX file and returns a list of
# $summary -- single row summary of workout
# $laps -- one row per lap of workout
# $segments -- one row per 'trackpoint' (less one so as to create segments with distance)

require( 'XML' )


read.tcx<- function( f )
{
  cat( "reading ", f, " as TCX file\n" )
  
  dat<- xmlParse( readLines( f ), asText=TRUE )
  xml<- xmlToList( dat )

 
}