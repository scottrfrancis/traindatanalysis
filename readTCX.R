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
  
#  doc<- xmlTreeParse( f, getDTD=FALSE )
#  r<- xmlRoot( doc )

  doc<- xmlInternalTreeParse( f, fullNamespaceInfo=TRUE )
  # note specification of default namespace, d
  ns<- getNodeSet( doc, "//d:Trackpoint", c(d="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2") )
}