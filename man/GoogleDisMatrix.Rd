\name{GoogleDisMatrix}
\alias{GoogleDisMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Distance Matrix from Google Maps
}
\description{
  Returns distance and time from Google Maps between two points in space, origin and destination.
  
\usage{
  GoogleDisMatrix(orilat,orilot,deslat,deslot,mode,apikey,departure_time,arrival_time,transit_mode,transit_routing_preference)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orilat}{
    Latitude coordinate of the origin point in geodesic format. This could be numeric or string.
  }
  \item{orilot}{
    Longitude coordinate of the origin point in geodesic format. This could be numeric or string.
  }
  \item{deslat}{
    Latitude coordinate of the destination point in geodesic format. This could be numeric or string.
  }
  \item{deslot}{
    Longitude coordinate of the destination point in geodesic format. This could be numeric or string.
  }
  \item{Mode}{
    Travel mode in string format as Google requires it. Possible alternatives are: 
    \code{driving} (default): indicates distance calculation using the road network.
    \code{walking}: requests distance calculation for walking via pedestrian paths & sidewalks (where available).
    \code{bicycling}: requests distance calculation for bicycling via bicycle paths & preferred streets (where available). 
    \code{transit}: requests distance calculation via public transit routes (where available). This value may only be specified if the request includes an API key or a Google Maps API for Work client ID. If you set the mode to transit you can optionally specify either a \code{departure_time} or an \code{arrival_time}. If neither time is specified, the \code{departure_time} defaults to now (that is, the departure time defaults to the current time). You can also optionally include a \code{transit_mode} and/or a \code{transit_routing_preference}.
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  A list of different elements string and numeric containing distance in meters and minutes of a trip between two points.
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
  See the full documentation at \href{https://developers.google.com/maps/documentation/distance-matrix/}{https://developers.google.com/maps/documentation/distance-matrix/}
}
\author{
  Esteban Lopez
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  mysearch<-GooglePlaceSearch(query="restaurants in Champaign",apikey="yourAPIkeyhere")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

