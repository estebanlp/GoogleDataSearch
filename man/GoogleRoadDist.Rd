\name{GoogleRoadDist}
\alias{GoogleRoadDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Road Distance between two points from Google Maps
}
\description{
  Returns the road distance and time from Google Maps between two points in space, origin and destination.
}
\usage{
  GoogleRoadDist(orilat,orilon,deslat,deslon,mode,apikey,departure_time,arrival_time,transit_mode,transit_routing_preference)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orilat}{
    Latitude coordinate of the origin point in geodesic format. This could be numeric or string.
  }
  \item{orilon}{
    Longitude coordinate of the origin point in geodesic format. This could be numeric or string.
  }
  \item{deslat}{
    Latitude coordinate of the destination point in geodesic format. This could be numeric or string.
  }
  \item{deslon}{
    Longitude coordinate of the destination point in geodesic format. This could be numeric or string.
  }
  \item{mode}{
    Travel mode in string format as Google requires it. Possible alternatives are: 
    \itemize{
      \item \code{driving} (default): indicates distance calculation using the road network.
    \item \code{walking}: requests distance calculation for walking via pedestrian paths & sidewalks (where available).
    \item \code{bicycling}: requests distance calculation for bicycling via bicycle paths & preferred streets (where available). 
    \item \code{transit}: requests distance calculation via public transit routes (where available). This value may only be specified if the request includes an API key or a Google Maps API for Work client ID. If you set the mode to transit you can optionally specify either a \code{departure_time} or an \code{arrival_time}. If neither time is specified, the \code{departure_time} defaults to now (that is, the departure time defaults to the current time). You can also optionally include a \code{transit_mode} and/or a \code{transit_routing_preference}.
    }
  }
  \item{apikey}{
Your API Key from  \url{https://developers.google.com/places/web-service}
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  A list with two numeric elements containing distance in miles and minutes of a trip between two points.
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
  See the full documentation at \url{https://developers.google.com/maps/documentation/distance-matrix/}
}
\author{
  Esteban Lopez <lopezoch@illinois.edu>
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  orilat<-40.110796
  orilon<--88.206985
  deslat<-40.109909
  deslon<--88.227235
  mysearch<-GoogleRoadDist(orilat,orilon,deslat,deslon)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line