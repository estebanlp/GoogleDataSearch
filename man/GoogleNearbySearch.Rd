\name{GoogleNearbySearch}
\alias{GoogleNearbySearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search in Google Maps Nearby an Specific Location
}
\description{
Creates a \code{data.frame} object with the output of a query in Google Maps of places of given types nearby a given location.

User must have a valid API key from Google Places API Web Service to make this function work. For more information go to: \href{https://developers.google.com/places/web-service}{https://developers.google.com/places/web-service}
}
\usage{
GoogleNearbySearch(query, lat, lon, radius, apikey)
}

\arguments{
  \item{query}{
Is a string object with an specific query as it would be ask in the search bar of Google Maps. 
}
  \item{lat}{
Latitude coordinate of the specific place where to find the \code{query} nearby.
}
  \item{lon}{
Longitude coordinate of the specific place where to find the \code{query} nearby.
}
  \item{radius}{
Radius of search in meters. How nearby \code{lat} and \code{lon} should we search for \code{query}?
}
  \item{apikey}{
Your API Key from  \href{https://developers.google.com/places/web-service}{https://developers.google.com/places/web-service}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a \code{data.frame} object with the search results of  \code{query} nearby \code{lat} and \code{lon} within \code{radius}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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

mysearch<-GoogleNearbySearch(query="korean food",lat=40.107336,lon=-88.226110,radius=500,apikey="yourAPIkeyhere")
}