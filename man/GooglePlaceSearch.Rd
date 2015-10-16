\name{GooglePlaceSearch}
\alias{GooglePlaceSearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Searchs a general query in Google Maps
}
\description{
Creates a \code{data.frame} object with the output of a query in Google Maps of places of given a general given location.

User must have a valid API key from Google Places API Web Service to make this function work. For more information go to: \href{https://developers.google.com/places/web-service}{https://developers.google.com/places/web-service}
}
\usage{
GooglePlaceSearch(query, apikey)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{query}{
Is a string object with an specific query as it would be ask in the search bar of Google Maps. 
}
  \item{apikey}{
Your API Key from  \href{https://developers.google.com/places/web-service}{https://developers.google.com/places/web-service}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a \code{data.frame} object with the search results of  \code{query} that includes a general location.
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
mysearch<-GooglePlaceSearch(query="restaurants in Champaign",apikey="yourAPIkeyhere")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
