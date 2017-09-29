\name{rasterStudyArea}
\alias{rasterStudyArea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasterStudyArea(DistStak, maxDist, extSample = 0.025, extDetection = 0.15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DistStak}{
%%     ~~Describe \code{DistStak} here~~
}
  \item{maxDist}{
%%     ~~Describe \code{maxDist} here~~
}
  \item{extSample}{
%%     ~~Describe \code{extSample} here~~
}
  \item{extDetection}{
%%     ~~Describe \code{extDetection} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (DistStak, maxDist, extSample = 0.025, extDetection = 0.15) 
{
    maxExtDistSample = maxDist * extSample
    maxExtDistDect = maxDist * extDetection
    DistSample = DistStak[["DistSample"]]
    DistSample[] = NA
    DistSample[DistStak[["DistSample"]][] == 0] = 1
    DistSample = distance(DistSample)
    DistSample[DistSample[] > maxExtDistSample] = NA
    DistSample[DistSample[] <= maxExtDistSample] = 1
    DistDetec = DistStak[["DistDetec"]]
    DistDetec[] = NA
    DistDetec[DistStak[["DistDetec"]] == 0] = 1
    DistDetec = distance(DistDetec)
    DistDetec[DistDetec[] > maxExtDistDect] = NA
    DistDetec[DistDetec[] <= maxExtDistDect] = 1
    StudyArea = DistDetec * DistSample
    return(StudyArea)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
