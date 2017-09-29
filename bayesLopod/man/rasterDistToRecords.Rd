\name{rasterDistToRecords}
\alias{rasterDistToRecords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasterDistToRecords(rasterN, rasterY)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rasterN}{
%%     ~~Describe \code{rasterN} here~~
}
  \item{rasterY}{
%%     ~~Describe \code{rasterY} here~~
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
function (rasterN, rasterY) 
{
    DistSample = rasterN
    DistSample[] = NA
    DistSample[rasterN[] > 0] = 1
    DistSample = distance(DistSample)
    DistDetec = rasterY
    DistDetec[] = NA
    DistDetec[rasterY[] > 0] = 1
    DistDetec = distance(DistDetec)
    outStack = stack(DistSample, DistDetec)
    names(outStack) = c("DistSample", "DistDetec")
    return(outStack)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
