\name{lopodDens}
\alias{lopodDens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lopodDens(LopodModel, params = NULL, probs = c(0.05, 0.5, 0.95))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LopodModel}{
%%     ~~Describe \code{LopodModel} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
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
function (LopodModel, params = NULL, probs = c(0.05, 0.5, 0.95)) 
{
    if (class(LopodModel) != "LopodModel") 
        stop("Obeject needs to be a LopdModel")
    modelPar = modelParams(LopodModel)
    sumPars = modelPar$globalPars
    if (is.null(params) == F) {
        if (sum(params \%in\% sumPars) != length(params)) {
            stop(paste("For this model only the summary of the following global parameters can be returned:", 
                toString(sumPars)))
        }
        else {
            sumPars = params
        }
    }
    stan_dens(LopodModel@StanFit, pars = sumPars)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
