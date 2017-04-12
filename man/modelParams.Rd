\name{modelParams}
\alias{modelParams}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
modelParams(LopodModel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LopodModel}{
%%     ~~Describe \code{LopodModel} here~~
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
function (LopodModel) 
{
    if (class(LopodModel) != "LopodModel") 
        stop("Obeject needs to be a LopdModel")
    if (LopodModel@modelInfo$CAR == F) {
        if (LopodModel@modelInfo$varP == F) {
            if (is.null(LopodModel@modelInfo$q) == T) {
                globalPars = c("psy", "p", "q", "qRate")
                sampledPars = c("psy_Sampled", "pCorr", "pp", 
                  "cellpres_i", "sim_y", "sim_true_y", "sim_false_y")
                allCellsPars = NULL
            }
            if (is.null(LopodModel@modelInfo$q) == F) {
                globalPars = c("psy", "p", "qRate")
                sampledPars = c("psy_Sampled", "pCorr", "pp", 
                  "cellpres_i", "sim_y", "sim_true_y", "sim_false_y")
                allCellsPars = NULL
            }
        }
        if (LopodModel@modelInfo$varP == T) {
            if (is.null(LopodModel@modelInfo$q) == T) {
                globalPars = c("psy", "pmax", "pmin", "pRange", 
                  "q", "qRate")
                sampledPars = c("psy_Sampled", "pCorr", "pp", 
                  "cellpres_i", "sim_y", "sim_true_y", "sim_false_y")
                allCellsPars = NULL
            }
            if (is.null(LopodModel@modelInfo$q) == F) {
                globalPars = c("psy", "pmax", "pmin", "pRange", 
                  "qRate")
                sampledPars = c("psy_Sampled", "pCorr", "pp", 
                  "cellpres_i", "sim_y", "sim_true_y", "sim_false_y")
                allCellsPars = NULL
            }
        }
    }
    if (LopodModel@modelInfo$CAR == T) {
        if (LopodModel@modelInfo$varP == F) {
            if (is.null(LopodModel@modelInfo$q) == T) {
                globalPars = c("psy", "p", "q", "qRate", "tau", 
                  "alpha")
                sampledPars = c("pCorr", "sim_y", "sim_true_y", 
                  "sim_false_y")
                allCellsPars = c("psy_i", "pp", "cellpres_i")
            }
            if (is.null(LopodModel@modelInfo$q) == F) {
                globalPars = c("psy", "p", "qRate", "tau", "alpha")
                sampledPars = c("pCorr", "sim_y", "sim_true_y", 
                  "sim_false_y")
                allCellsPars = c("psy_i", "pp", "cellpres_i")
            }
        }
        if (LopodModel@modelInfo$varP == T) {
            if (is.null(LopodModel@modelInfo$q) == T) {
                globalPars = c("psy", "pmax", "pmin", "pRange", 
                  "q", "qRate", "tau", "alpha")
                sampledPars = c("pCorr", "sim_y", "sim_true_y", 
                  "sim_false_y")
                allCellsPars = c("psy_i", "pp", "cellpres_i")
            }
            if (is.null(LopodModel@modelInfo$q) == F) {
                globalPars = c("psy", "pmax", "pmin", "pRange", 
                  "q", "qRate", "tau", "alpha")
                sampledPars = c("pCorr", "sim_y", "sim_true_y", 
                  "sim_false_y")
                allCellsPars = c("psy_i", "pp", "cellpres_i")
            }
        }
    }
    return(list(globalPars = globalPars, sampledPars = sampledPars, 
        allCellsPars = allCellsPars))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
