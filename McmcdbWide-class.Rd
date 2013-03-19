\name{McmcdbWide-class}
\Rdversion{1.1}
\docType{class}
\alias{McmcdbWide-class}
\alias{[,McmcdbWide,ANY,ANY-method}
\alias{[[,McmcdbWide-method}
\alias{$,McmcdbWide-method}
\alias{mcmcdb_chains,McmcdbWide-method}
\alias{mcmcdb_data,McmcdbWide-method}
\alias{mcmcdb_flatpar_chains,McmcdbWide-method}
\alias{mcmcdb_flatpars,McmcdbWide-method}
\alias{mcmcdb_init,McmcdbWide-method}
\alias{mcmcdb_iters,McmcdbWide-method}
\alias{mcmcdb_metadata,McmcdbWide-method}
\alias{mcmcdb_parameters,McmcdbWide-method}
\alias{mcmcdb_pardims,McmcdbWide-method}
\alias{mcmcdb_paridx,McmcdbWide-method}
\alias{mcmcdb_unflatten,McmcdbWide,character-method}
\alias{mcmcdb_unflatten,McmcdbWide,missing-method}
\alias{show,McmcdbWide-method}

\title{Class \code{"McmcdbWide"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("McmcdbWide", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{samples}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{parameters}:}{Object of class \code{"McmcdbParameters"} ~~ }
    \item{\code{chains}:}{Object of class \code{"McmcdbChains"} ~~ }
    \item{\code{iters}:}{Object of class \code{"McmcdbIters"} ~~ }
    \item{\code{flatpar_chains}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"list"} ~~ }
    \item{\code{version}:}{Object of class \code{"character"} ~~ }
    \item{\code{parinit}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{model_data}:}{Object of class \code{"namedList"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "McmcdbWide", i = "ANY", j = "ANY")}: ... }
    \item{[[}{\code{signature(x = "McmcdbWide")}: ... }
    \item{$}{\code{signature(x = "McmcdbWide")}: ... }
    \item{mcmcdb_chains}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_data}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_flatpar_chains}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_flatpars}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_init}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_iters}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_metadata}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_parameters}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_pardims}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_paridx}{\code{signature(object = "McmcdbWide")}: ... }
    \item{mcmcdb_unflatten}{\code{signature(x = "McmcdbWide", parameters = "character")}: ... }
    \item{mcmcdb_unflatten}{\code{signature(x = "McmcdbWide", parameters = "missing")}: ... }
    \item{show}{\code{signature(object = "McmcdbWide")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("McmcdbWide")
}
\keyword{classes}
