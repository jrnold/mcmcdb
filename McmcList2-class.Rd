\name{McmcList2-class}
\Rdversion{1.1}
\docType{class}
\alias{McmcList2-class}
\alias{mcmcByIteration,McmcList2-method}

\title{Class \code{"McmcList2"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("McmcList2", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{parameters}:}{Object of class \code{"McmcParameterMeta"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{mcmc.list}"}, directly.
Class \code{"\linkS4class{list}"}, by class "mcmc.list", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "mcmc.list", distance 2.
Class \code{"\linkS4class{vector}"}, by class "mcmc.list", distance 3.
}
\section{Methods}{
  \describe{
    \item{mcmcByIteration}{\code{signature(object = "McmcList2")}: ... }
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
showClass("McmcList2")
}
\keyword{classes}
