\name{McmcParameterMeta-class}
\Rdversion{1.1}
\docType{class}
\alias{McmcParameterMeta-class}
\alias{mcmcUnflatten,McmcParameterMeta,ANY-method}
\alias{mcmcUnflatten,McmcParameterMeta,matrix-method}

\title{Class \code{"McmcParameterMeta"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("McmcParameterMeta", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{parameters}:}{Object of class \code{"character"} ~~ }
    \item{\code{skeleton}:}{Object of class \code{"list"} ~~ }
    \item{\code{indices}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{mcmcUnflatten}{\code{signature(metadata = "McmcParameterMeta", x = "ANY")}: ... }
    \item{mcmcUnflatten}{\code{signature(metadata = "McmcParameterMeta", x = "matrix")}: ... }
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
showClass("McmcParameterMeta")
}
\keyword{classes}
