\name{netmat}
\alias{netmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate the adjacent matrix using the zero-inflated latent Ising model
%%  ~~function to do ... ~~
}
\description{This is the main function that implement our algorithm.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
netmat(A, width)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{a binary matrix representing the transfomred data}
%%     ~~Describe \code{x} here~~
\item{width}{a vector indicating which colomns are belonging to the same categorical variable}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{a adjacent matrix indicating whether two variables is condionally indenpendent. 
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
Jie Zhou
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
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
