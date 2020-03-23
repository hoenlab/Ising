\name{Binary}
\alias{Binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate the design matrix for logistic regression
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is used to generate the design matrix from a the given  categorical observation.
}
\usage{
Binary(MultiLevel, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MultiLevel}{ refers to the categorical observations 
%%     ~~Describe \code{x} here~~
}
\item{N}{vector indicating the degree of freedom for each categorical variable}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ a list containing the following components
\item{BinaryLevel}{a data matrix each colomn of which stands for a binary variable}
\item{VariateGroup}{a vector indicating which colomns comes from the same categorical variable}
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
