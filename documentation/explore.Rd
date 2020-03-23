\name{MultiLevel}
\alias{MultiLevel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform all the variables into categorical variables with a upper bound for degree of freedom  
%%  ~~function to do ... ~~
}
\description{ Only a common upper bound MaxGroup for the degree of freedom is required for the data transformation. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MultiLevel(data, MaxGroup, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{n by p data matrix}
%%     ~~Describe \code{x} here~~
\item{MaxGroup}{the common upper bound for the number of levles}
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
\item{N}{a vector indicating the degree of freedom for each variable}
\item{MultiLevel}{the class label for each transformed data}
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
