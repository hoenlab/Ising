\name{CuttingPoint}
\alias{CuttingPoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Transform the continuous data into categorical data 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function uses  dynamic programming algorithm to transform the zero-inflated continuous variable into categorical variable that minimize the squared Euclidean distance.
}
\usage{
CuttingPoint(x, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector x representing the observations on continuous variable to be transformed.}
\item{N}{N representing the specified number of the levels}
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
a list containing the following items,
\item{LossFunction}{the final value of the loss function}
\item{CuttingPoint}{the boundary point}
\item{calss}{the label for different class}
\item{CuttingValue}{the value at the boundary point}
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
