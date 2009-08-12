\name{deadVolume}
\alias{deadVolume}
\title{Internal volume of tubing.}
\description{Calculate the internal volume of a defined length of tubing.}
\usage{deadVolume(id.um = 24, distance.cm = 45)}
\arguments{
    \item{id.um}{the internal diameter of the tubing, in micrometers; numeric value.}
    \item{distance.cm}{the length of tubing, in centimeters; numeric value.}
}
\value{Vector of the dead volume in microliters.}
\author{Nathan G. Dodder}
\seealso{\code{\link{flowTime}}}
\examples{deadVolume(id.um = 47, distance = 33)}