\name{flowTime}
\alias{flowTime}
\title{Solvent transit time.}
\description{Calculate the time required for a liquid to flow through a defined length of tubing.  Intended for conventional-flow or nano-flow liquid chromatography.}
\usage{flowTime(id.um = 24, distance.cm = 45, rate.uLmin = 0.3)}
\arguments{
    \item{id.um}{the internal diameter of the tubing, in micrometers; numeric value.}
    \item{distance.cm}{the length of tubing, in centimeters; numeric value.}
    \item{rate.uLmin}{the flow rate of the solvent in microliters per minute; numeric value}
}
\details{The calculation assumes the viscosity of the liquid is negligible.}
\value{Vector of the flow time in seconds.}
\author{Nathan G. Dodder}
\seealso{\code{\link{deadVolume}}}
\examples{flowTime(id.um = 47, distance = 33, rate.uLmin = 200)}