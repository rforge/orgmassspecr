\name{DrawChromatogram}
\alias{DrawChromatogram}
\title{Plot a Chromatogram}
\description{Plot a chromatogram, color the area under specified peak(s), and calculate the peak area(s).}
\usage{
DrawChromatogram(time, intensity, range = list(start, stop), color = "blue", 
                 title = "", units = "min")
}
\arguments{
    \item{time}{numeric vector containing the time points (the x-axis).}
    \item{intensity}{numeric vector containing the respective signal intensities at each time point (the y-axis).}
    \item{range}{list describing the start and stop time points for each peak, defined as \code{range = list(start, stop)}, where \code{start} is the numeric vector of starting time points and \code{stop} is the numeric vector of the respective ending time points.}
    \item{color}{vector of character strings specifying the color for each peak given in \code{range}.  If a single color is specified, it is applied to all peaks.}
    \item{title}{character string specifying the title for the chromatogram.}
    \item{units}{character string specifying the time units.}
}
\details{The area under the peak(s) is rendered using the \code{\link{polygon}} function.  The area calculation assumes that the polygon does not self-intersect.}
\value{
A data frame with the following column names.
    \item{retention.time}{retention times of the peaks specified in \code{range}}
    \item{area}{the respective peak areas}
    \item{apex.intensity}{the respective intensities at each peak apex} 
}
\author{Nathan G. Dodder}
\examples{
## single peak
x <- DrawChromatogram(chromatogramExample1$time, chromatogramExample1$intensity, 
                      range = list(start = 25.4, stop = 26.1), 
                      title = "example chromatogram 1")
# label peak with retention time and area
text(x$retentionTime, x$apexIntensity + 1500, 
     labels = paste("RT = ", round(x$retentionTime, digits = 1),
                    ", Area = ", round(x$peakArea), sep = ""), cex = 0.9) 

## multiple peaks 
y <- DrawChromatogram(chromatogramExample2$time, chromatogramExample2$intensity, 
                      range = list(start = c(21.5, 21.925, 23.1, 25.5, 27.35), 
                                    stop = c(21.925, 22.4, 23.6, 26.2, 28.0)),
                      color = c("blue", "red", "green", "yellow", "orange"), 
                      title = "example chromatogram 2")
## label peaks
text(y$retentionTime, y$apexIntensity + 50000, labels = c("a", "b", "c", "d", "e"))
}

