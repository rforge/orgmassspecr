\name{drawChrom}
\alias{drawChrom}
\title{Plot a Chromatogram}
\description{Plot a chromatogram, color the area under specified peak(s), and calculate the peak area(s).}
\usage{
drawChrom(time, intensity, range = list(start, stop), color = "blue", 
          title = "chromatogram", units = "min")
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
x <- drawChrom(chrom_example_1$time, chrom_example_1$intensity, 
               range = list(start = 25.4, stop = 26.1), 
               title = "example chromatogram")
# label peak with retention time and area
text(x$retention.time, x$apex.intensity + 1500, 
     labels = paste("RT = ", round(x$retention.time, digits = 1),
                    ", Area = ", round(x$area), sep = ""), cex = 0.9) 

## multiple peaks 
#y <- drawChrom(chrom_example_2$time, chrom_example_2$intensity, 
#               range = list(start = 25.4, stop = 26.1), 
#               title = "example chromatogram")
## label peaks using returned dataframe
#text(y$retention.time, y$apex.intensity, labels = c("a","b","c"......))
}