\name{maldiSpectrumYEVQGEVFTKPQLWP}
\alias{maldiSpectrumYEVQGEVFTKPQLWP}
\docType{data}
\title{Example MALDI-TOF Spectrum of Nitrogen-15 Labeled Peptide} 
\description{Example used in Vinegette.}
\usage{maldiSpectrumYEVQGEVFTKPQLWP}
\format{A data frame consisting of \emph{m/z} values in the first column and intensity in the second.}
\source{
Precursor ion spectrum of peptide YEVQGEVFTKPQLWP acquired on a MALDI-TOF/TOF.  The peptide is labeled with approximately 99\% nitrogen-15.
}
\author{Nathan G. Dodder}
\examples{
## plot the spectrum
library(lattice)
maldiExample <- transform(maldiSpectrumYEVQGEVFTKPQLWP, 
  intensity.percent = intensity / max(intensity) * 100)
xyplot(intensity.percent ~ mz,
       data = maldiExample,
       type = "l",
       main = "experimental spectrum",
       xlab = "m/z",
       ylab = "intensity (\%)")
}
\keyword{datasets}
