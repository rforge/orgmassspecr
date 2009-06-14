\name{exchAmides}
\alias{exchAmides}
\title{Determine the Number of Backbone Amide Hydrogens}
\description{Determine the number of backbone amide hydrogens given a protein/peptide sequence.  Used in hydrogen-deuterium exchange experiments.
}
\usage{exchAmides(sequence)}
\arguments{
    \item{sequence}{character vector containing one or more amino acid sequences.}
}
\details{
The number of backbone amide hydrogens in an amino acid sequence is the number of residues, minus the number of prolines, minus 1.
}
\value{A numeric vector containing the number of exchangeable hydrogens.}
\author{Nathan G. Dodder}
\seealso{\code{\link{isoDistHDX}}}
\examples{
exchAmides(c("VDVMCTA", "VSTPTL"))

## find the number of exchangeable amides for 
## each peptide in a digest of human serum albumin
x <- enzDigest(hsa, enzyme = "pepsin", IAA = FALSE, missed = 4)
transform(x, exchange = exchAmides(x$peptide))
}
