\name{IsotopicDistributionN}
\alias{IsotopicDistributionN}
\title{Isotopic Distribution of a Nitrogen-15 Labeled Peptide.}
\description{Simulates the isotopic distribution of a nitrogen-15 labeled peptide.  Intended for peptides from proteins grown in media with, for example, nitrogen-15 labeled ammonium chloride as the only nitrogen source.  
}
\usage{
IsotopicDistributionN(sequence, incorp, IAA = TRUE, charge = 1, 
                      custom = list(code = NULL, elements = NULL))
}
\arguments{
    \item{sequence}{character vector specifying the amino acid sequence.}
    \item{incorp}{numeric value from 0 to 1, specifying the fraction of nitrogen-15.}
    \item{IAA}{logical. \code{TRUE} specifies iodoacetylated cysteine and \code{FALSE} specifies unmodified cystine.}
    \item{charge}{numeric value specifying the number of positive charges (\eqn{[M + nH]^{n+}}{[M + nH]n+})}
    \item{custom}{a list specifying user defined residues as \code{custom = list(code, mass)}, where \code{code} is a vector of one letter characters and \code{mass} is a vector of the respective monoisotopic masses.}
}
\details{
The amino acid residues must be specified by the one letter codes defined in the help for \code{\link{Digest}}.  If a custom residue code is identical to a predefined residue code, the custom residue mass will be used in place of the predefined mass.

The natural nitrogen-15 incorporation is 0.00368. 

The argument \code{IAA} specifies treatment of the protein with iodoacetamide.  This treatment produces iodoacetylated cysteine residues (elemental formula C5H8N2O2S).  The nitrogen(s) added is not labeled.

The algorithm used in this function is based on \code{\link{sample}}, and will give a slightly different result each time it is run.  It is similar to algorithm used in reference 1.  \code{IsotopicDistributionN} has not been tested for sequences over approximately 3000 amu and charges greater that 3+.
}
\references{
1. "Method for Estimating the Isotopic Distributions of Metabolically Labeled Proteins by MALDI-TOFMS: Application to NMR Samples." Choudhary K, Spicer VL, Donald LJ, Duckworth HW, Ens W, Loewen PC, Standing KG, \emph{Analytical Chemistry}, 2006, 78, 5419-5423.

2. "Perturbation and Interpretation of Nitrogen Isotope Distribution Patterns in Proteomics" Snijders APL, de Koning B, Wright PC, \emph{Journal of Proteome Research}, 2005, 4, 2185-2191.

3. "Measurement of the Isotope Enrichment of Stable Isotope-Labeled Proteins Using High-Resolution Mass Spectra of Peptides" MacCoss MJ, Wu CC, Matthews DE, and Yates III JR, \emph{Analytical Chemistry}, 2005, 77, 7646-7653.

The relative atomic masses of the isotopes are from the NIST Physical Reference Data Website \url{http://physics.nist.gov/PhysRefData/Compositions/}.  The molar mass of a proton (H+) is from the NIST CODATA Website \url{http://physics.nist.gov/cuu/Constants/index.html}.

Note that in reference 3 "a biologically relevant 13C/12C isotope ratio of 1.096\% was used instead of the more carbonate specific ratio of 1.112\%."  The 13C/12C ratio used by \code{isoDistN} is 1.081\%.  
}
\value{
A data frame with the following column names.

    \item{mz}{the \emph{m/z} value}
    \item{intensity}{the number of counts at each \emph{m/z} value.  The total is 10000.}
    \item{percent}{the intensity at each \emph{m/z} value, expressed as a percent of the maximum intensity.}
}
\author{Nathan G. Dodder}
\seealso{\code{\link{IsotopicDistribution}}, \code{\link{IsotopicDistributionHDX}}, \code{\link{sample}}}
\examples{
## simulate a peptide with 0.368 to 100 percent incorporation and plot
dn <- IsotopicDistributionN("NECFLQHK", incorp = 0.00368)
dn$t <- "incorp = 0.368\%"
d25 <- IsotopicDistributionN("NECFLQHK", incorp = 0.25)
d25$t <- "incorp = 25\%"
d50 <- IsotopicDistributionN("NECFLQHK", incorp = 0.5)
d50$t <- "incorp = 50\%"
d75 <- IsotopicDistributionN("NECFLQHK", incorp = 0.75)
d75$t <- "incorp = 75\%"
d99 <- IsotopicDistributionN("NECFLQHK", incorp = 0.99)
d99$t <- "incorp = 99\%"
x <- rbind(dn, d25, d50, d75, d99)
library(lattice)
xyplot(percent ~ mz | t, data = x,
    type = "h", 
    xlab = "m/z", 
    ylab = "intensity (\%)",
    main = "Isotopic distribution of peptide NECFLQHK with 
            varying nitrogen-15 incorporation")
}





