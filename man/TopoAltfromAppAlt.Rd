% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Declination-Altitude-functions.R
\name{TopoAltfromAppAlt}
\alias{TopoAltfromAppAlt}
\title{Determine Topocentric altitude from Apparent altitude}
\usage{
TopoAltfromAppAlt(AppAlt, TempE = TempDefault, PresE = PressureDefault)
}
\arguments{
\item{AppAlt}{the Apparent altitude (deg), double}

\item{TempE}{Temperature at height eyes (C, default=15C), double}

\item{PresE}{Airpressure at height eyes (mbar, default=1013.25mbar), double}
}
\value{
TopoAlt thee Topocentric altitude (deg), double
}
\description{
The astronomical refraction is determined using the Sinclair's formula. This formula is based
on the International Standard Atmosphere (ISA: which by the way does not mean the 'average' atmosphere;-).
Due to the uncertainties aorund the atmsopheric condition of the Boundary Layers, the calcuated
refractions will not be very accurate (say with 0.5deg).
Below an Apparent altitude of -3.5deg, the Topocentric altitude is made equal.
}
\examples{
TopoAltfromAppAlt(0,15,1013.25)
#-0.5598886443

TopoAltfromAppAlt(c(-4,-0.5,0,2,20))
#-4.0000000000 -1.1817229649 -0.5598886443  1.7010159897 19.9561578991

}
\references{
Bennett, G.G. 1982. 'The calculation of astronomical refraction in marine navigation',
Journal of Inst. navigation, Vol 35: pp. 255-59.
}
\seealso{
\url{http://www.archaeocosmology.org/eng/refract.htm}
}
\author{
Victor Reijs, \email{lists@archaeocosmology.org}
}
