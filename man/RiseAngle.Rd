% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Declination-Altitude-functions.R
\name{RiseAngle}
\alias{RiseAngle}
\title{Determine the rise angle of the celestial object}
\usage{
RiseAngle(Lat, GeoDec, AppAlt, TempE = TempDefault,
  PresE = PressureDefault, ObjectDist, Rim = RimDefault)
}
\arguments{
\item{Lat}{the apparent altitude (deg), double  vector}

\item{GeoDec}{the geocentric declination (deg), double  vector}

\item{AppAlt}{the apparent altitude (deg), double vector}

\item{TempE}{Temperature at Height eyes (C), default=15C, double vector}

\item{PresE}{Airpressure at height eye (mbar, default=1013.25mbar), double vector}

\item{ObjectDist}{the name of celestial object ("moonavg","moonnearest","moonfurthest","sun","star","topo"]), charector vector}

\item{Rim}{the place to be taken on the clestial object's disc (default=0), integer (bottom=-1, centre=0, top=1) vector}
}
\value{
RiseAngle the apparent rise angle (deg), double
}
\description{
blabla
}
\examples{
RiseAngle(55,12,0,10,1000,"moonavg",0) 
#28.33387469

}
