flowTime <- function(id.um = 24, distance.cm = 45, rate.uLmin = 0.3) {
	id.mm <- id.um / 1000
	distance.mm <- distance.cm * 10
	vol <- pi * ((id.mm / 2) ^ 2) * distance.mm   # mm^3 or uL
	time = (vol / rate.uLmin) * 60   # sec
	message("time in seconds:")
	return(time)
}