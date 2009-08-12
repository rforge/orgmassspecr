deadVolume <- function(id.um = 24, distance.cm = 45) {
	id.mm <- id.um / 1000
	distance.mm <- distance.cm * 10
	vol <- pi * ((id.mm / 2) ^ 2) * distance.mm   # mm^3 or uL
	message("volume in microliters:")
	return(vol)
}