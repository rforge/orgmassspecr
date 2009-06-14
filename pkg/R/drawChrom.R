drawChrom <- function(time, intensity, range = list(start, stop), color = "blue", 
                      title = "chromatogram", units = "min") { 

### !!!!! error message when range is not specified, don't want to label peaks
### !!!!! is error message when multiple peaks working???

    if(length(range$start) != length(range$stop)) stop("start and stop vectors in the range list must be equal")
    if(length(color) != 1 | length(color) != length(range$start))
        stop("color vector length must be 1 or equal to the vectors in the range list")

    plot(time, intensity, type = "l",
         ylim = c(0, max(intensity) + 0.1*max(intensity)), 
         xlab = paste("retention time ", "(", units, ")", sep = ""), 
         ylab = "intensity", main = title)

    retention.time <- vector(mode = "numeric", length = length(range$start))
    area <- vector(mode = "numeric", length = length(range$start))
    apex.intensity <- vector(mode = "numeric", length = length(range$start))
    for(i in 1:length(range$start)) {
        tmp_time <- time[time >= range$start[i] & time <= range$stop[i]]
        tmp_intensity  <- intensity[time >= range$start[i] & time <= range$stop[i]]
        if(length(color) == 1) peak_color <- color
        else peak_color <- color[i]
        polygon(tmp_time, tmp_intensity, col = peak_color)

        ## calculate polygon area
        n <- length(tmp_time)
        tmp_area <- vector(mode = "numeric", length = n)
        for(j in 1:n) {
            k <- (j %% n) + 1
            tmp_area[j] <- tmp_time[j] * tmp_intensity[k] - tmp_time[k] * tmp_intensity[j]
        }

        area[i] <- abs(sum(tmp_area) / 2)

        retention.time[i] <- tmp_time[tmp_intensity == max(tmp_intensity)]
        apex.intensity[i] <- max(tmp_intensity)

    }

    return(data.frame(retention.time, area, apex.intensity))

}
