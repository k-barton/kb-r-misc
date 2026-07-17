dateAxis <-
function(f = .5) {
    
    #f <- .25
    xlim <- par("usr")[1:2]
    class(xlim) <- "Date"
    dlt <- as.POSIXlt(xlim)
    if(diff(dlt$year) > 0) {
        d <- dlt
        d$year <- d$year + 1
        ylen <- as.numeric(d - dlt, units = "days")
        dtoend <- c(ylen[1] - dlt$yday[1], dlt$yday[2])
        yrs <- dlt$year
        yrs <- yrs + c(1, -1) * (dtoend / ylen < f)
        yrs <- seq(yrs[1], yrs[2]) + 1900
        at <- as.Date(paste0(yrs, "-01-01")) + (365/2)
        
        ddt <- as.Date(dlt)
        if(ddt[1] > at[1])
            at[1] <- ddt[1] + dtoend[1]/2
        if(ddt[2] < at[length(at)])
            at[length(at)] <- ddt[2] - dtoend[2]/2
    
        
        axis.Date(side = 1, at = at, format = "%Y", tcl = 0, mgp = c(3,1.66,0))
    }
    at <- round(dlt, "months")
    at <- as.Date(seq(at[1], at[2], by = "months"))
    at <- as.Date(round(as.POSIXct(at), "months"))
    axis.Date(side = 1, at = at, format = "%n", tcl = -.5)
    axis.Date(side = 1, at = at + 15, format = "%b", tcl = 0, mgp = c(3,.66,0))
    #at <- as.Date(round(as.POSIXct(xlim), "years"))
    #x <- c(xlim[1], at, xlim[2])
    #dx <- as.numeric(diff(x), units = "days")
    #at <- x[-length(x)]  + (dx/2)
    #axis.Date(side = 1, at = at, format = "%Y", tcl = 0, mgp = c(3,1.66,0))
}
