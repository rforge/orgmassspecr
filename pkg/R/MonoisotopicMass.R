MonoisotopicMass <- function(formula = list(), isotopes = list(), charge = 0) {
    
    tmp_formula <- list(C = 0, H = 0, N = 0, O = 0, S = 0, P = 0, x = 0)
    tmp_formula[names(formula)] <- formula   # replace default values with argument values
    
    tmp_isotopes <- list(C = 12, 
                         H = 1.0078250321, 
                         N = 14.0030740052, 
                         O = 15.9949146221, 
                         S = 31.97207069, 
                         P = 30.97376151, 
                         x = 0)

    tmp_isotopes[names(isotopes)] <- isotopes

    if(charge < 0 & abs(charge) > tmp_formula$H)
        stop("the number of negative charges exceeds the number of hydrogens in the formula list")

    mass <- (tmp_formula$C * tmp_isotopes$C + tmp_formula$H * tmp_isotopes$H +
             tmp_formula$N * tmp_isotopes$N + tmp_formula$O * tmp_isotopes$O +
             tmp_formula$S * tmp_isotopes$S + tmp_formula$P * tmp_isotopes$P +
             tmp_formula$x * tmp_isotopes$x)

    if(charge != 0) mass <- abs((mass + charge * 1.007276466) / charge)

    return(mass)

}
