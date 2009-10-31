MolecularWeight <- function(formula = list(), amu = list()) {
    
    tmp_formula <- list(C = 0, H = 0, N = 0, O = 0, S = 0, P = 0, x = 0)
    tmp_formula[names(formula)] <- formula   # replace default values with argument values
    
    tmp_amu <- list(C = 12.0107, 
                    H = 1.00794, 
                    N = 14.0067, 
                    O = 15.9994, 
                    S = 32.065, 
                    P = 30.973761, 
                    x = 0)

    tmp_amu[names(amu)] <- amu

    mw <- (tmp_formula$C * tmp_amu$C + tmp_formula$H * tmp_amu$H +
             tmp_formula$N * tmp_amu$N + tmp_formula$O * tmp_amu$O +
             tmp_formula$S * tmp_amu$S + tmp_formula$P * tmp_amu$P +
             tmp_formula$x * tmp_amu$x)

    return(round(mw, digits = 3))

}
