

# Convert to `'fmx'` Class

## Convert package **`mixsmsn`** returns to `'fmx'`


#' @title Convert `Skew.normal` Object to \linkS4class{fmx}
#' 
#' @description 
#' To convert `Skew.normal` object (from package \CRANpkg{mixsmsn}) 
#' to \linkS4class{fmx} class.
#' 
#' @param x `'Skew.normal'` object, 
#' returned from \link[mixsmsn]{smsn.mix} with parameter 
#' `family = 'Skew.normal'`.
#' 
#' @param data \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' \link[mixsmsn]{smsn.mix} does not offer a parameter to keep the input data, as of 2021-10-06.
#' 
#' @returns 
#' Function [as.fmx.Skew.normal()] returns an \linkS4class{fmx} object.
#' 
#' @examples
#' library(mixsmsn)
#' ag1 = c(mu = 5, sigma2 = 9, lambda = 5, nu = 5)
#' ag2 = c(mu = 20, sigma2 = 16, lambda = -3, nu = 5)
#' ag3 = c(mu = 35, sigma2 = 9, lambda = -6, nu = 5)
#' set.seed(120); x = rmix(n = 1e3L, p = c(.5, .2, .3), family = 'Skew.t', 
#'  arg = list(unname(ag1), unname(ag2), unname(ag3)))
#'  
#' library(fmx)
#' smsn.mix(x, nu = 3, g = 3, family = 'Skew.normal', calc.im = FALSE) |>
#'   as.fmx(data = x)
#' 
#' smsn.mix(x, nu = 3, g = 3, family = 'Normal', calc.im = FALSE) |> 
#'   as.fmx(data = x)
#'   
#' smsn.mix(x, nu = 3, g = 3, family = 'Skew.t', calc.im = FALSE) |> 
#'   as.fmx(data = x)
#' 
#' smsn.mix(x, nu = 3, g = 3, family = 't', calc.im = FALSE) 
#' # |> as.fmx(m4, data = x) # not ready yet!!
#' @keywords internal
#' @importFrom fmx as.fmx
#' @importClassesFrom fmx fmx 
#' @importFrom methods new
#' @method as.fmx Skew.normal
#' @export as.fmx.Skew.normal
#' @export
as.fmx.Skew.normal <- function(x, data, ...) {
  x <- sort.Skew.normal(x, decreasing = FALSE)
  
  ret <- new(Class = 'fmx', pars = cbind(
    xi = x[['mu']],
    omega = sqrt(x[['sigma2']]),
    alpha = x[['shape']]
  ), 
  w = x[['pii']],
  distname = 'sn')
  
  if (!missing(data)) {
    if (!length(data) || !is.numeric(data) || anyNA(data)) stop('illegal `data`')
    ret@data <- data
    # use ?methods::initialize
    #ret@Kolmogorov <- Kolmogorov_fmx(ret)
    #ret@CramerVonMises <- CramerVonMises_fmx(ret)
    #ret@KullbackLeibler <- KullbackLeibler_fmx(ret)
  }
  
  return(ret)
}





#' @title Convert `Normal` fit from \CRANpkg{mixsmsn} to \linkS4class{fmx}
#' 
#' @description
#' To convert `Normal` object (from package \CRANpkg{mixsmsn}) 
#' to \linkS4class{fmx} class.
#' 
#' @param x `'Normal'` object, 
#' returned from \link[mixsmsn]{smsn.mix} with parameter 
#' `family = 'Normal'`
#' 
#' @param data \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' \link[mixsmsn]{smsn.mix} does not offer a parameter to keep the input data, as of 2021-10-06.
#' 
#' @returns 
#' Function [as.fmx.Normal()] returns an \linkS4class{fmx} object.
#' 
#' @keywords internal
#' @importFrom fmx as.fmx
#' @importClassesFrom fmx fmx 
#' @importFrom methods new
#' @method as.fmx Normal
#' @export as.fmx.Normal
#' @export
as.fmx.Normal <- function(x, data, ...) {
  x <- sort.Normal(x, decreasing = FALSE)
  
  ret <- new(Class = 'fmx', pars = cbind(
    mean = x[['mu']],
    sd = sqrt(x[['sigma2']])
  ), 
  w = x[['pii']],
  distname = 'norm')
  
  if (!missing(data)) {
    if (!length(data) || !is.numeric(data) || anyNA(data)) stop('illegal `data`')
    ret@data <- data
    # use ?methods::initialize
    #ret@Kolmogorov <- Kolmogorov_fmx(ret)
    #ret@CramerVonMises <- CramerVonMises_fmx(ret)
    #ret@KullbackLeibler <- KullbackLeibler_fmx(ret)
  }
  
  return(ret)
  
}






#' @title Convert `Skew.t` fit from \CRANpkg{mixsmsn} to \linkS4class{fmx}
#' 
#' @description
#' To convert `Skew.t` object (from package \CRANpkg{mixsmsn}) 
#' to \linkS4class{fmx} class.
#' 
#' @param x `'Skew.t'` object, 
#' returned from \link[mixsmsn]{smsn.mix} with parameter 
#' `family = 'Skew.t'` 
#' 
#' @param data \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' \link[mixsmsn]{smsn.mix} does not offer a parameter to keep the input data, as of 2021-10-06.
#' 
#' @returns 
#' Function [as.fmx.Skew.t()] returns an \linkS4class{fmx} object.
#' 
#' @keywords internal
#' @importFrom fmx as.fmx
#' @importClassesFrom fmx fmx 
#' @importFrom methods new
#' @method as.fmx Skew.t
#' @export as.fmx.Skew.t
#' @export
as.fmx.Skew.t <- function(x, data, ...) {
  x <- sort.Skew.t(x, decreasing = FALSE)
  
  K <- length(x[['mu']]) # number of components
  if (length(x[['nu']]) != 1L) stop('\\pkg{mixsmsn} update to enable multiple `nu`? Modify ?npar.fmx') 
  
  ret <- new(Class = 'fmx', pars = cbind(
    xi = x[['mu']],
    omega = sqrt(x[['sigma2']]),
    alpha = x[['shape']],
    nu = x[['nu']]
  ), 
  w = x[['pii']],
  distname = 'st')
  
  if (!missing(data)) {
    if (!length(data) || !is.numeric(data) || anyNA(data)) stop('illegal `data`')
    ret@data <- data
    # use ?methods::initialize
    #ret@Kolmogorov <- Kolmogorov_fmx(ret)
    #ret@CramerVonMises <- CramerVonMises_fmx(ret)
    #ret@KullbackLeibler <- KullbackLeibler_fmx(ret)
  }
  
  return(ret)
}






#' @title Convert `t` fit from \CRANpkg{mixsmsn} to \linkS4class{fmx}
#' 
#' @description
#' To convert `t` object (from package \CRANpkg{mixsmsn}) 
#' to \linkS4class{fmx} class.
#' 
#' @param x `'t'` object, 
#' returned from \link[mixsmsn]{smsn.mix} with parameter 
#' `family = 't'`
#' 
#' @param data \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' \link[mixsmsn]{smsn.mix} does not offer a parameter to keep the input data, as of 2021-10-06.
#' 
#' @returns 
#' Function [as.fmx.t()] has not been completed yet
#' 
#' @keywords internal
#' @importFrom fmx as.fmx
#' @importClassesFrom fmx fmx 
#' @importFrom methods new
#' @method as.fmx t
#' @export
as.fmx.t <- function(x, data, ...) {
  if (!length(data) || !is.numeric(data) || anyNA(data)) stop('illegal `data`')
  x <- sort.t(x, decreasing = FALSE)
  stop('need to programe scale_and_shift_t')
  #new(Class = 'fmx', pars = cbind(
  #  mean = x[['mu']],
  #  sd = sqrt(x[['sigma2']])
  #), 
  #w = x[['pii']],
  #distname = 'norm', 
  #data = data)
}
