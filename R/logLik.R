

#' @importFrom fmx logLik.fmx
#' @export
logLik.Skew.normal <- function(object, data = stop('must provide data explicitly'), ...) {
  as.fmx.Skew.normal(object, data = data, ...) |> 
    logLik.fmx()
}

#' @importFrom fmx logLik.fmx
#' @export
logLik.Skew.t <- function(object, data = stop('must provide data explicitly'), ...) {
  as.fmx.Skew.t(object, data = data, ...) |> 
    logLik.fmx()
}

