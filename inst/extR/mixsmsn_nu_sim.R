
library(patchwork)


# nu_cand = 1:10
#' @importFrom rlang .data
smsn.mix_nu <- function(
    y, 
    nu_inits, 
    family,
    ...,
    mu, sigma2, shape, nu, pii
) {
  
  N <- length(nu_inits)
  mods_raw <- lapply(nu_inits, FUN = \(i_nu) smsn.mix(y = y, nu = i_nu, family = family, ...))
  
  mods <- lapply(mods_raw, FUN = sort) # my [sort.Normal], etc.
  
  tmp_fig <- function(par, trueval) { # (par = 'mu')
    if (!(par %in% names(mods[[1L]]))) return(invisible())
    m <- do.call(rbind, args = lapply(mods, FUN = \(i) i[[par]]))
    if (all(m == 0)) return(invisible())
    parlab <- paste(par, seq_len(ncol(m)), sep = ':')
    d <- data.frame(
      nu = nu_inits, value = c(m), txt = sprintf(fmt = '%.3f', c(m)),
      parameter = rep(parlab, each = N), 
      check.names = FALSE)
    true_d <- data.frame(value = trueval, parameter = parlab)
    ggplot() + 
      geom_line(data = d, mapping = aes(x = .data$nu, y = .data$value, group = .data$parameter, color = .data$parameter), linewidth = 1.5, linetype = 2) + 
      ggrepel::geom_label_repel(data = d, mapping = aes(x = .data$nu, y = .data$value, label = .data$txt)) + 
      geom_hline(data = true_d, mapping = aes(yintercept = .data$value, group = .data$parameter, colour = .data$parameter)) +
      labs(x = NULL, y = par)
  }
  
  
    tmp_fig('mu', trueval = mu) +
    tmp_fig('sigma2', trueval = sigma2) +
    tmp_fig('shape', trueval = shape) +
    tmp_fig('nu', trueval = nu) +
    tmp_fig('pii', trueval = pii)
  
}




?smsn.mix

mu1 <- 5; mu2 <- 20
sigma2.1 <- 9; sigma2.2 <- 16
lambda1 <- 5; lambda2 <- -3
nu = 5

mu <- c(mu1,mu2)
sigma2 <- c(sigma2.1,sigma2.2)
shape <- c(lambda1,lambda2)
pii <- c(0.7,0.3)

arg1 = c(mu1, sigma2.1, lambda1, nu)
arg2 = c(mu2, sigma2.2, lambda2, nu)
y <- rmix(n=1000, p=pii, family = "Skew.t", arg=list(arg1,arg2))

library(ggplot2)
library(grid)
theme_set(theme_bw())

grid.draw(smsn.mix_nu(y = y, nu_inits = 1:10, g = 2, family = "Normal", get.init = TRUE, criteria = TRUE, group = TRUE, calc.im = FALSE, mu = mu, sigma2 = sigma2, shape = shape, nu = nu, pii = pii))

grid.draw(smsn.mix_nu(y = y, nu_inits = 1:10, g = 2, family = "Skew.normal", get.init = TRUE, criteria = TRUE, group = TRUE, calc.im = FALSE, mu = mu, sigma2 = sigma2, shape = shape, nu = nu, pii = pii))

grid.draw(smsn.mix_nu(y = y, nu_inits = 1:10, g = 2, family = "t", get.init = TRUE, criteria = TRUE, group = TRUE, calc.im = FALSE, mu = mu, sigma2 = sigma2, shape = shape, nu = nu, pii = pii))

grid.draw(smsn.mix_nu(y = y, nu_inits = 1:10, g = 2, family = "Skew.t", get.init = TRUE, criteria = TRUE, group = TRUE, calc.im = FALSE, mu = mu, sigma2 = sigma2, shape = shape, nu = nu, pii = pii))

QLMDe(y, distname = 'GH', K = 2L)


# new simulation:
# generate with 't' and estimate with GH
# generate with 'Skew.t' and estimate with GH


# ggplot() + autolayer(dGH, g = 1:2) + xlim(-2, 4)
