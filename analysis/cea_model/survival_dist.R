# =============================================================================
# survival_dist.R — Six parametric survival families, each calibrated to the
# same published anchor points used for the base-case Weibull (METHODS_LOG.md
# Section 10). Used by 09_scenario_distributions.R for the extrapolation
# sensitivity analysis. Guarded by tests/test_survival_dist.R (TDD).
#
# Families: exp, weibull, lnorm, llogis, gompertz, gengamma
# Gompertz and generalised gamma use flexsurv's parameterisations.
# =============================================================================

if (!requireNamespace("flexsurv", quietly = TRUE)) {
  stop("flexsurv package required for gompertz/gengamma survival functions")
}

# --- S(t) for each family, vectorised over t --------------------------------
dist_S <- function(family, t, params) {
  switch(family,
    exp      = exp(-params$rate * t),
    weibull  = pweibull(t, shape = params$shape, scale = params$scale, lower.tail = FALSE),
    lnorm    = plnorm(t, meanlog = params$mulog, sdlog = params$sigmalog, lower.tail = FALSE),
    llogis   = 1 / (1 + (t / params$scale)^params$shape),
    gompertz = flexsurv::pgompertz(t, shape = params$shape, rate = params$rate, lower.tail = FALSE),
    gengamma = flexsurv::pgengamma(t, mu = params$mu, sigma = params$sigma, Q = params$Q, lower.tail = FALSE),
    stop("unknown family: ", family)
  )
}

# --- number of free parameters per family -----------------------------------
n_params <- function(family) {
  switch(family, exp = 1L, weibull = 2L, lnorm = 2L, llogis = 2L,
         gompertz = 2L, gengamma = 3L, stop("unknown family: ", family))
}

# --- default initial values (natural scale), used as optimiser starting point ---
default_init <- function(family) {
  switch(family,
    exp      = list(rate = 0.05),
    weibull  = list(shape = 1.4, scale = 15),
    lnorm    = list(mulog = 2.5, sigmalog = 0.8),
    llogis   = list(shape = 1.5, scale = 12),
    gompertz = list(shape = 0.02, rate = 0.04),
    gengamma = list(mu = 2.5, sigma = 0.8, Q = 0.3),
    stop("unknown family: ", family)
  )
}

# --- transform natural params <-> unconstrained optimiser space -------------
# Positive params optimised in log space; unconstrained params (mulog, shape
# in gompertz, Q in gengamma) optimised directly.
to_unconstrained <- function(family, params) {
  switch(family,
    exp      = c(log_rate = log(params$rate)),
    weibull  = c(log_shape = log(params$shape), log_scale = log(params$scale)),
    lnorm    = c(mulog = params$mulog, log_sigmalog = log(params$sigmalog)),
    llogis   = c(log_shape = log(params$shape), log_scale = log(params$scale)),
    gompertz = c(shape = params$shape, log_rate = log(params$rate)),
    gengamma = c(mu = params$mu, log_sigma = log(params$sigma), Q = params$Q),
    stop("unknown family: ", family)
  )
}

to_natural <- function(family, theta) {
  switch(family,
    exp      = list(rate = exp(theta[["log_rate"]])),
    weibull  = list(shape = exp(theta[["log_shape"]]), scale = exp(theta[["log_scale"]])),
    lnorm    = list(mulog = theta[["mulog"]], sigmalog = exp(theta[["log_sigmalog"]])),
    llogis   = list(shape = exp(theta[["log_shape"]]), scale = exp(theta[["log_scale"]])),
    gompertz = list(shape = theta[["shape"]], rate = exp(theta[["log_rate"]])),
    gengamma = list(mu = theta[["mu"]], sigma = exp(theta[["log_sigma"]]), Q = theta[["Q"]]),
    stop("unknown family: ", family)
  )
}

# --- SSE objective: distance between S(anchors$t; params) and anchors$S ----
anchor_objective <- function(theta, family, anchors) {
  params <- to_natural(family, theta)
  Sv <- dist_S(family, anchors$t, params)
  sum((Sv - anchors$S)^2)
}

# --- fit a family's parameters to a set of (t, S) anchor points ------------
# 1-parameter families (exp) use optimize(); multi-parameter families use
# Nelder-Mead with a refinement pass for robustness, matching the procedure
# used to derive the base-case Weibull fit (direct SSE minimisation).
fit_anchors <- function(family, anchors, init = NULL) {
  anchors <- list(t = as.numeric(anchors$t), S = as.numeric(anchors$S))
  if (is.null(init)) init <- default_init(family)
  theta0 <- to_unconstrained(family, init)

  if (length(theta0) == 1L) {
    nm <- names(theta0)
    opt <- optimize(function(x) {
      th <- x; names(th) <- nm
      anchor_objective(th, family, anchors)
    }, interval = c(theta0 - 20, theta0 + 20), tol = 1e-12)
    theta_hat <- opt$minimum; names(theta_hat) <- nm
    sse <- opt$objective
  } else {
    obj <- function(th) anchor_objective(th, family, anchors)
    opt <- optim(theta0, obj, method = "Nelder-Mead",
                 control = list(maxit = 5000, reltol = 1e-14))
    opt2 <- optim(opt$par, obj, method = "Nelder-Mead",
                  control = list(maxit = 5000, reltol = 1e-14))
    theta_hat <- opt2$par; sse <- opt2$value
  }

  list(params = to_natural(family, theta_hat), sse = sse)
}

# --- restricted mean survival time: integral of S(t) dt from 0 to tmax -----
rmst <- function(family, params, tmax) {
  integrate(function(t) dist_S(family, t, params), lower = 0, upper = tmax,
            subdivisions = 2000L, rel.tol = 1e-8)$value
}

# --- landmark survival at a given time (vectorised) -------------------------
landmark <- function(family, params, t) dist_S(family, t, params)
