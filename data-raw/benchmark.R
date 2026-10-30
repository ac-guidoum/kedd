## Execution time of every exported function of kedd.
## Run from the package root:  source("data-raw/benchmark.R")
## Writes data-raw/benchmark_*.csv and prints markdown tables.
suppressMessages(library(kedd))
set.seed(2027)

## mean time per call (ms): one warm-up call, then repeat until >= min_time s
tm <- function(f, min_time = 0.3, max_rep = 200L) {
  f()
  k <- 0L; t0 <- proc.time()[["elapsed"]]
  repeat {
    f(); k <- k + 1L
    el <- proc.time()[["elapsed"]] - t0
    if (el >= min_time || k >= max_rep) break
  }
  1000 * el / k
}
null_dev <- function(f) function() { grDevices::pdf(NULL); on.exit(grDevices::dev.off()); f() }
fmt <- function(v) ifelse(v < 10, sprintf("%.2f", v), ifelse(v < 1000, sprintf("%.1f", v),
                                                           format(round(v), big.mark = ",")))
md <- function(df) {
  out <- c(paste0("| ", paste(names(df), collapse = " | "), " |"),
           paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
           apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
  cat(out, sep = "\n"); cat("\n")
}
cat(sprintf("R %s | %s | %d logical cores | OpenMP threads (nthreads = 0): %d\n\n",
            getRversion(), R.version$platform, parallel::detectCores(), Cpp_omp_threads(0L)))

## 1. functions whose cost depends on n ----------------------------------------
ns <- c(100L, 500L, 1000L, 2000L)
xs <- lapply(ns, function(n) c(rnorm(0.6 * n, -1, 0.5), rnorm(0.4 * n, 1.5, 0.7)))
by_n <- list(
  "h.amise(x)"              = function(x) h.amise(x),
  "h.ucv(x)"                = function(x) h.ucv(x),
  "h.bcv(x)  (BCV1)"        = function(x) h.bcv(x),
  "h.bcv(x, whichbcv = 2)"  = function(x) h.bcv(x, whichbcv = 2),
  "h.ccv(x)"                = function(x) h.ccv(x),
  "h.mcv(x)"                = function(x) h.mcv(x),
  "h.tcv(x)"                = function(x) h.tcv(x),
  "h.scv(x)"                = function(x) h.scv(x),
  "h.mlcv(x)"               = function(x) h.mlcv(x),
  "h.ucv(x, deriv.order = 1)" = function(x) h.ucv(x, deriv.order = 1),
  "dkde(data = x, h = 0.4)" = function(x) dkde(data = x, h = 0.4),
  "dkde(data = x)  (h by UCV)" = function(x) dkde(data = x),
  "Cpp_h_ucv(0.4, ...)  (1 evaluation)" = function(x) Cpp_h_ucv(0.4, "gaussian", 0L, x, 0L),
  "plot(<bandwidth>)  (100 h)" = local({ cache <- list(); function(x) {
      key <- as.character(length(x))
      if (is.null(cache[[key]])) cache[[key]] <<- h.ucv(x)
      null_dev(function() plot(cache[[key]]))() } }),
  "lines(<bandwidth>)  (100 h)" = local({ cache <- list(); function(x) {
      key <- as.character(length(x))
      if (is.null(cache[[key]])) cache[[key]] <<- h.bcv(x)
      lines(cache[[key]]) } })
)
t1 <- sapply(by_n, function(f) sapply(xs, function(x) suppressWarnings(tm(function() f(x)))))
t1 <- t(t1); colnames(t1) <- paste0("n = ", ns)
write.csv(t1, "data-raw/benchmark_by_n.csv")
cat("### Time per call (ms) by sample size, gaussian kernel, r = 0, nthreads = 0\n\n")
md(data.frame(Function = rownames(t1), apply(t1, 2, fmt), check.names = FALSE, row.names = NULL))

## 2. functions independent of n ---------------------------------------------
x500 <- xs[[2]]; bw <- h.ucv(x500); d <- dkde(data = x500, h = 0.4)
kf <- kernel.fun(kernel = "biweight"); kc <- kernel.conv(kernel = "biweight")
u <- seq(-3, 3, length.out = 1e5)
fixed <- list(
  "kernel.pow(2, 3)"                         = function() kernel.pow(2, 3),
  "kernel.fun(kernel = \"gaussian\")"        = function() kernel.fun(),
  "kernel.fun(kernel = \"pow(1.5,2.5)\", deriv.order = 2)" = function() kernel.fun(kernel = "pow(1.5,2.5)", deriv.order = 2),
  "kernel.conv(kernel = \"gaussian\")"       = function() kernel.conv(),
  "kernel.conv(kernel = \"logistic\")"       = function() kernel.conv(kernel = "logistic"),
  "kernel.conv(kernel = \"pow(2,3)\")"       = function() kernel.conv(kernel = "pow(2,3)"),
  "plot(<kfun>)"                             = null_dev(function() plot(kf)),
  "plot(<kconv>)"                            = null_dev(function() plot(kc)),
  "plot(<dkde>)  (n = 500)"                  = null_dev(function() plot(d)),
  "show(<bandwidth>)"                        = function() capture.output(show(bw)),
  "Cpp_kernel_deriv(\"gaussian\", u, 2)  (1e5 points)" = function() Cpp_kernel_deriv("gaussian", u, 2L),
  "Cpp_kernel_conv(\"biweight\", u, 1)  (1e5 points)"  = function() Cpp_kernel_conv("biweight", u, 1L),
  "Cpp_kernel_constants(\"pow(2,3)\", 2)"    = function() Cpp_kernel_constants("pow(2,3)", 2L),
  "Cpp_hos(\"tricube\", 1, 500)"             = function() Cpp_hos("tricube", 1L, 500L),
  "Cpp_plot_cv(\"CCV\", 200 h, n = 500)"     = function() Cpp_plot_cv("CCV", seq(0.1, 1, length.out = 200), "gaussian", 0L, x500),
  "Cpp_conv2conv(0, 0, 0, gaussian)  (1 point)" = function() Cpp_conv2conv(0, 0L, 0L, "gaussian", "gaussian", 0L),
  "Cpp_omp_threads()"                        = function() Cpp_omp_threads()
)
t2 <- sapply(fixed, function(f) tm(f))
write.csv(data.frame(fun = names(t2), ms = t2), "data-raw/benchmark_fixed.csv", row.names = FALSE)
cat("### Time per call (ms), functions independent of n\n\n")
md(data.frame(Function = names(t2), `Time (ms)` = fmt(t2), check.names = FALSE, row.names = NULL))

## 3. threads -------------------------------------------------------------------
x2k <- xs[[4]]
thr <- list(UCV = h.ucv, BCV1 = h.bcv, CCV = h.ccv, MCV = h.mcv, TCV = h.tcv,
            SCV = h.scv, MLCV = h.mlcv)
t3 <- t(sapply(thr, function(f) c(
  one = suppressWarnings(tm(function() f(x2k, nthreads = 1L))),
  all = suppressWarnings(tm(function() f(x2k, nthreads = 0L))))))
write.csv(t3, "data-raw/benchmark_threads.csv")
cat(sprintf("### nthreads = 1 vs nthreads = 0 (%d threads), n = 2000\n\n", Cpp_omp_threads(0L)))
md(data.frame(Selector = rownames(t3), `nthreads = 1 (ms)` = fmt(t3[, "one"]),
              `nthreads = 0 (ms)` = fmt(t3[, "all"]),
              Speed_up = sprintf("x %.1f", t3[, "one"] / t3[, "all"]),
              check.names = FALSE, row.names = NULL))

## 4. kernels -------------------------------------------------------------------
x1k <- xs[[3]]
ks <- c("gaussian", "epanechnikov", "uniform", "triangular", "biweight", "triweight",
        "tricube", "cosine", "silverman", "logistic", "pow(2,3)", "pow(1.5,2.5)")
t4 <- t(sapply(ks, function(k) c(
  UCV = suppressWarnings(tm(function() h.ucv(x1k, kernel = k))),
  BCV1 = suppressWarnings(tm(function() h.bcv(x1k, kernel = k))),
  dkde = tm(function() dkde(data = x1k, h = 0.4, kernel = k)))))
write.csv(t4, "data-raw/benchmark_kernels.csv")
cat("### Time per call (ms) by kernel, n = 1000, r = 0\n\n")
md(data.frame(Kernel = ks, `h.ucv` = fmt(t4[, "UCV"]), `h.bcv` = fmt(t4[, "BCV1"]),
              `dkde (h = 0.4)` = fmt(t4[, "dkde"]), check.names = FALSE, row.names = NULL))
