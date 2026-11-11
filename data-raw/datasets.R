## Datasets of kedd: samples of size 200 from five normal mixtures of
## Marron and Wand (1992), "Exact mean integrated squared error",
## The Annals of Statistics, 20, 712--736.
##
## The files data/*.rda contain the samples distributed since version 1 of the
## package; they are kept unchanged so that results published with earlier
## versions remain reproducible. This script documents the mixtures and shows how
## samples of the same kind are generated (rmix() below). Run from the package
## root:  source("data-raw/datasets.R")  -- with  overwrite = TRUE  it would
## regenerate data/*.rda with new samples (not done for the released package).

## Marron-Wand mixtures: weights w, means m, standard deviations s
mixtures <- list(
  kurtotic = list(w = c(2/3, 1/3),        m = c(0, 0),          s = c(1, 1/10)),   # #4
  outlier  = list(w = c(1/10, 9/10),      m = c(0, 0),          s = c(1, 1/10)),   # #5
  bimodal  = list(w = c(1/2, 1/2),        m = c(-3/2, 3/2),     s = c(1/2, 1/2)),  # #7
  trimodal = list(w = c(9/20, 9/20, 1/10), m = c(-6/5, 6/5, 0), s = c(3/5, 3/5, 1/4)), # #9
  claw     = list(w = c(1/2, rep(1/10, 5)), m = c(0, (0:4) / 2 - 1),
                  s = c(1, rep(1/10, 5))))                                         # #10

## sample of size n from a normal mixture
rmix <- function(n, mix) {
  j <- sample.int(length(mix$w), n, replace = TRUE, prob = mix$w)
  stats::rnorm(n, mean = mix$m[j], sd = mix$s[j])
}

## density of a normal mixture (true density, for comparisons)
dmix <- function(x, mix)
  rowSums(vapply(seq_along(mix$w), function(j)
    mix$w[j] * stats::dnorm(x, mix$m[j], mix$s[j]), numeric(length(x))))

overwrite <- FALSE
if (overwrite) {
  set.seed(2013)
  for (nm in names(mixtures)) {
    assign(nm, rmix(200L, mixtures[[nm]]))
    save(list = nm, file = file.path("data", paste0(nm, ".rda")), compress = "bzip2")
  }
}
