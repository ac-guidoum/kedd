## kedd hex logo (ggplot2 + showtext + magick). Run from the package root:
##   source("data-raw/logo.R")
## Writes man/figures/logo.png (README, help) and vignettes/logo.png (vignette).
suppressMessages({ library(ggplot2); library(showtext) })
font_add("segoe", regular = "C:/Windows/Fonts/segoeui.ttf", bold = "C:/Windows/Fonts/segoeuib.ttf")
showtext_auto(); showtext_opts(dpi = 600)

out <- "man/figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

col_bg <- "#14213D"; col_border <- "#E9A23B"; col_fill <- "#2EC4B6"
col_d1 <- "#E9A23B";  col_d2 <- "#FF6B6B";   col_txt <- "#FFFFFF"

## pointy-top hexagon centred at (1, 1), circumradius 1
hexagon <- function(r = 1) {
  a <- (90 + 60 * 0:6) * pi / 180
  data.frame(x = 1 + r * cos(a), y = 1 + r * sin(a))
}

## a few observations, their Gaussian kernels, the estimate and its derivatives
obs <- c(-1.9, -1.35, -1.0, -0.6, 0.55, 1.05, 1.4, 2.0)
h   <- 0.45
t   <- seq(-3.4, 3.4, length.out = 600)
Kmat <- sapply(obs, function(o) dnorm((t - o) / h) / (length(obs) * h))
f0 <- rowSums(Kmat)
u  <- outer(t, obs, "-") / h
f1 <- rowSums(-u * dnorm(u)) / (length(obs) * h^2)
f2 <- rowSums((u^2 - 1) * dnorm(u)) / (length(obs) * h^3)

## map the plot window [-3.4, 3.4] x curves into the upper part of the hexagon
X  <- function(v) 1 + v / 3.4 * 0.62
y0 <- 1.15                                  # baseline of the density
sc <- 0.46 / max(f0)
Y  <- function(v, k = sc) y0 + v * k
kern <- do.call(rbind, lapply(seq_along(obs), function(i)
  data.frame(x = X(t), y = Y(Kmat[, i]), g = i)))
dens <- data.frame(x = X(t), ymin = y0, ymax = Y(f0))
d1   <- data.frame(x = X(t), y = Y(f1, 0.17 / max(abs(f1))))
d2   <- data.frame(x = X(t), y = Y(f2, 0.12 / max(abs(f2))))
rug  <- data.frame(x = X(obs))

p <- ggplot() +
  geom_polygon(data = hexagon(), aes(x, y), fill = col_bg, colour = NA) +
  geom_ribbon(data = dens, aes(x, ymin = ymin, ymax = ymax), fill = col_fill, alpha = 0.30) +
  geom_line(data = kern, aes(x, y, group = g), colour = col_fill, linewidth = 0.35, alpha = 0.75) +
  geom_line(data = transform(dens, y = ymax), aes(x, y), colour = col_fill, linewidth = 1.1) +
  geom_line(data = d1, aes(x, y), colour = col_d1, linewidth = 0.8) +
  geom_line(data = d2, aes(x, y), colour = col_d2, linewidth = 0.6, linetype = "22") +
  geom_segment(aes(x = X(-3.4), xend = X(3.4), y = y0, yend = y0), colour = "#FFFFFF40", linewidth = 0.3) +
  geom_segment(data = rug, aes(x = x, xend = x, y = y0 - 0.045, yend = y0 - 0.005),
               colour = col_txt, linewidth = 0.45) +
  annotate("text", x = 1, y = 0.68, label = "kedd", family = "segoe", fontface = "bold",
           colour = col_txt, size = 7.8) +
  annotate("text", x = 1, y = 0.425, label = "f  \u00b7  f\u2032  \u00b7  f\u2033",
           family = "segoe", colour = col_border, size = 1.8) +
  annotate("text", x = 1, y = 0.305, label = "kernel density & derivatives",
           family = "segoe", colour = "#CFD8E3", size = 1.05) +
  geom_polygon(data = hexagon(0.975), aes(x, y), fill = NA, colour = col_border, linewidth = 2.2) +
  coord_fixed(xlim = c(1 - cos(pi / 6), 1 + cos(pi / 6)), ylim = c(0, 2), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = margin(0, 0, 0, 0))

w <- 2 * cos(pi / 6)   # 1.732 x 2 ratio
ggsave(file.path(out, "logo.png"), p, width = w, height = 2, dpi = 600, bg = "transparent")

## light copy for the vignette (240 x 278 px)
img <- magick::image_read(file.path(out, "logo.png"))
magick::image_write(magick::image_resize(img, "x278"), "vignettes/logo.png")
print(magick::image_info(img)[, c("width", "height", "filesize")])
