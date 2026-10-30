## CRAN download statistics of kedd (RStudio/Posit CRAN mirror logs).
## Run from the package root:  source("data-raw/downloads.R")
## Writes man/figures/downloads.png and data-raw/downloads_monthly.csv,
## and prints the figures quoted in README.md.
suppressMessages({ library(cranlogs); library(dlstats); library(ggplot2) })
my <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%Y"))  # locale-free

pkg   <- "kedd"
first <- as.Date("2014-08-01")            # kedd 1.0.1 released 2014-08-18
last  <- Sys.Date() - 1

## daily counts (cranlogs) ----------------------------------------------------
daily <- cran_downloads(pkg, from = first, to = last)
total <- sum(daily$count)
lm    <- sum(cran_downloads(pkg, when = "last-month")$count)
lw    <- sum(cran_downloads(pkg, when = "last-week")$count)

## monthly counts: full history from the cranlogs daily counts ----------------
daily$month <- as.Date(format(daily$date, "%Y-%m-01"))
monthly <- aggregate(count ~ month, daily, sum)
names(monthly) <- c("start", "downloads")
monthly <- monthly[monthly$start >= as.Date(format(daily$date[which(daily$count > 0)[1]], "%Y-%m-01")), ]
monthly$end <- c(monthly$start[-1] - 1, last)
monthly$cumulative <- cumsum(monthly$downloads)

## cross-check with dlstats (it only returns recent months) --------------------
dl <- cran_stats(pkg)
dl <- dl[dl$end <= last, c("start", "downloads")]
chk <- merge(monthly[, c("start", "downloads")], dl, by = "start", suffixes = c(".cranlogs", ".dlstats"))
cat(sprintf("dlstats check: %d common months (%s to %s), max |difference| = %d\n",
            nrow(chk), format(min(chk$start), "%Y-%m"), format(max(chk$start), "%Y-%m"),
            max(abs(chk$downloads.cranlogs - chk$downloads.dlstats))))
write.csv(monthly[, c("start", "end", "downloads", "cumulative")],
          "data-raw/downloads_monthly.csv", row.names = FALSE)

yearly <- aggregate(downloads ~ format(start, "%Y"), monthly, sum)
names(yearly) <- c("year", "downloads")
best <- monthly[which.max(monthly$downloads), ]

cat(sprintf("as of %s\n", format(last)))
cat(sprintf("total downloads      : %s\n", format(total, big.mark = ",")))
cat(sprintf("last month           : %s\n", format(lm, big.mark = ",")))
cat(sprintf("last week            : %s\n", format(lw, big.mark = ",")))
cat(sprintf("monthly mean / median: %.0f / %.0f\n", mean(monthly$downloads), median(monthly$downloads)))
cat(sprintf("best month           : %s (%s)\n", my(best$start), format(best$downloads, big.mark = ",")))
cat("\nper year:\n"); print(yearly, row.names = FALSE)

## figure ---------------------------------------------------------------------
sc <- max(monthly$downloads) / max(monthly$cumulative)
p <- ggplot(monthly, aes(start)) +
  geom_col(aes(y = downloads), fill = "#1FA89C", width = 25) +
  geom_line(aes(y = cumulative * sc), colour = "#E9A23B", linewidth = 1) +
  scale_y_continuous("monthly downloads", labels = scales::comma,
                     sec.axis = sec_axis(~ . / sc, name = "cumulative downloads",
                                         labels = scales::comma)) +
  scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "kedd: monthly CRAN downloads",
       subtitle = sprintf("RStudio/Posit CRAN mirror, %s to %s  |  total %s",
                          my(min(monthly$start)), paste(format(last, "%d"), my(last)),
                          format(total, big.mark = ",")),
       caption = "Source: cranlogs / dlstats") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", colour = "#14213D"),
        axis.title.y.right = element_text(colour = "#B8790F"),
        axis.title.y.left = element_text(colour = "#14736B"),
        panel.grid.minor = element_blank())
ggsave("man/figures/downloads.png", p, width = 8, height = 3.6, dpi = 150, bg = "white")
