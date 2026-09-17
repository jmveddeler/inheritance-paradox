# Concentration sweep under the GRADIENT scenario, where Gini(NWX) is valid in
# all 18 countries, plus a second allocation rule (proportional to NWX) and the
# key comparison: can the Gini tell the ACTUAL allocation from a lump sum?
suppressMessages({library(dplyr)})
source("R/00_prepped_contract.R"); source("R/measures_battery.R")
prepped <- readRDS("results/hfcs_w50_prepped.rds")
QS <- c(100, 50, 20, 10, 5, 1)

.rate <- function(g) c("0"=-0.05,"1"=-0.05,"2"=-0.02,"3"=0.00,"4"=0.02,"5"=0.05,"6"=0.075)[as.character(g)]
.grp <- function(nwx, w) {
  g <- integer(length(nwx)); pos <- is.finite(nwx) & nwx > 0 & is.finite(w) & w > 0
  if (sum(pos) < 7) { g[pos] <- 3L; return(g) }
  br <- cummax(Hmisc::wtd.quantile(nwx[pos], weights = w[pos], probs = c(.2,.4,.6,.8,.95)))
  g[pos] <- as.integer(cut(nwx[pos], breaks = unique(c(-Inf, br, Inf)), labels = FALSE)); g
}
.recap <- function(wt, cpi, r) {
  h <- ifelse(wt > 0 & cpi > 0, log(wt/cpi)/0.03, 0); ifelse(cpi > 0, cpi*exp(r*h), 0)
}
alloc_top <- function(nwx, w, V, q) {
  o <- order(nwx, decreasing = TRUE); cw <- cumsum(w[o])/sum(w)
  it <- rep(FALSE, length(nwx)); it[o[cw <= q/100]] <- TRUE
  if (!any(it)) it[o[1]] <- TRUE
  ifelse(it, V/sum(w[it]), 0)
}

res <- list()
for (ctry in names(prepped$data)) {
  d <- prepped$data[[ctry]]; d <- d[d$implicate == min(d$implicate), ]
  nw <- d$nw; w <- d$w
  wt <- .recap(d$wt, d$wt_cpi_adj, .rate(.grp(d$nwx, d$w)))   # gradient transfer stock
  nwx <- nw - wt
  ok <- is.finite(nwx) & is.finite(nw) & is.finite(w) & w > 0
  nwx <- nwx[ok]; nw <- nw[ok]; w <- w[ok]; wt <- wt[ok]
  V <- sum(wt*w); g0 <- mb_gini(nwx, w)$value
  r <- list(country = ctry, gini_nwx = g0, mu_ratio = .mb_mu_ratio(nwx, w),
            actual = mb_gini(nwx + wt, w)$value / g0)
  for (q in QS) r[[paste0("q",q)]] <- mb_gini(nwx + alloc_top(nwx,w,V,q), w)$value / g0
  o <- order(nwx, decreasing = TRUE); cw <- cumsum(w[o])/sum(w)
  r$share_top10 <- sum(wt[o[cw<=.10]]*w[o[cw<=.10]])/V
  r$share_top20 <- sum(wt[o[cw<=.20]]*w[o[cw<=.20]])/V
  res[[ctry]] <- as.data.frame(r, stringsAsFactors = FALSE)
}
R <- bind_rows(res)
R$valid <- is.finite(R$gini_nwx) & R$gini_nwx > 0 & R$gini_nwx <= 1 & R$mu_ratio > 0
V <- R[R$valid, ]
cat("\n=== GRADIENT scenario:", nrow(V), "of", nrow(R), "countries have a valid Gini(NWX) ===\n\n")
cat("Countries where the Gini still calls it EQUALISING, by concentration:\n")
for (q in QS) {
  v <- V[[paste0("q",q)]]
  cat(sprintf("  volume to the top %3d%%: %2d/%d equalising   median ratio %.3f\n",
              q, sum(v<1,na.rm=TRUE), nrow(V), median(v,na.rm=TRUE)))
}
cat(sprintf("\n  ACTUAL allocation:      %2d/%d equalising   median ratio %.3f\n",
            sum(V$actual<1,na.rm=TRUE), nrow(V), median(V$actual,na.rm=TRUE)))
cat("\n=== CAN THE GINI TELL THE ACTUAL ALLOCATION FROM A UNIVERSAL LUMP SUM? ===\n")
d <- V$actual - V$q100
cat(sprintf("  median |actual - lump sum| in the Gini ratio: %.4f\n", median(abs(d),na.rm=TRUE)))
cat(sprintf("  max    |actual - lump sum|:                   %.4f  (%s)\n",
            max(abs(d),na.rm=TRUE), V$country[which.max(abs(d))]))
cat(sprintf("  countries where they differ by more than 0.05: %d of %d\n",
            sum(abs(d)>0.05,na.rm=TRUE), nrow(V)))
cat("\n=== how concentrated IS the actual allocation? ===\n")
cat(sprintf("  median share of transfer volume to the top 10%% by NWX: %.1f%%  (population share 10%%)\n",
            100*median(V$share_top10,na.rm=TRUE)))
cat(sprintf("  median share to the top 20%%:                           %.1f%%  (population share 20%%)\n",
            100*median(V$share_top20,na.rm=TRUE)))
write.csv(R, "results/hfcs_w50/hfcs_w50_21_concentration_gradient.csv", row.names = FALSE)
cat("\nwrote results/hfcs_w50/hfcs_w50_21_concentration_gradient.csv\n")
