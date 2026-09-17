# ============================================================
# test_20_measure_battery_synthetic.R
# Assertions for Ch07. Run:  Rscript R/test_20_measure_battery_synthetic.R
# ============================================================
# Closed-form checks wherever one exists, per the project rule that an identity
# which must hold analytically should be asserted rather than eyeballed. The
# synthetic data here deliberately carries negative wealth, a large zero mass in
# transfers and heavy right skew, because that is where these measures break.
# This does NOT replace the real-data run on LWS Italy - neither substitutes
# for the other.
# ============================================================

suppressMessages({library(dplyr); library(purrr)})
source("R/00_prepped_contract.R")
source("R/measures_battery.R")
src <- readLines("R/07_challenge_measure_battery.R")
eval(parse(text = paste(src[seq_len(grep("^# RUN$", src)[1] - 1L)], collapse = "\n")))

ok <- function(msg) cat("  ok   ", msg, "\n")

set.seed(20)
n   <- 4000
nw  <- c(rlnorm(round(n * 0.9), 11, 1.5), -abs(rnorm(n - round(n * 0.9), 2e4, 3e4)))
wt  <- pmax(0, rlnorm(n, 9, 2)) * rbinom(n, 1, 0.4)     # ~60% zero mass
w   <- runif(n, 200, 3000)
ex  <- list(wt = wt, wt_cpi_adj = wt * 0.8)

# --- 1. the baseline scenario must be the IDENTITY on WT ---------------------
cat("--- 1. baseline_3pct leaves WT untouched (exact identity) ---\n")
stopifnot(identical(.b_scen_wt("baseline_3pct", nw, nw - wt, w, wt, ex$wt_cpi_adj), wt))
ok("baseline_3pct returns WT unchanged, so NWX is unchanged")

# --- 2. the cap is a genuine cap ---------------------------------------------
cat("--- 2. capped_3pct: the Tiefensee/Westermeier cap actually binds ---\n")
wt_c <- .b_scen_wt("capped_3pct", nw, nw - wt, w, wt, ex$wt_cpi_adj)
stopifnot(all(wt_c <= wt + 1e-9))                       # never increases WT
stopifnot(all(wt_c <= pmax(nw, 0) + 1e-9))              # never exceeds own wealth
pos <- nw >= 0
stopifnot(all((nw - wt_c)[pos] >= -1e-9))               # closed form: NWX >= 0
stopifnot(sum(wt_c < wt - 1e-9) > 0)                    # and it binds somewhere
ok(sprintf("cap binds on %d of %d households; NWX >= 0 wherever NW >= 0",
           sum(wt_c < wt - 1e-9), n))

# --- 3. the returned vector has a FIXED, unique shape -------------------------
cat("--- 3. stat vector shape is fixed and names are unique ---\n")
r <- ch07_fn(nw, nw - wt, w, ex)
expected <- length(CH07_SCENARIOS) * (length(.ch07_shape) * 6 + 2)
stopifnot(length(r) == expected)
stopifnot(!any(duplicated(names(r))))
stopifnot(!any(is.na(names(r))), !any(names(r) == ""))
ok(sprintf("%d stats = %d scenarios x (%d measures x 6 + 2), all names unique",
           length(r), length(CH07_SCENARIOS), length(.ch07_shape)))

# --- 4. zero transfers => the two arms are the SAME distribution ------------
# Every ratio must then be exactly 1 and every validity flag must agree. This is
# the strongest closed-form check available: it exercises all 32 measures at once
# and would catch any measure that silently treats the arms asymmetrically.
cat("--- 4. with WT = 0 every measure must give ratio exactly 1 ---\n")
r0 <- ch07_fn(nw, nw, w, list(wt = rep(0, n), wt_cpi_adj = rep(0, n)))
bad <- character(0)
for (sc in CH07_SCENARIOS) for (m in .ch07_shape) {
  v  <- r0[[paste0(sc, "_", m, "_ratio")]]
  vn <- r0[[paste0(sc, "_", m, "_nw")]]
  vx <- r0[[paste0(sc, "_", m, "_nwx")]]
  if (is.finite(vn) && is.finite(vx)) {
    if (!isTRUE(all.equal(vn, vx, tolerance = 1e-10))) bad <- c(bad, paste0(sc, "/", m))
    else if (is.finite(v) && !isTRUE(all.equal(v, 1, tolerance = 1e-8)))
      bad <- c(bad, paste0(sc, "/", m, " (ratio)"))
  }
}
if (length(bad)) stop("arms differ under WT=0: ", paste(bad, collapse = ", "))
ok("all 32 measures x 3 scenarios identical across arms when WT = 0")

# --- 5. the domain flag must fire when the mean goes non-positive -------------
# This is the Austria case: mean NWX < 0 makes every RELATIVE index
# meaningless (it is what produced a Gini ratio of -0.09 in earlier results),
# while translation-invariant measures remain well defined.
cat("--- 5. domain flag fires on a negative mean, and only for relative indices ---\n")
nw_neg <- nw - 1.15 * sum(nw * w) / sum(w)              # shift the mean below zero
stopifnot(sum(nw_neg * w) / sum(w) < 0)
b <- suppressWarnings(mb_battery(nw_neg, w, kolm_scale = 1))
rownames(b) <- b$measure
rel <- c("gini", "cv", "top10", "top1", "sgini4")   # trimmed battery, 2026-08-28
stopifnot(all(!b[rel, "ok"]))
stopifnot(isTRUE(b["gini_abs", "ok"]), isTRUE(b["variance", "ok"]))
ok(sprintf("%d relative indices flagged invalid; absolute Gini and variance still valid",
           length(rel)))
stopifnot(.mb_mu_ratio(nw_neg, w) < 0)
ok("mu_ratio is negative, which is the reportable diagnostic rather than a cut-off")

# --- 6. Kolm uses ONE scale across both arms ---------------------------------
# A per-arm scale would make the two Kolm numbers incomparable, which is the
# defect this challenge was written to avoid.
cat("--- 6. both arms share a single Kolm scale ---\n")
ksc <- sqrt(mb_variance(nw, w)$value)
k_direct <- mb_kolm(nw - wt, w, kappa = 1, scale = ksc)$value
k_inrun  <- r[[paste0("baseline_3pct_kolm1_nwx")]]
stopifnot(isTRUE(all.equal(k_direct, k_inrun, tolerance = 1e-10)))
ok("Kolm(NWX) reproduces exactly when recomputed with the NW-derived scale")

# --- 7. names are identical across scenarios ---------------------------------
cat("--- 7. every scenario emits the same measure names ---\n")
nm <- lapply(CH07_SCENARIOS, function(sc)
  sub(paste0("^", sc, "_"), "", grep(paste0("^", sc, "_"), names(r), value = TRUE)))
stopifnot(length(unique(lapply(nm, sort))) == 1L)
ok("all scenarios share one name set, so nothing shifts between them")

cat("\nAll Ch07 assertions passed.\n")
