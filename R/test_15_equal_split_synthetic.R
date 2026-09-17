# Local smoke-test for R/15_challenge_equal_split_counterfactual.R on SYNTHETIC
# data. No HFCS/LWS microdata is touched. Purpose: catch bugs before spending a
# LISSY round-trip, and verify the closed-form CV identity actually holds in
# the code as written.
# Run from the project root: Rscript R/<this file>
suppressPackageStartupMessages(source("R/00_prepped_contract.R"))

set.seed(42)
mk_country <- function(ctry, yr, n = 1500, n_imp = 5) {
  purrr::map_dfr(seq_len(n_imp), function(m) {
    nwx <- rlnorm(n, 11, 1.6) - rlnorm(n, 8, 1.2)      # some negatives, like real NWX
    recv <- rbinom(n, 1, 0.30)
    wt   <- recv * rlnorm(n, 10, 1.5)
    tibble::tibble(
      country = ctry, year = yr, implicate = m,
      hid = as.character(seq_len(n)),
      nw = nwx + wt, nwx = nwx, wt = wt,
      wt_cpi_adj = ifelse(wt > 0, wt / exp(0.03 * runif(n, 1, 40)), 0),
      w = runif(n, 50, 500),
      nhhmem = sample(1:5, n, TRUE, prob = c(.3, .3, .2, .15, .05))
    )
  })
}

prepped <- list(
  data = list(XX_2011 = mk_country("XX", 2011), YY_2014 = mk_country("YY", 2014)),
  rep_weights = NULL
)

cat("--- running challenge 15 on synthetic data ---\n")
out <- try(source("R/15_challenge_equal_split_counterfactual.R", echo = FALSE), silent = TRUE)
if (inherits(out, "try-error")) {
  cat("\n*** SCRIPT ERROR ***\n", attr(out, "condition")$message, "\n")
  quit(status = 1)
}

cat("\n================ TEST ASSERTIONS ================\n")
ok <- TRUE

# 1. closed form: CV ratio under equal_hh must equal p1 exactly
cv_err <- max(abs(eqsplit_validation$err[eqsplit_validation$measure == "cv"]), na.rm = TRUE)
cat(sprintf("1. CV closed form   max|err| = %.3e  %s\n", cv_err,
            if (cv_err < 1e-8) "PASS" else "FAIL"))
ok <- ok && cv_err < 1e-8

# 2. Gini closed form (reported, not asserted — negatives may break it)
g_err <- max(abs(eqsplit_validation$err[eqsplit_validation$measure == "gini"]), na.rm = TRUE)
cat(sprintf("2. Gini closed form max|err| = %.3e  %s\n", g_err,
            if (g_err < 1e-8) "holds" else "DOES NOT HOLD (expected w/ negatives)"))

# 3. Wolff identity must close
res <- unlist(eqsplit_wolff[grepl("identity_resid", eqsplit_wolff$stat_name), "estimate"])
cat(sprintf("3. Wolff identity   max|resid| = %.3e  %s\n", max(abs(res)),
            if (max(abs(res)) < 1e-6) "PASS" else "FAIL"))
ok <- ok && max(abs(res)) < 1e-6

# 4. equal split must preserve the aggregate volume
d <- prepped$data$XX_2011 |> dplyr::filter(implicate == 1)
v_act <- sum(d$w * d$wt)
v_hh  <- sum(d$w * .alloc_equal_hh(d$wt, d$w))
v_pc  <- sum(d$w * .alloc_equal_pc(d$wt, d$w, d$nhhmem))
cat(sprintf("4. volume preserved  hh err = %.3e  pc err = %.3e  %s\n",
            abs(v_hh / v_act - 1), abs(v_pc / v_act - 1),
            if (max(abs(c(v_hh, v_pc) / v_act - 1)) < 1e-10) "PASS" else "FAIL"))
ok <- ok && max(abs(c(v_hh, v_pc) / v_act - 1)) < 1e-10

# 5. all four counterfactual variants present, and CI blocks produced
cat(sprintf("5. allocations = %s\n", paste(sort(unique(eqsplit_measures$allocation)), collapse = ", ")))
cat(sprintf("   gap_ci rows = %d, wolff rows = %d, coverage rows = %d\n",
            nrow(eqsplit_gap_ci), nrow(eqsplit_wolff), nrow(eqsplit_coverage)))
ok <- ok && length(unique(eqsplit_measures$allocation)) == 4
cat(sprintf("   regimes = %s\n", paste(sort(unique(eqsplit_measures$regime)), collapse = ", ")))
ok <- ok && length(unique(eqsplit_measures$regime)) == 3
# `permuted` was removed 2026-08-14 — see the block comment in the challenge
# script. Its assertion is gone with it.

# 5c. capped regime must remove negative NWX where nw > 0
d1 <- prepped$data$XX_2011 |> dplyr::filter(implicate == 1)
capped_wt <- .cap_wt(d1$wt, d1$nw)
bad <- sum((d1$nw - capped_wt) < 0 & d1$nw > 0)
cat(sprintf("5c. capped: households with nwx<0 while nw>0 = %d  %s\n", bad,
            if (bad == 0) "PASS" else "FAIL"))
ok <- ok && bad == 0

# 5d. Lerman-Yitzhaki identity must close
lyr <- unlist(eqsplit_ly[grepl("identity_resid", eqsplit_ly$stat_name), "estimate"])
cat(sprintf("5d. L-Y identity   max|resid| = %.3e  %s\n", max(abs(lyr), na.rm = TRUE),
            if (max(abs(lyr), na.rm = TRUE) < 1e-8) "PASS" else "FAIL"))
ok <- ok && max(abs(lyr), na.rm = TRUE) < 1e-8

# 6. Shapley shares must sum exactly to total inequality
s_err <- max(abs(eqsplit_shapley$check_sum), na.rm = TRUE)
cat(sprintf("6. Shapley adds up  max|err| = %.3e  %s\n", s_err,
            if (s_err < 1e-10) "PASS" else "FAIL"))
ok <- ok && s_err < 1e-10

cat("\nRESULT:", if (ok) "ALL CRITICAL ASSERTIONS PASSED" else "FAILURES PRESENT", "\n")
