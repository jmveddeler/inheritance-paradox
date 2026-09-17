# ============================================================
# 01b_hfcs_w15_bonke_replication.R
# Bönke, von Werder & Westermeier — Table 2, replicated on HFCS Wave 1.5
# ============================================================
# Replaces an earlier replication script, which left two steps unimplemented
# and rested one on a false premise (see below).
#
# TARGET: Table 2, "Contribution of inheritances to overall wealth inequality",
# in the FU Berlin Discussion Paper 2016/26 (Nov 2016). Published as
# Economics Letters 159 (2017), 217-220, doi:10.1016/j.econlet.2017.08.007 —
# cite the 2017 version; the DP is kept for the appendix tables, which did not
# survive the 15 -> 4 page cut.
#
# THREE KNOWN, DOCUMENTED LIMITS ON HOW CLOSE THIS CAN GET:
#
#   1. VINTAGE. Bönke used UDB 1.0 (Apr 2013) — his citation is "HFCS (2013)"
#      and the DP predates UDB 1.2. Only UDB 1.5 (Mar 2020) is now obtainable.
#      Four sub-releases intervene, two of which revised PORTUGAL specifically
#      (1.3 corrected HD0601-0603; 1.4 added HB0100). Expect PT to differ.
#
#   2. EAST GERMANY. Bönke keeps West Germany only. UDB 1.5 has NO region
#      variable of any kind, so the exclusion cannot be reproduced. DE here is
#      all-Germany and will overstate dispersion. Do not chase it.
#
#   3. SPAIN'S REFERENCE YEAR — tested here rather than assumed. UDB 1.5 split
#      Spain: "ES" is now 2011, and the 2008 data Bönke used moved to code
#      "E1". The archived script used "ES" and left the switch as a TODO, yet
#      still matched Bönke's Spanish CV(NW) to four significant figures, which
#      is not plausible across different reference years. So this script runs
#      BOTH and reports which one actually matches. That settles it empirically.
#
# Output:
#   results/bonke_replication/hfcs_w15_bonke_prepped.rds      (prepped object, cached)
#   results/bonke_replication/hfcs_w15_bonke_table2.csv       (our replication)
#   results/bonke_replication/hfcs_w15_bonke_comparison.csv   (ours vs Bönke, with deviations)
# ============================================================

source("R/00_prepped_contract.R")
suppressMessages(library(haven))

DATA_DIR <- file.path(
  "Data", "data-raw", "HFCS",
  "DG-S - HFCS - Data dissemination - All countries",
  "HFCS_UDB_1_5"
)

SURVEY_YEAR  <- 2010L   # Wave 1 fieldwork 2009/10
REAL_RATE    <- 0.03    # Bönke fn 4
MIN_YEAR_CAP <- 1960L   # Bönke fn 4: pre-1960 treated as received in 1960
AGE_MIN      <- 21L     # Bönke fn 3

# Bönke's eight, PLUS both Spanish codes so the reference-year question is
# answered by the data rather than by inference.
COUNTRIES <- c("AT", "BE", "CY", "DE", "ES", "E1", "FR", "GR", "PT")

CPI_RDS <- "results/cpi/cpi_wb_2010base_hfcs.rds"   # includes LU and the E1 alias
if (!file.exists(CPI_RDS)) {
  stop("Missing ", CPI_RDS, " — run R/util_extend_cpi_hfcs.R first.")
}

# --- Bönke's published Table 2 (transcribed from the DP, verified 2026-08-15) -
# Columns in the paper are Austria, Belgium, Cyprus, France, Germany, Greece,
# Portugal, Spain, US. The US column is Wolff (2015), not his own computation,
# so it is not a replication target and is omitted here.
# "E1" carries Spain's row: in UDB 1.5 it IS Bönke's Spanish sample (2008), so
# his ES column is the comparison target for both codes. Including both rows
# keeps E1 in the inner_join below — omitting it silently dropped E1 from the
# comparison on the first run even though it built correctly.
BONKE <- data.frame(
  country   = c("AT", "BE", "CY", "FR", "DE", "GR", "PT", "ES", "E1"),
  cv_nw_b   = c(2.926, 1.625, 2.478,  3.582, 2.826,  1.271,   3.767,  4.062,  4.062),
  cv_nwx_b  = c(12.265, 6.760, 3.967, 105.574, 7.113, -7.683, -93.875, 14.143, 14.143),
  cv_wt_b   = c(4.379, 16.395, 6.121, 12.835, 3.325,  5.823,  26.928, 16.288, 16.288),
  p1_b      = c(0.324, 0.712, 0.732,  0.111, 0.418, -3.100,  -0.411,  0.565,  0.565),
  p2_b      = c(0.676, 0.288, 0.268,  0.889, 0.582,  4.100,   1.411,  0.435,  0.435),
  cor_b     = c(-0.645, -0.942, -0.522, -0.952, -0.400, -0.999, -0.995, -0.805, -0.805),
  stringsAsFactors = FALSE
)

message("=== Bönke Wave 1.5 replication ===")
message("Countries: ", paste(COUNTRIES, collapse = " "),
        "  (both ES and E1 — see header note 3)")
t0 <- Sys.time()

prepped <- build_prepped(
  source           = "hfcs",
  data_dir         = DATA_DIR,
  countries        = COUNTRIES,
  cpi_rds          = CPI_RDS,
  survey_year      = SURVEY_YEAR,
  rate             = REAL_RATE,
  min_year         = MIN_YEAR_CAP,
  age_min          = AGE_MIN,
  load_rep_weights = FALSE,    # Bönke bootstraps; we report point estimates
  # OFF for replication. Our stable-era guard suppresses CPI deflation before
  # the year a country's raw CPI reaches 10 — GR 1984, PT 1980 — which is
  # exactly the 1970s-80s hyperinflation Bönke's footnote 7 says produces his
  # extreme Greek and Portuguese values. With the guard on, the first run gave
  # GR p2 0.360 against his 4.100 and PT 0.164 against 1.411. It stays ON for
  # our own analysis.
  stable_era_guard = FALSE,
  # ON for replication — this is a Bönke-comparability run, and dropping East
  # Germany is his convention. It has NO EFFECT on W1.5, which carries no
  # region variable at all, so DE remains all-Germany here regardless; the
  # builder warns. Stated explicitly anyway so the intent is on the record and
  # this call stays correct if it is ever pointed at a later wave.
  # The project default is now all-Germany; see R/00_prepped_contract.R.
  west_germany_only = TRUE
)

message("Built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min.")
dir.create("results", showWarnings = FALSE)
saveRDS(prepped, "results/bonke_replication/hfcs_w15_bonke_prepped.rds")

# --- Health check: did every requested country come back? --------------------
returned <- sub("_[0-9]{4}$", "", names(prepped$data))
absent   <- setdiff(COUNTRIES, returned)
if (length(absent) > 0) {
  warning("Requested but ABSENT: ", paste(absent, collapse = ", "), call. = FALSE)
}
message("Returned: ", paste(sort(returned), collapse = " "))

# --- Wolff (1987) CV² decomposition, per country x implicate -----------------
decomp <- bind_rows(prepped$data) |>
  group_by(country, implicate) |>
  summarise(
    n          = n(),
    mean_nw    = weighted.mean(nw,  w),
    mean_wt    = weighted.mean(wt,  w),
    mean_nwx   = weighted.mean(nwx, w),
    var_nw     = sum(w * (nw  - mean_nw )^2) / sum(w),
    var_wt     = sum(w * (wt  - mean_wt )^2) / sum(w),
    var_nwx    = sum(w * (nwx - mean_nwx)^2) / sum(w),
    cov_nwx_wt = sum(w * (nwx - mean_nwx) * (wt - mean_wt)) / sum(w),
    .groups = "drop"
  ) |>
  mutate(
    cv_nw      = sqrt(var_nw)  / mean_nw,
    cv_nwx     = sqrt(var_nwx) / mean_nwx,
    cv_wt      = sqrt(var_wt)  / mean_wt,
    p1         = mean_nwx / mean_nw,
    p2         = mean_wt  / mean_nw,
    cor_nwx_wt = cov_nwx_wt / (sqrt(var_nwx) * sqrt(var_wt))
  )

# Rubin point estimate = mean across implicates.
# Form each ratio WITHIN the implicate, then average — a ratio of means is
# not a mean of ratios.
table2 <- decomp |>
  group_by(country) |>
  summarise(across(c(n, cv_nw, cv_nwx, cv_wt, p1, p2, cor_nwx_wt), mean),
            .groups = "drop") |>
  arrange(country)

write.csv(table2, "results/bonke_replication/hfcs_w15_bonke_table2.csv", row.names = FALSE)

# --- Compare against Bönke ---------------------------------------------------
cmp <- table2 |>
  mutate(iso = sub("_[0-9]{4}$", "", country)) |>
  inner_join(BONKE, by = c("iso" = "country")) |>
  mutate(
    d_cv_nw_pct = round(100 * (cv_nw - cv_nw_b) / cv_nw_b, 1),
    d_p2        = round(p2 - p2_b, 3),
    d_cor       = round(cor_nwx_wt - cor_b, 3)
  ) |>
  select(iso, n, cv_nw, cv_nw_b, d_cv_nw_pct, p1, p1_b, p2, p2_b,
         cor_nwx_wt, cor_b, d_cor)

write.csv(cmp, "results/bonke_replication/hfcs_w15_bonke_comparison.csv", row.names = FALSE)

message("\n=== OURS vs BÖNKE ===")
print(as.data.frame(cmp |> mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

# --- The Spain question ------------------------------------------------------
message("\n=== SPAIN: which reference year did Bönke use? ===")
es_rows <- cmp |> filter(iso %in% c("ES", "E1"))
if (nrow(es_rows) == 2) {
  for (i in seq_len(nrow(es_rows))) {
    r <- es_rows[i, ]
    message(sprintf("  %s (%s): CV(NW) %.4f vs Bönke 4.062  ->  %+.1f%%",
                    r$iso,
                    if (r$iso == "E1") "2008 data" else "2011 data",
                    r$cv_nw, r$d_cv_nw_pct))
  }
  winner <- es_rows$iso[which.min(abs(es_rows$d_cv_nw_pct))]
  message("  => closer match: ", winner,
          if (winner == "E1") "  (2008 — as the paper implies)"
          else "  (2011 — which would be surprising; investigate)")
} else {
  message("  Only one Spanish code returned — cannot compare.")
}

message("\n⚠️ Expected deviations, documented in §3a — do NOT treat as errors:")
message("   DE: East Germany cannot be excluded (no region variable in UDB 1.5)")
message("   PT: UDB 1.3/1.4 revised Portuguese wealth variables after Bönke's 1.0")
message("\nSaved: results/bonke_replication/hfcs_w15_bonke_{prepped.rds,table2.csv,comparison.csv}")
