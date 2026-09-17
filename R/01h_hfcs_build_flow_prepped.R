# ============================================================
# 01h_hfcs_build_flow_prepped.R
# Nominal-transfer prepped for Challenge 14 (survey-to-macro flow coverage)
# ============================================================
# Challenge 14 reconstructs an aggregate annual inheritance FLOW from reported
# receipts and compares it against independent macro estimates. It needs the
# raw NOMINAL transfer amounts and their receipt years — pia1-4 / piy1-4 in LWS
# naming — which the standard W5.0 cache does not carry: build_prepped_hfcs()
# keeps only the capitalised aggregate `wt`. Hence this separate builder.
#
# NO CAPITALISATION, NO DEFLATION, NO CAPPING — by design, not by omission.
#   Challenge 14 compares a nominal flow received in year Y against a macro flow
#   figure for year Y. Capitalising or CPI-deflating would destroy exactly that
#   comparability, and the challenge header is explicit that this is what
#   removes the project's largest sensitivity (Ch06/Ch09) from the comparison.
#   So this script deliberately does NOT call capitalise_transfers().
#
# WAVE 5.0, AND ALL 18 TRANSFER-DATA COUNTRIES
#   W5.0 is the project's baseline wave and stays the baseline here. An earlier
#   draft of this script proposed W1.5/W4.1 instead, on the grounds that the
#   hand-collected macro comparator sits at 2010 and 2020 while W5.0 is 2023.
#   That reasoning was rejected, and correctly: the comparator's own
#   source_notes say "years are only approximate based on available data
#   +-2-3 years", so a figure labelled 2020 may in fact be 2018-2022. The
#   decade labels are not sharp enough to justify abandoning the baseline wave.
#   The right response to a thin comparator is to go and find better comparator
#   data, not to switch wave.
#
#   All 18 countries are built, not just the four that currently have a macro
#   benchmark, so that the survey side is ready the moment a new benchmark is
#   located. Countries without a comparator simply have no denominator yet;
#   their survey-side flow is still computed and reported.
#
# TRANSFER MAPPING (HFCS -> LWS pia/piy slots)
#   pia1/piy1 <- inherited share of the main residence: res_weight * HB0800,
#                received HB0700. res_weight follows the settled HB0600 rule
#                (codes 3,4 = 1.0; code 5 = 0.5; Spanish code 6 = 0.5).
#   pia2..4   <- HH0401..HH0403, received HH0201..HH0203.
#   HFCS records three non-residence transfers to LWS's four, so the residence
#   takes slot 1 and all four slots are used.
#
# Usage: Rscript R/01h_hfcs_build_flow_prepped.R
# Output: results/hfcs_flow_prepped.rds
#         results/hfcs_w50/hfcs_w50_14_survey_flow_diag.csv
# ============================================================

suppressMessages({library(haven); library(dplyr); library(purrr)})

BASE <- file.path("Data", "data-raw", "HFCS",
                  "DG-S - HFCS - Data dissemination - All countries")

# The 18 W5.0 countries carrying transfer data (matches R/01c). CZ, FI and IT
# report 0% on hh0401 and are excluded there for the same reason.
COUNTRIES <- c("AT", "BE", "CY", "DE", "EE", "ES", "FR", "GR", "HR",
               "HU", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")

WAVES <- list(
  list(id = "W5.0", dir = file.path(BASE, "HFCS_UDB_5_0"), year = 2023L)
)

AGE_MIN <- 21L   # matches the LWS Challenge 14 build, for comparability

.read_lc <- function(path) {
  x <- haven::read_dta(path)
  names(x) <- tolower(names(x))
  x
}

H_VARS <- c("sa0010", "sa0100", "hb0600", "hb0700", "hb0800",
            "hh0201", "hh0202", "hh0203", "hh0401", "hh0402", "hh0403")
D_VARS <- c("sa0010", "sa0100", "hw0010", "dhageh1", "dhageh1b", "dhregion")

build_wave <- function(wv) {
  message("=== ", wv$id, " (", wv$year, ") ===")
  if (!dir.exists(wv$dir)) {
    warning("Missing directory: ", wv$dir, call. = FALSE); return(NULL)
  }

  all_hd <- bind_rows(lapply(1:5, function(i) {
    message("  [implicate ", i, "/5] reading h", i, " + d", i, " ...")
    h <- .read_lc(file.path(wv$dir, paste0("h", i, ".dta")))
    d <- .read_lc(file.path(wv$dir, paste0("d", i, ".dta")))

    d <- d |> filter(sa0100 %in% COUNTRIES)
    h <- h |> filter(sa0010 %in% d$sa0010)

    hd <- h |>
      select(any_of(H_VARS)) |>
      left_join(d |> select(any_of(D_VARS)), by = c("sa0010", "sa0100"))

    hd <- hd |>
      mutate(age = coalesce(dhageh1, dhageh1b)) |>
      filter(age >= AGE_MIN | (is.na(dhageh1) & dhageh1b >= (AGE_MIN - 1L)))

    # ALL-GERMANY here, deliberately. Challenge 14
    # divides the survey flow by a macro aggregate, and every German macro
    # figure — the DIW flow estimates in our comparator file, national income,
    # household counts — is all-Germany. A West-only numerator over an
    # all-Germany denominator understated the coverage rate by ~22% (18.0%
    # instead of ~23%). East Germany is therefore NOT dropped. See the
    # west_germany_only flag in R/00_prepped_contract.R.

    hd |>
      mutate(
        res_weight = case_when(
          hb0600 %in% c(3, 4) ~ 1.0,
          hb0600 == 5         ~ 0.5,
          hb0600 == 6         ~ 0.5,
          TRUE                ~ 0.0
        ),
        pia1 = res_weight * hb0800,
        piy1 = hb0700,
        pia2 = hh0401, piy2 = hh0201,
        pia3 = hh0402, piy3 = hh0202,
        pia4 = hh0403, piy4 = hh0203,
        implicate = i
      ) |>
      # A zero-weighted residence is "not inherited", not a zero-value
      # inheritance: blank it so it cannot enter the transfer count.
      mutate(pia1 = if_else(res_weight > 0, pia1, NA_real_),
             piy1 = if_else(res_weight > 0, as.numeric(piy1), NA_real_)) |>
      transmute(
        hid = as.character(sa0010), country = sa0100,
        implicate, w = hw0010, year = wv$year,
        # CONTRACT PLACEHOLDERS. Challenge 14 reads only the nominal pia/piy
        # columns — it never touches nw, nwx or wt. But validate_prepped()
        # requires all three to be present, so they are carried as zeroes rather
        # than NA. `wt` was originally omitted, which is what made Ch14 fail on
        # HFCS with a `validate_prepped()` error that looked like a data problem
        # and was really a missing column.
        # These three are NOT meaningful here. Nothing downstream may read
        # them from this cache; use results/hfcs_w50_prepped.rds for that.
        nw = 0, nwx = 0, wt = 0,
        pia1, pia2, pia3, pia4,
        piy1 = as.numeric(piy1), piy2 = as.numeric(piy2),
        piy3 = as.numeric(piy3), piy4 = as.numeric(piy4)
      )
  }))

  out <- split(all_hd, all_hd$country)
  names(out) <- paste0(names(out), "_", wv$year)
  out
}

data_list <- flatten(compact(lapply(WAVES, build_wave)))
if (length(data_list) == 0) stop("No waves built.")

# --- Survey-side diagnostic --------------------------------------------------
# Reported here (not only inside Challenge 14) so that the magnitude a macro
# benchmark would have to match is visible while hunting for benchmarks.
# flow_2018_2022_bn annualises transfers received in the five years centred on
# 2020 — the window that lines up with a comparator labelled "2020" once its
# own +-2-3 year approximation is taken into account.
message("\nBuilt ", length(data_list), " country-waves.\n")
diag <- map_dfr(names(data_list), function(k) {
  df  <- data_list[[k]] |> filter(implicate == 1)
  amt <- unlist(df[paste0("pia", 1:4)], use.names = FALSE)
  yr  <- unlist(df[paste0("piy", 1:4)], use.names = FALSE)
  wp  <- rep(df$w, times = 4)
  ok  <- is.finite(amt) & amt > 0 & is.finite(wp) & wp > 0
  win <- ok & is.finite(yr) & yr >= 2018 & yr <= 2022
  tibble(
    country            = df$country[1],
    n_hh               = nrow(df),
    hh_millions        = round(sum(df$w, na.rm = TRUE) / 1e6, 2),
    n_transfers        = sum(ok),
    pct_dated          = round(mean(is.finite(yr[ok])), 3),
    n_transfers_1822   = sum(win),
    flow_2018_2022_bn  = round(sum(amt[win] * wp[win]) / 5 / 1e9, 3)
  )
}) |> arrange(country)

print(as.data.frame(diag), row.names = FALSE)
message("\n⚠️ hh_millions should approximate the country's HOUSEHOLD COUNT in ",
        "millions. HFCS hw0010 is already a grossing weight, so Challenge 14's ",
        "`_prepped_` columns are the correct ones for HFCS and its `_rawwt_` ",
        "columns (w/100, an LWS convention) are spurious here.")

dir.create("results/hfcs_w50", showWarnings = FALSE, recursive = TRUE)
write.csv(diag, "results/hfcs_w50/hfcs_w50_14_survey_flow_diag.csv",
          row.names = FALSE)
saveRDS(list(data = data_list, rep_weights = NULL), "results/hfcs_flow_prepped.rds")
message("\nWrote results/hfcs_flow_prepped.rds")
