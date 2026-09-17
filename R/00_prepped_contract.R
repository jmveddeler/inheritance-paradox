# ============================================================
# 00_prepped_contract.R
# Source-agnostic data contract, shared capitalisation, and
# Rubin's rules infrastructure for the inheritance paradox.
# ============================================================
#
# This file is sourced by BOTH the HFCS and LWS baseline scripts
# and by all challenge scripts. It provides:
#
#   1. capitalise_transfers()   — shared two-step CPI + real cap
#   2. build_prepped()          — dispatcher (source = "hfcs" | "lws")
#   3. rubin_combine()          — generic Rubin combiner
#   4. compute_with_rubin()     — orchestrator for Tier 1 SEs
#   5. validate_prepped()       — contract assertion helper
#
# THE CONTRACT:
#
#   build_prepped() returns a named list with two components:
#
#   $data — named list of per-country tibbles:
#     tibble(country, year, implicate, hid, nw, nwx, wt, w)
#
#   $rep_weights — named list of per-country tibbles (or NULL):
#     tibble(hid, implicate, wr0001, wr0002, ..., wrNNNN)
#
#   Columns:
#     country   : chr, ISO2 (e.g. "AT")
#     year      : int, survey/reference year
#     implicate : int, 1..M (M = 5 for HFCS and LWS)
#     hid       : chr, household ID (unique within country × implicate)
#     nw        : dbl, net worth (uncapped; may be < 0)
#     nwx       : dbl, net-of-transfer wealth = nw - wt
#     wt        : dbl, capitalised wealth transfers (uncapped)
#     w         : dbl, main household survey weight (> 0)
#
# TIERED SE APPROACH:
#   Tier 1 (reported numbers): full Rubin + replicate weights via
#     compute_with_rubin(). Used for Gini, CV, isogini at key points,
#     Lorenz difference at intersections.
#   Tier 2 (visualization): Rubin-averaged point estimates only
#     (mean across implicates, main weights). No per-point SEs.
#
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)


# === 1. SHARED CAPITALISATION =================================================

#' Two-step capitalisation of wealth transfers (Boenke et al. 2016, fn 4).
#'
#' Step 1: Deflate nominal transfer value to ref_year prices using CPI.
#' Step 2: Capitalise at a real rate from transfer year to ref_year.
#'
#' For transfers received before the country's "stable era" (CPI < 10,
#' indicating hyperinflationary original-currency period), only real
#' capitalisation is applied (no CPI deflation — the euro-denominated
#' value already reflects post-stabilisation prices).
#'
#' @param value Numeric vector of nominal transfer values (euros).
#' @param year Numeric vector of transfer years.
#' @param country_vec Character vector of ISO2 country codes.
#' @param cpi_lkp Named list of named numeric vectors: cpi_lkp[["AT"]]["1980"].
#'   Values are CPI index numbers with any base year (ratios are what matter).
#' @param stable_tbl Data frame with columns (geo, stable_from). First year
#'   where CPI >= 10 per country.
#' @param rate Real capitalisation rate per annum (default 0.03).
#' @param ref_year Target year for present values.
#' @param min_year Floor year: transfers before this are treated as received
#'   in min_year (default 1960, Boenke fn 5).
#' @return Numeric vector of capitalised present values (ref_year euros).
capitalise_transfers <- function(value, year, country_vec,
                                 cpi_lkp, stable_tbl,
                                 rate = 0.03,
                                 ref_year,
                                 min_year = 1960) {
  n  <- length(value)
  pv <- rep(0.0, n)
  yr <- pmax(year, min_year, na.rm = TRUE)

  for (ctry in unique(country_vec)) {
    idx <- which(country_vec == ctry)
    if (length(idx) == 0) next

    cpi_c   <- cpi_lkp[[ctry]]
    cpi_ref <- if (!is.null(cpi_c)) cpi_c[as.character(ref_year)] else NA_real_

    # Determine stable era start for this country
    stable_from <- if (is.data.frame(stable_tbl)) {
      sf <- stable_tbl$stable_from[stable_tbl$geo == ctry]
      if (length(sf) == 0) Inf else sf
    } else if (is.list(stable_tbl)) {
      sf <- stable_tbl[[ctry]]
      if (is.null(sf)) Inf else sf
    } else {
      Inf
    }

    vals  <- value[idx]
    yrs   <- yr[idx]
    valid <- !is.na(vals) & !is.na(year[idx]) & vals > 0

    for (j in which(valid)) {
      y <- yrs[j]
      if (!is.null(cpi_c) && !is.na(cpi_ref) && y >= stable_from) {
        # Full two-step: CPI deflation + real capitalisation
        cpi_y <- cpi_c[as.character(y)]
        if (is.na(cpi_y)) {
          avail   <- as.integer(names(cpi_c))
          nearest <- avail[which.min(abs(avail - y))]
          cpi_y   <- cpi_c[as.character(nearest)]
          y       <- nearest
        }
        pv[idx[j]] <- vals[j] * (cpi_ref / cpi_y) * exp(rate * (ref_year - y))
      } else {
        # Pre-stable era or no CPI: real capitalisation only
        pv[idx[j]] <- vals[j] * exp(rate * (ref_year - y))
      }
    }
  }
  pv
}


# === 2. CONTRACT VALIDATOR ====================================================

#' Validate that a prepped object conforms to the contract.
#' Errors informatively if anything is wrong.
#' @param prepped List with $data (and optionally $rep_weights).
validate_prepped <- function(prepped) {
  stopifnot(
    "prepped must be a list with a $data component" = is.list(prepped) && "data" %in% names(prepped)
  )

  required_cols <- c("country", "year", "implicate", "hid", "nw", "nwx", "wt", "w")

  for (nm in names(prepped$data)) {
    df <- prepped$data[[nm]]
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      stop("prepped$data[['", nm, "']] is missing columns: ",
           paste(missing, collapse = ", "))
    }
    if (any(df$w <= 0 | !is.finite(df$w))) {
      stop("prepped$data[['", nm, "']] has non-positive or non-finite weights (w).")
    }
  }

  if (!is.null(prepped$rep_weights)) {
    for (nm in names(prepped$rep_weights)) {
      rw <- prepped$rep_weights[[nm]]
      if (!all(c("hid", "implicate") %in% names(rw))) {
        stop("prepped$rep_weights[['", nm, "']] must have 'hid' and 'implicate' columns.")
      }
    }
  }

  invisible(TRUE)
}


# === 3. RUBIN'S RULES ========================================================

#' Combine point estimates and within-imputation variances via Rubin (1987).
#'
#' @param theta_m Numeric vector of per-implicate point estimates (length M).
#' @param V_m Numeric vector of per-implicate within-imputation variances
#'   (length M). If NULL, only between-imputation variance is used (equivalent
#'   to ignoring sampling variance — use for quick runs without rep weights).
#' @return Named list: estimate, se, ci_lo, ci_hi (95% CI, normal approx).
rubin_combine <- function(theta_m, V_m = NULL) {
  M <- length(theta_m)
  theta_bar <- mean(theta_m, na.rm = TRUE)
  B <- var(theta_m, na.rm = TRUE)  # between-imputation variance


  if (!is.null(V_m) && any(!is.na(V_m))) {
    W_bar <- mean(V_m, na.rm = TRUE)  # mean within-imputation variance
    T_var <- W_bar + (1 + 1 / M) * B
  } else {
    # No within-imputation variance available (no rep weights)
    # Report between-imputation component only — this UNDERSTATES total SE
    T_var <- (1 + 1 / M) * B
  }

  se <- sqrt(max(T_var, 0))
  list(
    estimate = theta_bar,
    se       = se,
    ci_lo    = theta_bar - 1.96 * se,
    ci_hi    = theta_bar + 1.96 * se
  )
}


#' Compute a statistic with full Rubin's rules + replicate-weight SEs.
#'
#' Orchestrator for Tier 1 statistics. For each country:
#'   1. Compute stat_fn per implicate (main weight) → point estimates
#'   2. Compute stat_fn per implicate × replicate weight → within-imp variance
#'   3. Combine via rubin_combine()
#'
#' @param prepped A validated prepped object (from build_prepped).
#' @param stat_fn Function with signature f(x_nw, x_nwx, w, ...) that returns
#'   a named numeric vector (e.g. c(gini_nw = 0.8, gini_nwx = 0.85)).
#'   The function receives the nw, nwx, and weight vectors for one
#'   country-implicate slice, plus any extra args via `...`.
#'   If `extra_cols` is given, stat_fn additionally receives a named list
#'   `extra` (one vector per requested column, aligned with nw/nwx/w) as
#'   its fourth argument: f(nw, nwx, w, extra, ...).
#' @param ... Extra arguments passed to stat_fn.
#' @param countries Character vector of countries to process (default: all).
#' @param extra_cols Character vector of additional prepped$data column
#'   names to pass to stat_fn (e.g. c("wt", "wt_cpi_adj") for scenario
#'   recomputation). Held fixed per implicate slice; the SAME values are
#'   passed in every replicate-weight call (standard shortcut: derived
#'   per-household variables are not re-simulated across replicates —
#'   only the weight vector varies).
#' @return Tibble with columns: country, stat_name, estimate, se, ci_lo, ci_hi.
compute_with_rubin <- function(prepped, stat_fn, ...,
                               countries = NULL,
                               extra_cols = NULL) {

  if (is.null(countries)) countries <- names(prepped$data)
  has_rw <- !is.null(prepped$rep_weights)
  has_extra <- !is.null(extra_cols) && length(extra_cols) > 0

  results <- map_dfr(countries, function(ctry) {
    df <- prepped$data[[ctry]]
    if (is.null(df) || nrow(df) == 0) return(tibble())

    if (has_extra) {
      missing_cols <- setdiff(extra_cols, names(df))
      if (length(missing_cols) > 0) {
        stop("extra_cols not found in prepped$data[['", ctry, "']]: ",
             paste(missing_cols, collapse = ", "))
      }
    }

    implicates <- sort(unique(df$implicate))
    M <- length(implicates)

    # Get replicate weight columns for this country (if available)
    rw_df <- if (has_rw) prepped$rep_weights[[ctry]] else NULL
    wr_cols <- if (!is.null(rw_df)) {
      grep("^wr", names(rw_df), value = TRUE)
    } else {
      character(0)
    }
    R <- length(wr_cols)

    # Call helper: with or without the extra list, keeping the legacy
    # signature untouched when extra_cols is not requested.
    call_stat <- function(nw, nwx, w, extra) {
      if (has_extra) stat_fn(nw, nwx, w, extra, ...) else stat_fn(nw, nwx, w, ...)
    }

    # --- Per-implicate computation ---
    imp_results <- map(implicates, function(m) {
      slice_m <- df |> filter(implicate == m)
      extra_m <- if (has_extra) as.list(slice_m[extra_cols]) else NULL

      # Point estimate with main weight
      theta_m <- call_stat(slice_m$nw, slice_m$nwx, slice_m$w, extra_m)

      # Within-implicate variance from replicate weights
      V_m <- NULL
      if (R > 0 && !is.null(rw_df)) {
        rw_m <- rw_df |> filter(implicate == m)
        # Merge replicate weights to slice by hid
        slice_rw <- slice_m |>
          left_join(rw_m |> select(hid, all_of(wr_cols)), by = "hid")
        extra_rw <- if (has_extra) as.list(slice_rw[extra_cols]) else NULL

        # Compute stat with each replicate weight
        theta_mr <- map(wr_cols, function(wr_col) {
          w_r <- slice_rw[[wr_col]]
          # Skip replicates where all weights are 0 or NA
          if (all(is.na(w_r) | w_r == 0)) return(rep(NA_real_, length(theta_m)))
          call_stat(slice_rw$nw, slice_rw$nwx, w_r, extra_rw)
        })
        theta_mr_mat <- do.call(rbind, theta_mr)  # R × K matrix

        # V_m = (1/R) × Σ_r (θ̂_mr − θ̂_m)² per stat
        V_m <- colMeans((sweep(theta_mr_mat, 2, theta_m))^2, na.rm = TRUE)
      }

      list(theta_m = theta_m, V_m = V_m)
    })

    # --- Combine across implicates via Rubin ---
    stat_names <- names(imp_results[[1]]$theta_m)
    K <- length(stat_names)

    theta_mat <- do.call(rbind, map(imp_results, "theta_m"))  # M × K
    V_mat <- if (!is.null(imp_results[[1]]$V_m)) {
      do.call(rbind, map(imp_results, "V_m"))  # M × K
    } else {
      NULL
    }

    map_dfr(seq_len(K), function(k) {
      theta_vec <- theta_mat[, k]
      V_vec <- if (!is.null(V_mat)) V_mat[, k] else NULL
      combined <- rubin_combine(theta_vec, V_vec)
      tibble(
        country   = ctry,
        stat_name = stat_names[k],
        estimate  = combined$estimate,
        se        = combined$se,
        ci_lo     = combined$ci_lo,
        ci_hi     = combined$ci_hi
      )
    })
  })

  results
}


# === 4. BUILDERS ==============================================================

#' Build a prepped object from HFCS or LWS data.
#'
#' @param source One of "hfcs" or "lws".
#' @param ... Arguments passed to the source-specific builder.
#' @return A prepped object (list with $data and $rep_weights).
build_prepped <- function(source = c("hfcs", "lws"), ...) {
  source <- match.arg(source)
  prepped <- switch(source,
    hfcs = build_prepped_hfcs(...),
    lws  = build_prepped_lws(...)
  )
  validate_prepped(prepped)
  prepped
}


# --- 4a. HFCS builder --------------------------------------------------------

#' Build prepped from HFCS Wave 4 (or other waves) local .dta files.
#'
#' @param data_dir Path to HFCS UDB directory containing h1-h5.dta, d1-d5.dta, w.dta.
#' @param countries Character vector of target ISO2 country codes.
#' @param cpi_rds Path to CPI RDS file (World Bank, columns: geo, year, cpi).
#' @param survey_year Integer reference year for capitalisation (default 2021).
#' @param rate Real capitalisation rate (default 0.03).
#' @param min_year Floor year (default 1960).
#' @param age_min Minimum age of household head (default 21, Boenke convention).
#' @param load_rep_weights Logical: load replicate weights from w.dta? (default TRUE).
#' @return A prepped object.
build_prepped_hfcs <- function(data_dir,
                               countries,
                               cpi_rds = "results/cpi/cpi_wb_2010base_18countries.rds",
                               survey_year = 2021L,
                               rate = 0.03,
                               min_year = 1960L,
                               age_min = 21L,
                               load_rep_weights = TRUE,
                               stable_era_guard = TRUE,
                               west_germany_only = FALSE) {

  if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' required for HFCS.")

  # --- CPI setup ---
  cpi_data <- readRDS(cpi_rds)
  cpi_lkp <- split(cpi_data, cpi_data$geo) |>
    lapply(function(df) setNames(df$cpi, df$year))

  # The "stable era" guard: CPI deflation is applied only from the first year a
  # country's raw CPI reaches 10, with earlier transfers getting real
  # capitalisation and NO deflation. Added for Wave 4 because Croatian and
  # Slovenian hyperinflationary-era CPI ratios otherwise inflated WT ~10^6x.
  #
  # It is a MODELLING CHOICE, not a correctness fix, and it is the reason our
  # Greek and Portuguese figures diverge from Bönke. His thresholds would
  # be GR 1984 and PT 1980; Greek CPI in 1970 is 1.38 on a 2010 base, so the
  # guard suppresses roughly a 72-fold uprating of pre-1984 transfers. Bönke
  # deliberately keeps that amplification — his footnote 7 attributes his
  # extreme p2 values to exactly this — so replicating him requires
  # stable_era_guard = FALSE.
  #
  # Default TRUE: our own analysis wants it. Set FALSE only to replicate a paper
  # that did not apply it, and say so in the output.
  stable_tbl <- if (isTRUE(stable_era_guard)) {
    cpi_data |>
      filter(cpi >= 10) |>
      group_by(geo) |>
      summarise(stable_from = min(year), .groups = "drop")
  } else {
    message("  NOTE: stable-era CPI guard DISABLED — full CPI deflation back to ",
            "the earliest available year. Use only for replication.")
    cpi_data |>
      group_by(geo) |>
      summarise(stable_from = -Inf, .groups = "drop")
  }

  # --- Process implicates ---
  message("Building prepped (HFCS, ", length(countries), " countries)...")

  # Column sets. `any_of()` rather than `select()` because the variables differ
  # across waves — `dhregion` exists from W4 onward but NOT in W1.5, and a bare
  # select() on it made this builder error outright on Bönke's own wave.
  #
  # Do NOT "optimise" these reads with haven's `col_select`. Measured
  # 2026-08-15 on h1.dta (216 MB, 974 columns): full read 31.9 s vs col_select
  # on 12 columns 193.8 s — the column filter is ~6x SLOWER, because the Stata
  # reader still walks every row and pays extra to discard. Read whole, then
  # select.
  H_VARS <- c("sa0010", "sa0100", "hb0600", "hb0700", "hb0800",
              "hh0100", "hh0201", "hh0202", "hh0203",
              "hh0401", "hh0402", "hh0403")
  # dh0001 (number of household members) added 2026-08-16 so Ch17 (unit of
  # analysis) can run on HFCS — it needs `nhhmem` to convert household weights
  # to person weights. Purely ADDITIVE: it changes no existing value, so caches
  # rebuilt with it produce identical nw/nwx/wt/w and no downstream result moves.
  # LWS's `nhhmem17` has NO exact HFCS counterpart (dh0006 is 16+, dh14p is
  # 14+), so it is deliberately NOT mapped. Ch17 skips its per-adult scale when
  # the column is absent, which is better than silently substituting a variable
  # with a different age cut and reporting it as comparable to LWS.
  D_VARS <- c("sa0010", "sa0100", "dn3001", "hw0010",
              "dhageh1", "dhageh1b", "dheduh1", "dhregion", "dh0001")

  # Case normalisation is REQUIRED, not cosmetic. UDB 1.5's `d*.dta` files
  # carry UPPERCASE names (SA0100, IM0100, DN3001) while its `h*.dta` files are
  # lowercase — and every other wave is lowercase throughout. Without this,
  # any_of() would silently drop every derived-file variable on Bönke's own
  # wave and the join would produce garbage rather than an error.
  .read_lc <- function(path) {
    x <- haven::read_dta(path)
    names(x) <- tolower(names(x))
    x
  }

  all_hd <- bind_rows(lapply(1:5, function(i) {
    message("  [implicate ", i, "/5] Reading h", i, ".dta + d", i, ".dta ...")
    h <- .read_lc(file.path(data_dir, paste0("h", i, ".dta")))
    d <- .read_lc(file.path(data_dir, paste0("d", i, ".dta")))

    if (i == 1L) {
      # Hard-fail on anything the pipeline cannot run without; only the
      # genuinely wave-varying extras are allowed to be absent.
      OPTIONAL <- c("dhregion", "dheduh1", "hh0100")
      required <- setdiff(c(H_VARS, D_VARS), OPTIONAL)
      missing_req <- setdiff(required, c(names(h), names(d)))
      if (length(missing_req) > 0) {
        stop("HFCS wave at '", data_dir, "' is missing required variables: ",
             paste(missing_req, collapse = ", "),
             ".\nCheck the wave layout — UDB 1.5 nests differently and uses ",
             "uppercase names in d*.dta.", call. = FALSE)
      }
      missing_opt <- setdiff(OPTIONAL, c(names(h), names(d)))
      if (length(missing_opt) > 0) {
        message("    note: this wave lacks ", paste(missing_opt, collapse = ", "),
                " — proceeding without")
      }
    }

    d <- d |> filter(sa0100 %in% countries)
    h <- h |> filter(sa0010 %in% d$sa0010)

    hd <- h |>
      select(any_of(H_VARS)) |>
      left_join(d |> select(any_of(D_VARS)), by = c("sa0010", "sa0100"))

    # Age filter
    hd <- hd |>
      mutate(age = coalesce(dhageh1, dhageh1b)) |>
      filter(age >= age_min | (is.na(dhageh1) & dhageh1b >= (age_min - 1L)))

    # --- East Germany: a ROBUSTNESS DIMENSION, not a constant -----------------
    # Default changed to all-Germany 2026-08-16. Previously
    # East Germany was dropped unconditionally, following Bönke.
    #
    # Bönke's rationale is sound but narrow: he capitalises historical transfers
    # forward, and pre-1990 East Germany was not a market economy (housing
    # largely state-owned, businesses nationalised), so a GDR-era transfer is
    # not commensurable with a West German one. It is the same argument he uses
    # to DELETE Slovakia and Slovenia outright, and the same concern our own
    # stable-era CPI guard addresses for Croatia and Slovenia.
    #
    # Four reasons it should not be applied blindly to our analysis:
    #   1. It is a 1990 argument applied to 2023 data. W5.0 is 33 years after
    #      reunification and the pre-1990 share of the transfer stock shrinks
    #      every wave. For Ch14, which uses only recent nominal receipts,
    #      essentially nothing in the window predates reunification.
    #   2. It is NOT distributionally neutral, and this paper is about
    #      distribution. East German households hold materially less wealth, so
    #      dropping them removes mass from the BOTTOM of the distribution —
    #      exactly where the bottom-weighted measures (Atkinson(2), MLD) that
    #      carry the headline are sensitive.
    #   3. `dhregion` exists only in W3.4/W4.1/W5.0. Applying it unconditionally
    #      made DE all-Germany in 2010 and 2014 but West-only from 2017, so the
    #      Ch08/Ch12 temporal series contained a pure sample-definition break
    #      that those challenges read as real change.
    #   4. Every German macro aggregate is all-Germany, so a West-only numerator
    #      over an all-Germany denominator understates Ch14 coverage by ~22%.
    #
    # Bönke's own replication wave (W1.5) has NO region variable at all
    # (verified 2026-08-15 across every labels_*.do), so his restriction cannot
    # be reproduced there regardless — the documented cause of our +19% DE
    # deviation. Set west_germany_only = TRUE only for explicit
    # Bönke-comparability runs, and report results both ways.
    if (isTRUE(west_germany_only)) {
      if ("dhregion" %in% names(hd)) {
        hd <- hd |> filter(!(sa0100 == "DE" & dhregion == "DEOS"))
      } else if ("DE" %in% countries) {
        warning("west_germany_only = TRUE but this wave has no `dhregion` — ",
                "East Germany CANNOT be excluded, so DE is all-Germany here ",
                "and will not match Bönke's figures. See _project_memory.md §3a.",
                call. = FALSE)
      }
    }

    # Residence inheritance
    hd <- hd |>
      mutate(
        # HB0600 way of acquiring the main residence.
        #   1 Purchased · 2 Own construction · 3 Inherited · 4 Gift
        #   5 = half inherited (later waves only; absent from W1.5)
        #   6 = Spain-only "Other"
        #
        # Code 6 — the two ECB sources differ, so this is a judgement call.
        # The support team emailed (2 Jun) that it is "a data error and you can
        # disregard those values". The current variable catalogue (refreshed
        # 2026-08-15) is more specific: "National-specific code 6 - Other used
        # in Spain to cover some particular situations, which were not recoded
        # into the standardised HFCS codes (for example, half inherited and
        # half purchased)."
        #
        # We follow the catalogue and weight it 0.5, because (a) it explains the
        # data exactly — code 6 occurs ONLY in Spain, 34 households in ES and 9
        # in E1, and nowhere else in any wave — and (b) treating it
        # as 0 asserts "not inherited", which the catalogue says is wrong.
        # Immaterial either way: 43 households out of ~12,300 Spanish, and
        # Spain replicates Bönke to 0.02% under the previous 0-weight rule.
        res_inherited = hb0600 %in% c(3, 4, 5, 6) & !is.na(hb0600),
        res_weight = case_when(
          hb0600 %in% c(3, 4) ~ 1.0,
          hb0600 == 5          ~ 0.5,
          hb0600 == 6          ~ 0.5,
          TRUE                 ~ 0.0
        )
      )

    # Capitalise transfers.
    #
    # Two arms come out of ONE constructor: the real-rate arm (`wt`, the
    # Bönke 3% baseline) and the CPI-only arm (`wt_cpi_adj`, rate = 0), which
    # the gradient scenarios in Ch06/07/09 re-capitalise from. They are
    # generated by a single function rather than two hard-coded blocks
    # precisely because a second copy is how divergences creep in silently —
    # so every other step is shared by construction.
    .wt_at_rate <- function(r) {
      res_pvwt <- ifelse(
        hd$res_inherited,
        hd$res_weight * capitalise_transfers(
          hd$hb0800, hd$hb0700, hd$sa0100, cpi_lkp, stable_tbl,
          rate = r, ref_year = survey_year, min_year = min_year
        ),
        0
      )
      t1_pvwt <- capitalise_transfers(
        hd$hh0401, hd$hh0201, hd$sa0100, cpi_lkp, stable_tbl,
        rate = r, ref_year = survey_year, min_year = min_year
      )
      t2_pvwt <- capitalise_transfers(
        hd$hh0402, hd$hh0202, hd$sa0100, cpi_lkp, stable_tbl,
        rate = r, ref_year = survey_year, min_year = min_year
      )
      t3_pvwt <- capitalise_transfers(
        hd$hh0403, hd$hh0203, hd$sa0100, cpi_lkp, stable_tbl,
        rate = r, ref_year = survey_year, min_year = min_year
      )
      res_pvwt + t1_pvwt + t2_pvwt + t3_pvwt
    }

    hd$wt_cap     <- .wt_at_rate(rate)
    hd$wt_cpi_adj <- .wt_at_rate(0)

    message("  [implicate ", i, "/5] Capitalising + constructing NW/NWX (",
            nrow(hd), " HH)...")

    hd |> mutate(
      wt  = wt_cap,
      nw  = dn3001,
      nwx = nw - wt,
      implicate = i
    ) |>
      mutate(nhhmem = if ("dh0001" %in% names(hd)) dh0001 else NA_real_) |>
      select(
        hid = sa0010, country = sa0100,
        implicate, nw, nwx, wt, wt_cpi_adj, w = hw0010, nhhmem
      ) |>
      mutate(
        year = survey_year,
        hid  = as.character(hid)
      )
  }))

  # --- Split into list-of-tibbles by country (keyed as country_year) ---
  data_list <- split(all_hd, all_hd$country) |>
    lapply(function(df) df |> select(country, year, implicate, hid,
                                     nw, nwx, wt, wt_cpi_adj, w, nhhmem))
  names(data_list) <- paste0(names(data_list), "_", survey_year)

  # --- Replicate weights ---
  rep_weights <- NULL
  if (load_rep_weights && file.exists(file.path(data_dir, "w.dta"))) {
    message("  Loading replicate weights (w.dta, 1000 columns — may take a moment)...")
    w_file <- haven::read_dta(file.path(data_dir, "w.dta"))
    w_file <- w_file |>
      filter(sa0100 %in% countries) |>
      mutate(hid = as.character(sa0010))

    wr_cols <- grep("^wr", names(w_file), value = TRUE)

    # Rep weights are household-level (not per-implicate in HFCS).
    # Expand to all implicates.
    rep_weights <- split(w_file, w_file$sa0100) |>
      lapply(function(df_c) {
        # Replicate for each implicate
        bind_rows(lapply(1:5, function(m) {
          df_c |>
            select(hid, all_of(wr_cols)) |>
            mutate(implicate = m)
        }))
      })
    names(rep_weights) <- paste0(names(rep_weights), "_", survey_year)
  }

  list(data = data_list, rep_weights = rep_weights)
}


# --- 4b. LWS builder ---------------------------------------------------------

#' Build prepped from LWS data via lissyrtools (LISSY-ready).
#'
#' Uses lissyrtools::deflators for CPI (no hardcoded values).
#'
#' @param codes Character vector of LWS country-year codes (e.g. "at11").
#' @param rate Real capitalisation rate (default 0.03).
#' @param min_year Floor year (default 1960).
#' @param age_max Maximum age of household head (default NULL = no cap, matching
#'   Bönke convention of age >= 21 only). Pass an integer (e.g. 80L) to apply
#'   an upper cap. Note: age < 65 was the LWS workshop convention for working-age
#'   analysis but is NOT appropriate for inheritance-paradox challenges — always
#'   pass NULL here unless you have a deliberate reason to cap.
#' @param age_min Minimum age of household head (default 21, Bönke convention).
#' @param cpi_version_year Which LIS deflator series to use (default 2021).
#' @param load_rep_weights Logical: load replicate weight files? (default TRUE).
#' @return A prepped object.
build_prepped_lws <- function(codes,
                              rate = 0.03,
                              min_year = 1960L,
                              age_min = 21L,
                              age_max = NULL,
                              cpi_version_year = 2021L,
                              load_rep_weights = TRUE,
                              extra_vars = NULL) {

  if (!requireNamespace("lissyrtools", quietly = TRUE)) {
    stop("Package 'lissyrtools' required for LWS builder.")
  }

  # --- CPI from lissyrtools::deflators ---
  defl <- lissyrtools::deflators |>
    filter(version_year == cpi_version_year, !is.na(cpi))

  # --- Load LWS data ---
  core_vars <- c("dnw",
                 "pia1", "pia2", "pia3", "pia4",
                 "piy1", "piy2", "piy3", "piy4",
                 "pir", "hpopwgt", "relation", "age",
                 "inum", "year", "iso2", "hid")
  all_vars <- unique(c(core_vars, extra_vars))

  lws_data <- lissyrtools::lissyuse(
    data = codes,
    vars = all_vars,
    lws = TRUE
  )

  # --- Build CPI lookup and stable_from per country ---
  build_cpi_for_country <- function(iso2_lower, survey_yr) {
    ctry_defl <- defl |> filter(iso2 == iso2_lower)
    if (nrow(ctry_defl) == 0) return(list(cpi = NULL, stable_from = Inf))

    # Rebase to survey_year = 100
    base_val <- ctry_defl$cpi[ctry_defl$year == survey_yr]
    if (length(base_val) == 0 || is.na(base_val[1])) {
      # If exact year not available, use nearest
      nearest_yr <- ctry_defl$year[which.min(abs(ctry_defl$year - survey_yr))]
      base_val <- ctry_defl$cpi[ctry_defl$year == nearest_yr]
    }
    rebased <- setNames(ctry_defl$cpi / base_val[1] * 100, ctry_defl$year)

    # Stable era: first year with raw CPI >= 10
    stable_from <- ctry_defl |>
      filter(cpi >= 10) |>
      pull(year) |>
      min()
    if (length(stable_from) == 0) stable_from <- Inf

    list(cpi = rebased, stable_from = stable_from)
  }

  # --- Prepare per country ---
  prep_country <- function(df) {
    iso2_lower <- tolower(unique(df$iso2)[1])
    iso2_upper <- toupper(iso2_lower)
    survey_yr  <- max(df$year, na.rm = TRUE)

    # Apply filters (all implicates)
    # age_min: lower bound (Bönke convention: >= 21); age_max: NULL = no cap
    df <- df |>
      filter(
        relation == 1000,
        if (!is.null(age_min)) age >= age_min else TRUE,
        if (!is.null(age_max)) age < age_max else TRUE
      )

    if (nrow(df) == 0) return(NULL)

    # Build CPI for this country
    cpi_info <- build_cpi_for_country(iso2_lower, survey_yr)
    cpi_lkp  <- setNames(list(cpi_info$cpi), iso2_upper)
    stable_tbl <- setNames(list(cpi_info$stable_from), iso2_upper)

    # Capitalise transfers
    # pia1–4 are HH-level (verified on LISSY 2026-07-03: constant within hid
    # for IT14, US13, AT11, ES11). relation == 1000 filter gives whole HH's
    # inheritances, matching Bönke's HFCS HH0401–03.
    pia_cols <- intersect(c("pia1", "pia2", "pia3", "pia4"), names(df))
    piy_cols <- intersect(c("piy1", "piy2", "piy3", "piy4"), names(df))

    wt_cap     <- rep(0, nrow(df))
    wt_cpi_adj <- rep(0, nrow(df))   # CPI-deflated only (rate = 0); used by challenge 06
    for (i in seq_along(pia_cols)) {
      pia <- df[[pia_cols[i]]]
      piy <- if (i <= length(piy_cols)) df[[piy_cols[i]]] else rep(survey_yr, nrow(df))
      cap_args <- list(
        value       = pia,
        year        = piy,
        country_vec = rep(iso2_upper, nrow(df)),
        cpi_lkp     = cpi_lkp,
        stable_tbl  = stable_tbl,
        ref_year    = survey_yr,
        min_year    = min_year
      )
      wt_cap     <- wt_cap + do.call(capitalise_transfers, c(cap_args, list(rate = rate)))
      wt_cpi_adj <- wt_cpi_adj + do.call(capitalise_transfers, c(cap_args, list(rate = 0)))
    }

    df |> mutate(
      country    = iso2_upper,
      year       = survey_yr,
      implicate  = as.integer(inum),
      hid        = as.character(hid),
      wt         = wt_cap,
      wt_cpi_adj = wt_cpi_adj,
      nw         = as.numeric(dnw),
      nwx        = nw - wt,
      w          = as.numeric(hpopwgt * 100)
    ) |>
      select(country, year, implicate, hid, nw, nwx, wt, wt_cpi_adj, w, any_of(extra_vars))
  }

  all_prepped <- map(lws_data, prep_country) |> discard(is.null)
  # Key by country_year (handles multiple waves for same country)
  data_list <- setNames(
    all_prepped,
    map_chr(all_prepped, ~ paste0(unique(.x$country), "_", unique(.x$year)))
  )

  # --- Replicate weights ---
  rep_weights <- NULL
  if (load_rep_weights) {
    message("  Loading LWS replicate weights...")
    rep_weights <- map(codes, function(code) {
      tryCatch({
        # LISSY built-in read.LIS() — NOT lissyrtools::read_LIS()
        rw_code <- paste0(code, "r")
        message("    Trying read.LIS('", rw_code, "')...")
        rw <- read.LIS(file_name = rw_code, col_select = NULL)
        if (is.null(rw) || nrow(rw) == 0) return(NULL)

        # Rename hrwgt* → wr* for uniformity
        hr_cols <- grep("^hrwgt", names(rw), value = TRUE)
        if (length(hr_cols) == 0) {
          message("    No hrwgt* columns in '", rw_code, "' — skipping")
          return(NULL)
        }
        message("    Found ", length(hr_cols), " replicate weight columns")
        new_names <- paste0("wr", sprintf("%04d", seq_along(hr_cols)))
        names(rw)[names(rw) %in% hr_cols] <- new_names

        rw <- rw |>
          mutate(hid = as.character(hid)) |>
          select(hid, all_of(new_names))

        # Expand to all implicates (rep weights are HH-level)
        iso2_upper <- toupper(substr(code, 1, 2))
        key_match <- grep(paste0("^", iso2_upper, "_"), names(data_list), value = TRUE)
        n_imp <- if (length(key_match) > 0) {
          max(data_list[[key_match[1]]]$implicate, na.rm = TRUE)
        } else 5L

        bind_rows(lapply(seq_len(n_imp), function(m) {
          rw |> mutate(implicate = m)
        }))
      }, error = function(e) {
        message("    Warning: no rep weight file for '", code, "': ", e$message)
        NULL
      })
    })
    # Match keys: use country_year from data_list
    non_null <- !map_lgl(rep_weights, is.null)
    rep_weights <- rep_weights[non_null]
    # Name by matching the country codes to data_list keys
    rw_codes <- codes[non_null]
    rw_keys <- map_chr(rw_codes, function(code) {
      iso2_upper <- toupper(substr(code, 1, 2))
      matching <- grep(paste0("^", iso2_upper, "_"), names(data_list), value = TRUE)
      if (length(matching) > 0) matching[1] else paste0(iso2_upper, "_unknown")
    })
    rep_weights <- setNames(rep_weights, rw_keys)

    # Pad to max number of wr columns across countries
    if (length(rep_weights) > 0) {
      all_wr <- unique(unlist(map(rep_weights, ~ grep("^wr", names(.x), value = TRUE))))
      rep_weights <- map(rep_weights, function(rw) {
        missing_cols <- setdiff(all_wr, names(rw))
        for (col in missing_cols) rw[[col]] <- 0
        rw |> select(hid, implicate, all_of(sort(all_wr)))
      })
    }
  }

  list(data = data_list, rep_weights = rep_weights)
}


# === 5. POINT ESTIMATE HELPERS (TIER 2) =======================================

#' Compute Rubin-averaged point estimates (mean across implicates, main weights).
#' Use for Tier 2 visualizations (full curves, robustness tables).
#'
#' @param prepped A validated prepped object.
#' @param stat_fn Function f(nw, nwx, w, ...) returning named numeric vector.
#' @param ... Extra args to stat_fn.
#' @param countries Countries to process (default: all).
#' @return Tibble with columns: country, stat_name, estimate (no SE).
compute_point_estimates <- function(prepped, stat_fn, ...,
                                    countries = NULL,
                                    extra_cols = NULL) {
  if (is.null(countries)) countries <- names(prepped$data)
  # extra_cols mirrors compute_with_rubin(): when supplied, stat_fn is called as
  # stat_fn(nw, nwx, w, extra, ...) with `extra` a named list of those columns.
  # Added 2026-08-21 for Ch18, which needs a Tier-2 pass over a parameter grid
  # too large to run through the replicate-weight loop. Backward compatible:
  # with extra_cols = NULL the call signature is unchanged.
  has_extra <- !is.null(extra_cols) && length(extra_cols) > 0

  map_dfr(countries, function(ctry) {
    df <- prepped$data[[ctry]]
    if (is.null(df) || nrow(df) == 0) return(tibble())

    if (has_extra) {
      missing_cols <- setdiff(extra_cols, names(df))
      if (length(missing_cols) > 0) {
        stop("extra_cols not found in prepped$data[['", ctry, "']]: ",
             paste(missing_cols, collapse = ", "))
      }
    }

    implicates <- sort(unique(df$implicate))

    theta_mat <- map(implicates, function(m) {
      slice_m <- df |> filter(implicate == m)
      if (has_extra) {
        stat_fn(slice_m$nw, slice_m$nwx, slice_m$w,
                as.list(slice_m[extra_cols]), ...)
      } else {
        stat_fn(slice_m$nw, slice_m$nwx, slice_m$w, ...)
      }
    }) |> do.call(rbind, args = _)

    # Rubin point estimate = mean across implicates
    theta_bar <- colMeans(theta_mat, na.rm = TRUE)

    tibble(
      country   = ctry,
      stat_name = names(theta_bar),
      estimate  = as.numeric(theta_bar)
    )
  })
}


# ============================================================
# harmonise_parented()
# ============================================================
# Harmonises edmom_c / eddad_c from country-specific codings
# into a common 3-category ordered factor: low / medium / high.
#
# Typical usage with a prepped object:
#   prepped$data <- imap(prepped$data, function(df, key) {
#     harmonise_parented(df, country_code = tolower(substr(key, 1, 2)))
#   })
#
# Mapping is anchored to ISCED 2011:
#   low    = ISCED 0–2 (no education through lower secondary)
#   medium = ISCED 3–4 (upper secondary / post-secondary non-tertiary)
#   high   = ISCED 5–8 (tertiary)
#
# Countries: IT (it14), LU (lu18), UK (uk19), US (us22).
# All other countries return NA — they do not carry these variables.
#
# The full mapping table, distributions and caveats are documented in the
# author's research notes, available on request.
# ============================================================

harmonise_parented <- function(df, country_code) {

  recode_it <- function(x) {
    dplyr::case_when(
      x %in% c(0, 10, 20) ~ "low",
      x == 30             ~ "medium",
      x %in% c(50, 60)    ~ "high",
      TRUE                ~ NA_character_
    )
  }

  recode_lu <- function(x) {
    dplyr::case_when(
      x %in% c(1, 2) ~ "low",
      x == 3         ~ "medium",
      x == 4         ~ "high",
      TRUE           ~ NA_character_
    )
  }

  recode_uk <- function(x) {
    # Age-left-school scale
    dplyr::case_when(
      x %in% c(1, 2)    ~ "low",
      x %in% c(3, 4, 5) ~ "medium",
      x == 6            ~ "high",
      TRUE              ~ NA_character_
    )
  }

  recode_us <- function(x) {
    dplyr::case_when(
      x %in% c(0, 1) ~ "low",
      x %in% c(2, 3) ~ "medium",
      x == 4         ~ "high",
      TRUE           ~ NA_character_
    )
  }

  recode_fn <- switch(
    tolower(country_code),
    "it" = recode_it,
    "lu" = recode_lu,
    "uk" = recode_uk,
    "us" = recode_us,
    NULL
  )

  levels <- c("low", "medium", "high")

  if (is.null(recode_fn)) {
    # Country does not carry parental education; return NAs
    if ("edmom_c" %in% names(df)) df$edmom_3cat <- factor(NA, levels = levels, ordered = TRUE)
    if ("eddad_c" %in% names(df)) df$eddad_3cat <- factor(NA, levels = levels, ordered = TRUE)
  } else {
    if ("edmom_c" %in% names(df))
      df$edmom_3cat <- factor(recode_fn(df$edmom_c), levels = levels, ordered = TRUE)
    if ("eddad_c" %in% names(df))
      df$eddad_3cat <- factor(recode_fn(df$eddad_c), levels = levels, ordered = TRUE)
  }

  df
}


# === STRATIFICATION HELPERS (shared: Ch05, Ch05b, etc.) ===========================

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

.wmedian <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

.wquantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cdf <- cumsum(as.numeric(w)) / sum(as.numeric(w))
  sapply(probs, function(p) x[which(cdf >= p)[1]])
}

.wgini <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  ord <- order(x); x <- x[ord]; w <- w[ord]
  p <- cumsum(w) / sum(w)
  L <- cumsum(w * x) / sum(w * x)
  p0 <- c(0, p); L0 <- c(0, L)
  area <- sum((L0[-1] + L0[-length(L0)]) * diff(p0) / 2)
  1 - 2 * area
}


#' CV² between/within decomposition.
#' Primary measure (consistent with Boenke baseline; clean additive split).
#' Works with the full distribution including negative wealth.
#' @return Named vector: cv2_total, cv2_within, cv2_between.
#'   Note: cv2_total = cv2_within + cv2_between (exact identity).
cv2_decomp <- function(x, w, g) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & !is.na(g)
  x <- x[ok]; w <- w[ok]; g <- g[ok]
  if (length(x) < 10) {
    return(c(cv2_total = NA_real_, cv2_within = NA_real_, cv2_between = NA_real_))
  }

  W     <- sum(w)
  mu    <- sum(x * w) / W
  if (!is.finite(mu) || mu == 0) {
    return(c(cv2_total = NA_real_, cv2_within = NA_real_, cv2_between = NA_real_))
  }

  cv2_total <- sum(w * (x - mu)^2) / W / mu^2

  groups <- unique(g)
  var_within <- sum(vapply(groups, function(grp) {
    idx   <- g == grp
    s_g   <- sum(w[idx]) / W
    mu_g  <- sum(x[idx] * w[idx]) / sum(w[idx])
    var_g <- sum(w[idx] * (x[idx] - mu_g)^2) / sum(w[idx])
    s_g * var_g
  }, numeric(1)), na.rm = TRUE)

  var_between <- sum(vapply(groups, function(grp) {
    idx  <- g == grp
    s_g  <- sum(w[idx]) / W
    mu_g <- sum(x[idx] * w[idx]) / sum(w[idx])
    s_g * (mu_g - mu)^2
  }, numeric(1)), na.rm = TRUE)

  c(
    cv2_total   = unname(cv2_total),
    cv2_within  = unname(var_within  / mu^2),
    cv2_between = unname(var_between / mu^2)
  )
}


#' MLD (GE(0)) between/within decomposition.
#' Secondary measure: additively decomposable, no overlap residual.
#' Applied to positive-wealth subsample only (log transform requires x > 0).
#' @return Named vector: mld_total, mld_within, mld_between.
mld_decomp <- function(x, w, g) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & x > 0 & !is.na(g)
  x <- x[ok]; w <- w[ok]; g <- g[ok]
  if (length(x) < 10) {
    return(c(mld_total = NA_real_, mld_within = NA_real_, mld_between = NA_real_))
  }

  W  <- sum(w)
  mu <- sum(x * w) / W
  mld_total <- log(mu) - sum(w * log(x)) / W

  groups <- unique(g)

  mld_within <- sum(vapply(groups, function(grp) {
    idx  <- g == grp
    s_g  <- sum(w[idx]) / W
    mu_g <- sum(x[idx] * w[idx]) / sum(w[idx])
    mld_g <- log(mu_g) - sum(w[idx] * log(x[idx])) / sum(w[idx])
    s_g * mld_g
  }, numeric(1)), na.rm = TRUE)

  mld_between <- sum(vapply(groups, function(grp) {
    idx  <- g == grp
    s_g  <- sum(w[idx]) / W
    mu_g <- sum(x[idx] * w[idx]) / sum(w[idx])
    s_g * log(mu / mu_g)
  }, numeric(1)), na.rm = TRUE)

  c(mld_total = unname(mld_total), mld_within = unname(mld_within), mld_between = unname(mld_between))
}


#' Weighted variance and weighted CV² — small helpers used by the per-group
#' profile in Ch05f. Both defined for ANY real x (negatives included);
#' .wcv2() additionally requires a non-zero mean.
.wvar <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mu <- sum(x * w) / sum(w)
  sum(w * (x - mu)^2) / sum(w)
}

.wcv2 <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mu <- sum(x * w) / sum(w)
  if (!is.finite(mu) || mu == 0) return(NA_real_)
  .wvar(x, w) / mu^2
}


#' ABSOLUTE variance between/within decomposition (classic ANOVA split).
#'
#' var_total = var_within + var_between, exactly, for ANY partition.
#'
#' WHY THIS MATTERS FOR THIS PROJECT — it is the absolute counterpart to
#' cv2_decomp(), and the stratification chapter previously had no absolute
#' decomposition at all, despite the paper's whole argument being that
#' relative measures mislead. Three properties make it the most robust
#' decomposition available for wealth:
#'   1. No positivity requirement anywhere — unlike MLD/Theil (need x > 0,
#'      which truncates the sample and, per the Ch05e audit, biases results)
#'      and unlike gini_hs_decomp() (needs positive subgroup MEANS).
#'   2. Exactly two terms, no residual/overlap — unlike any Gini decomposition.
#'   3. The between/total SHARE is translation-invariant: adding a constant to
#'      every household leaves all variances unchanged.
#'
#' CORRECTION (2026-08-13, after the Ch05f run): an earlier version of this
#' note claimed the variance share is translation-invariant "in a way CV²'s
#' share is not", implying an advantage over cv2_decomp(). That was WRONG.
#' The two shares are the SAME NUMBER by algebra:
#'     cv2_between/cv2_total = (var_between/mu^2)/(var_total/mu^2)
#'                           = var_between/var_total
#' the mu^2 cancelling exactly. Verified empirically on the Ch05f output: max
#' |d_cv2_share_between - d_var_share_between| = 1.0e-16 across all 40 cells.
#' So CV²'s SHARE was always immune to the mean-shift artefact; only its LEVELS
#' are affected, which is why the project's "report the share, not the level"
#' convention was already correct. Do not present the variance decomposition as
#' fixing a problem in CV² — it does not.
#'
#' What variance genuinely adds is the LEVEL: absolute currency units, directly
#' interpretable as dispersion in euros, where a CV² level is a unitless ratio.
#' That is its only non-redundant contribution here.
#' The between/total share is also exactly eta-squared for the grouping — the
#' share of wealth variance explained by group membership — which is a
#' familiar and easily communicated quantity.
#'
#' CAVEAT: the variance LEVEL is in squared currency units and is therefore
#' not comparable across countries (or against CV²/Gini). Compare levels only
#' within a country between NW and NWX; use the share for anything
#' cross-national.
#' @return Named vector: var_total, var_within, var_between, share_between.
var_decomp <- function(x, w, g) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & !is.na(g)
  x <- x[ok]; w <- w[ok]; g <- g[ok]
  if (length(x) < 10) {
    return(c(var_total = NA_real_, var_within = NA_real_,
             var_between = NA_real_, share_between = NA_real_))
  }

  W  <- sum(w)
  mu <- sum(x * w) / W
  var_total <- sum(w * (x - mu)^2) / W

  groups <- unique(g)
  var_within <- sum(vapply(groups, function(grp) {
    idx  <- g == grp
    s_g  <- sum(w[idx]) / W
    mu_g <- sum(x[idx] * w[idx]) / sum(w[idx])
    s_g * (sum(w[idx] * (x[idx] - mu_g)^2) / sum(w[idx]))
  }, numeric(1)), na.rm = TRUE)

  var_between <- sum(vapply(groups, function(grp) {
    idx  <- g == grp
    s_g  <- sum(w[idx]) / W
    mu_g <- sum(x[idx] * w[idx]) / sum(w[idx])
    s_g * (mu_g - mu)^2
  }, numeric(1)), na.rm = TRUE)

  c(var_total     = unname(var_total),
    var_within    = unname(var_within),
    var_between   = unname(var_between),
    share_between = unname(if (is.finite(var_total) && var_total > 0)
                             var_between / var_total else NA_real_))
}


#' Gini between/within decomposition (Pyatt 1976).
#' Simple 3-term split: gini_total = gini_within + gini_between + gini_overlap.
#' Restricted to positive-wealth subsample — same restriction as mld_decomp(),
#' because the Lorenz-curve logic behind Gini (and behind gini_between's
#' "smoothed distribution" construction) is only well-behaved for non-negative
#' values. This supersedes the Phase 2 decision to report "Gini total only"
#' (Dagum's fuller overlap-term formula breaks under negative wealth); Pyatt's
#' simpler residual-as-overlap approach sidesteps that specific problem by
#' restricting to x > 0 instead, mirroring the MLD treatment.
#' NOTE: gini_total here (positive-wealth subsample) will differ slightly from
#' any gini_total computed on the FULL sample (e.g. via .wgini(nw, w) directly)
#' — always compare on the same subsample when cross-checking.
#' gini_between = Gini of the "smoothed" distribution where each unit's value
#' is replaced by its own group's weighted mean (population-share weighted).
#' gini_within = Sum over groups of (population share x income share x
#' within-group Gini) — the classic Pyatt "within" term.
#' gini_overlap = gini_total - gini_within - gini_between (residual; Pyatt
#' shows this is >= 0 for a proper decomposition, reflecting the extent to
#' which subgroup distributions overlap in range).
#' @return Named vector: gini_total, gini_within, gini_between, gini_overlap.
gini_decomp <- function(x, w, g) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & x > 0 & !is.na(g)
  x <- x[ok]; w <- w[ok]; g <- g[ok]
  if (length(x) < 10) {
    return(c(gini_total = NA_real_, gini_within = NA_real_,
             gini_between = NA_real_, gini_overlap = NA_real_))
  }

  W  <- sum(w)
  mu <- sum(x * w) / W
  if (!is.finite(mu) || mu <= 0) {
    return(c(gini_total = NA_real_, gini_within = NA_real_,
             gini_between = NA_real_, gini_overlap = NA_real_))
  }
  gini_total <- .wgini(x, w)

  groups <- unique(g)

  group_mu <- setNames(vapply(groups, function(grp) {
    idx <- g == grp
    sum(x[idx] * w[idx]) / sum(w[idx])
  }, numeric(1)), groups)

  within_terms <- vapply(groups, function(grp) {
    idx  <- g == grp
    s_g  <- sum(w[idx]) / W                                  # population share
    y_g  <- (group_mu[[grp]] * sum(w[idx])) / (mu * W)        # income share
    G_g  <- .wgini(x[idx], w[idx])
    s_g * y_g * G_g
  }, numeric(1))
  gini_within <- sum(within_terms, na.rm = TRUE)

  # "Smoothed" distribution: each unit's value replaced by its group mean.
  mu_vec <- group_mu[g]
  gini_between <- .wgini(mu_vec, w)

  gini_overlap <- gini_total - gini_within - gini_between

  c(
    gini_total   = unname(gini_total),
    gini_within  = unname(gini_within),
    gini_between = unname(gini_between),
    gini_overlap = unname(gini_overlap)
  )
}


#' Weighted mean absolute difference (population version), O(n log n).
#' MD = E[|X-Y|] for X,Y drawn independently (with replacement) from the
#' weighted empirical distribution of x. Purely rank-based via a sort +
#' cumulative-weight identity — well-defined for ANY real x (positive,
#' negative, or mixed). This is what gini_hs_decomp() is built on, in place
#' of the Lorenz-curve-area method .wgini() uses, because the Lorenz-area
#' method is only reliably well-behaved for non-negative x.
#' @return Scalar MD (>= 0), or NA if fewer than 2 valid observations.
.wmad <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  ord <- order(x); x <- x[ord]; w <- w[ord]
  W <- sum(w)
  A <- cumsum(w) - w  # cumulative weight strictly before each element
  S <- 2 * sum(w * x * (2 * A + w - W))
  S / W^2
}


#' Gini coefficient via the mean-absolute-difference formula: G = MD / (2*mean).
#'
#' CORRECTION (2026-08-13, verified numerically + algebraically): this is
#' ALGEBRAICALLY IDENTICAL to the Lorenz-area formula .wgini() computes — for
#' ANY real x with non-zero mean, negatives included, NOT only for x > 0. An
#' earlier version of this comment claimed the two "will not numerically agree"
#' under negative values and warned against substituting .wgini(); that claim
#' was WRONG. Checked over 2000 randomised trials spanning negative values and
#' negative group means: max absolute difference 3.9e-11 (pure floating point).
#' The trapezoid-Lorenz sum and the sorted mean-absolute-difference sum reduce
#' to the same expression; neither derivation assumes positivity anywhere.
#'
#' So .wgini() and .wgini_md() are interchangeable on the maths. This function
#' is retained because (a) the MAD route is the form Heikkuri & Schief (2026)
#' state their theorem in, so gini_hs_decomp() reads directly against the paper,
#' and (b) it makes the mu <= 0 guard explicit rather than silently returning a
#' meaningless number. It is NOT a numerical correction to .wgini(), and results
#' computed with .wgini() elsewhere in this project (Ch05 etc.) are unaffected by
#' this distinction — do not "fix" them to use this instead.
#'
#' Note the Gini can exceed 1 when x contains negative values; that is a genuine
#' property of the measure, not an error.
#' @return Scalar Gini coefficient, or NA if <2 obs or mean <= 0.
.wgini_md <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mu <- sum(x * w) / sum(w)
  if (!is.finite(mu) || mu <= 0) return(NA_real_)
  .wmad(x, w) / (2 * mu)
}


#' Gini within/between decomposition (Heikkuri & Schief 2026, Theorem 2,
#' "Subgroup Decomposition of the Gini Coefficient: A New Solution to an
#' Old Problem", Econometrica 94(1), 169-192).
#' Exact 2-term additive decomposition — gini_total = gini_within +
#' gini_between, no residual/overlap term, for ANY partition (including
#' overlapping subgroup ranges). This is what distinguishes it from
#' gini_decomp() (Pyatt 1976), which needs a third overlap term and was
#' the reason the Pyatt-based stratification challenge (05c) was dropped.
#' Formula (Theorem 2, eq. 2-3):
#'   G^W = ( sum_k sqrt(pi_k * theta_k * G_k) )^2
#'   G^B = G_total - G^W        [residual; algebraically equals the direct
#'                                formula (1/2mu)(Theta - sum_k!=l pi_k pi_l
#'                                sqrt(Delta_k Delta_l)) per the paper, but
#'                                the paper itself notes the residual is
#'                                "easier to compute in practice" — used here]
#' where pi_k/theta_k/G_k are subgroup k's population share/income share/
#' Gini coefficient.
#' NEGATIVE-VALUE ROBUSTNESS (Proposition 6): the within/between SHARES of
#' this decomposition are translation-invariant (x -> a*x+b, a>0) as long as
#' each SUBGROUP MEAN stays positive — not each individual value. This is
#' why the full sample (household net worth, including negative values) can
#' be used directly here, unlike gini_decomp()/Pyatt, which required
#' restricting to the positive-wealth subsample. Internally uses
#' .wgini_md()/.wmad() because that is the form the paper states its theorem
#' in — NOT because .wgini() would give a different number (it would not; see
#' the correction note on .wgini_md()).
#'
#' THE BINDING CONSTRAINT IS SUBGROUP MEANS, NOT INDIVIDUAL VALUES, BUT IT
#' STILL BINDS HARD IN PRACTICE. Every subgroup must have a positive weighted
#' mean or the whole decomposition returns NA. On LWS NWX under the uncapped
#' 3% baseline the bottom NWX tercile has a NEGATIVE mean in 9/10 countries
#' (AT -255,780; ES -126,538; US -112,431 — see Ch05e diagnostics), so Tier-3-
#' style wealth-tercile splits mostly return NA there. Ch05e shows this is
#' largely a CAPITALISATION artefact, not genuine indebtedness: under
#' capped_3pct the bottom-tercile mean turns positive in 8/10 countries.
#' NA patterns reflect these domain conditions, not missing data.
#' @return Named vector: gini_total, gini_within, gini_between. NA for all
#'   three if any subgroup has <2 valid observations or a non-positive mean.
gini_hs_decomp <- function(x, w, g) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & !is.na(g)
  x <- x[ok]; w <- w[ok]; g <- g[ok]
  if (length(x) < 10) {
    return(c(gini_total = NA_real_, gini_within = NA_real_, gini_between = NA_real_))
  }

  W  <- sum(w)
  mu <- sum(x * w) / W
  if (!is.finite(mu) || mu <= 0) {
    return(c(gini_total = NA_real_, gini_within = NA_real_, gini_between = NA_real_))
  }
  gini_total <- .wgini_md(x, w)

  groups <- unique(g)
  terms <- vapply(groups, function(grp) {
    idx  <- g == grp
    W_k  <- sum(w[idx])
    if (W_k <= 0) return(NA_real_)
    pi_k <- W_k / W
    mu_k <- sum(x[idx] * w[idx]) / W_k
    if (!is.finite(mu_k) || mu_k <= 0) return(NA_real_)
    theta_k <- (W_k * mu_k) / (W * mu)
    G_k <- .wgini_md(x[idx], w[idx])
    if (!is.finite(G_k)) return(NA_real_)
    sqrt(pi_k * theta_k * G_k)
  }, numeric(1))

  if (anyNA(terms)) {
    return(c(gini_total = gini_total, gini_within = NA_real_, gini_between = NA_real_))
  }

  gini_within  <- sum(terms)^2
  gini_between <- gini_total - gini_within

  c(gini_total = gini_total, gini_within = gini_within, gini_between = gini_between)
}


#' Monti & Santoro stratification index I.
#' O(n log n) via sorting + cumulative weights.
#' @return Named vector: I, n_tr (transvariations), n_pairs.
stratification_index <- function(x_upper, w_upper, x_lower, w_lower) {
  ok_l <- is.finite(x_lower) & is.finite(w_lower) & w_lower > 0
  ok_u <- is.finite(x_upper) & is.finite(w_upper) & w_upper > 0
  x_l <- x_lower[ok_l]; w_l <- w_lower[ok_l]
  x_u <- x_upper[ok_u]; w_u <- w_upper[ok_u]

  if (length(x_l) < 5 || length(x_u) < 5) {
    # MUST carry ALL FIVE names, not just the original three. Callers extract
    # with [["beta"]] and [["I_norm"]], and `[[` on a name absent from a named
    # atomic vector throws "subscript out of bounds" rather than returning NA -
    # so a short return here aborts the whole stat function for that country.
    # Found 2026-08-28 while auditing callers after this function grew from three
    # return values to five; it would have silently dropped small education and
    # occupation cells from Ch05f in a run there is no way to debug on LISSY.
    return(c(I = NA_real_, n_tr = NA_real_, n_pairs = NA_real_,
             beta = NA_real_, I_norm = NA_real_))
  }

  total_w_l <- sum(as.numeric(w_l))
  total_w_u <- sum(as.numeric(w_u))
  n_pairs   <- total_w_l * total_w_u

  ord_l   <- order(x_l)
  x_l_s   <- x_l[ord_l]; w_l_s <- w_l[ord_l]
  cum_w_l <- cumsum(as.numeric(w_l_s))

  n_tr <- sum(vapply(seq_along(x_u), function(j) {
    pos              <- findInterval(x_u[j], x_l_s, left.open = FALSE)
    weight_below_eq  <- if (pos == 0) 0 else cum_w_l[pos]
    weight_above     <- total_w_l - weight_below_eq
    as.numeric(w_u[j]) * weight_above
  }, numeric(1)))

  I <- 1 - 2 * n_tr / n_pairs

  # --- beta: the LOWER BOUND of I (Monti & Santoro 2009, eq. 14-15, p.8) ------
  # I is bounded above at 1 (perfect stratification, no transvariations) but its
  # FLOOR is distribution-dependent, so a raw I is not directly comparable across
  # samples with different skewness. beta gives that floor:
  #     MaxN_TR = n_a*n_c - q_a*p_c
  #     beta    = 1 - 2*MaxN_TR/(n_a*n_c) = 2*q_a*p_c/(n_a*n_c) - 1
  # where q_a = members of the RICHER group above its own mean, and
  #       p_c = members of the POORER group weakly below its own mean.
  #
  # THE PUBLISHED eq. (15) HAS A TYPO: it prints the denominator as n_c*n_c.
  # Substituting eq. (14) gives n_a*n_c, and the authors' own stated special
  # case - beta = -1/2 when both distributions are symmetric about their means -
  # only holds with n_a*n_c (with n_c^2 it would need n_a == n_c). We implement
  # n_a*n_c and assert the -1/2 case in the test file.
  #
  # Note the -1/2 is a MAXIMUM, not a typical value. If both groups share a
  # common shape then p_c/n_c = 1 - q_a/n_a, so q_a*p_c/(n_a*n_c) = p(1-p) <= 1/4
  # and hence beta <= -1/2, with equality only in the symmetric case. Wealth is
  # heavily right-skewed, so our beta sits BELOW -1/2 (~-0.56 on lognormal test
  # data) and differs between NW and NWX. That is exactly why a raw I is not
  # comparable across arms, and why I_norm is emitted alongside it.
  mu_u <- sum(x_u * w_u) / total_w_u
  mu_l <- sum(x_l * w_l) / total_w_l
  q_a  <- sum(w_u[x_u >  mu_u])      # richer group, strictly above its mean
  p_c  <- sum(w_l[x_l <= mu_l])      # poorer group, weakly below its mean
  beta <- 2 * q_a * p_c / n_pairs - 1

  c(I = I, n_tr = n_tr, n_pairs = n_pairs, beta = beta,
    I_norm = if (is.finite(beta) && beta < 1) (I - beta) / (1 - beta) else NA_real_)
}
