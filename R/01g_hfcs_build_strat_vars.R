# ============================================================
# 01g_hfcs_build_strat_vars.R
# Attach stratification grouping variables to the cached W5.0 prepped object
# ============================================================
# WHY A SATELLITE RATHER THAN A BUILDER CHANGE
#   build_prepped_hfcs() reads only h*.dta + d*.dta + w.dta. The grouping
#   variables Ch05f needs live partly in the PERSON files (p*.dta, pn*.dta),
#   which the builder never touches. Rather than widen the builder — which
#   would force a ~20 min rebuild of the validated W5.0 cache and put the
#   Bönke replication at risk — this script builds the grouping variables
#   separately and merges them onto the existing cache by (country, hid,
#   implicate). Precedent: R/05b_satellite_parental_education.R did the same
#   for LWS.
#
#   Because the merge is a left join onto the cache, the analysis sample
#   (age >= 21, East DE excluded, transfer-data countries only) is inherited
#   automatically — this script must NOT re-apply those filters.
#
# EMITTED UNDER LWS-CANONICAL NAMES AND CODINGS, BY DESIGN.
#   Ch05f is source-agnostic: it asks for `hitotal`, `hicapital`, `hiprivate`,
#   `educ`, `occa1`, `eddad_3cat`. Emitting HFCS variables under those exact
#   names and codings means Ch05f runs unmodified on either data source, the
#   same way `dn3001` is emitted as `nw`. Any recoding therefore happens HERE,
#   once, and is documented below.
#
# Usage: Rscript R/01g_hfcs_build_strat_vars.R
# Output: results/hfcs_w50_strat_prepped.rds  (cache + grouping columns)
#         results/hfcs_w50/hfcs_w50_strat_var_coverage.csv (diagnostic)
# ============================================================

suppressMessages({
  library(haven); library(dplyr); library(purrr); library(tidyr)
})

DATA_DIR <- file.path(
  "Data", "data-raw", "HFCS",
  "DG-S - HFCS - Data dissemination - All countries", "HFCS_UDB_5_0"
)
CACHE_IN  <- "results/hfcs_w50_prepped.rds"
CACHE_OUT <- "results/hfcs_w50_strat_prepped.rds"
OUTDIR    <- file.path("results", "hfcs_w50")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

# The person-file join key. sa0010 alone is NOT unique — household ids
# repeat across countries, and joining without sa0100 silently produces a
# many-to-many blow-up (84,755 reference persons -> 138,922 rows, verified
# 2026-08-16). Country must be in the key.
PKEY <- c("sa0100", "sa0010", "ra0010")

.read_lc <- function(path) {
  x <- haven::read_dta(path)
  names(x) <- tolower(names(x))
  x
}

# --- Code scheme -------------------------------------------------------------
# HFCS UDB collapses ISCED into FOUR categories {1, 2, 3, 5}, not the 0-8 scale
# printed in valuelabels_P.do / valuelabels_PN.do. Established 2026-08-16 by
# cross-tabulating dheduh1 against the reference person's own pa0200 (strongly
# diagonal), and confirmed by the observed value set of pna0600a/b:
#   1 = primary or below      2 = lower secondary
#   3 = upper secondary       5 = tertiary
#
# THE TERTIARY CODE IS 5, NOT 3. LWS codes `educ` 1/2/3 with 3 = high, and
# Ch05f's educ grouping tests `educ == 3`. Passing dheduh1 through unmapped
# would classify upper-secondary households as "high" and tertiary ones as
# "low" — a silent inversion of the entire Tier 2a grouping. Hence the
# explicit remap to the LWS 1/2/3 convention below.
.isced4_to_lws3 <- function(x) {
  dplyr::case_when(
    x %in% c(1, 2) ~ 1L,   # low    (ISCED 0-2)
    x == 3         ~ 2L,   # medium (ISCED 3-4)
    x == 5         ~ 3L,   # high   (ISCED 5-8)
    TRUE           ~ NA_integer_
  )
}

# Parental education on the same 4-category scale, emitted as the low/medium/
# high factor that harmonise_parented() produces for LWS, so Ch05f's
# `eddad_3cat == "high"` test carries across unchanged.
.isced4_to_3cat <- function(x) {
  dplyr::case_when(
    x %in% c(1, 2) ~ "low",
    x == 3         ~ "medium",
    x == 5         ~ "high",
    TRUE           ~ NA_character_
  )
}

# ISCO major group -> LWS occa1 (1 upper / 2 middle / 3 lower).
# pe0300 is a CHARACTER column holding the major group times ten ("10".."90"),
# with "", "-1", "-2" for missing/DK/NA.
#   1 Managers, 2 Professionals, 3 Technicians          -> upper
#   4 Clerical, 5 Service/sales, 6 Skilled agricultural,
#   7 Craft, 8 Plant and machine operators              -> middle
#   9 Elementary occupations                            -> lower
#   0 Armed forces                                      -> NA (not rankable)
.isco_to_occa1 <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    x %in% c("10", "20", "30")                   ~ 1L,
    x %in% c("40", "50", "60", "70", "80")       ~ 2L,
    x == "90"                                    ~ 3L,
    TRUE                                         ~ NA_integer_
  )
}

.valid <- function(z) !is.na(z) & z >= 0

# =============================================================================
# Build the grouping table, one implicate at a time
# =============================================================================

message("=== 01g: building W5.0 stratification variables ===")

strat_by_imp <- map_dfr(1:5, function(i) {
  message("  [implicate ", i, "/5] reading d", i, ", p", i, ", pn", i, " ...")

  d <- .read_lc(file.path(DATA_DIR, paste0("d", i, ".dta"))) |>
    select(any_of(c("sa0100", "sa0010", "di2000", "di1300", "di1400",
                    "di1700", "dheduh1")))

  p <- .read_lc(file.path(DATA_DIR, paste0("p", i, ".dta"))) |>
    select(any_of(c(PKEY, "ra0100", "pa0200", "pe0300", "pe0350", "pe0370")))

  pn <- .read_lc(file.path(DATA_DIR, paste0("pn", i, ".dta"))) |>
    select(any_of(c(PKEY, "pna0600a", "pna0600b")))

  # --- household-level income and reference-person education -----------------
  hh <- d |>
    transmute(
      sa0100, sa0010,
      # LWS hitotal = total household gross income
      hitotal   = di2000,
      # LWS hicapital = income from capital. di1400 already contains di1410
      # (financial assets) and di1420 (non-self-employment private business),
      # so summing those children as well would double-count.
      hicapital = coalesce(di1300, 0) + coalesce(di1400, 0),
      # LWS hiprivate = regular private (inter-household) transfers. Ch05f's
      # inc_diag block reports its share so the grouping can be checked for
      # containing part of the treatment.
      hiprivate = di1700,
      educ      = .isced4_to_lws3(dheduh1)
    )

  # --- reference person: occupation and own parents' education ---------------
  rp <- p |>
    filter(ra0100 == 1) |>
    transmute(
      sa0100, sa0010, ra0010,
      # Current job first; fall back to the retired and then the unemployed
      # variants. Current-job ISCO alone covers only ~44-78% of reference
      # persons in an age>=21-uncapped sample because retirees have no current
      # job — and retirees are precisely the inheritance-relevant households.
      # With the fallbacks coverage reaches ~86-98% in most countries.
      isco_any = dplyr::coalesce(
        na_if(na_if(na_if(trimws(as.character(pe0300)), ""), "-1"), "-2"),
        na_if(na_if(na_if(trimws(as.character(pe0370)), ""), "-1"), "-2"),
        na_if(na_if(na_if(trimws(as.character(pe0350)), ""), "-1"), "-2")
      )
    ) |>
    mutate(occa1 = .isco_to_occa1(isco_any)) |>
    select(-isco_any)

  rp_par <- p |>
    filter(ra0100 == 1) |>
    select(all_of(PKEY)) |>
    left_join(pn, by = PKEY) |>
    transmute(
      sa0100, sa0010,
      pared_father = .isced4_to_3cat(if_else(.valid(pna0600a), pna0600a, NA_real_)),
      pared_mother = .isced4_to_3cat(if_else(.valid(pna0600b), pna0600b, NA_real_))
    )

  # --- partner-max (dominance) variant, for robustness -----------------------
  # Highest parental education across the reference person AND their spouse or
  # partner, over both parents. This is the "dominance" convention common in
  # social-mobility research. It is a CLASSIFICATION sensitivity check, not a
  # coverage-expanding one: in CY and PT parental education is collected for
  # all adults or none (coverage 98.6 -> 99.0 and 100 -> 100), so it changes
  # only households where RP and partner differ in parental background.
  .rank3 <- function(z) dplyr::case_when(z == "high" ~ 3L, z == "medium" ~ 2L,
                                         z == "low" ~ 1L, TRUE ~ NA_integer_)
  pmax_tbl <- p |>
    filter(ra0100 %in% c(1, 2)) |>
    select(all_of(PKEY)) |>
    left_join(pn, by = PKEY) |>
    mutate(
      r = pmax(.rank3(.isced4_to_3cat(if_else(.valid(pna0600a), pna0600a, NA_real_))),
               .rank3(.isced4_to_3cat(if_else(.valid(pna0600b), pna0600b, NA_real_))),
               na.rm = TRUE)
    ) |>
    group_by(sa0100, sa0010) |>
    summarise(rmax = suppressWarnings(max(r, na.rm = TRUE)), .groups = "drop") |>
    mutate(
      rmax = if_else(is.finite(rmax), rmax, NA_integer_),
      eddad_3cat_max = dplyr::case_when(rmax == 3L ~ "high", rmax == 2L ~ "medium",
                                        rmax == 1L ~ "low",  TRUE ~ NA_character_)
    ) |>
    select(sa0100, sa0010, eddad_3cat_max)

  out <- hh |>
    left_join(rp,      by = c("sa0100", "sa0010")) |>
    left_join(rp_par,  by = c("sa0100", "sa0010")) |>
    left_join(pmax_tbl, by = c("sa0100", "sa0010")) |>
    mutate(implicate = i)

  rm(d, p, pn, hh, rp, rp_par, pmax_tbl); gc(verbose = FALSE)
  out
})

# --- primary Tier 2b variable: father, falling back to mother ----------------
# Mirrors R/05b_satellite_parental_education.R exactly: prefer the reference
# person's father's education, and fall back to the mother's only where the
# father's coverage in that country is below 30%. In CY and PT father and
# mother coverage are near-identical, so the fallback is not expected to fire;
# it is kept so the HFCS and LWS Tier 2b definitions are literally the same rule.
father_cov <- strat_by_imp |>
  filter(implicate == 1) |>
  group_by(sa0100) |>
  summarise(pct_father = mean(!is.na(pared_father)), .groups = "drop")

strat_by_imp <- strat_by_imp |>
  left_join(father_cov, by = "sa0100") |>
  mutate(
    eddad_3cat = if_else(pct_father >= 0.30, pared_father, pared_mother),
    edmom_3cat = pared_mother
  ) |>
  select(-pct_father)

# =============================================================================
# Merge onto the cached prepped object
# =============================================================================

if (!file.exists(CACHE_IN)) stop("Missing ", CACHE_IN)

# Free the person-file working set before pulling in the ~875 MB cache.
# Without this the run holds five implicates of accumulated person data AND the
# cache at the same time, and on 2026-08-16 that produced
# `readRDS: Lesefehler aus Verbindung` — a memory failure that reads like file
# corruption. The cache was verified intact immediately afterwards. Same class
# of failure as the 2026-08-15 partial multiwave build: concurrency and
# memory, never a bug in the logic.
gc(verbose = FALSE)
message("Reading cache (~875 MB) ...")
prepped <- readRDS(CACHE_IN)
message("\nLoaded cache: ", length(prepped$data), " countries")

STRAT_COLS <- c("hitotal", "hicapital", "hiprivate", "educ", "occa1",
                "eddad_3cat", "edmom_3cat", "eddad_3cat_max")

join_tbl <- strat_by_imp |>
  transmute(country = sa0100, hid = as.character(sa0010), implicate,
            across(all_of(STRAT_COLS)))

prepped$data <- imap(prepped$data, function(df, key) {
  n_before <- nrow(df)
  out <- df |> left_join(join_tbl, by = c("country", "hid", "implicate"))
  if (nrow(out) != n_before) {
    stop("Join changed row count for ", key, " (", n_before, " -> ", nrow(out),
         ") — the person-file key is not unique. Check PKEY.")
  }
  out
})

# =============================================================================
# Coverage diagnostic
# =============================================================================

coverage <- map_dfr(names(prepped$data), function(key) {
  df <- prepped$data[[key]] |> filter(implicate == 1)
  tibble(
    country        = df$country[1],
    n_hh           = nrow(df),
    pct_hitotal    = mean(!is.na(df$hitotal)),
    pct_educ       = mean(!is.na(df$educ)),
    pct_educ_high  = mean(df$educ == 3, na.rm = TRUE),
    pct_occa1      = mean(!is.na(df$occa1)),
    pct_pared      = mean(!is.na(df$eddad_3cat)),
    pct_pared_max  = mean(!is.na(df$eddad_3cat_max)),
    tier2b_usable  = mean(!is.na(df$eddad_3cat)) >= 0.30
  )
}) |>
  mutate(across(starts_with("pct"), ~ round(.x, 3))) |>
  arrange(country)

message("\n--- Grouping variable coverage (implicate 1) ---")
print(as.data.frame(coverage), row.names = FALSE)

message("\nTier 2b usable (>=30% parental education): ",
        paste(coverage$country[coverage$tier2b_usable], collapse = ", "))

write.csv(coverage, file.path(OUTDIR, "hfcs_w50_strat_var_coverage.csv"),
          row.names = FALSE)
saveRDS(prepped, CACHE_OUT)
message("\nWrote ", CACHE_OUT)
message("Done.")
