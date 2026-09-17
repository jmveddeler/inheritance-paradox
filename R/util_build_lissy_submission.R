# ============================================================
# util_build_lissy_submission.R
# Concatenate 00 + baseline + challenge(s) into one LISSY-ready script
# ============================================================
# Run each stage independently on LISSY — each job is a fresh R session
# so every submission must include the baseline to rebuild `prepped`.
#
# Usage (run locally, then paste output file into LissyWeb):
#
#   source("R/util_build_lissy_submission.R")
#   build_lissy_submission(challenges = NULL)   # Step 1: baseline replication only
#   build_lissy_submission(challenges = "03")   # Step 2: + isogini challenge
#   build_lissy_submission(challenges = "04")   # Step 3: + measure robustness
#   build_lissy_submission(challenges = "05")   # Step 4: + stratification groups
#   build_lissy_submission(challenges = "05c")  # Step 4c: + Gini within/between/overlap
#     #         by NWX tercile (Pyatt 1976; positive-wealth subsample; mirrors
#     #         Ch05 Tier 3's grouping exactly, Gini instead of CV² — see script header)
#     #         DROPPED from paper 2026-08-13 — kept for record only.
#   build_lissy_submission(challenges = "05d")  # Step 4d: + Heikkuri-Schief (2026) Gini decomp
#     #         (2-term, no residual, negative-wealth-robust; sociological tiers
#     #         2a/2a+/2b — Gini twin to Monti-Santoro I; see script header)
#   build_lissy_submission(challenges = "05e")  # Step 4e: + H&S x capitalisation scenarios
#     #         Diagnoses why Tier 3 returns NA under the uncapped baseline
#     #         (negative bottom-tercile NWX mean), AND — since 2026-08-13 —
#     #         computes H&S Gini + CV² + MLD Tier 3 on IDENTICAL rows under all
#     #         4 scenarios, which is what makes the measure-comparison a real
#     #         test rather than a cross-scenario artefact. Use the
#     #         `cap_tier3_measures` section, not `hs_cap_tier3`.
#   build_lissy_submission(challenges = "05f")  # Step 4f: CONSOLIDATED stratification
#     #         battery. Supersedes 05 (Tiers 2a/2a+/2b), 05c (dropped) and 05d.
#     #         Does NOT supersede 05b (wave-locked to lu18/us16) or 05e
#     #         (capitalisation-scenario axis) — both remain valid standalone.
#     #         5 clean groupings (income x2, education, occupation, parental
#     #         education); NO NWX groupings by design. Measures: CV2, VARIANCE
#     #         (absolute), H&S Gini, Monti-Santoro I, MLD (guarded). Emits a
#     #         per-group profile — which group compresses, and where transfer
#     #         volume lands — which the old aggregate-only output could not show.
#   build_lissy_submission(challenges = "06")   # Step 5: + assumption robustness
#   build_lissy_submission(challenges = "07")   # Step 6: + measures under gradient (04x06)
#   build_lissy_submission(challenges = "07b")  # Step 6b: + precision check (Atkinson/MLD CIs)
#   build_lissy_submission(challenges = "08", include_baseline = FALSE)
#     # Step 7: temporal robustness — builds its OWN data (65 waves);
#     #         skip the 10-country baseline to halve runtime.
#   build_lissy_submission(challenges = "09")   # Step 8: + gradient-schedule sensitivity
#     #         + Atkinson(2) outlier/composition diagnostic
#   build_lissy_submission(challenges = "12", include_baseline = FALSE)
#     # Step 9: temporal x measure-dependence (ES/US/AT, gradient vs baseline);
#     #         self-contained — skips the 10-country baseline to reduce runtime.
#
# Output: writes to lissy_submissions/submission_LABEL_YYYYMMDD_HHMM.R
# ============================================================

build_lissy_submission <- function(
  challenges = NULL,
  baseline   = "R/02_lws_baseline_replication_lissy.R",
  contract   = "R/00_prepped_contract.R",
  outdir     = "lissy_submissions",
  include_baseline = TRUE,
  # One capitalisation scenario per submission. Added 2026-08-28 after LISSY
  # held two jobs for manual review: LIS flags a job whose LISTING is excessively
  # long and tells you to "split your program code into smaller parts". Running
  # all three scenarios in one job roughly tripled the listing, past the largest
  # that has ever come back. NULL keeps the old all-scenarios behaviour, which is
  # right for local runs and wrong for LISSY.
  scenario   = NULL
) {
  if (!is.null(scenario)) {
    stopifnot(length(scenario) == 1L)
    valid <- c("baseline_3pct", "capped_3pct", "gradient")
    if (!scenario %in% valid)
      stop("scenario must be one of: ", paste(valid, collapse = ", "))
  }

  challenge_files <- c(
    "03"  = "R/03_challenge_stratification_isogini.R",
    "05"  = "R/05_challenge_stratification_groups.R",
    "05b" = "R/05b_satellite_parental_education.R",
    "05c" = "R/05c_challenge_gini_decomposition.R",
    "05d" = "R/05d_challenge_gini_hs_decomposition.R",
    "05e" = "R/05e_challenge_gini_hs_cap_scenarios.R",
    "05f" = "R/05f_challenge_stratification_consolidated.R",
    # Sub-challenge to 05f (2026-08-16): self-grouped shape decomposition.
    # Asks whether the fall in measured inequality is within-layer compression
    # or the layers converging. Cheap — no replicate-weight loop.
    "05g" = "R/05g_challenge_selfgrouped_decomposition.R",
    "06"  = "R/06_challenge_assumption_robustness.R",
    "07"  = "R/07_challenge_measure_battery.R",   # the measure battery
    "07b" = "R/07b_challenge_gradient_measures_ci.R",
    "08"  = "R/08_challenge_temporal_robustness.R",
    "09"  = "R/09_challenge_gradient_sensitivity.R",
    "11"  = "R/11_challenge_pension_wealth.R",
    "12"  = "R/12_challenge_temporal_x_measures.R",
    "13"  = "R/13_challenge_stabilisation_receipt_year.R",
    "14"  = "R/14_challenge_macro_flows.R",
    "15"  = "R/15_challenge_equal_split_counterfactual.R",
    "16"  = "R/16_challenge_coverage_correction.R",
    "17"  = "R/17_challenge_unit_of_analysis.R",
    # "16" and "17" were each listed TWICE here until 2026-08-22. R keeps both
    # entries in a named vector and `challenge_files["16"]` silently returns the
    # first, so the bundle was correct — but a divergent second entry would have
    # been invisible. Exactly the "parallel hard-coded lists" failure mode.
    # Ch19 (2026-08-22): Raffinetti-Siletti-Vernizzi Gini normalisation, valid
    # when wealth is negative. Needs only contract columns, so no extra_vars.
    "19"  = "R/19_challenge_negative_wealth_gini.R"

  )

  if (!is.null(challenges)) {
    stopifnot(all(challenges %in% names(challenge_files)))
  }
  selected <- challenge_files[challenges]

  # Read file and remove only source() calls and file-exists guards
  read_for_lissy <- function(path) {
    lines <- readLines(path)

    # Remove lines that source other files
    lines <- lines[!grepl('^\\s*source\\("R/', lines)]

    # Remove the specific 3-line guard pattern:
    #   if (!file.exists("R/00_prepped_contract.R")) {
    #     stop("Cannot find R/00_prepped_contract.R. Run from project root.")
    #   }
    # Strategy: find the if-line, then remove it + the next line (stop) + the closing }
    guard_starts <- grep('^if \\(!file\\.exists\\("R/', lines)
    if (length(guard_starts) > 0) {
      remove_idx <- unlist(lapply(guard_starts, function(i) {
        # Look ahead for the matching closing }
        end <- i
        for (j in (i + 1):min(i + 3, length(lines))) {
          if (grepl('^\\s*\\}', lines[j])) { end <- j; break }
        }
        i:end
      }))
      lines <- lines[-remove_idx]
    }

    paste(lines, collapse = "\n")
  }

  # Build concatenated script
  ch_desc <- if (length(selected) == 0) "none (baseline only)" else paste(basename(unname(selected)), collapse = ", ")
  parts <- c(
    "# ============================================================",
    paste0("# LISSY SUBMISSION — generated ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste0("# Baseline: ", if (include_baseline) basename(baseline) else "none (self-contained challenge)"),
    paste0("# Challenges: ", ch_desc),
    paste0("# Scenario: ", if (is.null(scenario)) "all three (NOT for LISSY - see `scenario`)" else scenario),
    "# ============================================================",
    "",
    if (!is.null(scenario)) c(
      "# === PART 0: single-scenario restriction ===",
      "# Split submission: this job runs ONE capitalisation scenario, to keep the",
      "# listing short enough not to trip LIS's excessive-listing check. Submit the",
      "# other scenarios as separate jobs and combine the parsed CSVs locally.",
      paste0('LISSY_SCENARIO <- "', scenario, '"'),
      ""
    ) else character(0),
    "# === PART 1: Shared contract (00_prepped_contract.R) ===",
    "",
    read_for_lissy(contract),
    "",
    # PART 1b: the shared measure battery. Added 2026-08-28 because Ch20 (and
    # any future challenge) sources it, and read_for_lissy() STRIPS source()
    # calls - so without this the bundle would run on LISSY with every mb_*
    # function undefined. Included unconditionally: it is a few hundred lines
    # of pure function definitions with no side effects.
    "# === PART 1b: Shared measure battery (measures_battery.R) ===",
    if (file.exists("R/measures_battery.R"))
      read_for_lissy("R/measures_battery.R") else character(0),
    ""
  )
  if (include_baseline) {
    parts <- c(parts,
      "# === PART 2: LWS Baseline (02_lws_baseline_replication_lissy.R) ===",
      "",
      read_for_lissy(baseline),
      ""
    )
  }

  for (ch in names(selected)) {
    parts <- c(parts,
      paste0("# === PART 3.", ch, ": Challenge (", basename(selected[ch]), ") ==="),
      "",
      read_for_lissy(selected[ch]),
      ""
    )
  }

  # Write output
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  ch_label  <- if (length(selected) == 0) "baseline" else paste(names(selected), collapse = "_")
  if (!is.null(scenario))
    ch_label <- paste0(ch_label, "_", sub("_3pct$", "", scenario))
  outfile   <- file.path(outdir, paste0("submission_", ch_label, "_", timestamp, ".R"))
  writeLines(parts, outfile)

  message("Written: ", outfile)
  message("Lines: ", length(readLines(outfile)))
  message("\nPaste the contents of this file into LissyWeb.")
  invisible(outfile)
}
