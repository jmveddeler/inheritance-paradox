# ============================================================
# util_parse_lissy_log.R
# Split a raw LISSY output log into individual CSV files.
# ============================================================
# Usage:
#   source("R/util_parse_lissy_log.R")
#   parse_lissy_log("results/Lissy/raw/submission_baseline_20260703_1103.txt")
#
# Output (baseline): writes 4 CSVs into results/Lissy/processed/:
#   lws_baseline_availability.csv
#   lws_baseline_summary.csv
#   lws_lorenz_export.csv
#   lws_wt_quintile_export.csv
#
# NOTE: Challenge 03 (isogini) upgraded 2026-08-06 to use the
#   SECTION:/END: marker convention. Parse with parse_lissy_sections():
#   parse_lissy_sections("results/Lissy/raw/submission_03_<ts>_results.txt",
#                         prefix = "lws_03")
#   This produces: lws_03_iso_tier1.csv, lws_03_iso_tier2.csv,
#                  lws_03_iso_tier3.csv
#
# Challenge 04 still uses the legacy write.csv()-anchored format.
# ============================================================

parse_lissy_log <- function(
  log_file,
  outdir = "results/Lissy"
) {

  lines <- readLines(log_file, warn = FALSE)

  # Anchor on the write.csv() echo lines (> write.csv(...)) — robust to
  # section ordering in the script; the CSV output follows the echo immediately.
  sections <- c(
    lws_baseline_availability = "> write.csv(availability",
    lws_baseline_summary      = "> write.csv(baseline_summary",
    lws_lorenz_export         = "> write.csv(lorenz_export",
    lws_wt_quintile_export    = "> write.csv(wt_quintile_export",
    # Challenge 04: measure robustness (legacy write.csv() format)
    lws_04_tier1_results      = "> write.csv(tier1_results",
    lws_04_tier1_diffs        = "> write.csv(tier1_diffs",
    lws_04_dominance_summary  = "> write.csv(dominance_summary",
    lws_04_intersection_points = "> write.csv(intersection_points",
    lws_04_tier2_results      = "> write.csv(tier2_results"
    # Ch03 isogini: upgraded 2026-08-06 to SECTION:/END: — use parse_lissy_sections()
  )

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  for (name in names(sections)) {
    marker <- grep(sections[name], lines, fixed = TRUE)[1]
    if (is.na(marker)) {
      warning("Section not found in log: ", sections[name])
      next
    }

    # First CSV header line after the marker: starts with a double-quoted field name
    csv_start <- NA
    for (i in seq(marker + 1, length(lines))) {
      if (grepl('^"[A-Za-z_]', lines[i])) { csv_start <- i; break }
    }
    if (is.na(csv_start)) {
      warning("No CSV block found after section: ", sections[name])
      next
    }

    # Collect contiguous CSV lines (quoted strings, numbers, or negative numbers)
    csv_end <- csv_start
    for (i in seq(csv_start + 1, length(lines))) {
      if (i > length(lines)) break
      ln <- trimws(lines[i])
      if (nchar(ln) == 0) break                    # blank line = end of block
      if (!grepl('^["0-9-]', ln)) break             # non-CSV line = end of block
      csv_end <- i
    }

    outfile <- file.path(outdir, paste0(name, ".csv"))
    writeLines(lines[csv_start:csv_end], outfile)
    message("Written: ", outfile, "  (", csv_end - csv_start + 1, " lines)")
  }

  invisible(NULL)
}

# ============================================================
# parse_lissy_sections()
# Generic parser for the `SECTION:name` / `END:name` marker
# convention used from Challenge 06 onwards, and Ch03 (upgraded
# 2026-08-06). Extracts every SECTION block automatically.
#
# Usage:
#   source("R/util_parse_lissy_log.R")
#   parse_lissy_sections("results/Lissy/raw/submission_03_<ts>_results.txt",
#                         prefix = "lws_03")
#   # → lws_03_iso_tier1.csv, lws_03_iso_tier2.csv, lws_03_iso_tier3.csv
#
#   parse_lissy_sections("results/Lissy/raw/submission_07b_20260723_1909_results.txt",
#                         prefix = "lws_07b")
# ============================================================

parse_lissy_sections <- function(
  log_file,
  outdir = "results/Lissy/processed",
  prefix
) {

  lines <- readLines(log_file, warn = FALSE)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  starts <- grep("^SECTION:", lines)
  if (length(starts) == 0) {
    warning("No SECTION: markers found in log: ", log_file)
    return(invisible(NULL))
  }

  for (s in starts) {
    # TRIM (2026-09-10). Challenges are inconsistent about the marker: most
    # emit "SECTION:name", but Ch11 emits "SECTION: name" with a space, which
    # previously carried the space into the output FILENAME.
    name <- trimws(sub("^SECTION:", "", lines[s]))
    e <- grep(paste0("^END:[[:space:]]*", name, "[[:space:]]*$"), lines)
    e <- e[e > s][1]
    if (is.na(e)) {
      warning("No matching END marker for section: ", name)
      next
    }

    # CSV body sits between the SECTION/END markers, but LISSY echoes the
    # `> write.csv(...)`/`> cat(...)` command lines too — keep only lines
    # that look like CSV content (quoted field or leading digit/minus).
    body <- lines[(s + 1):(e - 1)]
    body <- body[grepl('^"|^[0-9-]', trimws(body))]

    # Some challenges already namespace their own section names (Ch11 emits
    # "lws_11_coverage", every other challenge emits a bare "coverage"), so
    # re-prefixing there would produce lws_11_lws_11_coverage.csv.
    stem <- if (startsWith(name, paste0(prefix, "_"))) name else paste0(prefix, "_", name)
    outfile <- file.path(outdir, paste0(stem, ".csv"))
    writeLines(body, outfile)
    message("Written: ", outfile, "  (", length(body), " lines)")
  }

  invisible(NULL)
}
