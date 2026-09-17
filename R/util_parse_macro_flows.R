# ============================================================
# util_parse_macro_flows.R
# Parse the hand-maintained macro inheritance-flow workbook export
# ============================================================
# The source file is maintained by hand and exported in a WIDE, semicolon-
# delimited layout chosen so it can be refreshed quickly from the working file:
#
#   Country;Metric;Detail;1900;1910;...;2020;;Comments;Sources;...
#
# Four things make it awkward and are handled explicitly below.
#   1. SEMICOLON delimiter, not comma.
#   2. GERMAN thousands separators — "6.052.000.000" is 6,052,000,000, NOT
#      6.052. Stripping dots is therefore mandatory, and a naive as.numeric()
#      would silently turn nine-figure sums into single digits.
#   3. NON-UTF8 encoding (Windows-1252): source notes contain "ä/ö/ü/€".
#   4. Multi-line quoted Sources fields, so the file has far more physical
#      lines than data rows.
#
# VALUES ARE IN LOCAL CURRENCY AT THE VALUE OF THE TIME, per the header
# note. Pre-euro observations are therefore in DEM/BEF/SEK etc., NOT euro, and
# must not be compared across the changeover without conversion. Only the
# post-1999 rows are euro for euro-area countries.
#
# Output: results/macro/macro_inheritance_flows_tidy.csv (long format)
#   country, iso2, year, metric, flow_type, value, source_notes
#
# Usage: Rscript R/util_parse_macro_flows.R
# ============================================================

suppressMessages({library(dplyr); library(tidyr); library(readr)})

SRC <- "Data/data/JMV 2024_inheritance volumes_fiscal and economic flows.csv"
OUT <- "results/macro/macro_inheritance_flows_tidy.csv"

if (!file.exists(SRC)) stop("Missing ", SRC)

raw <- readr::read_delim(
  SRC, delim = ";",
  locale = readr::locale(encoding = "Windows-1252"),
  col_types = readr::cols(.default = readr::col_character()),
  trim_ws = TRUE, show_col_types = FALSE
)

message("Read ", nrow(raw), " rows x ", ncol(raw), " cols")

YEAR_COLS <- intersect(as.character(seq(1900, 2020, by = 10)), names(raw))
message("Year columns found: ", paste(YEAR_COLS, collapse = " "))

# German thousands separator: strip dots, and any spaces / non-breaking spaces.
# Anything non-numeric after that ("not available", "") becomes NA.
.num <- function(x) {
  x <- gsub("\\.", "", x)                 # thousands separator
  x <- gsub("[[:space:] ]", "", x)
  x <- gsub(",", ".", x)                  # decimal comma, if ever used
  suppressWarnings(as.numeric(ifelse(grepl("^-?[0-9]+(\\.[0-9]+)?$", x), x, NA)))
}

ISO <- c(Germany="DE", France="FR", Belgium="BE", Austria="AT", Italy="IT",
         UK="UK", USA="US", Sweden="SE", Switzerland="CH", Japan="JP",
         SouthKorea="KR", Spain="ES", Netherlands="NL", Portugal="PT",
         Greece="GR", Hungary="HU", Luxembourg="LU", Slovenia="SI",
         Denmark="DK", Finland="FI", Ireland="IE", Norway="NO")

tidy <- raw |>
  filter(!is.na(Country), Country != "", !is.na(Metric), Metric != "") |>
  select(country = Country, metric = Metric, flow_type = Detail,
         all_of(YEAR_COLS), any_of(c("Comments", "Sources"))) |>
  pivot_longer(all_of(YEAR_COLS), names_to = "year", values_to = "value_raw") |>
  mutate(
    year  = as.integer(year),
    value = .num(value_raw),
    iso2  = unname(ISO[gsub("[^A-Za-z]", "", country)])
  ) |>
  filter(!is.na(value)) |>
  mutate(source_notes = paste0(
    ifelse(is.na(Comments), "", Comments),
    ifelse(is.na(Sources) | Sources == "", "", paste0(" | ", Sources)))) |>
  select(country, iso2, year, metric, flow_type, value, source_notes) |>
  arrange(country, metric, year)

message("\nParsed ", nrow(tidy), " observations")

# --- sanity checks -----------------------------------------------------------
bad_iso <- tidy |> filter(is.na(iso2)) |> distinct(country)
if (nrow(bad_iso) > 0) {
  warning("No ISO2 mapping for: ", paste(bad_iso$country, collapse = ", "),
          " — add them to ISO above.", call. = FALSE)
}

message("\n--- observations per country x flow_type ---")
print(as.data.frame(tidy |> count(country, iso2, flow_type)), row.names = FALSE)

message("\n--- modern window (>= 2000), the Challenge 14 comparators ---")
print(as.data.frame(
  tidy |> filter(year >= 2000) |>
    mutate(value_bn = round(value / 1e9, 2)) |>
    select(iso2, year, flow_type, metric, value_bn) |>
    arrange(iso2, year, flow_type)), row.names = FALSE)

dir.create("results/macro", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(tidy, OUT)
message("\nWrote ", OUT, " (", nrow(tidy), " rows)")
