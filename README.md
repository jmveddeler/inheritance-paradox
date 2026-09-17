# Is the inheritance paradox real?

**Code and aggregate results** for *Is the inheritance paradox real? Inequality measure choice,
capitalisation assumptions and stratification* — Jan-Marten Veddeler, University of Antwerp. Working paper, 2026.

The "inheritance paradox" is the finding that gifts and inheritances, although received
disproportionately by wealthier households, *reduce* measured wealth inequality. This paper tests
how far that finding depends on the choice of inequality index and on the return assumed on
inherited wealth, how much of it is a mechanical effect of adding a positive component to a sum,
and whether stratification between social groups rises even where the aggregate index falls. It uses
the ECB Household Finance and Consumption Survey (HFCS, Wave 5.0, 18 countries) and the Luxembourg
Wealth Study (LWS, 10 countries).

> **Work in progress.** The code and results correspond to the current working draft and may change
> before publication.

---

## What is here, and what is not

| | Included | Why |
|---|---|---|
| **Code** | The R code that builds the analysis samples, runs the analyses reported in the paper, and produces its tables and figures | Computational reproducibility |
| **Aggregate results** | Country-level estimates (CSV) from which the paper's numbers, tables and figures are read | Every number can be checked without microdata access |
| **Microdata** | **Not included** | HFCS and LWS are confidential and licensed per user — see below |

No household-level data are contained in this repository.

## Three levels of reproduction

**1. Check the paper's numbers — no data access needed (minutes).**

```r
source("R/paper_results.R")
res <- load_paper_results()                  # every result set the paper reads
source("R/paper_fig01_measure_collapse.R")   # regenerates a figure; likewise fig02–fig05
```

`load_paper_results()` also loads a few supplementary result sets (isogini curves, assumption
robustness, a five-wave trend summary, an earlier confidence-interval summary) that the paper does not report; the scripts behind them are
not part of this package.

**2. Re-run the HFCS analysis — requires HFCS access (hours to a day).**
Apply to the ECB for the HFCS User Database (Wave 5.0 for the cross-section, Waves 1–5 for the
temporal analysis) and place the files under `Data/data-raw/HFCS/` as referenced in
`R/01c_hfcs_w50_baseline.R`. Then, from the repository root:

```bash
Rscript R/01c_hfcs_w50_baseline.R                 # W5.0 analysis sample (~20 min)
Rscript R/01g_hfcs_build_strat_vars.R             # attach the stratification groupings
Rscript R/run_07_hfcs.R                           # inequality measures, 1,000 replicate weights (~5-8 h)
Rscript R/run_05f_hfcs_scenarios.R                # stratification, three return regimes (~6-24 h)
Rscript R/01d_hfcs_w50_run_challenge.R 15         # other analyses by number: 09, 10, 14, 15, 16, 17
Rscript R/util_concentration_sweep.R              # concentration counterfactual
Rscript R/01e_hfcs_build_multiwave.R              # five-wave sample, then:
Rscript R/01f_hfcs_run_multiwave_challenge.R 12   # temporal robustness
```

The Lorenz-dominance script is retired to `R/archive/`; move it to `R/` to run it through
`01d_hfcs_w50_run_challenge.R`.

**3. Re-run the LWS analysis — requires LIS registration.**
LWS microdata are accessible only through LISSY, the LIS remote-execution system. Build a
self-contained submission script, paste it into LISSY, save the returned log and parse it:

```r
source("R/util_build_lissy_submission.R")
build_lissy_submission(challenges = "11")    # writes one pasteable script
source("R/util_parse_lissy_log.R")           # parses the returned log into results/Lissy/processed/
```

## Repository structure

```
R/
  00_prepped_contract.R        shared pipeline: sample build, transfer capitalisation, Rubin combining
  measures_battery.R           the inequality measures (relative, absolute, intermediate, rank-based)
  01*_hfcs_*.R, run_*.R        HFCS sample builds and local runners
  02_lws_*.R, util_*lissy*.R   LWS sample build and the LISSY round-trip
  NN_challenge_*.R             one script per analysis — source-agnostic, runs on HFCS or LWS
  util_*.R                     concentration counterfactual, macro inheritance-flow comparators
  paper_*.R                    everything the manuscript reads: results loader, figures, tables
  test_*.R                     tests on synthetic data with known answers (no microdata)
  archive/                     a retired script whose output is still cited
results/
  hfcs_w50/                    HFCS Wave 5.0 cross-section
  hfcs_multiwave/              HFCS Waves 1–5 (temporal analysis)
  Lissy/processed/             LWS, parsed from LISSY logs
  rho_mech/, macro/, cpi/      critical-correlation simulation, macro flow comparators, CPI series
  bonke_replication/           replication of Bönke, von Werder & Westermeier (2017)
  figures/                     rendered figures
```

### Where each part of the paper comes from

| Paper element | Script | Results |
|---|---|---|
| Replication of Bönke et al. (2017) | `01b_hfcs_w15_bonke_replication.R` | `bonke_replication/` |
| Measure and return-assumption dependence | `07_challenge_measure_battery.R` | `hfcs_w50_07_*`, `lws_07_*` |
| Gradient-schedule sensitivity | `09_challenge_gradient_sensitivity.R` | `*_09_schedule_sensitivity_summary.csv` |
| Lorenz dominance | `archive/04_challenge_measure_robustness.R` | `hfcs_w50_04_dominance_summary.csv` |
| Mechanical effect, critical correlation ρ* | `10_challenge_rho_mech_simulation.R`, `paper_fig05_rho_threshold.R`, `15_…` (mean-shift split) | `rho_mech/`, `*_15_eqsplit_wolff.csv` |
| Equal-split and concentration counterfactuals | `15_challenge_equal_split_counterfactual.R`, `util_concentration_sweep.R` | `*_15_eqsplit_*`, `hfcs_w50_21_concentration_gradient.csv` |
| Stratification | `05f_challenge_stratification_consolidated.R` | `*_05f_strat_*` |
| Temporal robustness | `12_challenge_temporal_x_measures.R` | `hfcs_multiwave/` |
| Pension wealth · unit of analysis · survey coverage | `11_…`, `17_…`, `14_…` and `16_…` | `lws_11_verdicts.csv`, `macro/`, `hfcs_w50_14_*` |

Script headers document the design decisions behind each analysis.

## Software

R 4.6.0. Packages: `dplyr` 1.2.1, `purrr` 1.2.2, `tidyr` 1.3.2, `tibble` 3.3.1, `readr` 2.2.0,
`haven` 2.5.5, `Hmisc` 5.2.5, `ggplot2` 4.0.3, `scales` 1.4.0, `ggrepel` 0.9.8, `forcats` 1.0.1,
`jsonlite` 2.0.0, `eurostat` 4.0.0, and `lissyrtools` 0.2.4 for LWS.

Standard errors combine the five HFCS multiple-imputation implicates by Rubin's rules and, where
stated in the paper, the 1,000 HFCS replicate weights. Run times above are for one laptop core.

## Data sources and acknowledgements

- **HFCS.** This paper uses data from the Eurosystem Household Finance and Consumption Survey. The
  results published and the related observations and analysis may not correspond to results or
  analysis of the data producers. Access: [ECB — HFCS](https://www.ecb.europa.eu/stats/ecb_surveys/hfcs/html/index.en.html).
- **LWS.** Luxembourg Wealth Study (LWS) Database, <https://www.lisdatacenter.org> (multiple
  countries; microdata runs completed between July and September 2026). Luxembourg: LIS.
- **CPI.** World Bank consumer price indices.
- **Macro inheritance flows.** Hand-collected from the published literature; sources in
  `results/macro/macro_sources_references.md`.

## Use of AI tools

AI coding assistants were used extensively in writing the code in this repository, working to the
author's specifications and under the author's review. Results were verified through the synthetic-data
tests included here, test runs on real LWS sample data, replication of published estimates, and
agreement between two independent data sources. See the statement on the use of AI tools in the
paper for details.

## Licence and citation

Code: [MIT](LICENSE). Aggregate results and figures: [CC BY 4.0](LICENSE-results).
If you use this material, please cite the paper (see [`CITATION.cff`](CITATION.cff)).

Questions and comments are welcome — please open an issue or get in touch.
