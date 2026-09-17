# Ch14 — macro inheritance-flow reference sources

From the hand-collected `Data/data/JMV 2024_inheritance volumes_fiscal and economic flows.csv`,
tidied to `results/macro/macro_inheritance_flows_tidy.csv`. **All values are in national currency**
(an initial EUR label was an error, corrected 2026-08-13) — no FX step is needed, since LWS reports
in national currency too.

⚠️ Working notes formatted for a methods appendix, **not verified bibliographic records**. Check each
against the original before publication.

**Comparator preference used in Ch14:** `economic` > `adjusted fiscal` > `fiscal`. Economic estimates
(mortality-multiplier / Piketty-style modelling) target the true flow; fiscal series capture only the
taxable slice and are systematically far lower — e.g. US 2010: 1,057.5bn economic vs 113.3bn fiscal.
Where two or more points of the same type bracket a survey year, the comparator is **log-linearly
interpolated** (flows grow roughly geometrically); 16 of 24 usable rows use interpolation.

---

## Countries used in Ch14

### Austria
- **economic** — 2010–2020 (2 pts), EUR, quality `as_published`
  - for 2020 Grunberger et al 2024: Inheritances in Austria: A model estimation of intergenerational wealth transfers up to 2050 | estimate for 2025 scaled down to 2020 by CPI, not very accurate but better than the parliamentary inquiry data | for 2010: Altzinger and Humer 2013: Simulation des Aufkommen
- **fiscal** — 2010–2020 (2 pts), EUR, quality `as_published`
  - assumed to be fiscal as they are based on parliamentary inquiries | 2002 for 2000, 2007 for 2010 | Ertl, Michael: From Inheritances to Wealth: A Mortality Multiplier Approach for Austria

### France
- **economic** — 2020, EUR, quality `as_published`
  - owncalculationbasedonthe2020modellingofPikettyin:"OntheLong-RunEvolutionofInheritance:France1820-2050ThomasPikettyParisSchoolofEconomics*September2010**" | mortalityrate*µt*Wt=Inheritancein2009€,theninflatedto2020€s | ,worldbank,org/indicator/NY,GDP,DEFL,KD,ZG?locations=FR

### Italy
- **economic** — 1990–2020 (26 pts), EUR, quality `as_published`
  - The Concentration of Personal Wealth in Italy 1995–2016
- **economic (survey-simulation)** — 2016, PERCENT_OF_NETWORTH, quality `as_published`
  - Same paper, footnote 36: Cannari & D'Alessio (2008) mortality-multiplier applied to SHIW survey wealth gives 0.99% of net worth in 1995 rising to 1.52% in 2016 - the paper calls this 'substantially lower' than its own 2.3% of personal wealth. IMPLIED SURVEY/TAX COVERAGE ~66%: an independent publishe

### UK
- **adjusted fiscal** — 1900–2020 (13 pts), GBP, quality `as_published`
  - Unadjusted estate data (fiscal) + Plus correction for exempt assets and undervaluation a Plus gifts inter vivos [2010 data=2008 Atkinson data; 2000= average 1999/2001]. | Atkinson2018:WealthandinheritanceinBritainfrom1896tothepresent | For 2020: Broome, Corlett & Thwaites - | alternative figures: ba
  - <https://economy2030.resolutionfoundation.org/wp-content/uploads/2023/06/Tax-planning.pdf>
- **fiscal** — 1900–2010 (12 pts), GBP, quality `as_published`
  - Unadjusted estate data (fiscal) Plus gifts inter vivos [2010 data=2008 Atkinson data; 2000= average 1999/2001].

### USA
- **economic** — 1900–2020 (13 pts), USD, quality `as_published`
  - until2010:combiningALVAREDO,GARBINTIandPIKETTY(2017)bytdatamultiplyingwithYtdatafromPikettyandSaez2014 | 2020:24trillionover15yearsfrom2015,deloitte,com/content/dam/insights/us/articles/us-generational-wealth-trends/DUP_1371_Future-wealth-in-America_MASTER,pdf#:~:text=URL%3A%20https%3A%2F%2Fwww2,del
- **fiscal** — 1930–2020 (10 pts), USD, quality `as_published`
  - 1930-1990 inheritance:1968 for 1970, 1982 for 1980): IRS:,irs,gov/statistics/soi-tax-stats-historical-table-17 | 2000-2020: inheritance: IRS:,irs,gov/statistics/soi-tax-stats-historical-table-17 + Gifts: IRS "Table 1: Total gifts of donor, Total gifts, Deductions, Credits, and Net gift tax". 1997/20
  - <https://www.irs.gov/statistics/soi-tax-stats-gift-tax-statistics>

---

## Reserved for HFCS / context (not used in Ch14)

- **Belgium** — fiscal (13 pts, EUR)
- **Germany** — economic (10 pts, EUR); fiscal (11 pts, EUR)
- **Japan** — fiscal (8 pts, JPY); economic (13 pts, JPY)
- **SouthKorea** — economic (6 pts, KRW); fiscal (6 pts, KRW)
- **Sweden** — economic (11 pts, SEK); fiscal (11 pts, SEK)
- **Switzerland** — economic (4 pts, CHF)

---

## Known issues

1. ~~**Austria** — macro value doubtful.~~ **RESOLVED 2026-08-13.** Proper economic estimates were added (2010: €8.0bn, Altzinger & Humer 2013; 2020: €18.0bn, Grunberger et al. 2024), and the earlier Ertl parliamentary-inquiry figures were reclassified as **fiscal**, which is what they are. Austrian coverage falls from an impossible 114–272% to **54–102%**. Note the 2020 figure is scaled from a 2025 estimate by CPI — approximate, and the collector says so.
2. **Italy** — no absolute series is published. Values derive from ratios in Acciari, Alvaredo & Morelli (2024) × WID net national income (`mnninci999`); the ratio series is chart-read at ~±0.5pp. Document as derived.
3. **France** has a single macro point (2020), so pre-2020 waves use nearest-year rather than interpolation and their coverage is understated (the 2020 comparator exceeds the true contemporaneous flow). Only FR 2020 is a like-for-like comparison.
4. **Pre-2002 euro-area values** may be in legacy currency (DEM/ATS/ITL/FRF) — see `currency_pre_euro_ambiguous`. Irrelevant at 2010/2020, matters for the long series.
5. **France source string is mangled in the CSV** (URL split on commas). The underlying source is Piketty, *On the Long-Run Evolution of Inheritance: France 1820–2050* (PSE, 2010), with the collector's own mortality-rate × µt × Wt calculation inflated to 2020 euros.
