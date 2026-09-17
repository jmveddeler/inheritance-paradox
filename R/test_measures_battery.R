# ============================================================
# test_measures_battery.R
# Assertions for the shared measure battery. Run before trusting any re-run.
# ============================================================
source("R/measures_battery.R")

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
  cat("  ok  ", msg, "\n")
}
set.seed(23)

cat("\n--- 1. CLOSED FORM: S-Gini at nu=2 IS the standard Gini ---\n")
for (i in 1:4) {
  n <- 500; w <- runif(n, 1, 10); x <- rlnorm(n, 11, 1.2)
  g <- mb_gini(x, w)$value; s <- mb_sgini(x, w, 2)$value
  ok(abs(g - s) / g < 1e-9,
     sprintf("draw %d: gini %.8f == sgini(2) %.8f", i, g, s))
}

cat("\n--- 2. S-Gini rises with nu (more weight on the bottom) ---\n")
n <- 800; w <- runif(n, 1, 10); x <- rlnorm(n, 11, 1.2)
v <- sapply(c(2, 3, 4, 6), function(k) mb_sgini(x, w, k)$value)
ok(all(diff(v) > 0), sprintf("monotone in nu: %s", paste(round(v, 4), collapse = " < ")))

cat("\n--- 3. S-Gini stays DEFINED with negative wealth; Atkinson does not ---\n")
xn <- c(rnorm(200, -3e4, 2e4), rlnorm(600, 11, 1.2)); wn <- runif(800, 1, 10)
sg <- mb_sgini(xn, wn, 4); at <- mb_atkinson(xn, wn, 2)
cat(sprintf("     sgini(4)=%.4f ok=%s kept=%.3f | atkinson2=%.4f ok=%s kept=%.3f\n",
            sg$value, sg$ok, sg$kept_w, at$value, at$ok, at$kept_w))
ok(is.finite(sg$value) && sg$kept_w == 1, "S-Gini uses 100% of weight")
ok(at$kept_w < 1, sprintf("Atkinson(2) TRUNCATES: keeps only %.1f%% of weight", 100 * at$kept_w))

cat("\n--- 4. absolute measures are TRANSLATION-INVARIANT ---\n")
shift <- 5e5
for (f in list(list("gini_abs", mb_gini_abs), list("variance", mb_variance))) {
  a <- f[[2]](xn, wn)$value; b <- f[[2]](xn + shift, wn)$value
  ok(abs(a - b) / max(abs(a), 1e-9) < 1e-9,
     sprintf("%-9s unchanged by adding %.0e to everyone", f[[1]], shift))
}
ka <- mb_kolm(xn, wn, 1)$value; kb <- mb_kolm(xn + shift, wn, 1)$value
ok(abs(ka - kb) / max(abs(ka), 1e-9) < 1e-6, "kolm      unchanged by translation")

cat("\n--- 5. relative measures are SCALE-invariant (and absolute ones are not) ---\n")
xp <- rlnorm(600, 11, 1.2); wp <- runif(600, 1, 10)
ok(abs(mb_gini(xp, wp)$value - mb_gini(2 * xp, wp)$value) < 1e-12, "gini scale-invariant")
ok(abs(mb_gini_abs(xp, wp)$value * 2 - mb_gini_abs(2 * xp, wp)$value) < 1e-6,
   "absolute gini scales linearly, as it should")

cat("\n--- 6. the domain guard fires when the mean approaches zero ---\n")
xz <- c(rnorm(400, -1e5, 3e4), rnorm(400, 1.02e5, 3e4))   # mean ~ 0
wz <- rep(1, 800)
g <- mb_gini(xz, wz); s <- mb_sgini(xz, wz, 3); ab <- mb_gini_abs(xz, wz)
cat(sprintf("     mean = %.0f ; gini=%.3f ok=%s ; sgini3 ok=%s ; gini_abs ok=%s\n",
            sum(xz * wz) / sum(wz), g$value, g$ok, s$ok, ab$ok))
ok(!g$ok, "relative Gini flagged NOT ok when the mean is near zero")
ok(!s$ok, "S-Gini likewise flagged NOT ok")
ok(ab$ok, "absolute Gini remains ok — it does not divide by the mean")

cat("\n--- 7. battery returns a domain verdict for every measure ---\n")
b <- suppressWarnings(mb_battery(xn, wn, kolm_scale = 1e5))
cat(sprintf("     %d measures; %d flagged ok; %d positive-only\n",
            nrow(b), sum(b$ok), sum(b$family == "truncating")))
ok(all(c("gini", "sgini4", "gini_abs", "kolm1", "atkinson2",
         "inter2", "p75_p25", "atkinson2_bach1") %in% b$measure),
   "battery includes S-Gini, absolute, intermediate, rank-gap and both handlings")

drop_v <- b[b$family == "truncating" & !grepl("_bach1$", b$measure), ]
bach_v <- b[grepl("_bach1$", b$measure), ]
ok(all(drop_v$kept_w < 1),
   sprintf("TRUNCATE variants drop weight (keep %.1f%%)", 100 * max(drop_v$kept_w)))
ok(all(bach_v$kept_w == 1) && all(bach_v$n_alt > 0),
   sprintf("BACH1 variants keep everyone but flag %.1f%% as replaced",
           100 * max(bach_v$n_alt)))
ok(all(b$kept_w[!b$family %in% "truncating"] == 1),
   "no negative-safe measure drops anyone")

cat("\n--- 8. truncation and bach1 give DIFFERENT answers (neither is neutral) ---\n")
for (m in c("atkinson2", "ge_mld")) {
  a <- b$value[b$measure == m]; z <- b$value[b$measure == paste0(m, "_bach1")]
  cat(sprintf("     %-11s truncate %.4f | bach1 %.4f\n", m, a, z))
  ok(abs(a - z) / max(abs(a), 1e-9) > 1e-6,
     sprintf("%s: the two conventions genuinely disagree", m))
}

cat("\n--- 9. Cowell (2006) intermediate class spans relative to absolute ---\n")
kmin <- -min(xn)
ok(is.na(mb_intermediate(xn, wn, 2, k = kmin * 0.5)$value),
   sprintf("k below the largest debt (%.0f) correctly returns NA", kmin))
ks <- kmin * c(1.05, 10, 1000)
vs <- sapply(ks, function(k) mb_intermediate(xn, wn, 2, k = k)$value)
cat(sprintf("     theta=2: %s\n",
            paste(sprintf("k=%.1e -> %.3e", ks, vs), collapse = " ; ")))
ok(all(is.finite(vs)) && all(diff(vs) < 0),
   "index falls monotonically as k grows, moving from the relative toward the absolute view")
ok(mb_intermediate(xn, wn, 2)$kept_w == 1,
   "intermediate class uses 100% of weight — no truncation at any k")

cat("\n--- 10. ⚠️ Bach's 1-euro rule is NOT safe for bottom-sensitive indices ---\n")
a2 <- mb_atkinson(xn, wn, 2, "bach1")$value
cat(sprintf("     atkinson2 under bach1 = %.4f (maximum possible is 1)\n", a2))
ok(a2 > 0.99,
   "bach1 pins Atkinson(2) at its ceiling — the rule destroys the index it is meant to rescue")

cat("\nAll measure-battery assertions passed.\n")
