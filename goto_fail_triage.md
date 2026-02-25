# Goto-fail Pattern Triage

Analysis of 31 suspicious bare next-line control flow patterns found in
the 30 packages that ship with R. Inspired by Apple's
[goto fail bug](https://en.wikipedia.org/wiki/Unreachable_code#goto_fail_bug).

## Classification

### Likely bug (1)

**foreign :: writeForeignSAS.R:135** — `for` loop body is only the `cat()`;
the `if(strings[v])` runs once after the loop with `v = ncol(df)`, so only
the last column gets the `" $ "` string type marker in SAS codefile output.

```r
for(v in 1L:ncol(df))
    cat("\n", varnames[v], file = codefile, append = TRUE)
    if(strings[v]) cat(" $ ", file = codefile, append = TRUE)  # outside loop!
```

Fix: wrap in braces so both lines are inside the for loop.

### Misleading indent but correct (1)

**mgcv :: scasm.r:467** — The `stop()` never returns, so `ii <- ii + 1` and
subsequent lines only execute when `ii <= control$maxit`. The indentation
makes it look like they're inside the `if`, but they're correctly outside it
(inside the enclosing `while` loop). Not a bug.

```r
while (...) {
    if (ii > control$maxit)
      stop("inner loop 2; cannot correct step size", call. = FALSE)
      ii <- ii + 1        # runs only if stop() didn't fire
      start <- (start + coefold)/2
      ...
}
```

### Expression if-else spanning lines (17)

These are all value-producing `if (cond) value1 else value2` expressions
wrapped across multiple lines. The "same indent next line" is the `else`
branch or continuation — syntactically correct, just triggers the pattern
detector. Not bugs.

- base :: all.equal.R:357 — `c(if(...) paste0(...), if(...))`
- grDevices :: zzz.R:31 — `extras <- if(...) list(...) else list(...)`
- methods :: addedFunctions.R:56 — `stop(if(generic) ... else ...)`
- methods :: as.R:454 — `sig@package <- if(...) ... else ...`
- methods :: RClassUtils.R:952 — `paste0(how[i], if(...) ... else ...)`
- stats :: fisher.test.R:157 — `return(if(...) ... else ...)`
- stats :: nlm.R:138 — `stop(if(doX) ...)`
- tools :: dynamicHelp.R:458 — `titles[i] <- if(...) ... else ...`
- tools :: QC.R:8802 — `paste(if(...) ... else ..., ...)`
- tools :: QC.R:8808 — same pattern
- tools :: QC.R:8814 — same pattern
- tools :: toHTML.R:292 — `c(if(...) c(...), ...)`
- Matrix :: Auxiliaries.R:77 — `diag <- if(...) "U" else "N"`
- Matrix :: Auxiliaries.R:84 — same pattern
- Matrix :: Auxiliaries.R:99 — same pattern
- mgcv :: plots.r:273 — `linpred <- if(...) ... else ...`
- mgcv :: scasm.r:1281 — `object$bs <- if(...) ... else ...`

### Multi-line single expression (5)

The body is a single expression that wraps to the next line. The continuation
has the same indent but is part of the same expression (connected by an
operator or opening paren). Not bugs.

- base :: all.equal.R:31 — body is `.Deprecated(...)`, next line is
  `return(...)` at different indent level (tab vs spaces causes false match)
- methods :: show.R:22 — body is `.Deprecated(msg = ...)`, string continues
- stats :: contrast.R:34 — body is `warning(sprintf(...)`, string continues
- stats :: wilcox.test.R:380 — body is `warning(...)`, string continues
- mgcv :: gam.fit4.r:354 — body is `stop(...)`, string continues

### Chained bare for loops (4)

Nested `for` loops where the inner loop's `{` body is on the next line at
the same indent as the `for` keyword. Syntactically correct — the inner
`for` is the body of the outer `for`.

- lattice :: interaction.R:766 — `for (column ...) if (row != column) {`
- lattice :: print.trellis.R:818 — `for (row ...) for (column ...) {`
- lattice :: print.trellis.R:866 — same pattern
- lattice :: splom.R:184 — `for(i ...) for(j ...) {`

### Chained bare for + setMethod (1)

- Matrix :: Ops.R:841 — `for(Mcl ...) for(cl ...) setMethod(...)`.
  Both loops have a single bare body. Correct.

### Multi-line assignment as body (2)

The body is an assignment where the RHS wraps to the next line. The
continuation has the same indent as the body start. Not bugs — the
continuation is part of the same expression.

- nlme :: lme.R:2517 — `stdFixed <- stdFixed * sqrt(...)`
- nlme :: lme.R:2758 — `fit[, "fixed"] <- conLin$Xy[...] %*% lmeFit$beta`

## Summary

| Category | Count |
|----------|------:|
| Likely bug | 1 |
| Misleading indent (correct) | 1 |
| Expression if-else | 17 |
| Multi-line single expression | 5 |
| Chained bare loops | 5 |
| Multi-line assignment body | 2 |
| **Total** | **31** |

Out of ~12,500 bare next-line control flow bodies across 30 R packages,
only 1 appears to be a genuine goto-fail bug (0.008%). The misleading-indent
case in mgcv happens to be safe because `stop()` never returns.

## Implications for rformat

The `control_braces = "multi"` option (always add braces) would prevent
goto-fail bugs by making the scope explicit. However, R Core overwhelmingly
uses bare bodies (90% of single-statement bodies), and the bug rate is
extremely low. The default of leaving bodies alone (`control_braces = FALSE`)
is appropriate.
