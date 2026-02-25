# Bug: `write.foreign(package = "SAS")` omits `$` type markers for character columns

## Summary

`foreign::writeForeignSAS()` generates incorrect SAS INPUT statements for
data frames with character columns. Due to a missing brace, the `$ ` type
marker (which tells SAS a variable is character, not numeric) is only
checked for the last column instead of every column.

## Affected function

`foreign::writeForeignSAS()` (called via `write.foreign(df, datafile,
codefile, package = "SAS")`)

## Root cause

In `writeForeignSAS.R` lines 135-137:

```r
cat("INPUT", file = codefile, append = TRUE)
for(v in 1L:ncol(df))
    cat("\n", varnames[v], file = codefile, append = TRUE)
    if(strings[v]) cat(" $ ", file = codefile, append = TRUE)
```

The `for` loop body is only the first `cat()` call (line 136). The
`if(strings[v])` on line 137 is **outside the loop** — it executes once
after the loop completes, using the final value of `v` (which is
`ncol(df)`).

This is a variant of [Apple's goto fail
bug](https://en.wikipedia.org/wiki/Unreachable_code#goto_fail_bug):
misleading indentation makes it appear that both lines are inside the loop,
but R's bare (braceless) loop body only includes the single next statement.

## Fix

Add braces to include both statements in the loop body:

```r
cat("INPUT", file = codefile, append = TRUE)
for(v in 1L:ncol(df)) {
    cat("\n", varnames[v], file = codefile, append = TRUE)
    if(strings[v]) cat(" $ ", file = codefile, append = TRUE)
}
```

## Demonstration

```r
library(foreign)

# Create a data frame with character columns in different positions
df <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Carol"),
    score = c(95.5, 87.3, 91.2),
    grade = c("A", "B", "A"),
    stringsAsFactors = FALSE
)

# Write SAS code
datafile <- tempfile(fileext = ".csv")
codefile <- tempfile(fileext = ".sas")
write.foreign(df, datafile, codefile, package = "SAS")

# Show the generated SAS code
cat(readLines(codefile), sep = "\n")
```

### Minimal reproduction (no foreign install needed)

The bug is in the INPUT section generator. This isolates the buggy loop:

```r
df <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Carol"),
    score = c(95.5, 87.3, 91.2),
    grade = c("A", "B", "A"),
    stringsAsFactors = FALSE
)
strings <- vapply(df, is.character, NA)
varnames <- names(df)

cat("strings:", paste(varnames, "=", strings, collapse = ", "), "\n\n")

# Buggy (current code: if is outside the loop)
cat("=== ACTUAL (buggy) ===\nINPUT")
for(v in 1L:ncol(df))
    cat("\n", varnames[v])
    if(strings[v]) cat(" $ ")
cat("\n;\n\n")

# Fixed (braces added)
cat("=== EXPECTED (fixed) ===\nINPUT")
for(v in 1L:ncol(df)) {
    cat("\n", varnames[v])
    if(strings[v]) cat(" $ ")
}
cat("\n;\n")
```

### Actual output (buggy)

Using `write.foreign(df, datafile, codefile, package = "SAS")` with
foreign 0.8-91 on R 4.5.2:

```sas
* Written by R;
*  write.foreign(df, datafile, codefile, package = "SAS") ;

DATA  rdata ;
LENGTH
 name $ 5
 grade $ 1
;

INFILE  "/tmp/file.csv"
     DSD
     LRECL= 22 ;
INPUT
 id
 name
 score
 grade $
;
RUN;
```

Note: the LENGTH section correctly marks both `name` and `grade` as
character (`$`) — that code uses a different loop (line 110) that works
correctly. But the INPUT statement is wrong: only `grade` (the last column)
gets the `$` marker. `name` (column 2) is missing its `$`.

SAS will attempt to read `name` as numeric, causing errors or data loss.

### Expected output (fixed)

```sas
INPUT
 id
 name $
 score
 grade $
;
```

Both `name` and `grade` should have the `$` type marker.

## Impact

Any data frame where a character column is not the last column will produce
an incorrect SAS INPUT statement. SAS will misinterpret the character data
as numeric, causing:

- Read errors when the SAS program is executed
- Silent data loss if SAS coerces strings to missing numeric values

The bug has been present since at least `foreign` version 0.8-91 (current
CRAN release). The file header shows copyright 2004-2015, so this may have
been present for over 10 years.

## Versions

- foreign: 0.8-91 (CRAN)
- R: 4.5.2
- Bug found by static analysis of bare control flow bodies in R Core source
  (see [rformat-lab](https://github.com/cornball-ai/rformat-lab))

## How this was found

This bug was discovered during a style analysis of all 30 packages that
ship with R, checking for
[goto-fail](https://en.wikipedia.org/wiki/Unreachable_code#goto_fail_bug)-style
patterns: bare (braceless) control flow bodies where the next line has
matching indentation but is syntactically outside the body.

Out of ~12,500 bare next-line control flow bodies across all 30 packages,
this was the only instance that appears to be a genuine bug.
