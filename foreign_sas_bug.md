# write.foreign(package="SAS") INPUT statement omits `$` for non-last character columns

## Reproducible example

```r
library(foreign)

df <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Carol"),
    score = c(95.5, 87.3, 91.2),
    grade = c("A", "B", "A"),
    stringsAsFactors = FALSE
)

datafile <- tempfile(fileext = ".csv")
codefile <- tempfile(fileext = ".sas")
write.foreign(df, datafile, codefile, package = "SAS")
cat(readLines(codefile), sep = "\n")
```

## Actual output

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

The LENGTH section correctly marks both `name` and `grade` as character
(`$`). But the INPUT statement only marks `grade` (the last column). `name`
is missing its `$` marker, so SAS will attempt to read it as numeric.

## Expected output

The INPUT section should be:

```sas
INPUT
 id
 name $
 score
 grade $
;
```

Both character columns should have `$`.

## Impact

Any data frame where a character column is not the last column produces an
incorrect SAS INPUT statement. SAS will misinterpret the character data as
numeric, causing read errors or silent data loss.

## Environment

```
R version 4.5.2 (2025-10-31)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.3 LTS
foreign: 0.8-91 (CRAN)
```

## Notes

The cause is a missing brace in `writeForeignSAS()`. In the current source
(`writeForeignSAS.R`, lines 135-137):

```r
for(v in 1L:ncol(df))
    cat("\n", varnames[v], file = codefile, append = TRUE)
    if(strings[v]) cat(" $ ", file = codefile, append = TRUE)
```

The `for` loop body is only the first `cat()` call. The `if(strings[v])` is
outside the loop -- it executes once after the loop finishes, using the final
value of `v` (`ncol(df)`). Adding braces fixes it:

```r
for(v in 1L:ncol(df)) {
    cat("\n", varnames[v], file = codefile, append = TRUE)
    if(strings[v]) cat(" $ ", file = codefile, append = TRUE)
}
```

The bug was found by static analysis of bare control flow bodies across all
30 packages that ship with R.
