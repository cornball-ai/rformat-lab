#!/usr/bin/env r
# Test one file with one parameter combo (for use with gnu parallel)
# Usage: r stress_one_combo.R <file> <combo_index>
#   parallel -j10 r stress_one_combo.R file.R ::: $(seq 1 10)
library(rformat)

f <- argv[1]
ci <- as.integer(argv[2])
bn <- basename(f)
code <- paste(readLines(f, warn = FALSE), collapse = "\n")

combos <- list(
    list(cb = FALSE, ei = FALSE, bs = "kr", fs = FALSE, es = TRUE, w = "paren", ind = 4L, lw = 80L),
    list(cb = TRUE, ei = FALSE, bs = "kr", fs = FALSE, es = TRUE, w = "paren", ind = 4L, lw = 80L),
    list(cb = "single", ei = TRUE, bs = "kr", fs = FALSE, es = TRUE, w = "paren", ind = 8L, lw = 80L),
    list(cb = "multi", ei = TRUE, bs = "allman", fs = TRUE, es = TRUE, w = "fixed", ind = 8L, lw = 80L),
    list(cb = "same_line", ei = TRUE, bs = "kr", fs = TRUE, es = TRUE, w = "paren", ind = 4L, lw = 100L),
    list(cb = "next_line", ei = FALSE, bs = "allman", fs = FALSE, es = FALSE, w = "fixed", ind = 2L, lw = 120L),
    list(cb = FALSE, ei = TRUE, bs = "kr", fs = FALSE, es = FALSE, w = "paren", ind = 4L, lw = 80L),
    list(cb = TRUE, ei = TRUE, bs = "allman", fs = TRUE, es = TRUE, w = "fixed", ind = 8L, lw = 100L),
    list(cb = "multi", ei = FALSE, bs = "kr", fs = FALSE, es = TRUE, w = "paren", ind = 2L, lw = 80L),
    list(cb = "same_line", ei = FALSE, bs = "kr", fs = FALSE, es = TRUE, w = "fixed", ind = 4L, lw = 80L)
)

opts <- combos[[ci]]
cb_lbl <- if (is.logical(opts$cb)) { if (opts$cb) "T" else "F" } else opts$cb
olbl <- sprintf("cb=%s,ei=%s,bs=%s,fs=%s,es=%s,w=%s,in=%d,lw=%d",
    cb_lbl, opts$ei, opts$bs, opts$fs, opts$es, opts$w, opts$ind, opts$lw)

formatted <- tryCatch(
    suppressWarnings(rformat(code, control_braces = opts$cb,
        expand_if = opts$ei, brace_style = opts$bs,
        function_space = opts$fs, else_same_line = opts$es,
        wrap = opts$w, indent = opts$ind, line_limit = opts$lw)),
    error = function(e) NULL)
if (is.null(formatted)) { cat(sprintf("ERROR\t%s\t%s\n", bn, olbl)); quit(save = "no") }
ok <- !is.null(tryCatch(parse(text = formatted), error = function(e) NULL))
if (!ok) { cat(sprintf("FAIL\t%s\t%s\n", bn, olbl)); quit(save = "no") }
formatted2 <- tryCatch(
    suppressWarnings(rformat(formatted, control_braces = opts$cb,
        expand_if = opts$ei, brace_style = opts$bs,
        function_space = opts$fs, else_same_line = opts$es,
        wrap = opts$w, indent = opts$ind, line_limit = opts$lw)),
    error = function(e) NULL)
if (!is.null(formatted2) && formatted2 != formatted)
    cat(sprintf("IDEMP\t%s\t%s\n", bn, olbl))
