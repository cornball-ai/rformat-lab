#!/usr/bin/env r
#
# Analyze control flow body styles in R Core source code
#
# Examines all 22 packages that ship with R to classify how bare vs braced
# control flow bodies (if/for/while) are written:
#
#   bare_same_line  - if (x) y
#   bare_next_line  - if (x)\n    y
#   braced_single   - if (x) { y }
#   braced_multi    - if (x) {\n    y\n}
#
# Usage:
#   r analyze_control_bodies.R

# R Core source directory (extracted base R packages)
base_r_src <- path.expand("~/.cache/rformat_cran_src/base_r_src")
# Also check for CRAN-downloadable recommended packages
cran_cache <- path.expand("~/.cache/rformat_cran_src")

core_pkgs <- list.dirs(base_r_src, full.names = FALSE, recursive = FALSE)
# Add recommended packages if their tarballs are cached
rec_pkgs <- c("boot", "class", "cluster", "codetools", "foreign",
              "KernSmooth", "lattice", "MASS", "Matrix", "mgcv",
              "nlme", "nnet", "rpart", "spatial", "survival")

# Find R source files for a package
find_r_files <- function (pkg) {
    # Check base_r_src first
    r_dir <- file.path(base_r_src, pkg, "R")
    if (dir.exists(r_dir)) {
        return(list.files(r_dir, pattern = "\\.[Rrq]$", full.names = TRUE,
                          recursive = TRUE))
    }
    # Check for CRAN tarball
    tarballs <- list.files(cran_cache,
                           pattern = paste0("^", pkg, "_.*\\.tar\\.gz$"),
                           full.names = TRUE)
    if (length(tarballs) == 0) return(character(0))
    work_dir <- file.path(tempdir(), "rformat_analyze")
    dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
    untar(tarballs[1], exdir = work_dir)
    r_dir <- file.path(work_dir, pkg, "R")
    if (!dir.exists(r_dir)) return(character(0))
    list.files(r_dir, pattern = "\\.[Rrq]$", full.names = TRUE,
               recursive = TRUE)
}

# Classify control flow bodies in a file
classify_bodies <- function (path) {
    code <- tryCatch(
        paste(readLines(path, warn = FALSE), collapse = "\n"),
        error = function(e) NULL
    )
    if (is.null(code) || !nzchar(code)) return(NULL)

    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function(e) NULL
    )
    if (is.null(parsed)) return(NULL)

    pd <- getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) return(NULL)

    terminals <- pd[pd$terminal, ]
    terminals <- terminals[order(terminals$line1, terminals$col1), ]

    results <- data.frame(
        style = character(0),
        keyword = character(0),
        stringsAsFactors = FALSE
    )

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i, ]
        if (!tok$token %in% c("IF", "FOR", "WHILE")) next

        # Find opening ( after keyword
        open_idx <- i + 1L
        if (open_idx > nrow(terminals)) next
        if (terminals$token[open_idx] != "'('") next

        # Find matching )
        paren_depth <- 1L
        close_idx <- open_idx + 1L
        while (close_idx <= nrow(terminals) && paren_depth > 0L) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1L
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1L
            }
            if (paren_depth > 0L) close_idx <- close_idx + 1L
        }
        if (close_idx > nrow(terminals)) next

        # Next token after ) is the body start
        body_idx <- close_idx + 1L
        # Skip comments between ) and body
        while (body_idx <= nrow(terminals) &&
               terminals$token[body_idx] == "COMMENT") {
            body_idx <- body_idx + 1L
        }
        if (body_idx > nrow(terminals)) next

        body_tok <- terminals[body_idx, ]
        close_line <- terminals$line1[close_idx]

        if (body_tok$token == "'{'") {
            # Braced body — find matching } and count statements
            brace_depth <- 1L
            end_idx <- body_idx + 1L
            # Count top-level statements: each new line at depth 1
            # that starts a token is a statement
            stmt_lines <- integer(0)
            while (end_idx <= nrow(terminals) && brace_depth > 0L) {
                if (terminals$token[end_idx] == "'{'") {
                    brace_depth <- brace_depth + 1L
                } else if (terminals$token[end_idx] == "'}'") {
                    brace_depth <- brace_depth - 1L
                }
                if (brace_depth == 1L &&
                    !terminals$token[end_idx] %in% c("'}'", "COMMENT")) {
                    ln <- terminals$line1[end_idx]
                    if (!ln %in% stmt_lines) {
                        stmt_lines <- c(stmt_lines, ln)
                    }
                }
                if (brace_depth > 0L) end_idx <- end_idx + 1L
            }
            if (end_idx > nrow(terminals)) next

            n_stmts <- length(stmt_lines)
            is_single_line <- terminals$line1[end_idx] == body_tok$line1

            if (n_stmts > 1L) {
                style <- "braced_multi_stmt"
            } else if (is_single_line) {
                style <- "braced_single_line"
            } else {
                style <- "braced_single_stmt"
            }
        } else {
            # Bare body
            if (body_tok$line1 == close_line) {
                style <- "bare_same_line"
            } else {
                style <- "bare_next_line"
            }
        }

        results <- rbind(results, data.frame(
            style = style,
            keyword = tok$text,
            stringsAsFactors = FALSE
        ))
    }

    results
}

# Run analysis
cat("Analyzing R Core packages...\n")
all_results <- data.frame(
    style = character(0),
    keyword = character(0),
    package = character(0),
    stringsAsFactors = FALSE
)

for (pkg in unique(c(core_pkgs, rec_pkgs))) {
    files <- find_r_files(pkg)
    if (length(files) == 0) {
        cat("  ", pkg, ": not found\n")
        next
    }
    pkg_results <- do.call(rbind, lapply(files, classify_bodies))
    if (!is.null(pkg_results) && nrow(pkg_results) > 0) {
        pkg_results$package <- pkg
        all_results <- rbind(all_results, pkg_results)
    }
    cat("  ", pkg, ":", length(files), "files,",
        if (!is.null(pkg_results)) nrow(pkg_results) else 0,
        "control bodies\n")
}

cat("\n=== All Bodies ===\n")
tab <- table(all_results$style)
pct <- round(prop.table(tab) * 100, 1)
for (s in names(sort(tab, decreasing = TRUE))) {
    cat(sprintf("  %-20s %5d  (%4.1f%%)\n", s, tab[s], pct[s]))
}
cat(sprintf("  %-20s %5d\n", "TOTAL", sum(tab)))

# Single-statement bodies: the style choice that matters
# Multi-statement bodies MUST have braces — not a style decision
single_stmt <- all_results[all_results$style != "braced_multi_stmt", ]
multi_stmt <- all_results[all_results$style == "braced_multi_stmt", ]

cat(sprintf("\n=== Single-Statement vs Multi-Statement ===\n"))
cat(sprintf("  Single-statement:  %5d  (style choice)\n", nrow(single_stmt)))
cat(sprintf("  Multi-statement:   %5d  (braces required)\n", nrow(multi_stmt)))

cat("\n=== Single-Statement Body Styles (the interesting comparison) ===\n")
stab <- table(single_stmt$style)
spct <- round(prop.table(stab) * 100, 1)
for (s in names(sort(stab, decreasing = TRUE))) {
    cat(sprintf("  %-20s %5d  (%4.1f%%)\n", s, stab[s], spct[s]))
}
cat(sprintf("  %-20s %5d\n", "TOTAL", sum(stab)))

cat("\n=== Single-Statement by Keyword ===\n")
for (kw in c("if", "for", "while")) {
    sub <- single_stmt[single_stmt$keyword == kw, ]
    if (nrow(sub) == 0) next
    cat(kw, ":\n")
    kt <- table(sub$style)
    kp <- round(prop.table(kt) * 100, 1)
    for (s in names(sort(kt, decreasing = TRUE))) {
        cat(sprintf("  %-20s %5d  (%4.1f%%)\n", s, kt[s], kp[s]))
    }
}
