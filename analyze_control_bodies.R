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
        return(list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE,
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
    list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE,
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
            # Braced body — check if single-line or multi-line
            # Find matching }
            brace_depth <- 1L
            end_idx <- body_idx + 1L
            while (end_idx <= nrow(terminals) && brace_depth > 0L) {
                if (terminals$token[end_idx] == "'{'") {
                    brace_depth <- brace_depth + 1L
                } else if (terminals$token[end_idx] == "'}'") {
                    brace_depth <- brace_depth - 1L
                }
                if (brace_depth > 0L) end_idx <- end_idx + 1L
            }
            if (end_idx > nrow(terminals)) next

            if (terminals$line1[end_idx] == body_tok$line1) {
                style <- "braced_single"
            } else {
                style <- "braced_multi"
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

cat("\n=== Overall ===\n")
tab <- table(all_results$style)
pct <- round(prop.table(tab) * 100, 1)
for (s in names(sort(tab, decreasing = TRUE))) {
    cat(sprintf("  %-16s %5d  (%4.1f%%)\n", s, tab[s], pct[s]))
}
cat(sprintf("  %-16s %5d\n", "TOTAL", sum(tab)))

cat("\n=== Bare vs Braced ===\n")
bare <- sum(tab[grep("^bare", names(tab))])
braced <- sum(tab[grep("^braced", names(tab))])
total <- bare + braced
cat(sprintf("  Bare:   %5d  (%4.1f%%)\n", bare, bare / total * 100))
cat(sprintf("  Braced: %5d  (%4.1f%%)\n", braced, braced / total * 100))

cat("\n=== By Keyword ===\n")
for (kw in c("if", "for", "while")) {
    sub <- all_results[all_results$keyword == kw, ]
    if (nrow(sub) == 0) next
    cat(kw, ":\n")
    kt <- table(sub$style)
    kp <- round(prop.table(kt) * 100, 1)
    for (s in names(sort(kt, decreasing = TRUE))) {
        cat(sprintf("  %-16s %5d  (%4.1f%%)\n", s, kt[s], kp[s]))
    }
}
