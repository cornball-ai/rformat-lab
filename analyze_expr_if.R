#!/usr/bin/env r
#
# Analyze expression if-else usage in R Core source code
#
# Expression if-else is value-producing: x <- if (a) b else c
# This is what rformat's expand_if option controls.
#
# Classifies expression if-else as:
#   inline      - entire if-else on one line:  x <- if (a) b else c
#   multi_line  - if-else spans multiple lines (already expanded)
#
# Also counts statement if (not expression) for context.
#
# Usage:
#   r analyze_expr_if.R

# R Core source directory (extracted base R packages)
base_r_src <- path.expand("~/.cache/rformat_cran_src/base_r_src")
cran_cache <- path.expand("~/.cache/rformat_cran_src")

core_pkgs <- list.dirs(base_r_src, full.names = FALSE, recursive = FALSE)
rec_pkgs <- c("boot", "class", "cluster", "codetools", "foreign",
              "KernSmooth", "lattice", "MASS", "Matrix", "mgcv",
              "nlme", "nnet", "rpart", "spatial", "survival")

# Find R source files for a package
find_r_files <- function (pkg) {
    r_dir <- file.path(base_r_src, pkg, "R")
    if (dir.exists(r_dir))
        return(list.files(r_dir, pattern = "\\.[Rrq]$", full.names = TRUE,
                          recursive = TRUE))
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

# Classify all if/if-else in a file as statement vs expression,
# and expression as inline vs multi-line
classify_ifs <- function (path) {
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

    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

    results <- data.frame(
        type = character(0),       # "statement" or "expression"
        style = character(0),      # for expression: "inline", "expanded_short", "expanded_long"
        has_else = logical(0),     # does it have an else branch?
        stringsAsFactors = FALSE
    )

    if_indices <- which(terminals$token == "IF")

    for (ii in if_indices) {
        # Track paren depth before this IF to determine if it's an expression
        paren_depth <- 0L
        for (j in seq_len(ii - 1)) {
            tok <- terminals$token[j]
            if (tok == "'('") paren_depth <- paren_depth + 1L
            else if (tok == "')'") paren_depth <- paren_depth - 1L
        }

        # Also check if it's on the RHS of assignment at top level
        # Expression if-else: inside parens (paren_depth >= 1) OR
        # on RHS of <- / = assignment
        is_expr <- paren_depth >= 1L

        if (!is_expr) {
            # Check if preceded by LEFT_ASSIGN or EQ_ASSIGN
            # (the if is the RHS value)
            prev_idx <- ii - 1
            while (prev_idx >= 1 && terminals$token[prev_idx] == "COMMENT") {
                prev_idx <- prev_idx - 1
            }
            if (prev_idx >= 1 &&
                terminals$token[prev_idx] %in% c("LEFT_ASSIGN",
                                                  "EQ_ASSIGN",
                                                  "RIGHT_ASSIGN")) {
                is_expr <- TRUE
            }
        }

        # Find matching ELSE (if any)
        # First find closing ) of condition
        open_idx <- ii + 1
        if (open_idx > nrow(terminals)) next
        if (terminals$token[open_idx] != "'('") next

        pd_depth <- 1L
        close_idx <- open_idx + 1
        while (close_idx <= nrow(terminals) && pd_depth > 0) {
            if (terminals$token[close_idx] == "'('") pd_depth <- pd_depth + 1L
            else if (terminals$token[close_idx] == "')'") pd_depth <- pd_depth - 1L
            if (pd_depth > 0) close_idx <- close_idx + 1
        }
        if (close_idx > nrow(terminals)) next

        # Find end of true branch and look for ELSE
        else_idx <- NULL
        search_idx <- close_idx + 1
        nest_depth <- 0L
        brace_depth <- 0L

        while (search_idx <= nrow(terminals)) {
            stok <- terminals[search_idx, ]
            if (stok$token == "'{'") brace_depth <- brace_depth + 1L
            else if (stok$token == "'}'") {
                brace_depth <- brace_depth - 1L
                if (brace_depth < 0L) break
            }
            if (stok$token == "IF") nest_depth <- nest_depth + 1L
            else if (stok$token == "ELSE") {
                if (nest_depth == 0L && brace_depth == 0L) {
                    else_idx <- search_idx
                    break
                } else if (nest_depth > 0L) {
                    nest_depth <- nest_depth - 1L
                }
            }
            search_idx <- search_idx + 1
        }

        has_else <- !is.null(else_idx)

        if (!is_expr) {
            results <- rbind(results, data.frame(
                type = "statement", style = NA_character_,
                has_else = has_else, stringsAsFactors = FALSE
            ))
            next
        }

        # Expression if-else: determine inline vs multi-line
        if_line <- terminals$line1[ii]

        if (has_else) {
            # Find end of false expression
            false_start <- else_idx + 1
            if (false_start > nrow(terminals)) {
                end_line <- terminals$line2[else_idx]
            } else {
                # Walk to end of false expression
                false_end <- false_start
                fp_depth <- 0L
                fb_depth <- 0L
                fi_depth <- 0L
                false_start_line <- terminals$line1[false_start]

                while (false_end <= nrow(terminals)) {
                    ftok <- terminals[false_end, ]
                    if (ftok$token == "IF") fi_depth <- fi_depth + 1L
                    if (ftok$token == "ELSE") fi_depth <- max(0L, fi_depth - 1L)
                    if (ftok$token %in% c("'('", "'['")) fp_depth <- fp_depth + 1L
                    if (ftok$token == "LBB") fp_depth <- fp_depth + 2L
                    if (ftok$token == "','") {
                        if (fp_depth == 0L && fb_depth == 0L && fi_depth == 0L) {
                            false_end <- false_end - 1
                            break
                        }
                    }
                    if (ftok$token %in% c("')'", "']'")) {
                        if (fp_depth == 0L && fb_depth == 0L && fi_depth == 0L) {
                            false_end <- false_end - 1
                            break
                        }
                        fp_depth <- fp_depth - 1L
                    }
                    if (ftok$token == "']]'") fp_depth <- fp_depth - 2L
                    if (ftok$token == "'{'") fb_depth <- fb_depth + 1L
                    if (ftok$token == "'}'") fb_depth <- fb_depth - 1L
                    if (ftok$line1 > false_start_line && fp_depth <= 0L &&
                        fb_depth <= 0L && fi_depth == 0L) {
                        false_end <- false_end - 1
                        break
                    }
                    false_end <- false_end + 1
                }
                if (false_end > nrow(terminals)) false_end <- nrow(terminals)
                if (false_end < false_start) false_end <- false_start
                end_line <- terminals$line2[false_end]
            }
        } else {
            # No else — find end of true branch
            body_start <- close_idx + 1
            if (body_start > nrow(terminals)) {
                end_line <- terminals$line2[close_idx]
            } else if (terminals$token[body_start] == "'{'") {
                # Braced body — find matching }
                bd <- 1L
                ei <- body_start + 1
                while (ei <= nrow(terminals) && bd > 0L) {
                    if (terminals$token[ei] == "'{'") bd <- bd + 1L
                    else if (terminals$token[ei] == "'}'") bd <- bd - 1L
                    if (bd > 0L) ei <- ei + 1
                }
                end_line <- if (ei <= nrow(terminals)) terminals$line2[ei] else terminals$line2[body_start]
            } else {
                end_line <- terminals$line2[body_start]
            }
        }

        if (end_line == if_line) {
            style <- "inline"
        } else if (has_else) {
            # Multi-line expression if-else: could it fit inline?
            # Extract condition, true expr, false expr tokens
            cond_toks <- terminals[(open_idx + 1):(close_idx - 1), ]
            true_toks <- terminals[(close_idx + 1):(else_idx - 1), ]
            # Filter out braces and comments from true/false bodies
            true_toks <- true_toks[!true_toks$token %in%
                c("'{'", "'}'", "COMMENT"), ]
            false_toks_range <- if (exists("false_end") &&
                false_end >= false_start) {
                terminals[false_start:false_end, ]
            } else {
                terminals[false_start:false_start, ]
            }
            false_toks_range <- false_toks_range[!false_toks_range$token %in%
                c("'{'", "'}'", "COMMENT"), ]

            cond_text <- paste(cond_toks$text, collapse = " ")
            true_text <- if (nrow(true_toks) > 0) {
                paste(true_toks$text, collapse = " ")
            } else {
                "NULL"
            }
            false_text <- if (nrow(false_toks_range) > 0) {
                paste(false_toks_range$text, collapse = " ")
            } else {
                "NULL"
            }

            # Reconstruct inline form
            inline_form <- paste0("if (", cond_text, ") ", true_text,
                                  " else ", false_text)

            # Get the prefix (everything before the IF on its line)
            if_line_content <- lines[if_line]
            if_col <- terminals$col1[ii]
            prefix_width <- if_col - 1L

            total_width <- prefix_width + nchar(inline_form)

            # Check for suffix (closing parens etc on the last line)
            # Conservative: just use 80 char limit
            style <- if (total_width <= 80L) "expanded_short" else "expanded_long"
        } else {
            # Expression if without else, multi-line
            style <- "expanded_long"
        }

        results <- rbind(results, data.frame(
            type = "expression", style = style,
            has_else = has_else, stringsAsFactors = FALSE
        ))
    }

    results
}

# Run analysis
cat("Analyzing expression if-else in R Core packages...\n")
all_results <- data.frame(
    type = character(0), style = character(0),
    has_else = logical(0), package = character(0),
    stringsAsFactors = FALSE
)

for (pkg in unique(c(core_pkgs, rec_pkgs))) {
    files <- find_r_files(pkg)
    if (length(files) == 0) {
        cat("  ", pkg, ": not found\n")
        next
    }
    pkg_results <- do.call(rbind, lapply(files, classify_ifs))
    if (!is.null(pkg_results) && nrow(pkg_results) > 0) {
        pkg_results$package <- pkg
        all_results <- rbind(all_results, pkg_results)
    }
    n_expr <- if (!is.null(pkg_results)) sum(pkg_results$type == "expression") else 0
    n_stmt <- if (!is.null(pkg_results)) sum(pkg_results$type == "statement") else 0
    cat("  ", pkg, ":", length(files), "files,",
        n_stmt, "statement if,", n_expr, "expression if\n")
}

stmt <- all_results[all_results$type == "statement", ]
expr <- all_results[all_results$type == "expression", ]

cat("\n=== Overview ===\n")
cat(sprintf("  Statement if:    %5d  (control flow)\n", nrow(stmt)))
cat(sprintf("  Expression if:   %5d  (value-producing)\n", nrow(expr)))
cat(sprintf("  TOTAL:           %5d\n", nrow(all_results)))

cat("\n=== Statement if: has else? ===\n")
cat(sprintf("  Without else:    %5d  (%4.1f%%)\n",
            sum(!stmt$has_else),
            100 * sum(!stmt$has_else) / nrow(stmt)))
cat(sprintf("  With else:       %5d  (%4.1f%%)\n",
            sum(stmt$has_else),
            100 * sum(stmt$has_else) / nrow(stmt)))

cat("\n=== Expression if: has else? ===\n")
if (nrow(expr) > 0) {
    cat(sprintf("  With else:       %5d  (%4.1f%%)\n",
                sum(expr$has_else),
                100 * sum(expr$has_else) / nrow(expr)))
    cat(sprintf("  Without else:    %5d  (%4.1f%%)\n",
                sum(!expr$has_else),
                100 * sum(!expr$has_else) / nrow(expr)))
}

cat("\n=== Expression if-else: All Styles ===\n")
expr_with_else <- expr[expr$has_else, ]
if (nrow(expr_with_else) > 0) {
    stab <- table(expr_with_else$style)
    spct <- round(prop.table(stab) * 100, 1)
    for (s in names(sort(stab, decreasing = TRUE))) {
        cat(sprintf("  %-20s %5d  (%4.1f%%)\n", s, stab[s], spct[s]))
    }
    cat(sprintf("  %-20s %5d\n", "TOTAL", sum(stab)))
}

# The interesting comparison: among cases where inline IS an option
cat("\n=== Expression if-else: Where Inline is an Option (the interesting comparison) ===\n")
could_inline <- expr_with_else[expr_with_else$style %in%
    c("inline", "expanded_short"), ]
if (nrow(could_inline) > 0) {
    stab <- table(could_inline$style)
    spct <- round(prop.table(stab) * 100, 1)
    for (s in names(sort(stab, decreasing = TRUE))) {
        lbl <- if (s == "inline") "inline (kept)" else "expanded (by choice)"
        cat(sprintf("  %-25s %5d  (%4.1f%%)\n", lbl, stab[s], spct[s]))
    }
    cat(sprintf("  %-25s %5d\n", "TOTAL", sum(stab)))
}

cat("\n=== Expression if (no else) Style ===\n")
expr_no_else <- expr[!expr$has_else, ]
if (nrow(expr_no_else) > 0) {
    stab <- table(expr_no_else$style)
    spct <- round(prop.table(stab) * 100, 1)
    for (s in names(sort(stab, decreasing = TRUE))) {
        cat(sprintf("  %-20s %5d  (%4.1f%%)\n", s, stab[s], spct[s]))
    }
    cat(sprintf("  %-20s %5d\n", "TOTAL", sum(stab)))
}

cat("\n=== Expression if-else by Package (top 10) ===\n")
if (nrow(expr_with_else) > 0) {
    pkg_tab <- sort(table(expr_with_else$package), decreasing = TRUE)
    n_show <- min(10, length(pkg_tab))
    for (i in seq_len(n_show)) {
        pkg <- names(pkg_tab)[i]
        n <- pkg_tab[i]
        sub <- expr_with_else[expr_with_else$package == pkg, ]
        n_inline <- sum(sub$style == "inline")
        n_exp_short <- sum(sub$style == "expanded_short")
        n_exp_long <- sum(sub$style == "expanded_long")
        cat(sprintf("  %-15s %4d  (inline: %d, expanded-short: %d, expanded-long: %d)\n",
                    pkg, n, n_inline, n_exp_short, n_exp_long))
    }
}
