#!/usr/bin/env r
#
# Check for goto-fail-style bugs in R Core source code
#
# Looks for bare next-line control flow bodies where the line AFTER the body
# has the same indentation as the body line, suggesting the author might have
# intended both lines to be inside the if/for/while.
#
# Pattern we're looking for:
#   if (condition)
#       statement1      <-- body (indented)
#       statement2      <-- same indent, but NOT inside the if!
#
# Usage:
#   r analyze_goto_fail.R

base_r_src <- path.expand("~/.cache/rformat_cran_src/base_r_src")
cran_cache <- path.expand("~/.cache/rformat_cran_src")

core_pkgs <- list.dirs(base_r_src, full.names = FALSE, recursive = FALSE)
rec_pkgs <- c("boot", "class", "cluster", "codetools", "foreign",
              "KernSmooth", "lattice", "MASS", "Matrix", "mgcv",
              "nlme", "nnet", "rpart", "spatial", "survival")

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

# Get leading whitespace of a line
leading_ws <- function (line) {
    m <- regexpr("^[ \t]*", line)
    substring(line, 1, attr(m, "match.length"))
}

# Expand tabs to spaces (8-column tab stops) for comparison
expand_tabs <- function (s) {
    result <- ""
    for (ch in strsplit(s, "")[[1]]) {
        if (ch == "\t") {
            spaces <- 8L - (nchar(result) %% 8L)
            result <- paste0(result, strrep(" ", spaces))
        } else {
            result <- paste0(result, ch)
        }
    }
    result
}

check_file <- function (path) {
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

    suspects <- list()

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i, ]
        if (!tok$token %in% c("IF", "FOR", "WHILE")) next

        # Find closing ) of condition
        open_idx <- i + 1L
        if (open_idx > nrow(terminals)) next
        if (terminals$token[open_idx] != "'('") next

        paren_depth <- 1L
        close_idx <- open_idx + 1L
        while (close_idx <= nrow(terminals) && paren_depth > 0L) {
            if (terminals$token[close_idx] == "'('") paren_depth <- paren_depth + 1L
            else if (terminals$token[close_idx] == "')'") paren_depth <- paren_depth - 1L
            if (paren_depth > 0L) close_idx <- close_idx + 1L
        }
        if (close_idx > nrow(terminals)) next

        # Next non-comment token is the body
        body_idx <- close_idx + 1L
        while (body_idx <= nrow(terminals) &&
               terminals$token[body_idx] == "COMMENT") {
            body_idx <- body_idx + 1L
        }
        if (body_idx > nrow(terminals)) next

        body_tok <- terminals[body_idx, ]
        close_line <- terminals$line1[close_idx]

        # Only interested in bare next-line bodies
        if (body_tok$token == "'{'") next
        if (body_tok$line1 == close_line) next

        body_line_num <- body_tok$line1
        if (body_line_num >= length(lines)) next

        # Check the line AFTER the body line
        next_line_num <- body_line_num + 1L
        # Skip blank lines
        while (next_line_num <= length(lines) &&
               grepl("^\\s*$", lines[next_line_num])) {
            next_line_num <- next_line_num + 1L
        }
        if (next_line_num > length(lines)) next

        body_line <- lines[body_line_num]
        next_line <- lines[next_line_num]

        # Skip if next line is a comment
        if (grepl("^\\s*#", next_line)) next

        # Skip if next line is else (that's expected)
        if (grepl("^\\s*else\\b", next_line)) next
        # Skip if next line is } (closing a containing block)
        if (grepl("^\\s*\\}", next_line)) next

        # Compare indentation
        body_indent <- expand_tabs(leading_ws(body_line))
        next_indent <- expand_tabs(leading_ws(next_line))

        # Suspicious: next line has same indent as body
        if (nchar(next_indent) == nchar(body_indent) &&
            nchar(body_indent) > 0) {
            # Also check that the keyword line has LESS indent
            keyword_line <- lines[tok$line1]
            kw_indent <- expand_tabs(leading_ws(keyword_line))
            if (nchar(kw_indent) < nchar(body_indent)) {
                suspects[[length(suspects) + 1]] <- list(
                    file = path,
                    line = tok$line1,
                    keyword = tok$text,
                    context = lines[tok$line1:min(next_line_num, length(lines))]
                )
            }
        }
    }

    suspects
}

cat("Checking for goto-fail patterns in R Core packages...\n\n")
all_suspects <- list()

for (pkg in unique(c(core_pkgs, rec_pkgs))) {
    files <- find_r_files(pkg)
    if (length(files) == 0) next
    for (f in files) {
        suspects <- check_file(f)
        if (length(suspects) > 0) {
            for (s in suspects) {
                s$package <- pkg
                all_suspects[[length(all_suspects) + 1]] <- s
            }
        }
    }
}

cat("Found", length(all_suspects), "suspicious bare next-line patterns\n\n")

if (length(all_suspects) > 0) {
    for (s in all_suspects) {
        cat("--- ", s$package, " :: ", basename(s$file), ":",
            s$line, " (", s$keyword, ") ---\n", sep = "")
        for (l in s$context) cat("  ", l, "\n")
        cat("\n")
    }
}
