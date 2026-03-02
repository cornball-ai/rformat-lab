#!/bin/bash
# Extract R/ directories from all cached CRAN tarballs to a flat structure.
# Also symlinks base R packages from base_r_src/.
#
# Usage: bash extract_packages.sh
#
# Result: ~/.cache/rformat_cran_src/extracted/<pkg>/R/*.R
#         One directory per package, ready for stress testing.

cache="${HOME}/.cache/rformat_cran_src"
dest="${cache}/extracted"
mkdir -p "$dest"

# Extract each tarball - just the R/ directory
for tb in "$cache"/*.tar.gz; do
    [ -f "$tb" ] || continue
    pkg=$(tar tzf "$tb" 2>/dev/null | head -1 | cut -d/ -f1)
    [ -z "$pkg" ] && continue
    [ -d "$dest/$pkg/R" ] && continue
    tar xf "$tb" -C "$dest" "$pkg/R/" 2>/dev/null
done

# Symlink base R packages
for d in "$cache"/base_r_src/*/; do
    [ -d "$d" ] || continue
    pkg=$(basename "$d")
    [ -e "$dest/$pkg" ] && continue
    ln -s "$d" "$dest/$pkg"
done

echo "Extracted $(ls "$dest" | wc -l) packages to $dest"
du -shL "$dest"
