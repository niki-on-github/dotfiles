#!/bin/sh

# Description: Cleans the build files of a LaTeX/XeLaTeX build.
# HowTo: Add 'autocmd VimLeave *.tex !/path/to/tex_clean_up.sh %' to your vim config

clean() {
	find "$dir" -maxdepth 1 -type f -regextype gnu-awk -regex "^$base\\.(4tc|xref|tmp|pyc|pyo|fls|vrb|fdb_latexmk|bak|swp|aux|log|synctex\\(busy\\)|lof|lot|maf|idx|mtc|mtc0|nav|out|snm|toc|bcf|run\\.xml|synctex\\.gz|blg|bbl)" -delete
}

cleanTex() {
    file=$(readlink -f "$1")
	dir=$(dirname "$file")
    [ "$dir" = "." ] && parentDir=".." || parentDir=$(dirname "$dir")
	base="${file%.*}"
    clean

    if [ -f "${dir}/main.tex" ]; then
        file="${dir}/main.tex"
        base="${dir}/main"
        echo "clean main.tex build files"
        clean
    elif [ -f "${parentDir}/main.tex" ]; then
        file="${parentDir}/main.tex"
        dir="$parentDir"
        base="${parentDir}/main"
        echo "clean ../main.tex build files"
        clean
    fi
}

case "$1" in
	*.tex)  cleanTex ;;
	*)      printf "Require *.tex file as argument.\\n" ;;
esac
