#!/usr/bin/env bash

INFILE=$1
OUTFILE="$1.standalone.tex"
TEX=$(cat $1)

cat > $OUTFILE <<-ENDOFFILE
\documentclass[preview,multi=false]{standalone}
\usepackage{pgfplots}
\usepgfplotslibrary{groupplots}
\usepackage{tikz}
\usepackage[sfdefault]{FiraSans}
\usepackage[small]{eulervm}
\pgfplotsset{compat=1.15}
\usepackage{xcolor}
\begin{document}
$TEX
\end{document}
ENDOFFILE
pdflatex -interaction=nonstopmode $OUTFILE
