#!/usr/bin/env bash

INFILE=$1
OUTFILE=${INFILE%.tikz}.tex
TEX=$(cat $1)

cat > $OUTFILE <<-ENDOFFILE
\documentclass[preview,multi=false]{standalone}
\usepackage{pgfplots}
\usepgfplotslibrary{groupplots}
\usepackage{tikz}
\usepackage[sfdefault]{FiraSans}
\usepackage{mathpazo}
\pgfplotsset{compat=1.15}

\tikzset{font=\small}
\pgfplotsset{
    , ticklabel style={font=\footnotesize}
    , label style={font=\footnotesize}
    , title style={align=left}
    , group/.cd, vertical sep=20mm, horizontal sep=20mm
}

\begin{document}

$TEX

\end{document}
ENDOFFILE
pdflatex -interaction=nonstopmode $OUTFILE
