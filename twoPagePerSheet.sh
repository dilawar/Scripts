#! /bin/bash
pdf2ps $1 tmp_file.ps
psnup -1 -2 tmp_file.ps tmp_file.2.ps
ps2pdf tmp_file.2.ps $2
\rm tmp_file.2.ps tmp_file.ps

