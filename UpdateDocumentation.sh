#!/bin/bash
## Code to update bengaltiger documentation

## Update man files
R -e "library(devtools); library(roxygen2); document()"

## Remove existing manual
rm manual.pdf

## Genereate new manual
R CMD Rd2pdf . --title='Package bengaltiger' --output=./manual.pdf
