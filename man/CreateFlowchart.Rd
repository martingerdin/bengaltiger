% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateFlowchart.R
\name{CreateFlowchart}
\alias{CreateFlowchart}
\title{Create flowchart}
\usage{
CreateFlowchart(flowchart.elements = NULL, flowchart.font = NULL,
  flowchart.file.path = "./flowchart.tex", print.tikz = TRUE,
  save.tikz = TRUE, compile.flowchart = FALSE,
  train.test.split = FALSE, prettify = FALSE,
  read.from.results = TRUE, ...)
}
\arguments{
\item{flowchart.elements}{Character vector. Content for nodes. Length must be > 1. Remember to escape e.g. underscores in the text. Defaults to NULL.}

\item{flowchart.font}{Character vector of length 1. The font for flowchart nodes, e.g. LiberationSerif. Uses the mathspec package. Flowchart is compiled with XeLaTex or knitr depending on file extension in flowchart.file.path. Defaults to NULL.}

\item{flowchart.file.path}{Character vector of length 1. The flowchart file path. Defaults to "./flowchart.tex".}

\item{print.tikz}{Logical. If TRUE TikZ code is printed to console. Defaults to TRUE.}

\item{save.tikz}{Logical. If TRUE TikZ code for flowchart is saved to disk. Defaults to TRUE.}

\item{compile.flowchart}{Logical. If TRUE the flowchart is compiled using knitr or pdflatex, depending on the file format. Defaults to TRUE}

\item{train.test.split}{Logical. If TRUE last two elements of flowchart list are interpreted as "train" and "test" texts, i.e. text explaining how many patients are grouped in train and test samples. Second to last element is displayed to the left and last element is displayed to the right. Defaults to FALSE.}

\item{prettify}{Logical. If TRUE shadows are used for flowchart nodes. Requires TikZ package shadows.blur. Defaults to FALSE.}

\item{read.from.results}{Logical. If TRUE flowchart.elements is read from results.Rds. Defaults to TRUE.}

\item{...}{Additional arguments for system function.}
}
\description{
Generates flowchart using TikZ. Requires the xstring and tikz packages. Compiled with knitr (which in turn uses xelatex) or xelatex depending on file extension in flowchart.file.path.
}
