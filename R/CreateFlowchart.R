#' CreateFlowchart
#'
#' Generates flowchart using TikZ. May be compiled to pdf using knitr or pdflatex.
#' @param flowchart.elements Character vector. Content for nodes. Length must be > 1. Defaults to NULL.
#' @param flowchart.file.path String. The flowchart file path. Defaults to "./flowchart.rtex".
#' @param print.tikz Logical. If TRUE, TikZ code is printed to console. Defaults to TRUE.
#' @param save.tikz  Logical. If TRUE, TikZ code for flowchart is saved to disk. Defaults to TRUE.
#' @param compile.flowchart Logical. If TRUE, the flowchart is compiled using knitr or pdflatex, depending on the file format. Defaults to TRUE
#' @param prettify Logical. If TRUE, shadows are used for flowchart nodes. Requires TikZ package shadows.blur. Defaults to FALSE.
#' @param read.from.results Logical. If TRUE, flowchart.elements is read from results.Rds. Defaults to TRUE.
#' @export
CreateFlowchart <- function(flowchart.elements = NULL, flowchart.file.path = "./flowchart.rtex",
                            print.tikz = TRUE, save.tikz = TRUE, compile.flowchart = TRUE,
                            prettify = FALSE, read.from.results = TRUE)
{
    # Get file format of flowchart.file.path
    flowchart.file.format <- substr(flowchart.file.path,
                                    regexpr("\\.[^\\.]*$", flowchart.file.path)[1],
                                    nchar(flowchart.file.path))
    ## Error handling
    if (IsLength1(flowchart.elements)) stop("Length of flowchart.elements must be > 1.")
    if (!length(flowchart.elements) %% 2 != 0 & !read.from.results) stop("Length of flowchart.elements must be an odd integer.")
    if (compile.flowchart & !(flowchart.file.format %in% c(".rtex", ".tex"))) stop("Parameter flowchart.file.format must be either .tex or .rtex if compile.flowchart is set to TRUE.")
    if (!dir.exists(flowchart.file.path) & (!save.tikz & compile.flowchart)) stop("Parameter save.tikz is set to FALSE, yet compile.flowchart is set to TRUE. Path flowchart.file.path must exist for this combination of settings.")
    if (!is.null(flowchart.elements) & read.from.results) stop("Parameter read.from.results is set to TRUE, yet flowchart.elements is non-NULL. Set read.from.results to FALSE and re-run function.")
    if (read.from.results) {
        ## Read flowchart elements, remove newline and format
        flowchart.elements <- lapply(readRDS("results.Rds")$flowchart.list, function (node.text) {
            node.text <- gsub("-", "\\,", gsub("\\n", "", node.text)) 
            node.text.formatted <- sub("\\,", "", gsub(" \\, ", "\\, ", node.text))
            node.text.w.math <- gsub("([0-9<>]+)", "$\\1$", node.text.formatted)
            return (node.text.w.math)
        })
    }
    flowchart.lib <- ""; flowchart.set <- ""
    if (prettify){
        flowchart.lib <- "   shadows.blur,\n"
        flowchart.set <- "   blur shadow={shadow blur steps=8},\n"
    }
    latex.preamble <- sprintf("
\\documentclass[tikz, border = 5pt]{standalone}
\\usepackage{tikz}
\\usepackage{xstring}
\\usetikzlibrary{
%s   chains,
   arrows.meta,
   decorations.pathmorphing
}", flowchart.lib)

    flowchart.exclusion.node <- sprintf("
\\tikzset{rightNode/.style={
%s   align=center,
   text width=60mm,
   minimum height=15mm,
   draw,
   fill = white
  }
}", flowchart.set)

    flowchart.tikz.command <- paste("
\\newcommand{\\CreateFlowchart}[1]{
% Count number of commas in input array
\\StrCount{#1}{,}[\\numberOfCommas]
% Add one to get number of elements in array:
% https://tex.stackexchange.com/questions/66121/how-can-i-determine-the-size-of-an-array
\\pgfmathtruncatemacro\\arrayLength{\\numberOfCommas + 1}
\\foreach[evaluate=\\j using int(\\i - 1)] \\i in {1,...,\\arrayLength}{
  % Get array subset of values
  \\pgfmathsetmacro\\arrayElement{array({#1}, \\j)}
  % Every other node should be a middle block
  \\ifodd\\i{
    \\node[on chain,
      fill = white,\n   ",
flowchart.set, "      draw]
      (block \\i)
      {\\arrayElement};
    }
  % Every other should be invisible
  \\else
    \\node[on chain] (mid \\i) {};
    % Define exclusion nodes, i.e. nodes to contain information on excluded patients
    \\node[rightNode, right =2cm of mid \\i] (right \\i)
    {\\arrayElement};
    % Draw arrows from middle to exclusion nodes
    \\draw[->, shorten >=3pt, thick] (mid \\i.center) -- (right \\i);
    \\fi
  }
  % Draw arrows between middle nodes
  \\foreach[evaluate=\\j using int(\\i - 2)] \\i in {1, 3,...,\\arrayLength}{
    \\ifnum\\i=1{}
    \\else{
      \\draw[->, shorten >= 3pt, thick] (block \\j) -- (block \\i);
    }
    \\fi
  }
}", sep = "")
    ## Format input flowchart.elements to tikz 
    flowchart.elements <- sapply(seq_along(flowchart.elements), function (i)
    {
        ## Fe, me, and le respectively first, mid, and last element
        PastR <- function (fe, me, le){
            merged.string <- paste(fe, me, le, sep = "")
            return (merged.string)
        }       
        element <- PastR("{\"", flowchart.elements[i], "\"}")
        len <- length(flowchart.elements)
        if (i == 1){
            flowchart.elements[i] <- PastR("{", element, ",")
        } else if (i == len){
            flowchart.elements[i] <- PastR("", element, "}")
        } else {
            flowchart.elements[i] <- PastR("", element, ",")
        }
    })
    tikz.print <- paste0(latex.preamble,
                         flowchart.exclusion.node,
                         flowchart.tikz.command, "\n",
paste("\\def\\flowchartElements", paste(flowchart.elements, collapse = ""), "\n", sep = ""),"
\\begin{document}
\\begin{tikzpicture}
   [start chain=main going below,
     every on chain/.append style={
     align=center,
     text width = 60mm,
     minimum height=15mm},
     >={LaTeX[]}]
   \\CreateFlowchart{\\flowchartElements}
\\end{tikzpicture}
\\end{document}")
    ## Write flowchart to disk
    if (save.tikz) write(tikz.print, file = flowchart.file.path)
    ## Compile with either knitr or pdflatex, depending on file extension
    if (compile.flowchart) {
        if (flowchart.file.format == ".rtex") {
            knitr::knit2pdf(flowchart.file.path)
        } else if (flowchart.file.format == ".tex") {
            system(paste("pdflatex", flowchart.file.path, collapse = ""), ignore.stdout = TRUE)
            message (sprintf("\nPdflatex output surpressed. See %s for compile status.",
                             sub(flowchart.file.format, ".log", flowchart.file.path)))
        }
    }    
    if (print.tikz) cat(tikz.print)

    return (tikz.print)
}
