#' Create flowchart
#'
#' Generates flowchart using TikZ. Requires the xstring and tikz packages. Compiled with knitr or pdflatex depending on file extension in flowchart.file.path. However, if non-default font is used, flowchart is compiled using xelatex and the mathspec package is required.
#' @param flowchart.elements Character vector. Content for nodes. Length must be > 1. Defaults to NULL.
#' @param flowchart.font Character vector of length 1. The font for flowchart nodes, e.g. LiberationSerif. Uses the mathspec package. Flowchart is compiled with XeLaTex or knitr depending on file extension in flowchart.file.path. Defaults to NULL.
#' @param flowchart.file.path Character vector of length 1. The flowchart file path. Defaults to "./flowchart.tex".
#' @param print.tikz Logical. If TRUE TikZ code is printed to console. Defaults to TRUE.
#' @param save.tikz  Logical. If TRUE TikZ code for flowchart is saved to disk. Defaults to TRUE.
#' @param compile.flowchart Logical. If TRUE the flowchart is compiled using knitr or pdflatex, depending on the file format. Defaults to TRUE
#' @param train.test.split Logical. If TRUE last two elements of flowchart list are interpreted as "train" and "test" texts, i.e. text explaining how many patients are grouped in train and test samples. Second to last element is displayed to the left and last element is displayed to the right. Defaults to FALSE.
#' @param prettify Logical. If TRUE shadows are used for flowchart nodes. Requires TikZ package shadows.blur. Defaults to FALSE.
#' @param read.from.results Logical. If TRUE flowchart.elements is read from results.Rds. Defaults to TRUE.
#' @param ... Additional arguments for system function.
#' @export
CreateFlowchart <- function(flowchart.elements = NULL, flowchart.font = NULL,
                            flowchart.file.path = "./flowchart.tex", print.tikz = TRUE,
                            save.tikz = TRUE, compile.flowchart = TRUE, train.test.split = FALSE,
                            prettify = FALSE, read.from.results = TRUE, ...)
{
    # Get file format of flowchart.file.path
    flowchart.file.format <- substr(flowchart.file.path,
                                    regexpr("\\.[^\\.]*$", flowchart.file.path)[1],
                                    nchar(flowchart.file.path))
    ## Error handling
    if (length(flowchart.elements) < 1)
        stop("Length of flowchart.elements must be > 1.")
    if (!length(flowchart.elements) %% 2 != 0 & !read.from.results)
        stop("Length of flowchart.elements must be an odd integer.")
    if (compile.flowchart & !(flowchart.file.format %in% c(".rtex", ".tex")))
        stop("Parameter flowchart.file.format must be either .tex or .rtex if compile.flowchart is set to TRUE.")
    if (!dir.exists(flowchart.file.path) & (!save.tikz & compile.flowchart))
        stop("Parameter save.tikz is set to FALSE and compile.flowchart is set to TRUE. Path flowchart.file.path must exist for this combination of settings.")
    if (!is.null(flowchart.elements) & read.from.results)
        warning("Parameter read.from.results is set to TRUE and flowchart.elements is non-NULL, thus flowchart.elements will be \nignored. If you want custom flowchart.elements in flowchart, set read.from.results to FALSE and re-run function.")
    if (read.from.results) 
        ## Read flowchart elements, remove newline and format
        flowchart.elements <- lapply(readRDS("results.Rds")$flowchart.list, function (node.text) {
            node.text <- gsub("-", "\\,", gsub("\\n", "", node.text))
            node.text.formatted <- sub("\\,", "", gsub(" \\, ", "\\, ", node.text))
            node.text.as.math <- gsub("([0-9<>]+)", "$\\1$", node.text.formatted)
            return (node.text.as.math)
        })
    flowchart.node <- "
    \\node[on chain,
         nodeStyle]
         (block \\i)
         {\\arrayElement};"
    flowchart.lib <- ""; flowchart.set <- ""; train.test.set <- c("+", "", flowchart.node)
    if (prettify){
        flowchart.lib <- "   shadows.blur,\n"
        flowchart.set <- "   blur shadow={shadow blur steps=8},\n"
    }
    if (train.test.split) {
        train.test.set <- c(
"-",
"
  \\pgfmathsetmacro\\test{array({#1}, \\j + 1)}
  \\pgfmathsetmacro\\train{array({#1}, \\j + 2)}",
"
    \\ifnum\\i=\\arrayLength {
      \\node[on chain,
      nodeStyle]
      (block \\i)
      {\\arrayElement};
      %% Draw the cross-validation nodes
      \\node[below left = 3.75cm and 1.5cm of block \\i.south, nodeStyle] (test) {\\test};
      \\node[below right = 3.75cm and 1.5cm of block \\i.south, nodeStyle] (train) {\\train};
      %% Invisible helper node
      \\node[below = 1.75cm of block \\i] (helper) {};
      \\draw[shorten >= 3pt, thick] (block \\i) |- (helper.center);
      \\draw[->, shorten >= 3pt, thick] (helper.center) -| (test);
      \\draw[->, shorten >= 3pt, thick] (helper.center) -| (train);
    } \\else {
      \\node[on chain,
      nodeStyle]
      (block \\i)
      {\\arrayElement};
    }
    \\fi")
    }
    latex.preamble <- sprintf(
"\\documentclass[tikz, border = 5pt]{standalone}
\\usepackage{tikz}
\\usepackage{xstring}
\\usetikzlibrary{
%s   chains,
   arrows.meta,
   decorations.pathmorphing
}", flowchart.lib)
    if (!is.null(flowchart.font)){
        font.set <- gsub("%s", flowchart.font,
"\\usepackage{xstring}
\\usepackage{mathspec}
\\setmainfont{%s}
\\setmathfont(Digits){%s}",
fixed = TRUE)
        latex.preamble <- sub("\\usepackage{xstring}", font.set, latex.preamble, fixed = TRUE)
    } 
    flowchart.node.style <- sprintf("
\\tikzset{nodeStyle/.style={
%s   align=center,
   text width=60mm,
   minimum height=15mm,
   draw,
   fill = white,
   inner sep = 2mm}
}", flowchart.set)
    flowchart.tikz.command <- do.call(sprintf, c(list("
\\newcommand{\\CreateFlowchart}[1]{
%% Count number of commas in input array
\\StrCount{#1}{,}[\\numberOfCommas]
%% Add one to get number of elements in array; If train-test-split, subtract one
%% https://tex.stackexchange.com/questions/66121/how-can-i-determine-the-size-of-an-array
\\pgfmathtruncatemacro\\arrayLength{\\numberOfCommas %s 1}
\\foreach[evaluate=\\j using int(\\i - 1)] \\i in {1,...,\\arrayLength}{
  %% Get array elements
  \\pgfmathsetmacro\\arrayElement{array({#1}, \\j)}%s
  %% Every other node should be a middle block
  \\ifodd\\i{%s
  }
  %% Every other should be invisible
  \\else
    \\node[on chain] (mid \\i) {};
    %% Define exclusion nodes, i.e. nodes to contain information on excluded patients
    \\node[nodeStyle, right =1.5cm of mid \\i] (right \\i)
    {\\arrayElement};
    %% Draw arrows from middle to exclusion nodes
    \\draw[->, shorten >=3pt, thick] (mid \\i.center) -- (right \\i);
    \\fi
  }
  \\ifnum\\arrayLength>1{
    %% Draw arrows between middle nodes
    \\foreach[evaluate=\\j using int(\\i - 2)] \\i in {1, 3,...,\\arrayLength}{
      \\ifnum\\i=1{}
      \\else{
        \\draw[->, shorten >= 3pt, thick] (block \\j) -- (block \\i);
      }
      \\fi
    } 
  }\\else {} \\fi
}"), train.test.set))
    ## Format input flowchart.elements to tikz 
    flowchart.elements <- sapply(seq_along(flowchart.elements), function (i) {
        ## Fe, me, and le respectively first, mid, and last element
        PastR <- function(fe, me, le) paste(fe, me, le, sep = "")
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
                         flowchart.node.style,
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
    if (save.tikz)
        write(tikz.print, file = flowchart.file.path)
    ## Compile with either knitr, pdflatex, or xelatex depending on file extension and font param
    if (print.tikz)
        cat(tikz.print)
    if (compile.flowchart) {
        if (flowchart.file.format == ".rtex") {
            knitr::knit2pdf(flowchart.file.path)
        } else {
            compiler <- "pdflatex"
            if (!is.null(flowchart.font)) compiler <- "xelatex"
            system(paste(compiler, flowchart.file.path, collapse = ""), ...)
        }
    }    
    return (tikz.print)
}

