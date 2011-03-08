`page.Describe` <-
function (R, ...)
{ # @author Peter Carl
  require(Hmisc)
  desc=deparse(substitute(R))
  R = checkData(R, method="data.frame")
  options(xdvicmd='okular')
  d <- describe(R, desc=desc, ...)
  file = latex(d[sort(names(R))], file='R.tex')
  heading <- "\\documentclass[12pt,letterpaper,english]{article}\n\\usepackage[OT1]{fontenc}\n\\usepackage{relsize,setspace}\n\\begin{document}\n\\input{R.tex} \n\\end{document}"
  cat(heading, file = "Describe.tex")
  dvi(file, nomargins=FALSE, width=8.5, height=11)
}