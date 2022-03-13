\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{my-article}[2022/02/14 Personal article class]

\PassOptionsToPackage{dvipsnames,svgnames,table,hyperref}{xcolor}

\newif\ifshowsecnums
\DeclareOption{showsecnums}{\showsecnumstrue}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{scrartcl}}
\ProcessOptions\relax
\LoadClass{scrartcl}

% loads hyperref, use hypersetup. only works with lualatex
\RequirePackage[a-3u]{pdfx}

\RequirePackage{scrlayer-scrpage}
\RequirePackage{graphicx}
\RequirePackage{rotating}
\RequirePackage{longtable}
\RequirePackage{booktabs}
\RequirePackage{wrapfig}
\RequirePackage[normalem]{ulem}
\RequirePackage{capt-of}
\RequirePackage{mathtools, amsthm} % mathtools loads amsmath
\RequirePackage[backend=biber,style=ieee]{biblatex}

\RequirePackage{inputnormalization} % not yet in texlive, run l3build install on source to put in texmf
\RequirePackage{fontsetup} % loads fontspec and unicode-math
% \RequirePackage{emoji} % too experimental, no direct input or unicode output
% \setmainfont{Source Serif 4}
% \setmainfont{Source Sans 3}
% \setmathfont{STIX Two Math}
% \setmonofont{Iosevka}

\RequirePackage{microtype}

% hide section numbers by default, they are unnecessary for most documents. this
% allows the use of unstarred section commands, preserving the table of contents
% and PDF outline
\ifshowsecnums
\else
  \renewcommand{\@seccntformat}[1]{}
\fi
