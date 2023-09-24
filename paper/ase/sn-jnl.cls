%%

% \def\bibcommenthead{\if@bibcomment\begingroup\parindent=0pt\parskip=0pt%
% \removelastskip\vskip13pt\nointerlineskip%
%
% \vbox{\bibfont If you are submitting to one of the Nature Research journals, using the eJP
%  submission system, please include the references within the manuscript file itself. You may
%  do this by copying the reference list from your .bbl file, and pasting it into the bibliography
%  environment of the main manuscript .tex file.}\par%
% \removelastskip\nobreak\vskip13pt\nobreak%
% \endgroup\fi}%

\def\bibcommenthead{}%

\if@Spr@basic@refstyle%
  \usepackage[authoryear]{natbib}%
  \gdef\NumBib{NO}%
  \bibliographystyle{bst/sn-basic}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@Mathphys@refstyle%
  \usepackage[numbers,sort&compress]{natbib}%
  \gdef\NumBib{YES}%
  \bibliographystyle{bst/sn-mathphys}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@APS@refstyle%
  \usepackage[numbers,sort&compress]{natbib}%
  \gdef\NumBib{YES}%
  \bibliographystyle{bst/sn-aps}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@Vancouver@refstyle%
  \usepackage[numbers,sort&compress]{natbib}%
  \gdef\NumBib{YES}%
  \bibliographystyle{bst/sn-vancouver}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@APA@refstyle%
  \usepackage[natbibapa]{apacite}%
  \gdef\NumBib{NO}%
  \bibliographystyle{bst/sn-apacite}%
  \def\refdoi#1{\urlstyle{rm}\url{#1}}%
  \renewcommand{\doiprefix}{}%
  \AtBeginDocument{%
    \renewcommand{\BPBI}{.}% Period between initials - command from apacite.sty
  }%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@Chicago@refstyle%
  \usepackage[authoryear]{natbib}%
  \gdef\NumBib{NO}%
  \bibliographystyle{bst/sn-chicago}%
  \hypersetup{urlcolor=black,colorlinks=false,pdfborder={0 0 0}}\urlstyle{same}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@Standard@Nature@refstyle%
  \usepackage[numbers,sort&compress]{natbib}%
  \gdef\NumBib{YES}%
  \bibliographystyle{bst/sn-standardNature}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%
\if@Default@refstyle%
  \usepackage[numbers,sort&compress]{natbib}%
  \gdef\NumBib{YES}%
  \setlength{\bibsep}{1em}%
  \def\bibfont{\reset@font\fontfamily{\rmdefault}\normalsize\selectfont}%
\fi%

\AtBeginDocument{\allowdisplaybreaks}%

\def\eqnheadfont{\reset@font\fontfamily{\rmdefault}\fontsize{16}{18}\bfseries\selectfont}%

\newcommand{\eqnhead}[1]{\begingroup%
\begin{center}
{\eqnheadfont #1}\par%
\end{center}
\removelastskip\vskip24pt%
\thispagestyle{titlepage}%%
%%\thispagestyle{empty}%
\endgroup}

%% Macros for border matrix %%