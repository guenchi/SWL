\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cssm}[1997/06/08 CSSM 3]

\newif\if@openright
\newif\if@mainmatter \@mainmattertrue
\newif\ifdraft\draftfalse
\newcommand{\@ptsize}{}

%%% options
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\DeclareOption{10pt}{\renewcommand{\@ptsize}{0}}
\DeclareOption{11pt}{\renewcommand{\@ptsize}{1}}
\DeclareOption{12pt}{\renewcommand{\@ptsize}{2}}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\drafttrue}
\DeclareOption{final}{\draftfalse}
\DeclareOption{titlepage}
    {\ClassError{proc}{Option `titlepage' is not supported}{}}
\DeclareOption{notitlepage}{\relax}
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\DeclareOption{onecolumn}{\relax}
\DeclareOption{twocolumn}
    {\ClassError{proc}{Option `twocolumn' is not supported}{}}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\ExecuteOptions{letterpaper,10pt,twoside,onecolumn,final,openright}
\ProcessOptions
\input{tspl1\@ptsize.clo}
 
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand{\baselinestretch}{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand{\topfraction}{.7}
\setcounter{bottomnumber}{1}
\renewcommand{\bottomfraction}{.3}
\setcounter{totalnumber}{3}
\renewcommand{\textfraction}{.2}
\renewcommand{\floatpagefraction}{.5}
\setcounter{dbltopnumber}{2}
\renewcommand{\dbltopfraction}{.7}
\renewcommand{\dblfloatpagefraction}{.5}

%%% headers and footers
\if@twoside
  \def\ps@headings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \def\chaptermark##1{%
      \markboth{\if@mainmatter\@chapapp\ \thechapter.\ \fi##1}%
               {\if@mainmatter\@chapapp\ \thechapter.\ \fi##1}}
    \def\sectionmark##1{%
      \markright{\thesection.\ ##1}}}
\else
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \def\chaptermark##1{%
      \markright{\if@mainmatter\@chapapp\ \thechapter.\fi\ ##1}}}
\fi
\newcommand*{\chaptermark}[1]{}
\setcounter{secnumdepth}{2} % must be at least two
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter{exercise}[section]
\renewcommand{\thechapter}{\arabic{chapter}}
\renewcommand{\thesection}{\thechapter.\arabic{section}}
\renewcommand{\theexercise}{\thechapter.\arabic{section}.\arabic{exercise}}
\newcommand{\@chapapp}{\chaptername}

%%% illustrated chapter heads
\input epsf
\newlength{\chpicsize}
\setlength{\chpicsize}{\textwidth}
\addtolength{\chpicsize}{-\fboxrule}
\addtolength{\chpicsize}{-\fboxrule}
\def\chpic#1{\begingroup%
  \def\epsfsize##1##2{##1}
  \fboxsep=0pt
  \vbox{\noindent%
    \fbox{\vbox{\hbox to \chpicsize{\hfil\vbox to \chpicsize{\vfil%
      \epsfbox{#1}\vfil}\hfil}}}}\endgroup}
\def\picturechapterhead#1{
  \thispagestyle{empty}
  \null\vfill\vfill
  {\LARGE\bfseries\hbox to \textwidth{\hfil CHAPTER \thechapter}}
  \vskip -12pt
  {\Huge\bfseries\hbox to \textwidth{\leaders\hrule\hfil}}
  \vskip 0pt
  {\Huge\bfseries\hbox to \textwidth{\hfil\textsl{#1}}}
  \vfill\vfill\vfill
  \chpic{pic/ch\thechapter.eps}
  \break
  \thispagestyle{empty}
  \null
  \vfill
  \noindent
  {\it \input{pic/ch\thechapter.tex}}\par\break
  \thispagestyle{plain}}

%%% document structure
\newcommand{\frontmatter}{\cleardoublepage
            \@mainmatterfalse\pagenumbering{roman}}
\newcommand{\mainmatter}{\cleardoublepage
       \@mainmattertrue\pagenumbering{arabic}}
\newcommand{\backmatter}{\if@openright\cleardoublepage\else\clearpage\fi
      \@mainmatterfalse}
\def\chapter#1{
  \if@openright\cleardoublepage\else\clearpage\fi
  \global\@topnum\z@
  \if@mainmatter
    \refstepcounter{chapter}%
    \typeout{\@chapapp\space\thechapter.}%
    \addcontentsline{toc}{chapter}%
              {\protect\numberline{\thechapter}#1}%
  \else
    \addcontentsline{toc}{chapter}{#1}%
  \fi
  \addtocontents{lof}{\protect\addvspace{10\p@}}%
  \addtocontents{lot}{\protect\addvspace{10\p@}}%
  \chaptermark{#1}%
%  \if@mainmatter
%    \picturechapterhead{#1}%
%  \else
    \plainchapterhead{#1}%
%  \fi
  \@afterindentfalse
  \@afterheading}
\def\plainchapterhead#1{%
  \thispagestyle{plain}%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \reset@font
    \interlinepenalty\@M
    \if@mainmatter
      \Huge\bfseries \makebox[\hsize][r]{\textsl{\thechapter. #1}}\par\nobreak
    \else
      \Huge\bfseries \makebox[\hsize][r]{\textsl{#1}}\par\nobreak
    \fi
    \vskip 40\p@
  }}
\newcommand{\section}{\@startsection{section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\reset@font\Large\bfseries}}

%%% page layout
\setlength\leftmargini  {2.5em}
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\setlength\leftmarginv  {1em}
\setlength\leftmarginvi {1em}
\setlength\leftmargin    {\leftmargini}
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand{\theenumi}{\arabic{enumi}}
\renewcommand{\theenumii}{\alph{enumii}}
\renewcommand{\theenumiii}{\roman{enumiii}}
\renewcommand{\theenumiv}{\Alph{enumiv}}
\newcommand{\labelenumi}{\theenumi.}
\newcommand{\labelenumii}{(\theenumii)}
\newcommand{\labelenumiii}{\theenumiii.}
\newcommand{\labelenumiv}{\theenumiv.}
\renewcommand{\p@enumii}{\theenumi}
\renewcommand{\p@enumiii}{\theenumi(\theenumii)}
\renewcommand{\p@enumiv}{\p@enumiii\theenumiii}
\newcommand{\labelitemi}{$\m@th\bullet$}
\newcommand{\labelitemii}{\normalfont\bfseries --}
\newcommand{\labelitemiii}{$\m@th\ast$}
\newcommand{\labelitemiv}{$\m@th\cdot$}

\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{3\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*{\cal}{\@fontswitch{\relax}{\mathcal}}
\DeclareRobustCommand*{\mit}{\@fontswitch{\relax}{\mathnormal}}

%%% table of contents
\newcommand{\@pnumwidth}{1.55em}
\newcommand{\@tocrmarg} {2.55em}
\newcommand{\@dotsep}{4.5}
\setcounter{tocdepth}{2}
\newcommand*{\l@chapter}[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
\newcommand*{\l@section}      {\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand{\tableofcontents}{%
  \if@openright\cleardoublepage\else\clearpage\fi
  \thispagestyle{plain}%
  \global\@topnum\z@
  \chaptermark{\contentsname}%
  \plainchapterhead{\contentsname}%
  \@starttoc{toc}}

%%% bibliography
\newdimen\bibindent \bibindent=1.5em
\newcommand{\newblock}{}
\newenvironment{thebibliography}[1]
     {\chapter{Bibliography}
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand{\theenumiv}{\arabic{enumiv}}}%
      \renewcommand{\newblock}{\hskip .11em \@plus.33em \@minus.07em}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

%%% index
\newenvironment{theindex}
               {\if@openright\cleardoublepage\else\clearpage\fi
                \raggedright
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\plainchapterhead{\indexname}]%
                \addcontentsline{toc}{chapter}{\indexname}%
                \chaptermark{\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\clearpage}
\newcommand{\@idxitem}  {\par\hangindent 40\p@}
\newcommand{\subitem}   {\par\hangindent 40\p@ \hspace*{20\p@}}
\newcommand{\subsubitem}{\par\hangindent 40\p@ \hspace*{30\p@}}
\newcommand{\indexspace}{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}

%%% latex.ltx redefinitions
%% leave padding page blank (no header)
\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
    {\pagestyle{empty}\hbox{}\newpage\if@twocolumn\hbox{}\newpage\fi}\fi\fi}
%% change section headers to "1.1. foo" instead of "1.1  foo"
\def\@seccntformat#1{\csname the#1\endcsname. }
%% change numbered table of contents lines to "1.1. foo" instead of "1.1  foo"
\def\numberline#1{#1. }

%%% adapted from scheme.latex
\font\ninefivett=cmtt9 at 9.5pt
\newskip\ttglue
\ttglue=.5em plus .25em minus .15em
{\obeyspaces\global\let =\ }
\def\schtilde{\raisebox{-.5ex}{\hbox to .5em{\hss\large\rm\char`\~\hss}}}
\def\schdot{\hbox to .5em{\hfil\large\bf.\hfil}}
\def\schcarat{\char`\^}
\def\schbackslash{\char`\\}
\def\schatsign{\hbox to .5em{\hss\footnotesize\rm\char`\@\hss}}
\def\schunderscore{\hbox to .5em{\hss\large\_\hss}}
\def\scheme#1{\mbox{\small\tt\ninefivett\frenchspacing\spaceskip=\ttglue#1}}

\def\beforeschemedisplay{\penalty-100\vskip\parskip\vskip5pt}
\def\afterschemedisplay{\penalty-200\vskip5pt}

\def\noskip\schemedisplay{\begingroup%
  \parindent=0pt%
  \parskip=0pt%
  \def\is{\hbox to 2em{\hfil$\Rightarrow$\hfil}}%
  \def\si{\hbox to 2em{\hfil}}%
  \def\schemeblankline{\par\beforeschemedisplay}%
  \interlinepenalty=2000%
  \small\tt\ninefivett\obeyspaces\frenchspacing}
\def\schemedisplay{\beforeschemedisplay\noskip\schemedisplay}
\def\longcode\schemedisplay{\penalty-200\vskip 8pt plus 4pt%
  \kern3pt\hrule\kern5pt\nobreak\noskip\schemedisplay}
\def\endschemedisplay{\par\endgroup\afterschemedisplay}

\def\var#1{{\normalsize\textrm{\textit{#1}}}}

\def\itemvdots{\item[] \mbox{\vdots}}

\def\parheader#1 {\medskip\noindent{\bf #1.}~~}

\def\strong#1{{\bfseries #1}}

\newenvironment{grammar}
  {\begingroup
    \def\orbar{\hbox to 2em{\hfil}$\vert$\hbox to .75em{\hfil}}
    \def\longis{ $\longrightarrow$\hbox to .75em{\hfil}}
   \penalty-100\vskip 6pt plus 1pt\parindent=0pt\interlinepenalty=5000}
  {\penalty-200\vskip6pt plus 1pt\endgroup}

\def\bar{$\vert$}
\def\ang#1{$\langle${\small\rm{}#1}$\rangle$}
\def\kstar{\raise.5ex\hbox{\scheme{*}}}
\def\kplus{\raise.5ex\hbox{\scheme{+}}}

% for fft example in examples.stex
\def\W#1{W_{\!\!#1}}
\def\fftcases#1{\left\{\,\vcenter{\m@th\baselineskip=18pt
  \ialign{$##\hfil$&\quad##\hfil\crcr#1\crcr}}\right.}

%%% adapted from old tspl macros.tex
\def\exercise{
 \bigskip\refstepcounter{exercise}\noindent
 {\bf Exercise \arabic{chapter}.\arabic{section}.\arabic{exercise}. }}

\newcounter{alphacount}
\def\alphalabel{\textit{\alph{alphacount}}.}
\newenvironment{alphalist}
  {\begingroup\let\beforeschemedisplay=\relax\let\afterschemedisplay=\relax
   \begin{list}{\alphalabel}{\usecounter{alphacount}\itemsep=0pt\parsep=0pt%
        \topsep=0pt}}
  {\end{list}\endgroup}

\def\entryheader{\par\penalty-200\vskip15pt plus 6pt\noskipentryheader}
\def\noskipentryheader{\vbox\bgroup
    \def\formdef##1##2{\par\@@line{\scheme{##2}\hfil{\small\bf{##1}}}}
    \def\suppress\formdef##1##2{\relax} %% needed for old prep.ss only
    \def\returns{\noindent{\small\bf returns:} }}
\def\endnoskipentryheader\par{\egroup\nobreak\vskip6pt plus 1pt\relax}
\let\endentryheader=\endnoskipentryheader

\newwrite\forms
\openout\forms=forms.raw
\def\formsummary{\begingroup\@sanitize\addsummary{\noexpand\thepage}}
\def\addsummary#1#2#3#4{\endgroup
  \edef\formhead{\write\forms}%
  \edef\formentry{{"#2" \string\sfentry{\scheme{#4}}{#3}{#1}}}%
  \expandafter\formhead\formentry}

%%% final set up
\newcommand{\contentsname}{Contents}
\newcommand{\bibname}{Bibliography}
\newcommand{\indexname}{Index}
\newcommand{\chaptername}{Chapter}
\newcommand{\today}{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{headings}
\pagenumbering{arabic}

\if@twoside\else\raggedbottom\fi
\endinput
%%% end of tspl.cls
