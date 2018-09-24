%%
%% Class that mostly conforms to Oakland University's Thesis Format
%%
%% Adapted from various other thesis styles, inparticular
%%      Harvard, University of Michigan (umich-thesis),
%%      and University of California (ucthesis)
%% Adapted by Aiyesha Ma, October 2004
%%
%% Feel free to modify as neccessary
%% Most Recent Modifications By John Meyer, March 22, 2013

\DeclareMathSizes{12}{12}{12}{12}  %Set the Math Subscript and Superscript Fonts to 12 point

\LoadClass[12pt,oneside,letterpaper]{book}

\usepackage{subfigure}          % Written by Steven Douglas Cochran
\RequirePackage[left=1.5in,top=1.35in,body={6in,8.65in}]{geometry}% page layout
\RequirePackage{setspace}       % for \doublespacing and \singlespacing
\RequirePackage{titlesec}       % for formating headings
\RequirePackage{titletoc}       % for formating TOC headings
\RequirePackage{placeins}       % clear floats before starting new sections
\RequirePackage{fancyhdr}       % for page headings
\RequirePackage{chngpage}
\RequirePackage{ragged2e}       % extended support for justification
\RequirePackage{lscape}         % for landscape pages
\RequirePackage{caption}        % extended support for table captions
\RequirePackage{ifpdf}          % for checks to see if we are in PDF mode

% pull in graphics package
\ifpdf
    \usepackage[pdftex]{graphicx}
    \usepackage[pdftex,hypertexnames=false]{hyperref}
\else
    \usepackage{graphicx}
    \usepackage[hypertex]{hyperref}
\fi

% page layout - these need to add up to 6 1/2 inches
% for normal page width of 8 1/2 inches with margins
\setlength {\oddsidemargin}{0.4in}  % left margin   1.50 inch
\setlength {\textwidth}{6.25in}     % right margin  1 inch

% page layout - these need to add up to 9 inches (648 points)
% for 11 inch page height and 1 inch margins
\setlength {\topmargin}{-0.4in}     % top margin    1.00 inch
\setlength {\headheight}{0in}       % no header
\setlength {\headsep}{0in}          % no header separation
\setlength {\textheight}{655pt}     % bottom margin 1 inch minus 2 lines for page #
\setlength {\footskip}{24pt}        % baseline of last line of text to baseline of footer
\setlength {\footnotesep}{24pt}     % baseline skip for footnotes' 1st line

% provide a way to switch between a thesis and dissertation
\newcommand\degreetype{thesis~}

% define new fields (similar to the standard \author, \title, \date, etc.}:
\def\degree#1{\gdef\@degree{#1}}
\def\degreeyear#1{\gdef\@degreeyear{#1}}
\def\department#1{\gdef\@department{#1}}
\def\chairperson#1{\gdef\@chairperson{#1}}
\def\committeeone#1{\gdef\@committeeone{#1}}
\def\committeetwo#1{\gdef\@committeetwo{#1}}
\def\committeethree#1{\gdef\@committeethree{#1}}

% use \long definitions for the following since their contents
% may be more than a single paragraph:
\long\def\dedication#1{\gdef\@dedication{#1}}
\def\acknowledgmentfile#1{\gdef\@acknowledgmentfile{#1}}
\def\prefacefile#1{\gdef\@prefacefile{#1}}
\def\abstractfile#1{\gdef\@abstractfile{#1}}
\def\acrfile#1{\gdef\@acrfile{#1}}

\parindent  0.5in           % indent paragraph by this much
\parskip    0.mm            % space between paragraphs
\clubpenalty=10000  % Don't allow orphans
\widowpenalty=10000 % Don't allow widows
\renewcommand\floatpagefraction{.5}
\renewcommand\topfraction{.5}
\renewcommand\bottomfraction{.5}
\renewcommand\textfraction{.5}
\belowcaptionskip 12pt \intextsep 48pt \textfloatsep 48pt
\floatsep 48pt

% warn if fields are empty:
\def\@degree{\@latex@warning@no@line{No \noexpand\degree given}}
\def\@degreeyear{\@latex@warning@no@line{No \noexpand\degreeyear given}}
\def\@department{\@latex@warning@no@line{No \noexpand\department given}}
\def\@chairperson{\@latex@warning@no@line{No \noexpand\chairperson given}}
\def\@committeeone{\@latex@warning@no@line{No \noexpand\committeeone given}}
\def\@committeetwo{\@latex@warning@no@line{No \noexpand\committeetwo given}}
\def\@committeethree{\@latex@warning@no@line{No \noexpand\committeethree given}}
\def\@dedication{\@latex@warning@no@line{No [optional] \noexpand\dedication provided}}
\def\@acknowledgmentfile{\@latex@warning@no@line{No [optional] \noexpand\acknowledgmentfile given}}
\def\@prefacefile{\@latex@warning@no@line{No [optional]\noexpand\prefacefile given}}
\def\@abstractfile{\@latex@warning@no@line{No \noexpand\abstractfile given}}
\def\@acrfile{\@latex@warning@no@line{No \noexpand\acrfile given}}

\renewcommand{\chaptername}{CHAPTER}
\renewcommand{\appendixname}{APPENDIX}
\def\@chapapp{\chaptername}


\newif\ifhascopyright
\hascopyrighttrue
\def\hascopyright#1{\csname hascopyright#1\endcsname}

\def\@ifdefined#1#2{\@ifundefined{#1}{}{#2}}

% set up table captions according to OU format requirements
\captionsetup[table]{labelsep=newline,justification=raggedright,singlelinecheck=false}

\newcommand{\makefrontmatter}%
{
    \pagestyle {empty}
    \makecoverpage
    \maketitle
    \pagestyle {plain}
    \pagenumbering {roman}
    \setcounter {page}{2}
    \makecopyright
    \@ifdefined {@dedication}{\makededication}
    \ifx\@empty \@acknowledgmentfile \else {{\makeacknowledgments}} \fi
    \ifx\@empty \@prefacefile \makepreface \else  \fi
    \ifx\@empty \@abstractfile \else \makeabstract \fi
    \changepage{-.8in}{}{}{}{}{}{1.4cm}{.5cm}{}
    %\clearpage
    \tableofcontents
    \listoftables
    \listoffigures
  %  \listofacr
    \clearpage
    \setupforbody
}


% Cover Page Spacing
\newcommand{\makecoverpage}%
{
    \begin{center}
        \vspace*{.15cm}         % Adjust top margin here
        \begin{spacing}{1.0}    % single space title
            \large{\MakeUppercase{\@title}} \\
            \vskip 4.05cm       % Adjust here based on title size
            \large{ \MakeUppercase{\degreetype FOR THE DEGREE OF}} \\
            \large{ \MakeUppercase{\@degree}} \\
            \large{ \MakeUppercase{\@department}} \\
        \end{spacing}
        \vskip 4.05cm
        \large{\MakeUppercase{\@author}}\\
        \vskip 4.05cm
        \large{OAKLAND UNIVERSITY }\\
        \vskip 1.25cm
        \large{\@degreeyear }\\
    \end{center}
}

% Adjust Cover Page Spacing
% re-define the \maketitle command to do the title page:
\renewcommand{\maketitle}%
{
    \begin{titlepage}%
        \begin{spacing}{1}
            \vspace*{-.15cm} %Adjust to center vertically
            \begin{center}
                \MakeUppercase{\@title}\\
                \vskip 1.4 cm %% Adjust as needed
                by\\
                \vskip 1.4cm
                \MakeUppercase{\@author}\\
                \vskip 1.45cm
                A \degreetype submitted in partial fulfillment of the\\ requirements for the degree of \\
                \vskip .95cm
                \MakeUppercase{\@degree} \\
                \MakeUppercase{\@department} \\
                \vskip 1.25cm
                \@degreeyear \\
                \vskip 1.25cm
                Oakland University \\
                Rochester, Michigan
                \par
            \end{center}
            \vskip 3cm
            Thesis Advisory Committee: \\[12pt]     % adjust as needed
            \@chairperson, Ph.D., Chair \\
            \@committeeone, Ph.D. \\
            \@committeetwo, Ph.D. \\
            \@committeethree, Ph.D.
        \end{spacing}
    \end{titlepage}%
}

% the copyright:
\newcommand{\makecopyright}%
  { \newpage
    \mbox{}\par\vfill
    \ifhascopyright{
        \begin{center}
        \singlespace
        \vspace*{-.3cm}
        \copyright \hspace{0.5ex} Copyright by \@author, \@degreeyear\\
        All rights reserved
        \end{center}%
    }
    \vfill
    \global\let\makecopyright\relax
  }


% the dedication:
\newcommand{\makededication}%
{
    \newpage
    \mbox{}\par\vfill
        \vspace*{-.3cm}
        \begin{center}
    {\it \@dedication }
    \end{center}%
    \vfill
    \global\let\makededication\relax
    \global\let\dedication\relax
    \global\let\@dedication\@empty
}

% the acknowledgments:
  \def\@ackhere{none}
  \long\def\acknowledgment#1{\long\gdef\@acknowledgment{#1}}
  \gdef\@acknowledgment{none}
\newcommand{\makeacknowledgments}%
{
    \newpage
    \addcontentsline {toc}{chapter}{ACKNOWLEDGMENTS}
    \pagestyle{plain}
    ~ \\        % force some space so margin is correct
    \centering{ACKNOWLEDGMENTS} \\[0.5in]
    \hspace*{0.5in}  % Fake indent
    \ifx\@acknowledgment \@ackhere
        \raggedright\setlength{\parindent}{.5 in}\@input{\@acknowledgmentfile} %\par%
    \else
        \raggedright\setlength{\parindent}{.5 in}\@acknowledgment
    \fi
    \par
    \vspace{1.5cm}
    \hspace*{2.5in} \@author
    \newpage
}

% the preface:
  \def\@prehere{none}
  \long\def\preface#1{\long\gdef\@preface{#1}}
  \gdef\@preface{none}
\newcommand{\makepreface}%
{
    \newpage
    \addcontentsline {toc}{chapter}{PREFACE}
    ~ \\        % force some space so margin is correct
    \centering{PREFACE} \\[0.5in]
    \hspace*{0.5in} % Fake Indent
    \ifx\@preface \@prehere
        \@input{\@prefacefile}
    \else
        \@preface
    \fi
    \par
    \newpage
}

% the abstract:
% Abstract text may be included inside \abstract{ ... } or in an
% external file. Either \abstractfile{...} or \abstract will be used
  \def\@abshere{none}
  \long\def\abstract#1{\long\gdef\@abstract{#1}}
  \gdef\@abstract{none}

\newcommand{\makeabstract}%
{
    \newpage
    \pagestyle{plain}
    \addcontentsline {toc}{chapter}{ABSTRACT}
    \begin{center}
        ~ \\
        ABSTRACT \\
        \vskip 1.1cm
        \singlespacing\MakeUppercase\@title\\\doublespacing
        \vskip 0.1cm
        by\\
        \vskip 0.15cm
        \@author
    \end{center}%
    \vskip .15cm
    {\noindent Adviser: \@committeeone, Ph.D.\par}
    \vskip .6cm
    %%%---insert proper abstract text---
    \RaggedRight
    \parindent  0.5in   % indent paragraph by this much
    \hspace*{0.5in}     % Fake Indent
    \ifx\@abstract \@abshere
        \@input{\@abstractfile}
    \else
        \@abstract
    \fi
    \newpage
 }

% Remove fancy header line
\renewcommand{\headrulewidth}{0mm}
\renewcommand{\footrulewidth}{0mm}

% Format LOT, LOF, TOC
% - have "---Continued" on   suceeding pages
\renewcommand{\listoftables}%
{
    \@restonecolfalse
    \if@twocolumn\@restonecoltrue\onecolumn\fi
    \chapter*{LIST OF TABLES}
    \pagestyle{fancy}
    \chead[LIST OF TABLES---Continued]{LIST OF TABLES---Continued}
    \addcontentsline {toc}{chapter}{LIST OF TABLES}
    \vspace*{-.79cm}
    {\singlespace  \sloppy
        \@starttoc{lot}\if@restonecol\twocolumn\fi
    }
}

\renewcommand{\listoffigures}%
{
    \@restonecolfalse
    \if@twocolumn\@restonecoltrue\onecolumn\fi
    \chapter*{LIST OF FIGURES}
    \pagestyle{fancy}
    \chead[LIST OF FIGURES---Continued]{LIST OF FIGURES---Continued}
    \addcontentsline {toc}{chapter}{LIST OF FIGURES}
    \vspace*{-.79cm}
    {\singlespace  \sloppy
        \@starttoc{lof}\if@restonecol\twocolumn\fi
    }
}

\renewcommand{\tableofcontents}%
{
    \@restonecolfalse
    \if@twocolumn\@restonecoltrue\onecolumn\fi
    \newpage
    \chapter*{TABLE OF CONTENTS}
    \pagestyle{fancy}
    \chead[TABLE OF CONTENTS---Continued]{TABLE OF CONTENTS---Continued}
    {\singlespace   \sloppy
        \@starttoc{toc}\if@restonecol\twocolumn\fi
    }
}


\newcommand{\listofacr}%
{
    \@restonecolfalse
    \if@twocolumn\@restonecoltrue\onecolumn\fi
    \chapter*{LIST OF ACRONYMS}
    \pagestyle{fancy}
    \chead[LIST OF ACRONYMS---Continued]{LIST OF ACRONYMS---Continued \vspace*{+1.2cm}}
    \addcontentsline {toc}{chapter}{LIST OF ACRONYMS}
    \vspace*{-.79cm}
    {\singlespace  \sloppy
        \input{\@acrfile}
    }
    \clearpage
}

\newcommand{\setupforbody}
{
    \changepage{.8in}{}{}{}{}{}{-1.4cm}{-.5cm}{}
    \pagestyle{plain}
    \clearpage
    \pagenumbering {arabic}
    \setcounter {page}{1}
    \RaggedRight
    \parindent 0.5in
}

% Change table of contents, and lists of figure and tables formatting
\contentsmargin[1in plus 8ex]{0mm}
\titlecontents{chapter}[0pt]{\vspace*{.5cm} \singlespace\MakeUppercase\normalfont}{CHAPTER \thecontentslabel\\*[0pt] }{}{\hfill\makebox[8ex][r]{\thecontentspage}}%[.5cm]

\titlecontents{section}[1.35in]%
    {\vspace*{.5cm}\singlespace}%
    {\contentslabel{0.35in}}%
    {\hspace*{-1.35in}}{\hfill\makebox[8ex][r]{\thecontentspage}}
\titlecontents{subsection}[1.85in]%
    {\vspace*{.5cm}\singlespace}%
    {\contentslabel{0.5in}}%
    {}{\hfill\makebox[8ex][r]{\thecontentspage}}


\titlecontents{table}[1.2in]%
    {\vspace*{.45cm}\singlespace}%
    {\hspace*{-1.2in} \makebox[1.15in][l]{Table \thecontentslabel} }%
    {}{\hfill\makebox[8ex][r]{\thecontentspage}}

\titlecontents{figure}[1.2in]%
    {\vspace*{.45cm}\singlespace}%
    {\hspace*{-1.2in}\makebox[1.15in][l]{Figure \thecontentslabel} }%
    {}{\hfill\makebox[8ex][r]{\thecontentspage}}

% Change chapter and section formatting
\renewcommand{\chaptername}{CHAPTER}
\renewcommand{\appendixname}{APPENDIX}

\titleformat{\chapter}[display]%
    {\normalfont\filcenter}%
    {\vspace*{0cm} \MakeUppercase{\chaptername \hspace{1ex} \thechapter}}%
    {.1cm}{\MakeUppercase}
\titlespacing{\chapter}{0pt}{.4cm}{.5cm}

\titleformat{\section}[block]%
    {\normalfont \filcenter}%
    {\thesection}{1em}{\underline}
\titlespacing*{\section}{0pt}{.7cm}{.1cm}

\titleformat{\subsection}[block]%
    {\normalfont}{\thesubsection}{1em}{\underline}
\titlespacing{\subsection}{0pt}{.7cm}{.1cm}

\titleformat{\subsubsection}[block]%
    {\normalfont}{\thesubsubsection}{1em}{\underline}
\titlespacing{\subsubsection}{0.5in}{.7cm}{.1cm}

\titleformat{\subsubsubsection}[runin]%
    {\normalfont}{\thesubsubsubsection}{1em}{\underline}[. ]
\titlespacing{\subsubsubsection}{0.5in}{7cm}{0cm}

% Modify Chapter * Enviroment
% - Used for TOC, LOF, LOT
% - unadjust for heading space
\def\@makeschapterhead#1{%
  { \normalfont
    \begin{center}
        \vspace*{-1.95cm}
        \MakeUppercase{#1}
        \vspace{.1cm}
        \end{center}
  }}

%  Modify Appendix format
\renewcommand{\appendix}{%
  \addcontentsline{toc}{chapter}{APPENDICES}%
  \clearpage
  \renewcommand{\chaptername}{APPENDIX}
  \thispagestyle{plain}%    Do NOT use the \back@pagestyle macro here
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand\thechapter{\Alph{chapter}}
  \def\chapapp@chapter{\app@chapter}
  \def\chapapp@schapter{\app@schapter}
  \renewcommand{\thesection}{\thechapter.\@arabic\c@section}
}

\newcommand{\appendixchapter}[1]{%
    \newpage
    \refstepcounter{chapter}%
    \addcontentsline{toc}{section}{\protect\numberline {\thechapter.} #1}
    \mbox{}\par\vfill
    \vspace*{-1.3cm}
    \begin{center}
        \MakeUppercase{\appendixname\ \thechapter }\\
        \vskip 10pt  %JNA
        \MakeUppercase{#1}\par
    \end{center}%
    \vfill
    \newpage
    }

% For Appendix
\def\appendixsection#1{
    \refstepcounter{section}%
    \begin{center}
        \underline{\thesection ~~ #1}
    \end{center}
    \hfill
    \vspace{-1cm}
}

\def\appendixsubsection#1{
    \begin{center}#1\end{center}
    \hfill
    \vspace{-1cm}
}


% Modify figure environment
% - the -12pt unadjusts space after caption text
\renewenvironment{figure}[1][\fps@figure]{%
    \@float{figure}[#1]%
    \centering}{%
    \vspace*{-12pt} \end@float }

\renewenvironment{figure*}[1][\fps@figure]{%
    \@float{figure}[#1]%
    \centering}{%
    \vspace*{-12pt} \end@float }

% Modify Description Environment
% do not use bold text (use underline instead)
\renewcommand\descriptionlabel[1]{\normalfont\underline{#1}}
\renewenvironment{description}
               {\list{}{\labelwidth\z@
                   \leftmargin 0.5in
                   \rightmargin 0.5in
                   \itemindent -0.4in
                   \let\makelabel\descriptionlabel}}
               {\endlist}

% Do something about Bibliography
% - Set spacing of title to match chapter titles
% - Add to table of Contents

\renewenvironment{thebibliography}[1]{
  { \newpage
  \raggedbottom
    \begin{center}
        \vspace*{1.53cm}
        \MakeUppercase{\bibname}\\
        \vspace*{-.133cm}
    \end{center}
    \pagestyle{plain}% REFERENCES uses BODY pagestyle, not endmatter.
    \addcontentsline{toc}{chapter}{REFERENCES}
  }
  \list{\@biblabel{\@arabic\c@enumiv}}%
  { \singlespace%
    \settowidth\labelwidth{\@biblabel{#1}}%
      \leftmargin\labelwidth
      \advance\leftmargin\labelsep
      \@openbib@code
      \usecounter{enumiv}%
      \let\p@enumiv\@empty
      \renewcommand\theenumiv{\@arabic\c@enumiv}
  }%
  \sloppy
  \sfcode`\.\@m}
  {\def\@noitemerr
     {\@latex@warning{Empty ``thebibliography'' environment}
  \pagestyle{\back@pagestyle}
  }%
  \endlist
}

% turn on `doublespacing' [setspace]
\AtBeginDocument{\doublespacing}%\um@raggedright}

% For List of Symbols
% Reminder:  Adjust this symbol table layout along with margins
\def\addnotation #1: #2#3{
    \vskip 1.0em plus 1pt
    \noindent
    \begin{minipage}{3cm}{#1}\end{minipage}
    \begin{minipage}{9cm}{#2}\end{minipage}
    \begin{minipage}{2cm}{\hfill}\end{minipage}
    \begin{minipage}{.6cm}
    {
        \begin{flushright}
        \pageref{#3}
        \end{flushright}
    }
    \end{minipage} \\
}

\def\fudgenotation #1: #2#3{
    \vskip 1.0em plus 1pt
    \begin{minipage}{.75in}{}\end{minipage}
    \begin{minipage}{3in}{#2}\end{minipage}
    \begin{minipage}{.75in}{\hfill}\end{minipage}
    \begin{minipage}{.5in}{}\end{minipage} \\
}

\def\ns#1{\label{#1}}

\def\sb#1{_{#1}}

\setcounter{secnumdepth}{3}  %change if you don't want three layers of numbers for subsubsection

% For Formatting URL in bibliography
\def\url@leostyle{%
  \@ifundefined{selectfont}{\def}{%
    \def\UrlFont{}}\Url@do
} \makeatother

\urlstyle{leostyle}

%Hack to adjust spacing before a section line
\def\OUsection#1{
    \section{#1}
    \indent
    \vspace{-1cm}
}
