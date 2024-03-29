% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alternative.R
\docType{class}
\name{alternative}
\alias{alternative}
\title{Alternative class}
\description{
The `alternative` R6 class serves to define alternatives to be included into choice sets.
The object stores information about the available attributes and their possible values.
The values may be specified as vectors using `c()` function or as random declaration using statistical functions of `r` type.
The object is initialized without attributes and should be later populated with them.
}
\examples{
alt <- alternative$new()
alt$add_attributes(Quality = c(0, 1), Price = rnorm(mean = 5))
alt$get_attributes()
alt$get_laws()
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{attributes}}{A list of attributes definitions}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-add_attributes}{\code{alternative$add_attributes()}}
\item \href{#method-get_attributes}{\code{alternative$get_attributes()}}
\item \href{#method-get_laws}{\code{alternative$get_laws()}}
\item \href{#method-clone}{\code{alternative$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-add_attributes}{}}}
\subsection{Method \code{add_attributes()}}{
Append a list of `call` attributes definitions to the attributes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{alternative$add_attributes(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{The attributes should have a name and generation procedure defined
(ex: `Quality = c(0:1)`)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-get_attributes}{}}}
\subsection{Method \code{get_attributes()}}{
Get a vector of available attributes' names
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{alternative$get_attributes()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Character vector with unique attributes names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_laws"></a>}}
\if{latex}{\out{\hypertarget{method-get_laws}{}}}
\subsection{Method \code{get_laws()}}{
Get a list of alternatives' generation rules
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{alternative$get_laws()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Get a list of laws associated to alternative's attributes.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{alternative$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
