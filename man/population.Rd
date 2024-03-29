% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population.R
\docType{class}
\name{population}
\alias{population}
\title{Population class}
\description{
The `population` R6 class regroups different individuals profiles.
It serves as a wrapper for simultaneous interactions with multiple individual profiiles.
The object is used for population description and  generation procedures.
}
\examples{
# Create individuals
ind1 <- individual$new()
ind1$add_characteristics(Age = rnorm(mean = 40, sd = 10))
ind1$add_decision_rule(drule <- decision_rule$new())
ind2 <- individual$new()
ind2$add_characteristics(Age = rnorm(mean = 30, sd = 5))
ind2$add_decision_rule(drule <- decision_rule$new())

# Regroup individuals into population
pop <- population$new(profiles = list(ind1, ind2), n = list(10, 15))

# Add new profile
ind3 <- individual$new()
ind3$add_characteristics(Age = rnorm(mean = 50, sd = 4), Salary = runif(min = 1, max = 5))
ind3$add_decision_rule(drule <- decision_rule$new())
pop$add_profile(ind3, 5)
pop$get_chars()
pop$get_n()
pop$get_rules()
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{profiles}}{A list of individual profile.}

\item{\code{n}}{A list of individuals' numbers per profile (repecting the profiles' order).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{population$new()}}
\item \href{#method-add_profile}{\code{population$add_profile()}}
\item \href{#method-get_chars}{\code{population$get_chars()}}
\item \href{#method-get_n}{\code{population$get_n()}}
\item \href{#method-get_rules}{\code{population$get_rules()}}
\item \href{#method-clone}{\code{population$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new `population` object.
The function allows to create an object populated with individual profiles.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{population$new(profiles = list(NULL), n = list(NULL))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{profiles}}{A list of individual profiles for population.}

\item{\code{n}}{The associated numbers for each profile to appear in the dataset.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_profile"></a>}}
\if{latex}{\out{\hypertarget{method-add_profile}{}}}
\subsection{Method \code{add_profile()}}{
Add new individual profile and respective desired number of individuals.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{population$add_profile(individual, n, profile_name = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{individual}}{Individual profile to be added}

\item{\code{n}}{A number associate to the added profile}

\item{\code{profile_name}}{An added profile name, not required.
Is NULL by default.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_chars"></a>}}
\if{latex}{\out{\hypertarget{method-get_chars}{}}}
\subsection{Method \code{get_chars()}}{
Get a vector of available characteristics' names across all individual profiles in population.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{population$get_chars()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Character vector with unique characteristics names within populatoin.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_n"></a>}}
\if{latex}{\out{\hypertarget{method-get_n}{}}}
\subsection{Method \code{get_n()}}{
Get a vector regroupping individuals' numbers per profile
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{population$get_n()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Numeric vector with numbers of n by individual profile.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_rules"></a>}}
\if{latex}{\out{\hypertarget{method-get_rules}{}}}
\subsection{Method \code{get_rules()}}{
Extract `decision_rule` objects across individual profiles
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{population$get_rules()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of rules present within population.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{population$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
