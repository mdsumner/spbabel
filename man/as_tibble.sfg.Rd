% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf.r
\name{as_tibble.sfg}
\alias{as_tibble.sfg}
\title{Individual geometries as tibbles.}
\usage{
\method{as_tibble}{sfg}(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
)
}
\arguments{
\item{x}{sf geometry of type sfg}

\item{...}{Unused, for extensibility.}

\item{.rows}{The number of rows, useful to create a 0-column tibble or
just as an additional check.}

\item{.name_repair}{Treatment of problematic column names:
\itemize{
\item \code{"minimal"}: No name repair or checks, beyond basic existence,
\item \code{"unique"}: Make sure names are unique and not empty,
\item \code{"check_unique"}: (default value), no name repair, but check they are
\code{unique},
\item \code{"universal"}: Make the names \code{unique} and syntactic
\item a function: apply custom name repair (e.g., \code{.name_repair = make.names}
for names in the style of base R).
\item A purrr-style anonymous function, see \code{\link[rlang:as_function]{rlang::as_function()}}
}

This argument is passed on as \code{repair} to \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}}.
See there for more details on these terms and the strategies used
to enforce them.}

\item{rownames}{How to treat existing row names of a data frame or matrix:
\itemize{
\item \code{NULL}: remove row names. This is the default.
\item \code{NA}: keep row names.
\item A string: the name of a new column. Existing rownames are transferred
into this column and the \code{row.names} attribute is deleted.
No name repair is applied to the new column name, even if \code{x} already contains
a column of that name.
Use \code{as_tibble(rownames_to_column(...))} to safeguard against this case.
}

Read more in \link[tibble]{rownames}.}
}
\value{
tibble
}
\description{
Individual geometries as tibbles.
}
