\name{defineDataRoles}
\alias{defineDataRoles}
\title{defineDataRoles}
\description{Constructor for BIOdry role metadata
Attach variable-role metadata to multilevel ecological data}
\usage{defineDataRoles(rd, inputs = NULL, 
    response = NULL, 
    time = NULL, groups = NULL, 
    name_map = NULL, 
    validate = c("warn", 
        "stop", "none"))}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological or climatic data.}
  \item{inputs}{Optional \code{character} vector of raw input or
measurement variables. For climate data this can
contain more than one variable, for example
\code{c("pr", "tm")}; for \code{\link{wlai}}
precipitation comes first (columns named \code{pr}
and \code{tm} are used by name). If omitted, numeric
non-group, non-time columns are inferred.}
  \item{response}{Optional \code{character} scalar giving the
current active \code{response} variable. If omitted and
exactly one input variable is inferred, that
variable is used as the \code{response}; otherwise the
\code{response} remains unset.}
  \item{time}{Optional \code{character} vector naming one or
more ordered \code{time} variables. Examples include
\code{"year"} or \code{c("month", "year")}. The
constructor stores all supplied names in
\code{time_vars} and the first one as the primary
\code{time} variable in \code{time}.}
  \item{groups}{Optional grouping hierarchy, either as a
\code{character} vector or as a nested string
with \code{/} separators such as
\code{"plot/tree/sample"}. If omitted, the factor
and character columns other than the month column
are inferred, outer level first.}
  \item{name_map}{Optional named \code{character} vector mapping
older variable names to current names, for
example \code{c(year = "hydro_year_id")}. This
mapping is stored verbatim and can be updated by
later workflow steps.}
  \item{validate}{Validation mode. Use
\code{"warn"} to keep partial metadata with a
diagnostic, \code{"stop"} to require a fully
resolved declaration, or \code{"none"} for
quiet inference.}
}
\details{This constructor attaches a small
\code{BIOdry_meta} attribute to a
\code{data.frame} so that later workflow
steps can use declared roles. Functions such as
\code{\link{lmeForm}},
\code{\link{tdForm}},
\code{\link{frametoLme}}, and
\code{\link{FriedmanForm}} decide roles from
column types (see \code{\link{cClass}}); a
\code{time}, \code{groups} or \code{response}
declared here takes precedence over that rule.
Roles that are only inferred, and the metadata
that \code{\link{ringApply}} records
automatically, do not.

The constructor respects explicit user
definitions first. If the user supplies
\code{time}, \code{groups},
\code{inputs}, or \code{response}, these
values are stored as given after
validation. If they are not supplied, the
constructor infers them from column types:
the \code{time} variable is the first whole-number
numeric column, the \code{groups} are the factor and
character columns other than the month column,
outer level first, and the \code{inputs} are the
remaining numeric columns. A \code{time} variable is
not also placed in the grouping hierarchy
unless the user includes it in \code{groups}.

The \code{time} argument may be given as
a single variable or as a character vector
describing ordered \code{time} components such as
\code{c("month", "year")}. In that case,
all supplied names are stored in
\code{time_vars}, while a conservative
primary \code{time} variable, the first supplied
element, is also stored in \code{time} for
downstream functions that expect a single
covariate name.

The \code{groups} argument may be given as
a character vector such as
\code{c("plot", "tree", "sample")} or as a
nested string such as
\code{"plot/tree/sample"}. Whole-number
columns, such as \code{year}, are grouping
levels only when declared here; to use
\code{year} as a random grouping level, see
\code{\link{frametoLme}}.

The constructor is intentionally light. It
does not copy the data, change user-visible
columns, or fit any models. It only stores
compact metadata describing: raw input
measurement variables, the current active
\code{response} when available, the primary \code{time}
variable, all declared \code{time} components, the
grouping hierarchy, whether \code{time} was also
declared as part of that hierarchy, and an
optional \code{name_map} linking older
names to derived names such as
\code{year -> hydro_year_id}. This allows
later functions to preserve context across
successive transformations.

When automatic inference is not possible,
the function fails gracefully according to
\code{validate}. Use
\code{validate = "warn"} to see a
diagnostic while still returning the data
with partial metadata, or
\code{validate = "stop"} to require a
fully resolved declaration.}
\value{\code{data.frame} with a \code{BIOdry_meta} attribute.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## tree-ring widths: declare the time and the grouping hierarchy
data(Prings05, envir = environment())
dr1 <- defineDataRoles(Prings05, time = "year",
                       groups = "plot/tree/sample")
str(attr(dr1, "BIOdry_meta"))
## declared roles are used by the formula helpers
lmeForm(dr1)

## monthly climate: precipitation first, then temperature, as wlai()
## reads them; year and month as time components
data(PTclim05, envir = environment())
cl1 <- defineDataRoles(PTclim05, inputs = c("pr", "tm"),
                       time = c("year", "month"), groups = "plot")
str(attr(cl1, "BIOdry_meta"))
}
