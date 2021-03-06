% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface_hidden.R
\name{interface-wrappers}
\alias{interface-wrappers}
\alias{.selectInput.iSEE}
\alias{.selectizeInput.iSEE}
\alias{.checkboxInput.iSEE}
\alias{.checkboxGroupInput.iSEE}
\alias{.sliderInput.iSEE}
\alias{.numericInput.iSEE}
\alias{.radioButtons.iSEE}
\title{\pkg{iSEE} UI element wrappers}
\usage{
.selectInput.iSEE(x, field, label, ..., help = TRUE)

.selectizeInput.iSEE(x, field, label, ..., help = TRUE)

.checkboxInput.iSEE(x, field, label, ..., help = TRUE)

.checkboxGroupInput.iSEE(x, field, label, ..., help = TRUE)

.sliderInput.iSEE(x, field, label, ..., help = TRUE)

.numericInput.iSEE(x, field, label, ..., help = TRUE)

.radioButtons.iSEE(x, field, label, ..., help = TRUE)
}
\arguments{
\item{x}{A \linkS4class{Panel} object for which to construct an interface element.}

\item{field}{String containing the name of the parameter controlled by the interface element.}

\item{label}{String specifying the label to be shown.}

\item{...}{Further arguments to be passed to the corresponding \pkg{shiny} function.}

\item{help}{Logical scalar indicating whether a help icon should be added to the label.}
}
\value{
The output of \code{FUN(id, ..)} is returned where \code{FUN} is set the corresponding \pkg{shiny} function, e.g., \code{\link{selectInput}} for \code{.selectInput.iSEE}.
\code{id} is defined by concatenating \code{\link{.getEncodedName}(x)} and \code{field} (separated by an underscore).

If \code{.hideInterface(x, field)} is \code{TRUE}, the output is wrapped inside a \code{\link{hidden}} call.
}
\description{
Wrapper functions to create the standard \pkg{shiny} user interface elements,
accompanied by an optional help icon that opens an interactive tour describing the purpose of the element.
Also responds to requests to hide a particular element via \code{\link{.hideInterface}}.
}
\author{
Aaron Lun
}
