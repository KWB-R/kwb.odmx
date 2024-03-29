% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odmx_read.R
\name{readMainOdmObjects}
\alias{readMainOdmObjects}
\title{read main ODM objects from Excel files}
\usage{
readMainOdmObjects(folder)
}
\arguments{
\item{folder}{folder containing the files variables.xls, sources.xls,
sites.xls, methods.xls. In each Excel file the cell range making up the
actual table must be named "tabledata"}
}
\value{
list with elements \code{variables}, \code{sources}, \code{sites},
  \code{methods} each of which is a data frame with two columns (ID and code
  or a similar column identifying the object).
}
\description{
read main ODM objects from Excel files (ID and Code or corresponding column)
}
