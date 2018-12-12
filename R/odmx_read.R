# read_main_odm_objects --------------------------------------------------------
#' read main ODM objects from Excel files
#'
#' read main ODM objects from Excel files (ID and Code or corresponding column)
#'
#' @param folder folder containing the files variables.xls, sources.xls,
#'   sites.xls, methods.xls. In each Excel file the cell range making up the
#'   actual table must be named "tabledata"
#'
#' @return list with elements \code{variables}, \code{sources}, \code{sites},
#'   \code{methods} each of which is a data frame with two columns (ID and code
#'   or a similar column identifying the object).
#'
#' @export
readMainOdmObjects <- function(folder)
{
  objectFields <- list(
    variables = "VariableID,VariableCode",
    sources = "SourceID,Organization",
    sites = "SiteID,SiteCode",
    methods = "MethodID,MethodDescription"
  )

  objectNames <- names(objectFields)

  L <- lapply(objectNames, function(objectName) {
    cat("Getting", objectName, "...\n")
    hsGetTable(
      mdb = file.path(folder, paste0(objectName, ".xls")),
      tbl = "tabledata",
      fields = objectFields[[objectName]],
      dbg = FALSE
    )
  })

  structure(L, names = objectNames)
}
