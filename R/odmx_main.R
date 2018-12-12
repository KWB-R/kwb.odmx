# getLabValuesGreaterOrEqualZero -----------------------------------------------
#' Get lab results from database
#'
#' @param db name of ODBC database or full path to database file
#' @param stringsAsFactors Passed to \code{hsSqlQuery}. Default: \code{FALSE}#'
#' @param keep.only.best.quality if \code{TRUE} (default) data values in raw
#' data quality are removed if they occur in different quality levels
#'
#' @export
#'
#' @importFrom kwb.db hsSqlQuery
#' @importFrom kwb.db sqlForSelect
#' @importFrom kwb.odm sqlSourceForResolvedOdmDataValues
#'
getLabValuesGreaterOrEqualZero <- function
(
  db,
  stringsAsFactors = FALSE,
  keep.only.best.quality = TRUE
)
{
  #setCurrentSqlDialect("msaccess")

  fields <- paste0(
    "VariableGroupName,ValueID,LocalDateTime,DataValue,CensorCode,SiteCode,",
    "VariableCode,LabSampleCode,UnitsAbbreviation,QualityControlLevelID"
  )

  # Query DataValuesByVariableGroup:
  labResults <- hsSqlQuery(
    mdb = db,
    sql = sqlForSelect(
      tablename = sqlForSelect(
        fields = fields,
        tablename = sqlSourceForResolvedOdmDataValues()
      ),
      whereClause = "VariableGroupName <> 'OGRE' AND DataValue >= 0"
    ),
    stringsAsFactors = stringsAsFactors
  )

  # ValueID must be unique!
  if (nrow(labResults) != length(unique(labResults$ValueID))) {

    message("\n*** The ValueID is not unique in the returned data frame!\n",
            "*** This is ok if there are parameters that appear in different ",
            "parameter groups.")
  }

  if (keep.only.best.quality) {
    labResults <- keepOnlyBestQualityControlLevel(labResults)
  }

  labResults
}

# keepOnlyBestQualityControlLevel ----------------------------------------------
#' Remove raw data values if data values of better quality exist
#'
#' @param x data frame with columns \code{ValueID}, \code{LabSampleCode},
#'   \code{VariableCode}, \code{QualityControlLevelID}
#' @param dbg if \code{TRUE} (default) debug messages are shown else not
#'
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.utils firstElement
#' @importFrom stats as.formula
#' @importFrom stats aggregate
keepOnlyBestQualityControlLevel <- function
(
  x,
  dbg = TRUE
)
{
  x <- x[order(selectColumns(x, "QualityControlLevelID")), ]

  groupByFormula <- as.formula("ValueID ~ LabSampleCode + VariableCode")

  # keep only higher QualityControlLevelID if measurements are double
  y <- aggregate(groupByFormula, data = x, FUN = length)

  indices <- which(y$ValueID > 1)

  if (length(indices) > 0) {

    if (dbg) {
      message("The following data values exist in a better quality, I will ",
              "remove them:")
      print(x[indices, ])
    }

    y <- aggregate(groupByFormula, data = x, FUN = firstElement)

    x <- x[- match(y$ValueID[indices], x$ValueID), ]
  }

  x
}

# simplifyColumns --------------------------------------------------------------
#' merge VariableCode and Unit, use '<', '>' or '=' as CensorCode
#'
#' @param x data frame with columns \code{VariableCode, UnitsAbbreviation,
#'   CensorCode}
#' @param short.codes if \code{TRUE} (default) the censor codes are renamed:
#'   "lt" -> "<"; "gt" -> ">"; "nc" -> ""
#' @export
#'
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.utils multiSubstitute
#'
#' @examples
#' x <- data.frame(VariableCode = c("A", "B"),
#'                 UnitsAbbreviation = c("mg/l", "ug/l"),
#'                 CensorCode = c("lt", "nc"))
#'
#' simplifyColumns(x)
simplifyColumns <- function
### add unit to VariableCode and replace CensorCodes ("gt" -> ">", "lt" -> "<")
(
  x,
  short.codes = TRUE
)
{
  x$VariableCode <- paste(selectColumns(x, "VariableCode"),
                          selectColumns(x, "UnitsAbbreviation"), sep = "_")

  if (short.codes) {
    replacements <- list("lt" = "<", "gt" = ">", "nc" = "")

    cc <- selectColumns(x, "CensorCode")
    x$CensorCode <- multiSubstitute(cc, replacements)
  }

  x
}
