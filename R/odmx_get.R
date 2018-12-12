# getLabDataFromOdm ------------------------------------------------------------
#' @importFrom kwb.odm availableOdmDatasets
#' @importFrom kwb.odm resolveForeignKeys
getLabDataFromOdm <- function(siteCode, variableGroupName)
{
  siteID <- getSiteID(siteCode)

  variables <- getVariablesOfGroup(variableGroupName)

  datasetInfo <- availableOdmDatasets()

  condition <- datasetInfo$keyFields$SiteID == siteID &
    datasetInfo$keyFields$VariableID %in% variables$VariableID

  keyFields <- datasetInfo$keyFields[condition, ]

  allData <- getOdmDataAsMatrix(
    dsids = keyFields$dsid,
    datasetInfo = datasetInfo
  )

  dsInfo <- merge(
    resolveForeignKeys(keyFields)[, c("dsid", "SiteCode", "VariableCode")],
    keyFields[, c("dsid", "CensorCode")]
  )

  attr(allData, "dsInfo") <- dsInfo

  allData
}

# getOdmDataAsMatrix -----------------------------------------------------------
#' @importFrom utils tail
#' @importFrom kwb.odm getOdmDataset
#' @importFrom kwb.utils hsRenameColumns
getOdmDataAsMatrix <- function(dsids, datasetInfo)
{
  allData <- NULL

  for (dsid in dsids) {

    cat("dsid =", dsid, "/", tail(dsids, 1), "\n")

    dsData <- getOdmDataset(
      dsid = dsid,
      fields = "DateTimeUTC,DataValue",
      dsInfo = datasetInfo,
      dbg = FALSE
    )

    dsData <- hsRenameColumns(
      dsData,
      list(DataValue = paste("ds", dsid, sep = "."))
    )

    if (is.null(allData)) {
      allData <- dsData
    }
    else {
      allData <- merge(allData, dsData, by = "DateTimeUTC", all = TRUE)
    }
  }

  allData
}

# getVariablesOfGroup ----------------------------------------------------------
#' @importFrom kwb.db currentDb
#' @importFrom kwb.db hsGetTable
getVariablesOfGroup <- function(variableGroupName, db = currentDb())
{
  variableGroups <- hsGetTable(db, "VariableGroups")
  variableGroupVariable <- hsGetTable(db, "VariableGroupVariable")

  variableGroupID <- variableGroups$VariableGroupID[
    variableGroups$VariableGroupName == variableGroupName]

  variableIDs <- variableGroupVariable$VariableID[
    variableGroupVariable$VariableGroupID == variableGroupID]

  odm_Variables(where_VariableID = variableIDs)
}

# getVariableCodes -------------------------------------------------------------
#' @importFrom kwb.db currentDb
#' @importFrom kwb.odm odm_Variables
getVariableCodes <- function(siteID, db = currentDb())
{
  variables <- odm_Variables(where_VariableID = getVariableIDs(siteID, db))
  variables$VariableCode
}

# getVariableIDs ---------------------------------------------------------------
#' @importFrom kwb.db currentDb
getVariableIDs <- function(siteID, db = currentDb())
{
  sql <- paste("SELECT DISTINCT VariableID FROM dataValues WHERE SiteID =",
               siteID)

  hsSqlQuery(mdb = db, sql = sql)$VariableID
}

# getSiteID --------------------------------------------------------------------
#' @importFrom kwb.odm odmSites
getSiteID <- function(siteCode)
{
  odmSites(SiteCode = siteCode)$SiteID
}
