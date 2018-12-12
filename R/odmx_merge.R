# getLabResultsBySiteAndVariableGroup ------------------------------------------
getLabResultsBySiteAndVariableGroup <- function
(
  sampleInfo.all,
  labData.all,
  siteCodes,
  variableGroupNames
)
{
  result <- list()

  for (siteCode in siteCodes) {

    for (variableGroupName in variableGroupNames) {

      result.one <- mergeSampleInfoWithLabDataForSiteAndVariableGroup(
        sampleInfo.all = sampleInfo.all,
        labData.all = labData.all,
        siteCode = siteCode,
        variableGroupName = variableGroupName
      )

      result[[siteCode]][[variableGroupName]] <- result.one
    }
  }

  result
}

# mergeSampleInfoWithLabDataForSiteAndVariableGroup ----------------------------
mergeSampleInfoWithLabDataForSiteAndVariableGroup <- function
(
  sampleInfo.all,
  labData.all,
  siteCode,
  variableGroupName
)
{
  sampleInfo <- sampleInfo.all[sampleInfo.all$station == siteCode, ]

  valueMatrix <- extractSiteAndVariableGroupInWideForm(
    labData.all, siteCode, variableGroupName)

  merge(sampleInfo, valueMatrix, by.x = "LIMS_Nr", by.y = "LabSampleCode")
}

# writeCsvFilesForEachSiteAndVariableGroup -------------------------------------
writeCsvFilesForEachSiteAndVariableGroup <- function
(
  labData.all,
  siteCodes,
  variableGroupNames,
  out.dir
)
{
  for (siteCode in siteCodes) {

    for (variableGroupName in variableGroupNames) {

      filePath <- extractAndWriteSiteAndVariableGroup(
        labData.all = labData.all,
        siteCode = siteCode,
        variableGroupName = variableGroupName,
        out.dir = out.dir
      )
    }
  }
}

# extractAndWriteSiteAndVariableGroup ------------------------------------------
#' @importFrom kwb.utils hsSubstSpecChars
#' @importFrom kwb.utils createDirAndReturnPath
#' @importFrom utils write.table
extractAndWriteSiteAndVariableGroup <- function
(
  labData.all,
  siteCode,
  variableGroupName,
  out.dir
)
{
  cat("SiteCode =", siteCode, ", VariableGroupName =", variableGroupName, "...\n")

  labData.wide <- extractSiteAndVariableGroupInWideForm(
    labData.all = labData.all,
    siteCode = siteCode,
    variableGroupName = variableGroupName
  )

  writeSiteAndVariableGroupToFile(
    labData.wide = labData.wide,
    siteCode = siteCode,
    variableGroupName = variableGroupName,
    out.dir = out.dir
  )
}

# writeSiteAndVariableGroupToFile ----------------------------------------------
writeSiteAndVariableGroupToFile <- function (
  labData.wide,
  siteCode,
  variableGroupName,
  out.dir
)
{
  file.name <- sprintf(
    "labResults_%s_%s.csv",
    siteCode,
    hsSubstSpecChars(variableGroupName)
  )

  out.dir.site <- createDirAndReturnPath(file.path(out.dir, siteCode))

  filePath <- file.path(out.dir.site, file.name)

  write.table(
    labData.wide,
    file = filePath,
    sep = ";",
    dec = ",",
    na = "",
    row.names = FALSE
  )

  filePath
}

# extractSiteAndVariableGroupInWideForm ----------------------------------------
#' @importFrom kwb.utils removeColumns
#' @importFrom stats reshape
extractSiteAndVariableGroupInWideForm <- function
(
  labData.all,
  siteCode,
  variableGroupName
)
{
  selected <- labData.all$VariableGroupName == variableGroupName &
    labData.all$SiteCode == siteCode

  labData <- labData.all[selected, ]

  labData <- removeColumns(
    labData,
    columnsToRemove = c(
      "VariableGroupName", "SiteCode", "ValueID", "UnitsAbbreviation")
  )

  columns <- c("LocalDateTime", "LabSampleCode", "VariableCode",
               "CensorCode", "DataValue")

  labData <- labData[order(labData$LabSampleCode), columns]

  labData.wide <- reshape(
    data = labData,
    timevar = "VariableCode",
    idvar = c("LocalDateTime", "LabSampleCode"),
    direction = "wide"
  )

  names(labData.wide) <- multiSubstitute(
    strings = names(labData.wide),
    replacements = list(
      "^CensorCode.*" = "CC" ,
      "DataValue\\." = ""
    )
  )

  labData.wide
}
