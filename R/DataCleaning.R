########################################################################################################
########################################################################################################
#' getdataout
#' @param d a
#' @param S a
#' @export getdataout
getdataout <- function(d, S) {
  if (S == "mage") {
    out <- d$gastro
  } else if (S == "luft") {
    out <- d$respiratory
  }
  return(out)
}


#' Title
#'
#' @param In a
#' @param S a
#' @export getIN
getIN <- function(In, S) {
  if (S == "mage") {
    out <- In$mage
  } else if (S == "luft") {
    out <- In$luft
  }
  return(out)
}
########################################################################################################
########################################################################################################
#' IdentifyAllDatasets
#' @param raw a
#' @param clean a
#' @import data.table
#' @import fhi
#' @export IdentifyAllDatasets
IdentifyAllDatasets <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
                                clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
  # variables used in data.table functions in this function
  id <- isRaw <- isClean <- NULL
  # end

  raw <- data.table(raw)
  clean <- data.table(clean)

  raw[, id := gsub(".txt", "", gsub("partially_formatted_", "", raw))]
  raw[, isRaw := TRUE]
  clean[, id := gsub(".txt", "", gsub("done_", "", clean))]
  clean[, isClean := TRUE]
  res <- merge(raw, clean, by = "id", all = TRUE)
  setorder(res, id)

  return(res)
}

########################################################################################################
########################################################################################################
#' IdentifyInOutDoc
#'
#' @param raw a
#' @param fylke a
#' @param indoc a
#' @import data.table
#' @export IdentifyInOutDoc
IdentifyInOutDoc <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
                             fylke = fread(system.file("extdata", "fylke.csv", package = "sykdomspulspdf")),
                             indoc = list.files(fhi::DashboardFolder("data_raw"), "in_")) {
  # variables used in data.table functions in this function
  type <- V1 <- id <- NULL

  fylke <- data.table(unique(fylke$Fylkename))

  fylke[, type := "default"]

  indoc <- data.table(indoc)
  indoc[, type := gsub(".odt", "", gsub("in_", "", gsub("_mage", "", gsub("_luft", "", indoc))))]

  indoc$mage <- ifelse(grepl("mage", indoc$indoc, ignore.case = T), indoc$indoc, "in_default_mage.odt")
  indoc$luft <- ifelse(grepl("luft", indoc$indoc, ignore.case = T), indoc$indoc, "in_default_luft.odt")
  indoc[, V1 := type]
  indoc <- indoc[type != "default"]

  indoc$indoc <- indoc$type <- NULL


  res <- merge(fylke, indoc, by = "V1", all = TRUE)

  res$mage[is.na(res$mage)] <- "in_default_mage.odt"
  res$luft[is.na(res$luft)] <- "in_default_luft.odt"
  res <- res[, id := as.Date(gsub("_", "-", LatestRawID()))]

  return(res)
}

########################################################################################################
########################################################################################################
#' DeleteOldDatasets
#'
#' @param raw a
#' @param dat_in a
#' @param clean a
#' @import data.table
#' @export DeleteOldDatasets
DeleteOldDatasets <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
                              dat_in = list.files(fhi::DashboardFolder("data_raw"), "in_default"),
                              clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
  res <- IdentifyAllDatasets(raw = raw, clean = clean)
  if (nrow(res) > 0) {
    res <- res[-nrow(res)]
  }
  for (i in 1:nrow(res)) {
    unlink(file.path(fhi::DashboardFolder("data_raw"), res[i]$raw))
    unlink(file.path(fhi::DashboardFolder("data_clean"), sprintf("*%s*", res[i]$id)))
  }
}

########################################################################################################
########################################################################################################
#' IdentifyDatasets
#'
#' @param raw a
#' @param clean a
#' @import data.table
#' @export IdentifyDatasets
IdentifyDatasets <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
                             clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
  res <- IdentifyAllDatasets(raw = raw, clean = clean)
  if (nrow(res) > 0) res <- res[nrow(res)]

  return(res)
}

########################################################################################################
########################################################################################################
#' LatestRawID
#' @import data.table
#' @export LatestRawID
LatestRawID <- function() {
  f <- IdentifyDatasets()
  return(max(f$id))
}

########################################################################################################
########################################################################################################

#' DeleteLatestDoneFile
#'
#' @param file a
#' @import data.table
#' @export DeleteLatestDoneFile
DeleteLatestDoneFile <- function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
  try(unlink(file), TRUE)
  # try(unlink(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

########################################################################################################
########################################################################################################

#' CreateLatestDoneFile
#'
#' @param file a
#' @export CreateLatestDoneFile
CreateLatestDoneFile <- function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
  try(file.create(file), TRUE)
}

########################################################################################################
########################################################################################################
#' findLastWeek
#'
#' @param date a
#' @param data a
#' @export findLastWeek
findLastWeek <- function(date, data) {
  # lastweek <- as.Date(date)
  lastweek <- 53
  rows <- dim(data)[1]
  for (l in lastweek:1) {
    if (!is.na(data[rows - 1, l]) && !is.na(data[rows, l])) {
      if (data[rows - 1, l] / data[rows, l] < 1.3) {
        myweek <- l
        break
      }
    }
  }
  return(myweek)
}

########################################################################################################
########################################################################################################

#' CleanData
#'
#' @param d a
#' @import data.table
#' @export CleanData
CleanData <- function(d) {
  # variables used in data.table functions in this function
  date <- NULL
  municip <- NULL
  # end
  if (!"IDate" %in% class(d$date)) {
    d[, date := data.table::as.IDate(date)]
  }

  d[, Fylke := as.numeric(substr(d$municip, 8, 9))]
  d[, year := format.Date(date, "%G")]
  d <- d[year %in% c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")]
  d[, month := format.Date(date, "%m")]
  d[, week := format.Date(date, "%V")]

  d[, newage := NA]
  d$newage[d$age == "0-4"] <- 1
  d$newage[d$age == "5-14"] <- 2
  d$newage[d$age == "15-19"] <- 2
  d$newage[d$age == "20-29"] <- 3
  d$newage[d$age == "30-64"] <- 3
  d$newage[d$age == "65+"] <- 4

  return(d)
}

########################################################################################################
########################################################################################################

#' CleanDataByFylke
#'
#' @param d a
#' @param FylkeData a
#' @param myfylke a
#' @import data.table
#' @export CleanDataByFylke
CleanDataByFylke <- function(d, FylkeData, myfylke) {
  # variables used in data.table functions in this function
  date <- NULL
  municip <- NULL
  # end
  if (!"IDate" %in% class(d$date)) {
    d[, date := data.table::as.IDate(date)]
  }

  d[, Fylke := as.numeric(substr(d$municip, 8, 9))]

  d <- merge(d, FylkeData, by = "Fylke")
  d <- d[Fylkename %in% myfylke]
  d[, year := format.Date(date, "%G")]
  d <- d[year %in% c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")]
  d[, month := format.Date(date, "%m")]
  d[, week := format.Date(date, "%V")]

  d[, newage := NA]
  d$newage[d$age == "0-4"] <- 1
  d$newage[d$age == "5-14"] <- 2
  d$newage[d$age == "15-19"] <- 2
  d$newage[d$age == "20-29"] <- 3
  d$newage[d$age == "30-64"] <- 3
  d$newage[d$age == "65+"] <- 4

  return(d)
}

########################################################################################################
########################################################################################################

#' roundUpNice
#'
#' @param x a
#' @param nice a
#' @export roundUpNice
roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if (length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

########################################################################################################
########################################################################################################
#' selectAgeGroups
#'
#' @param d a
#' @param ageG a
#' @param S a
#' @export selectAgeGroups
selectAgeGroups <- function(d, ageG, S) {
  d <- d[newage == ageG, ]
  d1 <- tapply(getdataout(d, S), d[, c("year", "week")], sum) ## get all gastro consultations
  # d2<- tapply(d$consult, d[, c("year","week")], sum) ## get total consultations
  # if (type=="gastro") { return(d1)}
  # else if (type=="all") { return(d2)}
  return(d1)
}
########################################################################################################
########################################################################################################
