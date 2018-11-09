# devtools::load_all("fhi")
fhi::DashboardInitialiseOpinionated(
  NAME="sykdomspulspdf",
  PKG="sykdomspulspdf",
  PACKAGE_DIR=".")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))


files <- list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_")
mydate <- format(Sys.time(), "%d.%m.%y")

# fhi::DashboardIsDev()
fhi::DashboardMsg("/data_raw")
list.files("/data_raw")

fhi::DashboardMsg("/data_raw/sykdomspulspdf")
list.files("/data_raw/sykdomspulspdf")

if (length(files) == 0) {
  fhi::DashboardMsg("No data")
  quit(save = "no", status = 0)
} else {
  for (f in files) fhi::DashboardMsg(f)
  # grab the latest
  useFile <- max(files)

  if (file.exists(fhi::DashboardFolder("results", "DONE.txt")) & !fhi::DashboardIsDev()) {
    fhi::DashboardMsg("results DONE.txt exists")
    quit(save = "no", status = 0)
  }

<<<<<<< HEAD
  if(RAWmisc::IsFileStable(fhi::DashboardFolder("data_raw",useFile))==F) {
    fhi::DashboardMsg("file no stable")
    quit(save="no", status=0)
  }



  d <- fread(fhi::DashboardFolder("data_raw",useFile))
  fylke <-fread(system.file("extdata", "fylke.csv", package = "sykdomspulspdf"))
  lastestUpdate <- as.Date(gsub("_","-",LatestRawID()))
=======
  d <- fread(fhi::DashboardFolder("data_raw", useFile))
  fylke <- fread(system.file("extdata", "fylke.csv", package = "sykdomspulspdf"))
  lastestUpdate <- as.Date(gsub("_", "-", LatestRawID()))
>>>>>>> upstream/master

  fhi::DashboardMsg("Generating monthly pdf")

  # Alle konsultasjoner:
  data <- CleanData(d)
  alle <- tapply(data$gastro, data[, c("year", "week")], sum)
  weeknow <- findLastWeek(lastestUpdate, alle) ### need to be fixed
  cat(paste("Last week", weeknow, sep = " "))

  ## BY FYLKE
  for (SYNDROM in CONFIG$SYNDROMES) {
    sykdompulspdf_template_copy(fhi::DashboardFolder("data_raw"), SYNDROM)
    fhi::sykdompulspdf_resources_copy(fhi::DashboardFolder("data_raw"))

    if (SYNDROM == "mage") {
      add <- "magetarm"
      mytittle <- "Mage-tarminfeksjoner"
    } else if (SYNDROM == "luft") {
      add <- "luftvei"
      mytittle <- "Luftveisinfeksjoner"
    }

    ###########################################
    for (f in fylke$Fylkename) {
      fhi::DashboardMsg(sprintf("PDF: %s", f))

      Fylkename <- f
      data <- CleanDataByFylke(d, fylke, f)
      alle <- tapply(getdataout(data, SYNDROM), data[, c("year", "week")], sum)
      yrange <- max(alle, na.rm = T) + (roundUpNice(max(alle, na.rm = T)) * .20)


      # fhi::RenderExternally()
      rmarkdown::render(
        input = fhi::DashboardFolder("data_raw", paste("monthly_report_", SYNDROM, ".Rmd", sep = "")),
        output_file = paste(gsub(" ", "", f, fixed = TRUE), "_", add, ".pdf", sep = ""),
        output_dir = fhi::DashboardFolder("results", paste("PDF", mydate, sep = "_"))
      )
    }

    sykdompulspdf_template_remove(fhi::DashboardFolder("data_raw"), SYNDROM)
  }

  fhi::sykdompulspdf_resources_remove(fhi::DashboardFolder("data_raw"))
}

file.create(fhi::DashboardFolder("results", "DONE.txt"))
if (!fhi::DashboardIsDev()) quit(save = "no", status = 0)
