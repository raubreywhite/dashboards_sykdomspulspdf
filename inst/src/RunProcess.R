# devtools::load_all("fhi")
fhi::DashboardInitialiseOpinionated(
  NAME = "sykdomspulspdf",
  PKG = "sykdomspulspdf",
  PACKAGE_DIR = "."
)
fd::initialize("sykdomspulspdf")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))


files <- list.files(fd::path("data_raw"), "^partially_formatted_")
mydate <- format(Sys.time(), "%d.%m.%y")

# fhi::DashboardIsDev()
fd::msg("/data_raw")
list.files("/data_raw")

fd::msg("/data_raw/sykdomspulspdf")
list.files("/data_raw/sykdomspulspdf")

if (length(files) == 0) {
  fd::msg("No data")
  quit(save = "no", status = 0)
} else {
  for (f in files) fd::msg(f)
  # grab the latest
  useFile <- max(files)

  if (file.exists(fd::path("results", "DONE.txt")) & !fd::config$is_dev) {
    fd::msg("results DONE.txt exists")
    quit(save = "no", status = 0)
  }

  if (fhi::file_stable(fd::path("data_raw", useFile)) == F) {
    fd::msg("file no stable")
    quit(save = "no", status = 0)
  }

  fd::msg("Start sykdomspulspdf", slack = T)

  d <- fread(fd::path("data_raw", useFile))
  fylke <- fread(system.file("extdata", "fylke.csv", package = "sykdomspulspdf"))
  lastestUpdate <- as.Date(gsub("_", "-", LatestRawID()))

  fd::msg("Generating monthly pdf", slack = T)


  allfylkeresults <- list()
  allfylkeresultsdata <- list()
  allfylke <- NULL
  mylistyrange <- list()

  for (SYNDROM in CONFIG$SYNDROMES) {
    fd::msg("Checking that typetemplate exists")
    file <- glue::glue("typetemplate_{SYNDROM}.xlsx")
    file_with_dir <- fd::path("data_raw", file)

    if(!fs::file_exists(file_with_dir)){

      fd::msg("Type template does not exist. Copying default.")
      fs::file_copy(
        system.file("extdata", file, package = "sykdomspulspdf"),
        file_with_dir
      )
    }

    fd::msg("Copying over templates and resources")
    sykdompulspdf_template_copy(fd::path("data_raw"), SYNDROM)
    fhi::sykdompulspdf_resources_copy(fd::path("data_raw"))

    if (SYNDROM == "mage") {
      add <- "magetarm"
      mytittle <- "Mage-tarminfeksjoner"
      title="Mage-tarminfeksjoner, Norge, alle aldersgrupper"
      filename <- "gastro"
      # Alle konsultasjoner in Norway:
      data <- CleanData(d)
      data[,yrwk:=fhi::isoyearweek(date)]

      alle <- tapply(data$gastro, data[, c("year", "week")], sum)
      data_long <- data[,.(
        value = sum(gastro)
      ), keyby=.(
        year,
        week,
        yrwk
      )]
      data_long[,season:=fhi::season(yrwk)]
      data_long[,x:=fhi::x(as.numeric(week))]
    } else if (SYNDROM == "luft") {
      add <- "luftvei"
      mytittle <- "Luftveisinfeksjoner"
      title="Luftveisinfeksjoner, Norge, alle aldersgrupper"
      filename <- "respiratory"

      data <- CleanData(d)
      data[,yrwk:=fhi::isoyearweek(date)]

      alle <- tapply(data$respiratory, data[, c("year", "week")], sum)
      data_long <- data[,.(
        value = sum(respiratory)
      ), keyby=.(
        year,
        week,
        yrwk
      )]
      data_long[,season:=fhi::season(yrwk)]
      data_long[,x:=fhi::x(as.numeric(week))]
    }

    weeknow <- findLastWeek(lastestUpdate, alle) ### need to be fixed

    if (weeknow==30) {
      weeknow <-29
    }


    yrange <- max(alle, na.rm = T) + (roundUpNice(max(alle, na.rm = T)) * .20)

    q <- ggplot_CreatePlotsNorway(
      data_long = data_long,
      weeknow = weeknow,
      Ukenummer = Ukenummer,
      title,
      yrange
    )

    ggsave(
      filename=fd::path("results", glue::glue("{filename} Norge alle alder {Sys.Date()}.svg")),
      plot = q,
      width = 16,
      height = 12,
      units = "in"
      )

    # Alle konsultasjoner in Norway by age:
    data_long <- data[age!="Ukjent",.(
      value = sum(gastro)
    ), keyby=.(
      year,
      week,
      yrwk,
      newage
    )]
    data_long[,season:=fhi::season(yrwk)]
    data_long[,x:=fhi::x(as.numeric(week))]

    q <- ggplot_CreatePlotsNorwayByAge(
      data_long = data_long,
      weeknow = weeknow,
      Ukenummer = Ukenummer,
      Fylkename = f,
      S = SYNDROM,
      mytittle = mytittle
    )

    ggsave(
      filename=fd::path("results", glue::glue("{filename} Norge Aldersfordelt {Sys.Date()}.svg")),
      plot = q,
      width = 16,
      height = 12,
      units = "in"
    )

    ###########################################


    fd::msg("Reading in typetemplate")
    typetemplate <- readxl::read_excel(file_with_dir)
    setDT(typetemplate)
    ## BY FYLKE
    for (f in fylke$Fylkename) {
      fd::msg(sprintf("PDF: %s", f))


      Fylkename <- f
      data <- CleanDataByFylke(d, fylke, f)
      alle <- tapply(getdataout(data, SYNDROM), data[, c("year", "week")], sum)

      allfylkeresults[[f]] <- alle
      allfylkeresultsdata[[f]] <- data
      allfylke <- c(allfylke, f)

      yrange <- max(alle, na.rm = T) + (roundUpNice(max(alle, na.rm = T)) * .20)
      mylistyrange[[f]] <- yrange

      # fhi::RenderExternally()

      nametemplate <- unique(typetemplate[fylke == f, nametemplate])
      childtemplate <- unique(typetemplate[fylke == f, childtemplate])

      rmarkdown::render(
        input = fhi::DashboardFolder("data_raw", paste(nametemplate, SYNDROM, ".Rmd", sep = "")),
        output_file = paste(gsub(" ", "", f, fixed = TRUE), "_", add, ".pdf", sep = ""),
        output_dir = fhi::DashboardFolder("results", paste("PDF", mydate, sep = "_"))
      )
    }
    rmarkdown::render(
      input = fhi::DashboardFolder("data_raw", paste("monthly_report_", SYNDROM, "ALL.Rmd", sep = "")),
      output_file = paste("ALL", "_", add, ".pdf", sep = ""),
      output_dir = fhi::DashboardFolder("results", paste("PDF", mydate, sep = "_"))
    )


    sykdompulspdf_template_remove(fhi::DashboardFolder("data_raw"), SYNDROM)
    # sykdompulspdf_template_remove_ALL(fhi::DashboardFolder("data_raw"), SYNDROM)
  }

  fhi::sykdompulspdf_resources_remove(fhi::DashboardFolder("data_raw"))
}

file.create(fd::path("results", "DONE.txt"))
if (!fd::config$is_dev) quit(save = "no", status = 0)
