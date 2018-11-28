con <- file("/tmp/computer", "r")
COMPUTER_NAME <- readLines(con, n = 1)
close(con)
Sys.setenv(COMPUTER = COMPUTER_NAME)

for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"signalsystemmsis"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"signalsystemmsis",f))
  }
}

unlink(file.path("/junit","signalsystemmsis.xml"))

Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","signalsystemmsis.xml"), "w+")
a$start_context("sykdomspuls_log")

#if(FALSE){
  output <- processx::run("Rscript","/r/signalsystemmsis/src/RunProcess.R", error_on_status=F, echo=T)
  cat("\n\nstdout\n\n")
  cat(output$stdout)
  cat("\n\nstderr\n\n")
  cat(output$stderr)

  if(output$status==0){
    a$add_result("signalsystemmsis","RunAll",testthat::expectation("success","Pass"))
  } else {
    a$add_result("signalsystemmsis","RunAll",testthat::expectation("error","Fail"))
  }
#} else {
  a$add_result("signalsystemmsis","RunAll",testthat::expectation("success","Pass"))
#}

a$end_context("signalsystemmsis")
a$end_reporter()
close(a$out)
