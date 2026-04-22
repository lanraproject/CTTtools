run_app <- function() {
  appDir <- system.file("app", package = "CTTtools")
  
  if (appDir == "") {
    stop("Aplikasi tidak ditemukan")
  }
  
  shiny::runApp(appDir)
}
