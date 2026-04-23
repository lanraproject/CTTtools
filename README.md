# install sebagai paket
install.packages("remotes")
remotes::install_github("lanraproject/CTTtools") #pasang paket CTTtools
CTTtools::run_app() #jalankan paket

# jalankan langsung melalui GitHub
shiny::runGitHub("CTTtools", "lanraproject") #lebih ringkas
