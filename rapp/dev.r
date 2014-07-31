install.packages("testthat")
install.packages("roxygen2")
packrat::install_local("rapp.split_1.0.tar.gz", repos="q:/home/wsp/rapp2/rapp.split/rapp")
install.packages("q:/home/wsp/rapp2/rapp.split/rapp/rapp.split_1.0.tar.gz", type = "source", repos = NULL)

packrat::snapshot()
packrat::off()
packrat::on()
