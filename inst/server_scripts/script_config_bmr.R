# create the required directories
dir.create("./data-raw")
dir.create("./data-created")
dir.create("./bmrs-output")

# install packrat
install.packages("packrat")

# initialize packrat
packrat::init()

# install agrometeoR
devtools::install_github("pokyah/agrometeoR", ref = "dev_irm")
