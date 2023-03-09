source("renv/activate.R")


options(
  repos = c(
    RSPM = "https://packagemanager.rstudio.com/all/latest",
    CRAN = "https://cran.rstudio.com/"
  ),
  renv.config.auto.snapshot = TRUE
)

# Since RSPM does not provide Mac binaries, always install packages from CRAN
# on mac or windows, even if renv.lock specifies they came from RSPM
if (Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) {
  options(renv.config.repos.override = c(
    CRAN = "https://cran.rstudio.com/",
    INLA = "https://inla.r-inla-download.org/R/testing"
  ))
} else if (Sys.info()[["sysname"]] == "Linux") {
  options(renv.config.repos.override = c(
    RSPM = "https://packagemanager.rstudio.com/all/latest",
    INLA = "https://inla.r-inla-download.org/R/testing"
  ))
}


# make renv use scratch directory
slurm_ind <- any(grepl("^SLURM_", names(Sys.getenv())))
dir_exists_ind <- dir.exists(file.path("/scratch/", Sys.getenv("USER")))
if (slurm_ind && dir_exists_ind) {
  source("./scripts/R/hpc_renv_setup.R")
}

# add commonly-used dev functions and attached libraries
source("./scripts/R/dev.R")
