##### setup #####
library(openSTARS)

# give paths to GRASS and where to store the GRASS data base
# Linux e.g.
grass_program_path <- "/users/annabergstrom/BSU_drive/Projects/Wind_Rivers/windrivers_GRASS"
# Windows e.g.
# grass_program_path <- "c:/Program Files/GRASS GIS 7.6"

working_dir <- file.path(tempdir(), "grass_workflow")
grass_db_path <- file.path(working_dir, "grassDB")
dir.create(working_dir)
setwd(tempdir())

# specify the path to the digital elevation model
dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
setup_grass_environment(dem = dem_path, 
                        gisBase = grass_program_path,
                        gisDbase = grass_db_path,
                        location = "nc_openSTARS",
                        remove_GISRC = TRUE,
                        override = TRUE
)