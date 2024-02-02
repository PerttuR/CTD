rm(list = ls())

source("./db.R")

year <- 2023

trip <- read.dbTable("suomu", "trip", paste0("year=", year, " AND project_fk=0"))

species_table <- read.dbTable("suomu", "species")

view1 <- read.dbTable("suomu", "report_survey_individual")
view_ctd_data <- read.dbTable("suomu", "report_ctd")
