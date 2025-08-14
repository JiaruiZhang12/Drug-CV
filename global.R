# ---- Global dependencies ----
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(googlesheets4)
library(metafor)
library(metaviz)
library(readr)

# ---- Load data ----
# googlesheets4::gs4_deauth()
# googleSheetId <- "1-TRbnLo4ypCdlkhsWCRh-wVBXwtwbpPm0EhjfaesNhQ"
# 
# demodrugSummaryName     <- "demodrugSummary"
# demopublicationListName    <- "demopublicationList"
# demoinvivoPublicationListName <- "demoinvivoPublicationList"

# demodrugSummary <- read_sheet(googleSheetId, demodrugSummaryName)
# write.csv(demodrugSummary, 'data/demodrugSummary.csv', row.names = FALSE)

demodrugSummary        <- read_csv("data/ClinicalMSDrugSummary.csv", show_col_types = FALSE)
demopublicationList    <- read_csv("data/ClinicalPublicationList.csv", show_col_types = FALSE)
demoinvivoPublicationList <- read_csv("data/Preclinical.csv", show_col_types = FALSE)


demodrugList <- sort(unique(demodrugSummary$Drug))

# # ---- Build dropdown list of drugs ----
# demodrugList   <- sort(intersect(demodrugSummary$Drug, demopublicationList$Drug))
# demodrugNumber <- gsub("drug", "", demodrugList) |> as.numeric() |> sort()
# demodrugList   <- paste0("drug", demodrugNumber)


