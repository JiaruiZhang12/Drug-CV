library(shiny)
library(shinydashboard)

# ----------  DASHBOARD HEADER  ----------
header <- dashboardHeader(title = "Drug CV")

# ----------  SIDEBAR  ----------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Drug CV", tabName = "demodrugCV", icon = icon("tablets"))
  )
)

# ----------  BODY  ----------
body <- dashboardBody(
  tabItems(
    # ===== Drug CV tab =======================================================
    tabItem(
      tabName = "demodrugCV",
      h2("Drug CV"),
      fluidPage(
        # ----‑ Drug selector
        selectInput("demodrug", "Select drug", demodrugList, multiple = FALSE),
        
        # ----‑ Tabs within Drug CV
        tabsetPanel(
          type = "tabs",
          
          # --------------------------------------------------------------------
          # 1. Overview
          tabPanel(
            "Overview",
            fluidRow(
              box(width = 12, DT::dataTableOutput("demoselecteddrugranklist"))
            ),
            box(
              title  = "Clinical scores overview", width = 12, height = 500,
              p("Left: violin plot of the Drug Score distribution (selected drug highlighted); ",
                "Right: bubble plot of clinical sub‑scores (selected drug highlighted)."),
              fluidRow(
                column(width = 6, plotOutput("demoselecteddrugrankchart")),
                column(width = 6, plotOutput("demoselecteddrugbubblechart"))
              )
            )
          ),
          
          # --------------------------------------------------------------------
          # 2. Clinical
          tabPanel(
            "Clinical",
            h3("Clinical summary"),
            fluidRow(
              column(
                width = 4,
                box(
                  width = NULL, height = 600,
                  h4("Score summary"),
                  DT::dataTableOutput("demoselectedclinscoresummary")
                )
              ),
              column(
                width = 4,
                box(
                  width = NULL, height = 600,
                  title = "Study details",
                  plotlyOutput("demosb3")
                )
              ),
              column(
                width = 4,
                box(
                  width = NULL, height = 600,
                  title = "Number of patients",
                  plotlyOutput("demoptsb")
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                h4("Publications for selected drug"),
                DT::dataTableOutput("demodrugclinicalpublications"),
                downloadButton("demodownloadDrugPublications", "Download")
              )
            )
          ),
          
          # --------------------------------------------------------------------
          # 3. In vivo
          tabPanel(
            "In vivo",
            h3("In‑vivo summary"),
            fluidRow(
              # ---- Filter column
              column(
                width = 3,
                box(
                  width = NULL, title = "Disease models", height = 900,
                  pickerInput(
                    "demomodels", "Select model species",
                    c("mouse", "rat", "drosophila", "C.elegans","zebrafish", "xenopus","macaque","other"),
                    multiple  = TRUE,
                    selected  = c("mouse", "rat", "drosophila", "C.elegans",
                                  "zebrafish", "xenopus","macaque","other"),
                    options   = pickerOptions(actionsBox = TRUE)
                  ),
                  checkboxInput("EAE", "Include EAE studies", value = TRUE)
                )
              ),
              # ---- Forest plots
              column(
                width = 9,
                box(
                  width = NULL, height = 900, title = "Forest plot",
                  plotlyOutput("demoinvivosurvivalforest",    height = 200),
                  plotlyOutput("demoinvivobehavioralforest",  height = 200),
                  plotlyOutput("demoinvivobiochemicalforest", height = 200),
                  plotlyOutput("demoinvivohistologicalforest",height = 200)
                )
              )
            ),
            # ---- Publications table
            fluidRow(
              box(
                width = 12,
                h4("Publications for selected drug"),
                DT::dataTableOutput("demodruginvivopublications"),
                downloadButton("demodownloadinvivoDrugPublications", "Download")
              )
            )
          )
          
        ) # -- end tabsetPanel
      )   # -- end fluidPage
    )     # -- end tabItem
  )       # -- end tabItems
)

# ----------  BUILD PAGE  ----------
shinyUI(dashboardPage(skin = "blue", header, sidebar, body))
