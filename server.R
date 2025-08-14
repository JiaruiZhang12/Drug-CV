library(shiny)

shinyServer(function(input, output, session) {
  
  # ---- Drug scores prepared once for several outputs ----
  drugScores <- data.frame(
    x = demodrugSummary$Drug,
    y = as.numeric(demodrugSummary$distanceScore)
  )
  
  # ---- Helpers that react to the selected drug ----
  selectedDrugScore <- reactive({
    drugScores |> filter(x %in% input$demodrug) |> pull(y)
  })
  
  drugSafety <- reactive({
    demodrugSummary |> filter(Drug %in% input$demodrug) |> pull(safetyScore)
  })
  drugEfficacy <- reactive({
    demodrugSummary |> filter(Drug %in% input$demodrug) |> pull(efficacyScore)
  })
  
  # ===================== 1. OVERVIEW TAB =====================
  
  ## 1‑a | Overview table ------------------------------------
  demoselecteddrugranklist <- reactive({
    mydf <- demodrugSummary |>
      filter(Drug %in% input$demodrug) |>
      select(
        Drug, nPublication, distanceScore, 
        efficacyScore, safetyScore, nParticipants,
        qualityScore
      )
    
    cols <- c("nPublication","distanceScore","efficacyScore","safetyScore",
              "nParticipants","qualityScore")
    mydf[cols] <- lapply(mydf[cols], function(v) round(v, 2))
    mydf
  })
  
  output$demoselecteddrugranklist <- DT::renderDataTable(DT::datatable(
    demoselecteddrugranklist(),
    colnames = c(
      "Drug",
      "Clinical n(Pub)",
      "Clinical Drug Score",
      "Median efficacy (-2 – 4)", 
      "Median safety score (0 – 3)",
      "Median no. participants", 
      "Median quality score (out of 24)"),
    options = list(dom = 't'),
    rownames = FALSE
  ))
  
  ## 1‑b | Violin + dot plot of drug score -------------------
  output$demoselecteddrugrankchart <- renderPlot({
    ggplot(drugScores, aes(y = y, x = "")) +
      labs(y = "Drug Score", x = "") +
      geom_violin(trim = FALSE, fill = "cadetblue2") +
      geom_dotplot(dotsize = 0.5, binaxis = 'y', stackdir = 'center',
                   binwidth = 0.25) +
      annotate("point", x = "", y = selectedDrugScore(),
               shape = 13, colour = "red3", size = 5, stroke = 2)
  })
  
  ## 1‑c | Bubble plot of safety × efficacy ------------------
  output$demoselecteddrugbubblechart <- renderPlot({
    ggplot(
      demodrugSummary,
      aes(x = safetyScore, y = efficacyScore,
          size = nParticipants, color = qualityScore)
    ) +
      scale_color_gradient(low = "yellow", high = "red", limits = c(0, 24)) +
      geom_point(alpha = 1) +
      theme(legend.position = "right") +
      labs(
        x = "Safety Score", y = "Efficacy Score",
        color = "Quality Score", size = "nParticipants"
      ) +
      xlim(0, 3) + ylim(-2, 4) +
      annotate("point", x = drugSafety(), y = drugEfficacy(),
               shape = 13, size = 5, stroke = 1, color = "black")
  })
  # ===================== 2. CLINICAL TAB ====================
  
  ## 2‑a | Score‑summary table -------------------------------
  demoselectedclinscoresummary <- reactive({
    demodrugSummary |>
      filter(Drug %in% input$demodrug) |>
      select(distanceScore, efficacyScore, safetyScore,
             nParticipants, qualityScore) |>
      t()
  })
  
  output$demoselectedclinscoresummary <- DT::renderDataTable(
    DT::datatable(
      demoselectedclinscoresummary(),
      rownames = c("Drug Score", "Efficacy Score", "Safety Score",
                   "Study‑size Score", "Quality Score"),
      colnames = "Score",
      options   = list(dom = 't')
    ) |>
      formatStyle(
        0, target = "row",
        fontWeight      = styleEqual("Drug Score", "bold"),
        backgroundColor = styleEqual("Drug Score", "lightblue")
      )
  )
  
  ## 2‑b | Sunburst (Disease → Study‑type → Phase) -----------
  demosdsunburstdata <- reactive({
    dat <- demopublicationList |>
      filter(Drug %in% input$demodrug) |>
      select(Disease, studyType, phase, StudyIdStr) |>
      distinct() |>
      group_by(Disease, studyType, phase, StudyIdStr) |>
      tally(name = "value") |>
      ungroup()
    
    df0 <- dat |> group_by(Disease) |> summarise(value = sum(value))
    df1 <- dat |> group_by(Disease, studyType) |> summarise(value = sum(value))
    df2 <- dat |> group_by(Disease, studyType, phase) |> summarise(value = sum(value))
    
    rbind(
      data.frame(ids = df0$Disease, labels = df0$Disease,
                 parents = "", values = df0$value),
      data.frame(ids = paste(df1$Disease, df1$studyType, sep = "-"),
                 labels  = df1$studyType,
                 parents = df1$Disease, values = df1$value),
      data.frame(ids = paste(df2$Disease, df2$studyType, df2$phase, sep = "-"),
                 labels  = df2$phase,
                 parents = paste(df2$Disease, df2$studyType, sep = "-"),
                 values  = df2$value)
    )
  })
  
  output$demosb3 <- renderPlotly({
    plot_ly(
      demosdsunburstdata(),
      ids = ~ids, labels = ~labels, parents = ~parents,
      values = ~values, type = 'sunburst', branchvalues = "total",
      insidetextorientation = 'auto',
      marker = list(line = list(color = "white", width = 2))
    ) |>
      layout(height = 300, width = 300,
             margin = list(l = 0, r = 0, b = 0, t = 30))
  })
  
  ## 2‑c | Sunburst (patients) -------------------------------
  d_sdptsunburstdata <- reactive({
    dat <- demopublicationList |>
      filter(Drug %in% input$demodrug) |>
      select(Disease, studyType, phase, nPatients)
    
    df0 <- dat |> group_by(Disease) |> summarise(nPatients = sum(nPatients))
    df1 <- dat |> group_by(Disease, studyType) |> summarise(nPatients = sum(nPatients))
    df2 <- dat |> group_by(Disease, studyType, phase) |> summarise(nPatients = sum(nPatients))
    
    rbind(
      data.frame(ids = df0$Disease, labels = df0$Disease,
                 parents = "", values = df0$nPatients),
      data.frame(ids = paste(df1$Disease, df1$studyType, sep = "-"),
                 labels  = df1$studyType,
                 parents = df1$Disease, values = df1$nPatients),
      data.frame(ids = paste(df2$Disease, df2$studyType, df2$phase, sep = "-"),
                 labels  = df2$phase,
                 parents = paste(df2$Disease, df2$studyType, sep = "-"),
                 values  = df2$nPatients)
    )
  })
  
  output$demoptsb <- renderPlotly({
    plot_ly(
      d_sdptsunburstdata(),
      ids = ~ids, labels = ~labels, parents = ~parents,
      values = ~values, type = 'sunburst', branchvalues = "total",
      insidetextorientation = 'auto',
      marker = list(line = list(color = "white", width = 2))
    ) |>
      layout(height = 300, width = 300,
             margin = list(l = 0, r = 0, b = 0, t = 30))
  })
  
  ## 2‑d | Publications table (clinical) + download ----------
  demoselecteddrugclinicalpubtable <- reactive({
    demopublicationList |>
      filter(Drug %in% input$demodrug) |>
      select(Title, Disease, Drug, Year, Author, Journal, DOI,
             studyType, phase, nPatients)
  })
  
  output$demodrugclinicalpublications <- DT::renderDataTable(DT::datatable(
    demoselecteddrugclinicalpubtable(),
    colnames = c("Title", "Disease", "Drug", "Year", "Author", "Journal", "DOI",
                 "Type of study", "Study Phase", "n(patients)"),
    filter = "top"
  ))
  
  demoselecteddrugclinicalpublications <- reactive({
    demopublicationList |>
      filter(Drug %in% input$demodrug) |>
      select(Title, Disease, Drug, Year, Author, Journal, DOI, studyType, phase,
             nPatients, efficacyScore, safetyScore, nParticipants, qualityScore)
  })
  
  output$demodownloadDrugPublications <- downloadHandler(
    filename = function() paste0("clinicalpublications_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(demoselecteddrugclinicalpublications(), file, row.names = FALSE)
    }
  )
  
  # ===================== 3. IN‑VIVO TAB =====================
  getforestData<-reactive({
    df<-okk[outcome_type==input$outcomeType & Drug %in% str_to_title(input$demodrug) & Model %in% input$Model]
    if (!input$EAE) df <- df[eae_model != "NULL"]
    df[,lab:=paste(outcome_label,str_trunc(Title[1],20), Year, sep="~")]
    #df<-df[,.SD[order(Average)][1],.(lab)]## only keep one
    df<-df[Error!=0]
  })
  
  
  output$demoinvivoForest<-renderPlot({
    getforestData()->dodt
    #save(dodt,file='temp.RData')
    if(nrow(dodt)){
      tu<-viz_forest(dodt[,.(Average,Error)],
                     study_labels = dodt[,lab],
                     text_size = 5, xlab = "Behavioural outcomes")
      
      plot(tu)
    } else {
      ggplot() +
        geom_text(aes(x = 0, y = 0, label = "No data kept after filter"), 
                  size = 6, color = "red") +
        theme_void() +  
        theme(plot.margin = margin(1, 1, 1, 1, "cm")) 
    }
  })
  
  demoselecteddruginvivotable <- reactive({
    df <- okk |>
      filter(Drug %in% str_to_title(input$demodrug)) |>
      select(
        Title, Drug, Year, Author, Journal, DOI, Model, eae_model,
        outcome_type
      ) |> as.data.frame()
  })
  
  output$demodruginvivopublications <- renderDT({
    df<-demoselecteddruginvivotable()
    datatable(
      df,
      colnames = c("Title", "Drug", "Year", "Author", "Journal", "DOI",
                   "Model Species", "EAE model", "Type of Outcome"),
      filter = "top"
    )
  })
  
  demoselecteddruginvivopublications <- reactive({
    df <- okk2 |>
      filter(Drug %in% str_to_title(input$demodrug)) |>
      select(
        Title, Drug, Year, Author, Journal, DOI, Model, eae_model,
        mortalityOutcome, behavioralOutcome, biochemicalOutcome, histologicalOutcome
      ) |> as.data.frame()
    df
  })
  
  output$demodownloadinvivoDrugPublications <- downloadHandler(
    filename = function() paste0("invivopublications_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(demoselecteddruginvivopublications(), file, row.names = FALSE)})
  
})





