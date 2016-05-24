## 27/04/2016 : Shiny som sur iris - camemberts js

library(kohonen)

############################
## Fonction qui génère les paramètres à passer à JS
getPlotParams <- function(type, som, superclass, data, plotsize, varnames) {
  
  ## Paramètres communs à tous les graphiques
  somsize <- nrow(som$grid$pts)
  clustering <- factor(som$unit.classif, 1:nrow(som$grid$pts))
  clust.table <- table(clustering)
  
  gridInfo <- list(nbLines= som$grid$xdim,
                   nbColumns= som$grid$ydim,
                   topology= ifelse(som$grid$topo == "rectangular", 
                                    'rectangular', "hexagonal"))
  superclassColor <- substr(terrain.colors(length(unique(superclass))), 1, 7)
  
  res <- list(saveToPng= TRUE, 
              sizeInfo= plotsize, 
              gridInfo= gridInfo, 
              superclass= superclass, 
              superclassColor= superclassColor)
  
  ## Traitement data si besoin :
  if (type == "Camembert") {
    data <- as.factor(data)
    unique.values <- levels(data)
    nvalues <- nlevels(data)
  } else if (type %in% c("Radar", "Line", "Barplot", "Boxplot", "Color", "Star")) {
    if (is.null(dim(data))) {
      data <- data.frame(data)
      colnames(data) <- varnames
    }
    if (type == "Color") 
      data <- as.data.frame(sapply(data, as.numeric))
    
    nvar <- length(varnames)
    normDat <- as.data.frame(sapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
    if (type %in% c("Radar", "Line", "Barplot", "Color", "Star")) {
      normValues <- unname(lapply(split(normDat, clustering), 
                                  function(x) {
                                    if (!nrow(x)) return(rep(0, nvar))
                                    unname(colMeans(x))
                                  }))
      realValues <- unname(lapply(split(data, clustering), 
                                  function(x) {
                                    if (!nrow(x)) return(rep(0, nvar))
                                    unname(round(colMeans(x), 3))
                                  }))
    }
  }
  
  ## Paramètres spécifiques :
  if (type == "Camembert") {
    res$parts <- nvalues
    res$label <- unique.values
    res$labelColor <- substr(rainbow(nvalues), 1, 7)
    res$pieNormalizedSize <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$pieRealSize <- unname(clust.table)
    res$pieNormalizedValues <- unname(lapply(split(data, clustering), 
                                             function(x) {
                                               if (!length(x)) return(rep(1/nvalues, nvalues))
                                               unname(table(x) / length(x))
                                             }))
    res$pieRealValues <- unname(lapply(split(data, clustering), 
                                       function(x) unname(table(x))))
  } else if (type == "Radar") {
    res$parts <- nvar
    res$label <- varnames
    res$labelColor <- substr(rainbow(nvar), 1, 7)
    res$radarNormalizedSize <- unname(.9 * (clust.table > 0))
    res$radarRealSize <- unname(clust.table)
    res$radarNormalizedValues <- normValues
    res$radarRealValues <- realValues
  } else if (type == "Hitmap") {
    res$hitmapNormalizedValues <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$hitmapRealValues <- unname(clust.table)
  } else if (type == "Line") {
    res$nbPoints <- nvar
    res$lineNormalizedValues <- normValues
    res$lineRealValues <- realValues    
  } else if (type == "Barplot") {
    res$nbBatons <- nvar
    res$isHist <- FALSE
    res$label <- varnames
    res$labelColor <- substr(rainbow(nvar), 1, 7)
    res$batonNormalizedValues <- normValues
    res$batonRealValues <- realValues
  } else if (type == "Boxplot") {
    res$nbBox <- nvar
    res$label <- varnames
    res$labelColor <- substr(rainbow(nvar), 1, 7)
    
    boxes.norm <- lapply(split(normDat, clustering), boxplot, plot= F)
    boxes.real <- lapply(split(data, clustering), boxplot, plot= F)
    res$boxPlotNormalizedValues <- unname(lapply(boxes.norm, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$boxPlotRealValues <- unname(lapply(boxes.real, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$boxNormalizedExtremesValues <- unname(lapply(boxes.norm, function(x) unname(split(x$out, factor(x$group, levels= 1:nvar)))))
    res$boxRealExtremesValues <- unname(lapply(boxes.real, function(x) unname(split(x$out, factor(x$group, levels= 1:nvar)))))
  } else if (type == "Color") {
    res$activate <- TRUE
    res$colorNormalizedValues <- normValues
    res$colorRealValues <- realValues    
  } else if (type == "Star") {
    res$nbSommet <- nvar
    res$label <- varnames
    res$starPlotNormalizedValues <- normValues
    res$starPlotRealValues <- realValues
  } else if (type == "Names") {
    res$wordClouds <- unname(split(data, clustering))
    res$nbWord <- unname(sapply(res$wordClouds, length))
  }
  res
}


#########################
## Fonction principale serveur
shinyServer(function(input, output, session) {
  
  #############################################################################
  ## Panel "Import Data"
  #############################################################################

  # Current train data
  current.data <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    the.sep <- switch(input$sep, "Comma ','"=",", "Semicolon ';'"=";", 
                      "Tab"="\t", "Space"=" ")
    the.quote <- switch(input$quote, "None"="","Double Quote \""='"',
                        "Single Quote '"="'")
    the.dec <- switch(input$dec, "Period '.'"=".", "Comma ','"=",")
    if (input$rownames) {
      the.table <- read.table(input$file1$datapath, header=input$header, 
                              sep=the.sep, quote=the.quote, row.names=1,
                              dec=the.dec)
    } else {
      the.table <- read.table(input$file1$datapath, header=input$header, 
                              sep=the.sep, quote=the.quote, dec=the.dec)
    }
    
    the.table
  })
  
  # data preview table
  output$view <- renderTable({
    d.input <- current.data()
    if (is.null(d.input)) 
      return(NULL)

    if (!is.null(input$rownames.col))
      if (input$rownames.col != "(None)")
        if (!any(duplicated(d.input[, input$rownames.col])))
          try(row.names(d.input) <- as.character(d.input[, input$rownames.col]))

    if (ncol(d.input) > input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    
    head(d.input, n= input$nrow.preview)
  })

  # Update choices for rownames column
  output$rownames.col <- renderUI({
    if (is.null(current.data())) return()
    selectInput(inputId= "rownames.col", label= "Rownames var:", 
                choices= c("(None)", colnames(current.data())),
                selected= "(None)")
  })
  
  ## Current rownames
  current.rownames <- reactive({
    if (is.null(current.data()))
      return(NULL)
    if (input$rownames.col != "(None)")
      if (!any(duplicated(current.data()[, input$rownames.col]))) 
        return(as.character(current.data()[, input$rownames.col]))
    return(rownames(current.data()))
  })

  #############################################################################
  ## Panel "Train"
  #############################################################################
  
  ## Message de statut / infos sur la carte
  output$Message <- renderPrint({
    if (is.null(current.som())) 
      return(cat("No map trained yet, click Train button."))
    
    summary(current.som())
    table(factor(current.som()$unit.classif, 
                 levels= 1:nrow(current.som()$grid$pts)))
  })
  
  # Update train variable choice on data change
  output$varchoice <- renderUI({
    if (is.null(current.data())) return()
    checkboxGroupInput(inputId="varchoice", label="Training variables:",
                       choices=as.list(colnames(current.data())),
                       selected=as.list(colnames(current.data())[
                         sapply(current.data(), class) %in%
                           c("integer", "numeric")]))
  })
  # Update train variable choice on button click
  observe({
    input$varNum
    updateCheckboxGroupInput(session, "varchoice", label= NULL, choices= NULL, 
                             selected= isolate(as.list(colnames(current.data())[
                               sapply(current.data(), class) %in% c("integer", "numeric")])))
  })
  observe({
    input$varAll
    updateCheckboxGroupInput(session, "varchoice", label= NULL, choices= NULL, 
                             selected= isolate(as.list(colnames(current.data()))))
  })
  observe({
    input$varNone
    updateCheckboxGroupInput(session, "varchoice", label= NULL, choices= NULL, 
                             selected= NA)
  })
  
  # Train the SOM when the button is hit
  current.som <- reactive({   
    if (input$trainbutton > 0) {
      isolate({
        dat <- current.data()[, input$varchoice]
        rownames(dat) <- current.rownames()
        dat <- na.omit(dat)
        som(scale(dat), grid= somgrid(input$kohDimx, input$kohDimy, input$kohTopo))
      })
    } else NULL
  })
  
  # Compute superclasses when current.som or superclass changes
  current.hclust <- reactive({
    if(!is.null(current.som()))
      hclust(dist(current.som()$codes), "ward.D2")
  })
  current.sc <- reactive({
    if(!is.null(current.hclust()))
      cutree(current.hclust(), input$kohSuperclass)
  })
  
  
  
  #############################################################################
  ## Panel "Graph"
  #############################################################################
  
  ## Sélection de variables (en fonction du graphique)
  output$plotVarOne <- renderUI({
    if (is.null(current.data())) return()
    selectInput("plotVarOne", "Plot variable:", choices= colnames(current.data()))
  })
  output$plotVarMult <- renderUI({
    data <- current.data()
    if (is.null(data)) return()
    tmp.numeric <- sapply(data, is.numeric)
    selectInput("plotVarMult", "Plot variable:", multiple= T,
                choices= colnames(data)[tmp.numeric], 
                selected= colnames(data)[tmp.numeric][1:min(5, sum(tmp.numeric))])
  })
    
  ## Passer les données aux graphiques
  output$screeplot <- renderPlot({
    if (is.null(current.som())) return()
    plot(current.hclust())
    rect.hclust(current.hclust(), k= input$kohSuperclass)
  })
  
  output$thePie <- reactive({
    if (is.null(current.som()) | input$graphType != "Camembert" | is.null(input$plotVarOne)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    getPlotParams("Camembert", current.som(), current.sc(), 
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarOne], input$plotSize, input$plotVarOne)
  })
  
  output$theRadar <- reactive({
    if (is.null(current.som()) | input$graphType != "Radar" | is.null(input$plotVarMult)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Radar", current.som(), current.sc(), 
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarMult],
                  input$plotSize, input$plotVarMult)
  })

  output$theLigne <- reactive({
    if (is.null(current.som()) | input$graphType != "Line" | is.null(input$plotVarMult)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    getPlotParams("Line", current.som(), current.sc(), 
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarMult],
                  input$plotSize, input$plotVarMult)
  })

  output$theHitmap <- reactive({
    if (is.null(current.som()) | input$graphType != "Hitmap") 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Hitmap", current.som(), current.sc(), NULL, input$plotSize, NULL)
  })

  output$theBaton <- reactive({
    if (is.null(current.som()) | input$graphType != "Barplot" | is.null(input$plotVarMult)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Barplot", current.som(), current.sc(), 
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarMult], 
                  input$plotSize, input$plotVarMult)
  })
  
  output$theBoxplot <- reactive({
    if (is.null(current.som()) | input$graphType != "Boxplot" | is.null(input$plotVarMult)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Boxplot", current.som(), current.sc(),
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarMult], 
                  input$plotSize, input$plotVarMult)
  })
  
  output$theColor <- reactive({
    if (is.null(current.som()) | input$graphType != "Color" | is.null(input$plotVarOne)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Color", current.som(), current.sc(),
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarOne], 
                  input$plotSize, input$plotVarOne)
  })
  
  output$theStar <- reactive({
    if (is.null(current.som()) | input$graphType != "Star" | is.null(input$plotVarMult)) 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Star", current.som(), current.sc(),
                  current.data()[rowSums(is.na(current.data()[, input$varchoice])) == 0, 
                                 input$plotVarMult], 
                  input$plotSize, input$plotVarMult)
  })
  
  output$theWordcloud <- reactive({
    if (is.null(current.som()) | input$graphType != "Names") 
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    getPlotParams("Names", current.som(), current.sc(), 
                  current.rownames()[rowSums(is.na(current.data()[, input$varchoice])) == 0], 
                  input$plotSize, NULL)
  })
  
})
