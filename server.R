## 27/04/2016 : Shiny som sur iris - camemberts js

library(kohonen)
library(RColorBrewer)
library(viridis)
options(shiny.maxRequestSize=100*1024^2) # Max filesize

getPalette <- function(pal, n) {
  if(pal == "rainbow") return(substr(rainbow(n), 1, 7))
  if(pal == "heat") return(substr(heat.colors(n), 1, 7))
  if(pal == "terrain") return(substr(terrain.colors(n), 1, 7))
  if(pal == "topo") return(substr(topo.colors(n), 1, 7))
  if(pal == "cm") return(substr(cm.colors(n), 1, 7))
  if (pal == "viridis") {
    if (n == 1) return(substr(viridis(3), 1, 7)[1])
    if (n == 2) return(substr(viridis(3), 1, 7)[c(1,3)])
    return(substr(viridis(n), 1, 7))
  } else {
    if (n == 1) return(brewer.pal(3, pal)[1])
    if (n == 2) return(brewer.pal(3, pal)[c(1,3)])
    return(brewer.pal(n, pal))
  }
}

############################
## Fonction qui génère les paramètres à passer à JS
getPlotParams <- function(type, som, superclass, data, plotsize, varnames, 
                          normtype= c("range", "contrast"), palsc, palplot) {
  
  ## Paramètres communs à tous les graphiques
  somsize <- nrow(som$grid$pts)
  clustering <- factor(som$unit.classif, 1:nrow(som$grid$pts))
  clust.table <- table(clustering)
  
  gridInfo <- list(nbLines= som$grid$xdim,
                   nbColumns= som$grid$ydim,
                   topology= ifelse(som$grid$topo == "rectangular", 
                                    'rectangular', "hexagonal"))
  #   superclassColor <- substr(terrain.colors(length(unique(superclass))), 1, 7)
  n.sc <- length(unique(superclass))
  superclassColor <- getPalette(palsc, n.sc)
  
  res <- list(plotType= type, 
              saveToPng= TRUE, 
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
    } else data <- as.data.frame(data)
    if (type == "Color") 
      data <- as.data.frame(sapply(data, as.numeric))
    
    nvar <- length(varnames)
    
    if (type %in% c("Radar", "Line", "Barplot", "Color", "Star")) {
      ## Means by cell
      if (normtype == "range") {
        ## "Range" normalization : data range to [0,1], then means
        normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
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
      } else if (normtype == "contrast") {
        ## "Contrast" normalization : means on data, then range(means) -> [0,1]
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) {
                                              if (!nrow(x)) return(rep(0, nvar))
                                              unname(round(colMeans(x), 3))
                                            }))
        normValues <- apply(realValues, 2, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x)))
        realValues <- unname(as.list(as.data.frame(t(realValues))))
        normValues <- unname(as.list(as.data.frame(t(normValues))))
      }
      if (type == "Color") {
        ## 8 colors (equal-sized bins of values) of selected palette
        normValues <- do.call(rbind, normValues)
        normValues <- apply(normValues, 2, function(x) 
          getPalette(palplot, 8)[cut(x, seq(.049, .951, length.out= 9))])
      }
    } else if (type == "Boxplot") {
      normDat <- as.data.frame(sapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
      data <- as.data.frame(apply(data, 2, as.numeric)) # To prevent weird JS error (when a type is in integer)
    }
  }
  
  ## Paramètres spécifiques :
  if (type == "Camembert") {
    res$parts <- nvalues
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues)
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
    res$labelColor <- getPalette(palplot, nvar)
    res$radarNormalizedSize <- unname(clust.table > 0)
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
    res$labelColor <- getPalette(palplot, nvar)
    res$batonNormalizedValues <- normValues
    res$batonRealValues <- realValues
  } else if (type == "Boxplot") {
    res$nbBox <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar)
    
    boxes.norm <- lapply(split(normDat, clustering), boxplot, plot= F)
    boxes.real <- lapply(split(data, clustering), boxplot, plot= F)
    res$boxPlotNormalizedValues <- unname(lapply(boxes.norm, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$boxPlotRealValues <- unname(lapply(boxes.real, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$boxNormalizedExtremesValues <- unname(lapply(boxes.norm, function(x) unname(split(x$out, factor(x$group, levels= 1:nvar)))))
    res$boxRealExtremesValues <- unname(lapply(boxes.real, function(x) as.list(unname(split(x$out, factor(x$group, levels= 1:nvar))))))
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
  ok.data <- reactive({
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
    d.input <- ok.data()
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
    if (is.null(ok.data())) return()
    selectInput(inputId= "rownames.col", label= "Rownames var:", 
                choices= c("(None)", colnames(ok.data())),
                selected= "(None)")
  })
  
  ## Current rownames
  ok.rownames <- reactive({
    if (is.null(ok.data()))
      return(NULL)
    if (input$rownames.col != "(None)")
      if (!any(duplicated(ok.data()[, input$rownames.col]))) 
        return(as.character(ok.data()[, input$rownames.col]))
    return(rownames(ok.data()))
  })

  #############################################################################
  ## Panel "Train"
  #############################################################################
  
  # Update train variable choice on data change
  output$varchoice <- renderUI({
    if (is.null(ok.data())) return()
    checkboxGroupInput(inputId="varchoice", label="Training variables:",
                       choices=as.list(colnames(ok.data())),
                       selected=as.list(colnames(ok.data())[
                         sapply(ok.data(), class) %in%
                           c("integer", "numeric")]))
  })
  # Update train variable choice on button click
  observe({
    input$varNum
    updateCheckboxGroupInput(session, "varchoice", label= NULL, choices= NULL, 
                             selected= isolate(as.list(colnames(ok.data())[
                               sapply(ok.data(), class) %in% c("integer", "numeric")])))
  })
  observe({
    input$varAll
    updateCheckboxGroupInput(session, "varchoice", label= NULL, choices= NULL, 
                             selected= isolate(as.list(colnames(ok.data()))))
  })
  observe({
    input$varNone
    updateCheckboxGroupInput(session, "varchoice", label= NULL, choices= NULL, 
                             selected= NA)
  })
  
  ## Train the SOM when the button is hit
  ok.som <- reactive({
    if (input$trainbutton > 0) {
      isolate({
        dat <- ok.data()[, input$varchoice]
        msg <- NULL
        num.cols <- sapply(dat[, input$varchoice], is.numeric)
        if (any(!num.cols)) {
          msg <- paste0("Variables < ", 
                        ifelse(sum(!num.cols) == 1, input$varchoice[!num.cols], 
                               paste(input$varchoice[!num.cols], collape= ", ")), 
                        " > are not natively numeric, and will be forced to numeric.", 
                        " (This is probably a bad idea.)")
          dat[, input$varchoice] <- as.data.frame(sapply(dat[, input$varchoice], as.numeric))
        }
        rownames(dat) <- ok.rownames()
        dat <- as.matrix(na.omit(dat))
        if (input$trainscale) dat <- scale(dat)
        res <- som(dat, grid= somgrid(input$kohDimx, input$kohDimy, input$kohTopo))
        res$msg <- msg
        res
      })
    } 
  })
  
  ## Compute superclasses when ok.som or superclass changes
  ok.hclust <- reactive({
    if(!is.null(ok.som()))
      hclust(dist(ok.som()$codes), "ward.D2")
  })
  ok.sc <- reactive({
    if(!is.null(ok.hclust()))
      cutree(ok.hclust(), input$kohSuperclass)
  })
  
  ## Current training vars
  ok.trainvars <- reactive({
    if(!is.null(ok.som()))
      isolate(input$varchoice)
  })
  ## Current training rows (no NA)
  ok.trainrows <- reactive({
    if(!is.null(ok.som()))
      rowSums(is.na(ok.data()[, isolate(input$varchoice)])) == 0
  })
  
  ## Training message
  output$Message <- renderPrint({
    if (is.null(ok.som())) 
      return(cat("No map trained yet, click Train button."))
    
    if (!is.null(ok.som()$msg)) {
      cat("******************\nWarning:\n******************\n", 
          ok.som()$msg, "\n\n")
    }
    cat("## SOM summary:\n")
    summary(ok.som())
    cat("\n## Number of obs. per map cell:")
    table(factor(ok.som()$unit.classif, 
                 levels= 1:nrow(ok.som()$grid$pts)))
  })
  
  
  #############################################################################
  ## Panel "Graph"
  #############################################################################
  
  ## Update max nb superclasses
  observe({
    som <- ok.som()
    updateNumericInput(session, "kohSuperclass", max= som$grid$xdim * som$grid$ydim)
  })
  
  ## Sélection de variables (en fonction du graphique)
  output$plotVarOne <- renderUI({
    if (is.null(ok.data())) return()
    selectInput("plotVarOne", "Plot variable:", choices= colnames(ok.data()), 
                selected= ok.trainvars()[1])
  })
  output$plotVarMult <- renderUI({
    data <- ok.data()
    if (is.null(data)) return()
    tmp.numeric <- sapply(data, is.numeric)
    selectInput("plotVarMult", "Plot variable:", multiple= T,
                choices= colnames(data)[tmp.numeric],
                # selected= colnames(data)[tmp.numeric][1:min(5, sum(tmp.numeric))])
                selected= ok.trainvars()[1:min(5, length(ok.trainvars()))])
  })
    
  ## Scree plot
  output$screeplot <- renderPlot({
    if (is.null(ok.som())) return()
    plot(ok.hclust())
    rect.hclust(ok.hclust(), k= input$kohSuperclass)
  })
  
  ## Fancy JS Plots
  output$thePlot <- reactive({
    if (is.null(ok.som()) | !(input$graphType %in% c("Radar", "Camembert",
                                                          "Barplot", "Boxplot", 
                                                          "Color", "Star", 
                                                          "Hitmap", "Line", 
                                                          "Names")))
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    
    if (input$graphType %in% c("Radar", "Star", "Barplot", "Boxplot", "Line")) {
      if (is.null(input$plotVarMult)) return()
      plotVar <- input$plotVarMult
      data <- ok.data()[ok.trainrows(), plotVar]
    } else if (input$graphType %in% c("Color", "Camembert")) {
      if (is.null(input$plotVarOne)) return()
      plotVar <- input$plotVarOne
      data <- ok.data()[ok.trainrows(), plotVar]
    } else if (input$graphType %in% c("Hitmap")) {
      plotVar <- NULL
      data <- NULL
    } else if (input$graphType %in% c("Names")) {
      plotVar <- NULL
      #       data <- ok.rownames()[ok.trainrows()]
      data <- as.character(ok.data()[ok.trainrows(), input$plotVarOne])
    }
    
    contrast <- ifelse(input$contrast, "contrast", "range")
    getPlotParams(input$graphType, ok.som(), ok.sc(), 
                  data, input$plotSize, plotVar, contrast, 
                  input$palsc, input$palplot)
  })    
})
