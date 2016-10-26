## 27/04/2016 : Shiny SOM

library(kohonen)
library(RColorBrewer)
library(viridis)
options(shiny.maxRequestSize=1024*1024^2) # Max filesize

getPalette <- function(pal, n) {
  if(pal == "rainbow") return(as.list(substr(rainbow(n), 1, 7)))
  if(pal == "heat") return(as.list(substr(heat.colors(n), 1, 7)))
  if(pal == "terrain") return(as.list(substr(terrain.colors(n), 1, 7)))
  if(pal == "topo") return(as.list(substr(topo.colors(n), 1, 7)))
  if(pal == "cm") return(as.list(substr(cm.colors(n), 1, 7)))
  if (pal == "viridis") {
    if (n == 1) return(list(substr(viridis(3), 1, 7)[1]))
    if (n == 2) return(substr(viridis(3), 1, 7)[c(1,3)])
    return(substr(viridis(n), 1, 7))
  } else {
    if (n == 1) return(list(brewer.pal(3, pal)[1]))
    if (n == 2) return(brewer.pal(3, pal)[c(1,3)])
    return(brewer.pal(n, pal))
  }
}

############################
## Fonction qui génère les paramètres à passer à JS
getPlotParams <- function(type, som, superclass, data, plotsize, varnames, 
                          normtype= c("range", "contrast"), palsc, palplot, 
                          cellNames) {
  
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
              superclassColor= superclassColor, 
              cellNames= cellNames, 
              cellPop= unname(clust.table))
  
  ## Traitement data si besoin :
  if (type == "Camembert") {
    if (is.numeric(data)) if (length(unique(data)) > 100) data <- cut(data, 100)
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
                                      if (!nrow(x)) return(rep(NA, nvar))
                                      unname(colMeans(x))
                                    }))
        realValues <- unname(lapply(split(data, clustering), 
                                    function(x) {
                                      if (!nrow(x)) return(rep(NA, nvar))
                                      unname(round(colMeans(x), 3))
                                    }))
      } else if (normtype == "contrast") {
        ## "Contrast" normalization : means on data, then range(means) -> [0,1]
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) {
                                              if (!nrow(x)) return(rep(NA, nvar))
                                              unname(round(colMeans(x), 3))
                                            }))
        normValues <- apply(realValues, 2, function(x) 
          .05 + .9 * (x - min(x, na.rm= T)) / (max(x, na.rm= T) - min(x, na.rm= T)))
        realValues <- unname(as.list(as.data.frame(t(realValues))))
        normValues <- unname(as.list(as.data.frame(t(normValues))))
      }
      if (type == "Color") {
        ## 8 colors (equal-sized bins of values) of selected palette
        normValues <- do.call(rbind, normValues)
        normValues <- apply(normValues, 2, function(x) 
          getPalette(palplot, 8)[cut(x, seq(.049, .951, length.out= 9))])
        normValues[is.na(normValues)] <- "#FFFFFF"
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
    res$label <- varnames
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
    res$label <- varnames
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
  output$dataView <- renderDataTable({
    d.input <- ok.data()
    if (is.null(d.input)) 
      return(NULL)

    if (!is.null(input$rownames.col))
      if (input$rownames.col != "(None)")
        if (!any(duplicated(d.input[, input$rownames.col])))
          try(row.names(d.input) <- as.character(d.input[, input$rownames.col]))

    data.frame(rownames= rownames(ok.data()), d.input)
  })

  # Update choices for rownames column
  output$rownames.col <- renderUI({
    if (is.null(ok.data())) return()
    fluidRow(column(4, p("Rownames var:")), 
             column(8, selectInput(inputId= "rownames.col", label= NULL, 
                                   choices= c("(None)", colnames(ok.data())),
                                   selected= "(None)")))
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
    selected <- colnames(ok.data())[sapply(ok.data(), class) %in%
                                      c("integer", "numeric")]
    selected <- selected[apply(ok.data()[, selected], 2, sd, na.rm= T) != 0]
    checkboxGroupInput(inputId="varchoice", label="Training variables:",
                       choices=as.list(colnames(ok.data())),
                       selected=as.list(selected))
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
  # Update grid dimension on data update
  observe({
    if (is.null(ok.data())) return()
    tmp.dim <- max(4, min(10, ceiling(sqrt(nrow(ok.data()) / 10))))
    updateNumericInput(session, "kohDimx", value= tmp.dim)
    updateNumericInput(session, "kohDimy", value= tmp.dim)
  })
  # Update training radius on change of grid
  observe({
    if (is.null(ok.data())) return()
    tmpgrid <- somgrid(input$kohDimx, input$kohDimy, input$kohTopo)
    tmpgrid$n.hood <- ifelse(input$kohTopo == "hexagonal", "circular", "square")
    radius <- round(unname(quantile(unit.distances(tmpgrid, FALSE), .67)), 2)
    updateNumericInput(session, "trainRadius1", value= radius)
    updateNumericInput(session, "trainRadius2", value= -radius)
  })
  
  ## Train the SOM when the button is hit (first make the training dataset)
  ok.traindat <- reactive({
    if (input$trainbutton > 0) {
      isolate({
        dat <- ok.data()[, input$varchoice]
        msg <- NULL
        num.cols <- sapply(dat[, input$varchoice], is.numeric)
        if (any(!num.cols)) {
          ## TODO get this message to print
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
        dat
      })
    }
  })
  ok.som <- reactive({
    if (! is.null(ok.traindat())) {
      isolate({
        dat <- ok.traindat()
        ## Initialization
        if (input$kohInit == "random") {
          init <- dat[sample(nrow(dat), input$kohDimx * input$kohDimy, replace= T), ]
        } else if (input$kohInit %in% c("pca.sample", "pca")) {
          # the most detailed grid axis is assigned to the first component
          if (input$kohDimx >= input$kohDimy) {
            x.ev <- 1
            y.ev <- 2
          } else {
            x.ev <- 2
            y.ev <- 1
          }
          # perform PCA (TODO: make hex grid on pca ?)
          data.pca <- prcomp(dat, center= F, scale.= F)
          x <- seq(from= quantile(data.pca$x[,x.ev], .025), 
                   to= quantile(data.pca$x[,x.ev], .975),
                   length.out= input$kohDimx)
          y <- seq(from= quantile(data.pca$x[,y.ev], .025), 
                   to= quantile(data.pca$x[,y.ev], .975),
                   length.out= input$kohDimy)
          base <- as.matrix(expand.grid(x=x, y=y))
          if (input$kohInit == "pca.sample") {
            ## As in SOMbrero, init to observations closest to a 2D PCA grid
            closest.obs <- apply(base, 1, function(point) 
              which.min(colSums((t(data.pca$x[,c(x.ev,y.ev)])-point)^2)))
            init <- dat[closest.obs,]
          } else if (input$kohInit == "pca") {
            ## Pure PCA grid
            base <- cbind(base, matrix(0, nrow(base))[, rep(1, ncol(data.pca$x) - 2)])
            init <- base %*% t(data.pca$rotation)
          }
        } 
        res <- som(dat, grid= somgrid(input$kohDimx, input$kohDimy, input$kohTopo), 
                   rlen= input$trainRlen, alpha= c(input$trainAlpha1, input$trainAlpha2), 
                   radius= c(input$trainRadius1, input$trainRadius2, init= init))
        # res$msg <- msg
        res
      })
    } 
  })
  
  ## Get clustering when ok.som changes
  ok.clust <- reactive({
    factor(ok.som()$unit.classif, 1:nrow(ok.som()$grid$pts))
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
  
  ## Current quality measures when ok.som changes
  ok.qual <- reactive({
    if(!is.null(ok.som())) {
      ## BMU, Squared distance from obs to BMU
      bmu <- ok.som()$unit.classif
      sqdist <- rowSums((ok.traindat() - ok.som()$codes[bmu, ])^2)
      
      ## Quantization error
      err.quant <- mean(sqdist)

      ## Interclass variance ratio
      totalvar <- sum(apply(ok.traindat(), 2, var)) * 
        (nrow(ok.traindat()) - 1) / nrow(ok.traindat())
      err.varratio <- 100 - round(100 * err.quant / totalvar, 2)
      
      ## Topographic error
      bmu2 <- apply(ok.traindat(), 1, function(row) {
        dist <- colSums((t(ok.som()$codes) - row)^2)
        order(dist)[2]
      })
      proto.gridspace.dist <- as.matrix(dist(ok.som()$grid$pts))
      err.topo <- mean(round(proto.gridspace.dist[cbind(bmu, bmu2)], 3) > 1)
      
      ## Kaski-Lagus error
      proto.dataspace.dist <- as.matrix(dist(ok.som()$codes))
      proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
      err.kaski <- e1071::allShortestPaths(proto.dataspace.dist)$length[cbind(bmu, bmu2)]
      err.kaski <- mean(err.kaski + sqrt(sqdist))
      
      list(err.quant= err.quant, err.varratio= err.varratio, 
           err.topo= err.topo, err.kaski= err.kaski)
    }
  })
  
  ## Training message
  output$Message <- renderPrint({
    if (is.null(ok.qual())) 
      return(cat("No map trained yet, click Train button."))
    
    if (!is.null(ok.som()$msg)) {
      cat("******************\nWarning:\n******************\n", 
          ok.som()$msg, "\n\n")
    }
    cat("## SOM summary:\n")
    summary(ok.som())
    isolate(cat(paste0("Training options: rlen = ", input$trainRlen, 
                       " ; alpha = (", input$trainAlpha1, ", ", input$trainAlpha2, ") ; ",
                       "radius = (", input$trainRadius1, ", ", input$trainRadius2, ").")))
    cat("\n\n## Quality measures:\n")
    cat("* Quantization error     : ", ok.qual()$err.quant, "\n")
    cat("* (% explained variance) : ", ok.qual()$err.varratio, "\n")
    cat("* Topographic error      : ", ok.qual()$err.topo, "\n")
    cat("* Kaski-Lagus error      : ", ok.qual()$err.kaski, "\n")
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

  ## Update variable selection for graphs
  output$plotVarOne <- renderUI({
    if (is.null(ok.data())) return()
    fluidRow(column(4, p("Plot variable:")), 
             column(8, selectInput("plotVarOne", NULL, choices= colnames(ok.data()), 
                selected= ok.trainvars()[1])))
  })
  output$plotVarMult <- renderUI({
    data <- ok.data()
    if (is.null(data)) return()
    tmp.numeric <- sapply(data, is.numeric)
    fluidRow(column(4, p("Plot variables:")), 
             column(8, selectInput("plotVarMult", NULL, multiple= T,
                                   choices= colnames(data)[tmp.numeric],
                                   # selected= colnames(data)[tmp.numeric][1:min(5, sum(tmp.numeric))])
                                   # selected= ok.trainvars()[1:min(5, length(ok.trainvars()))])
                                   selected= ok.trainvars()[1:length(ok.trainvars())])))
  })
  output$plotNames <- renderUI({
    data <- ok.data()
    if (is.null(data)) return()
    tmp.numeric <- sapply(data, is.numeric)
    fluidRow(column(4, p("Names variable:")), 
             column(8, selectInput("plotNames", NULL,
                                   choices= c("(rownames)", colnames(data)),
                                   selected= "(rownames)")))
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
    
    # Obs names per cell for message box
    if (is.null(input$plotNames)) return()
    if (input$plotNames == "(rownames)") {
      plotNames.var <- rownames(ok.data()[ok.trainrows(), ])
    } else 
      plotNames.var <- as.character(ok.data()[ok.trainrows(), input$plotNames])
    cellNames <- unname(lapply(split(plotNames.var, ok.clust()), 
                        function(x) paste(x, collapse= ", "))) # "&#13;&#10;" "<br />"

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
                  input$palsc, input$palplot, cellNames)
  })    
  
  ## Fancy JS Plots
  output$plotWarning <- renderText({
    if ( ! input$palsc %in% c("viridis", "rainbow", "heat", "terrain", "topo", "cm")) {
      if (input$kohSuperclass > brewer.pal.info[input$palsc, "maxcolors"]) {
        return(paste0("WARNING: Palette ", input$palsc, 
                      " does not support more than ", 
                      brewer.pal.info[input$palsc, "maxcolors"], " colors."))
      }
    }
    return(NULL)
  })
  
  
  #############################################################################
  ## Panel "Clustered Data"
  #############################################################################
  
  # Update choices for rownames column
  output$clustVariables <- renderUI({
    if (is.null(ok.sc())) return()
    isolate(selectInput(inputId= "clustVariables", label= NULL, multiple= T,
                        choices= c("rownames", "Superclass", "SOM.cell", colnames(ok.data())),
                        selected= c("rownames", "Superclass", "SOM.cell", colnames(ok.data())[1])))
  })
  
  # Current clustered data table
  ok.clustTable <- reactive({
    if (is.null(ok.sc()) | is.null(input$clustVariables)) return()
    res <- data.frame(rownames= isolate(ok.rownames()), SOM.cell= ok.clust(), 
                      Superclass= ok.sc()[ok.clust()], 
                      isolate(ok.data()))[, input$clustVariables]
    rownames(res) <- isolate(ok.rownames())
    res
  })

  # Display clustered data  
  output$clustTable <- renderDataTable(ok.clustTable())

  # Download clustered data
  output$clustDownload <- 
    downloadHandler(filename= paste0("aweSOM-clust-", Sys.Date(), ".csv"), 
                    content= function(con) write.csv(ok.clustTable()[, colnames(ok.clustTable()) != "rownames"], con)) 
})
