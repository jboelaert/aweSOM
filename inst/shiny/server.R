## 27/04/2016 : Shiny SOM
options(shiny.maxRequestSize=1024*1024^2) # Max filesize

################################################################################
## Global Variables
################################################################################

## List of possible plots, by "what" type
plotChoices <- list(MapInfo= c("Population map"= "Hitmap",
                               # "Names"= "Names", 
                               "Superclass Dendrogram"= "Dendrogram",
                               "Superclass Scree plot"= "Screeplot",
                               "Neighbour distance"= "UMatrix", 
                               "Smooth distance"= "SmoothDist"), 
                    Numeric= c("Radar"= "Radar", 
                               "Barplot"= "Barplot", 
                               "Boxplot"= "Boxplot",
                               "Line"= "Line", 
                               # "Star"= "Star", 
                               "Heat"= "Color"), 
                    Categorical= c("Pie"= "Camembert", 
                                   "Barplot"= "CatBarplot"))

################################################################################
## Global Functions
################################################################################

##########
## Get plot colors from chosen palette
##########
getPalette <- function(pal, n, reverse= F) {
  if(pal == "grey") {
    res <- grey(1:n / n)
  } else if(pal == "rainbow") { 
    res <- substr(rainbow(n), 1, 7) 
  } else if(pal == "heat") { 
    res <- substr(heat.colors(n), 1, 7) 
  } else if(pal == "terrain") { 
    res <- substr(terrain.colors(n), 1, 7) 
  } else if(pal == "topo") { 
    res <- substr(topo.colors(n), 1, 7) 
  } else if(pal == "cm") { 
    res <- substr(cm.colors(n), 1, 7) 
  } else if (pal == "viridis") {
    if (n == 1) {
      res <- substr(viridis::viridis(3), 1, 7)[1]
    } else if (n == 2) {
      res <- substr(viridis::viridis(3), 1, 7)[c(1,3)]
    } else 
      res <- substr(viridis::viridis(n), 1, 7)
  } else {
    if (n == 1) {
      res <- RColorBrewer::brewer.pal(3, pal)[1]
    } else if (n == 2) {
      res <- RColorBrewer::brewer.pal(3, pal)[c(1,3)]
    } else 
      res <- RColorBrewer::brewer.pal(n, pal)
  }
  if (length(res) == 1) 
    res <- list(res)
  if (reverse) 
    res <- rev(res)
  res
}


##########
## Generate parameters to pass to javascript plotting functions
##########
getPlotParams <- function(type, som, superclass, data, plotsize, varnames, 
                          normtype= c("range", "contrast"), palsc, palplot, 
                          cellNames, plotOutliers, reversePal, options= NULL) {
  ## Paramètres communs à tous les graphiques
  somsize <- nrow(som$grid$pts)
  clustering <- factor(som$unit.classif, 1:nrow(som$grid$pts))
  clust.table <- table(clustering)
  
  gridInfo <- list(nbLines= som$grid$xdim,
                   nbColumns= som$grid$ydim,
                   topology= ifelse(som$grid$topo == "rectangular", 
                                    'rectangular', "hexagonal"))
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
  if (type %in% c("Camembert", "CatBarplot")) {
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
          getPalette(palplot, 8, reversePal)[cut(x, seq(.049, .951, length.out= 9))])
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
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    if (options$equalSize) {
      res$pieNormalizedSize <- rep(.9, length(clust.table))
    } else
      res$pieNormalizedSize <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$pieRealSize <- unname(clust.table)
    res$pieNormalizedValues <- unname(lapply(split(data, clustering), 
                                             function(x) {
                                               if (!length(x)) return(rep(1/nvalues, nvalues))
                                               unname(table(x) / length(x))
                                             }))
    res$pieRealValues <- unname(lapply(split(data, clustering), 
                                       function(x) unname(table(x))))
  } else if (type == "CatBarplot") {
    res$nbBatons <- nvalues
    res$isHist <- FALSE
    res$isCatBarplot <- TRUE
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    res$batonRealValues <- unname(lapply(split(data, clustering), 
                                         function(x) unname(table(x))))
    if (normtype == "contrast") {
      maxValue <- max(do.call(c, lapply(split(data, clustering), 
                                        function(x) {
                                          if (!length(x)) return(rep(0, nvalues))
                                          unname(table(x) / length(x))
                                        })))
    } else maxValue <- 1
    res$batonNormalizedValues <- unname(lapply(split(data, clustering), 
                                               function(x) {
                                                 if (!length(x)) return(rep(0, nvalues))
                                                 .02 + .98 * unname(table(x) / length(x)) / maxValue
                                               }))
  } else if (type == "Radar") {
    res$parts <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
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
    res$isCatBarplot <- FALSE
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    res$batonNormalizedValues <- normValues
    res$batonRealValues <- realValues
  } else if (type == "Boxplot") {
    res$nbBox <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    
    boxes.norm <- lapply(split(normDat, clustering), boxplot, plot= F)
    boxes.real <- lapply(split(data, clustering), boxplot, plot= F)
    res$boxPlotNormalizedValues <- unname(lapply(boxes.norm, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$boxPlotRealValues <- unname(lapply(boxes.real, function(x) unname(as.list(as.data.frame(x$stats)))))
    if (plotOutliers) {
      res$boxNormalizedExtremesValues <- unname(lapply(boxes.norm, function(x) unname(split(x$out, factor(x$group, levels= 1:nvar)))))
      res$boxRealExtremesValues <- unname(lapply(boxes.real, function(x) as.list(unname(split(x$out, factor(x$group, levels= 1:nvar))))))
    } else {
      res$boxNormalizedExtremesValues <- unname(lapply(boxes.norm, function(x) lapply(1:nvar, function(y) numeric(0))))
      res$boxRealExtremesValues <- unname(lapply(boxes.real, function(x) lapply(1:nvar, function(y) numeric(0))))
    }
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
  
  if (type == "CatBarplot")
    res$plotType <- "Barplot"
  
  res
}


################################################################################
## Main server function
################################################################################

shinyServer(function(input, output, session) {
  
  #############################################################################
  ## Panel "Import Data"
  #############################################################################

  # Current imported data
  ok.data <- reactive({
    if (is.null(input$dataFile))
      return(NULL)
    the.sep <- switch(input$sep, "Comma ','"=",", "Semicolon ';'"=";", 
                      "Tab"="\t", "Space"=" ")
    the.quote <- switch(input$quote, "None"="","Double Quote \""='"',
                        "Single Quote '"="'")
    the.dec <- switch(input$dec, "Period '.'"=".", "Comma ','"=",")
    data <- try(read.table(input$dataFile$datapath, header=input$header,
                           sep=the.sep, quote=the.quote, dec=the.dec))
    if(class(data) == "try-error") return(NULL)
    data
  })
  
  # data preview table
  output$dataView <- renderDataTable({
    d.input <- ok.data()
    if (is.null(d.input)) 
      return(NULL)
    data.frame(rownames= rownames(ok.data()), d.input)
  })
  
  # data import message
  output$dataImportMessage <- renderUI({
    if (is.null(input$dataFile)) 
      return(h4("Data preview should appear here after import."))
    if (! is.null(input$dataFile) & is.null(ok.data())) 
      return(h4("Error during import: try different import parameters, and check that file is a text or csv table."))
  })

  #############################################################################
  ## Panel "Train"
  #############################################################################
  
  # Update train variable options on data change
  output$trainVarOptions <- renderUI({
    if (is.null(ok.data())) return()
    varclass <- sapply(ok.data(), class)
    names(varclass) <- colnames(ok.data())
    isnum <- varclass %in% c("integer", "numeric")
    names(isnum) <- names(varclass) <- colnames(ok.data())
    
    lapply(colnames(ok.data()), function(var) {
      fluidRow(column(2, numericInput(paste0("trainVarWeight", var), NULL, value= 1, min= 0, max= 1e3)), 
               column(8, checkboxInput(paste0("trainVarChoice", var), var, unname(isnum[var]))),  
               column(2, p(varclass[var])))
    })
  })
  # Update train variable choice on button click
  observe({
    input$varNum
    if (is.null(ok.data())) return()
    selectVars <- sapply(ok.data(), class) %in% c("integer", "numeric")
    names(selectVars) <- colnames(ok.data())
    lapply(colnames(ok.data()), function(var) {
      updateCheckboxInput(session, paste0("trainVarChoice", var), value= unname(selectVars[var]))
    })
  })
  observe({
    input$varAll
    if (is.null(ok.data())) return()
    lapply(colnames(ok.data()), function(var) {
      updateCheckboxInput(session, paste0("trainVarChoice", var), value= T)
    })
  })
  observe({
    input$varNone
    if (is.null(ok.data())) return()
    euss <- rep(F, ncol(ok.data()))
    names(euss) <- colnames(ok.data())
    lapply(colnames(ok.data()), function(var) {
      updateCheckboxInput(session, paste0("trainVarChoice", var), value= unname(euss[var]))
    })
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
    tmpgrid <- class::somgrid(input$kohDimx, input$kohDimy, input$kohTopo)
    tmpgrid$n.hood <- ifelse(input$kohTopo == "hexagonal", "circular", "square")
    radius <- round(unname(quantile(kohonen::unit.distances(tmpgrid, FALSE), .67)), 2)
    updateNumericInput(session, "trainRadius1", value= radius)
    updateNumericInput(session, "trainRadius2", value= -radius)
  })
  
  ## Build training data when train button is hit
  ok.traindat <- reactive({
    if (input$trainbutton == 0) return(NULL)
    isolate({
      if (is.null(ok.data())) return(NULL)
      err.msg <- NULL
      
      # Get selected variables with non-zero weight
      varSelected <- as.logical(sapply(paste0("trainVarChoice", colnames(ok.data())), 
                                       function(var) input[[var]]))
      varWeights <- sapply(paste0("trainVarWeight", colnames(ok.data())), 
                           function(var) input[[var]])
      varSelected <- varSelected & varWeights > 0
      if (sum(varSelected) < 2) 
        return(list(dat= NULL, msg= "Select at least two variables (with non-zero weight)."))
      dat <- ok.data()[, varSelected]
      varWeights <- varWeights[varSelected]
      
      # Check that all variables are numeric, otherwise message and convert
      varNumeric <- sapply(dat, is.numeric)
      if (any(!varNumeric)) {
        err.msg$numeric <- paste0("Variables < ",
                                  paste(colnames(dat)[!varNumeric], collapse= ", "),
                                  " > are not natively numeric, and will be forced to numeric.",
                                  " (This is probably a bad idea.)")
        dat[, !varNumeric] <- as.data.frame(sapply(dat[, !varNumeric], as.numeric))
      }
      
      # Remove NAs
      nrow.withNA <- nrow(dat)
      dat <- as.matrix(na.omit(dat))
      if (nrow(dat) < nrow.withNA) {
        err.msg$NArows <- paste(nrow.withNA - nrow(dat), 
                                "observations contained missing values, and were removed.")
      }
      if (nrow(dat) == 0) {
        err.msg$NArows <- "All observations contain missing values, training impossible."
        return(list(dat= NULL, msg= err.msg))
      }
      
      # Check for constant variables (if so, exclude and message)
      varConstant <- apply(dat, 2, sd, na.rm= T) == 0
      if (any(varConstant)) {
        err.msg$constant <- paste0("Variables < ",
                                   ifelse(sum(varConstant) == 1, 
                                          colnames(dat)[varConstant], 
                                          paste(colnames(dat)[varConstant], collape= ", ")),
                                  " > are constant, and will be removed for training.")
        dat <- dat[, !varConstant]
        varWeights <- varWeights[!varConstant]
        if (sum(!varConstant) < 2) {
          err.msg$allconstant <- "Less than two selected non-constant variables, training impossible."
          return(list(dat= NULL, msg= err.msg))
        }
      }
      
      ## Scale variables and apply normalized weights
      if (input$trainscale) dat <- scale(dat)
      varWeights <- length(varWeights) * varWeights / sum(varWeights)
      dat <- t(sqrt(varWeights) * t(dat))
      
      list(dat= dat, msg= err.msg)
    })
  })
  
  ## Train SOM when button is hit
  ok.som <- reactive({
    dat <- ok.traindat()$dat
    if (is.null(dat)) return(NULL)
    isolate({
      ## Initialization
      set.seed(input$trainSeed)
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
      res <- kohonen::som(dat, grid= kohonen::somgrid(input$kohDimx, input$kohDimy, input$kohTopo), 
                          rlen= input$trainRlen, alpha= c(input$trainAlpha1, input$trainAlpha2), 
                          radius= c(input$trainRadius1, input$trainRadius2), init= init)
      ## save seed and set new
      res$seed <- input$trainSeed
      res$codes <- res$codes[[1]]
      updateNumericInput(session, "trainSeed", value= sample(1e5, 1))
      
      res
    })
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
      unname(cutree(ok.hclust(), input$kohSuperclass))
  })
  
  ## Current training vars
  ok.trainvars <- reactive({
    if (is.null(ok.som())) return(NULL)
    isolate(colnames(ok.traindat()$dat))
  })
  ## Current training rows (no NA)
  ok.trainrows <- reactive({
    if (is.null(ok.som())) return(NULL)
    isolate(rowSums(is.na(ok.data()[, ok.trainvars()])) == 0)
  })
  
  ok.dist <- reactive({
    if (is.null(ok.som())) return(NULL)
    # proto.gridspace.dist <- as.matrix(dist(ok.som()$grid$pts))
    proto.gridspace.dist <- kohonen::unit.distances(ok.som()$grid, F)
    proto.dataspace.dist <- as.matrix(dist(ok.som()$codes))
    neigh <- round(proto.gridspace.dist, 3) == 1
    proto.dataspace.dist.neigh <- proto.dataspace.dist
    proto.dataspace.dist.neigh[!neigh] <- NA
    list(proto.grid.dist= proto.gridspace.dist, 
         neigh.matrix= neigh, 
         proto.data.dist= proto.dataspace.dist, 
         proto.data.dist.neigh= proto.dataspace.dist.neigh)
  })
  
  ## Current quality measures when ok.som changes
  ok.qual <- reactive({
    if(!is.null(ok.som())) {
      traindat <- ok.traindat()$dat
      ## BMU, Squared distance from obs to BMU
      bmu <- ok.som()$unit.classif
      sqdist <- rowSums((traindat - ok.som()$codes[bmu, ])^2)
      
      ## Quantization error
      err.quant <- mean(sqdist)

      ## Interclass variance ratio
      totalvar <- sum(apply(traindat, 2, var)) * 
        (nrow(traindat) - 1) / nrow(traindat)
      err.varratio <- 100 - round(100 * err.quant / totalvar, 2)
      
      ## Topographic error
      bmu2 <- apply(traindat, 1, function(row) {
        dist <- colSums((t(ok.som()$codes) - row)^2)
        order(dist)[2]
      })
      err.topo <- mean(!ok.dist()$neigh.matrix[cbind(bmu, bmu2)])
      
      ## Kaski-Lagus error
      err.kaski <- e1071::allShortestPaths(ok.dist()$proto.data.dist.neigh)$length[cbind(bmu, bmu2)]
      err.kaski <- mean(err.kaski + sqrt(sqdist))
      
      list(err.quant= err.quant, err.varratio= err.varratio, 
           err.topo= err.topo, err.kaski= err.kaski)
    }
  })
  
  ## Training message
  output$Message <- renderPrint({
    if (!is.null(ok.traindat()$msg)) {
      cat(paste0("********** Warning: **********\n", 
                 paste("* ", ok.traindat()$msg, collapse= "\n"), 
                 "\n******************************\n\n"))
    }
    if (is.null(ok.qual())) 
      return(cat("No map trained yet, click Train button."))
    
    cat("## SOM summary:\n")
    summary(ok.som())
    isolate(cat(paste0("Training options: rlen = ", input$trainRlen, 
                       " ; alpha = (", input$trainAlpha1, ", ", input$trainAlpha2, ") ; ",
                       "radius = (", input$trainRadius1, ", ", input$trainRadius2, "), ", 
                       "random seed = ", ok.som()$seed, ".")))
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
  
  ## Update plot type choices on plot "what" selection
  observe({
    input$graphWhat
    isolate({
      if (is.null(ok.sc())) return(NULL)
      updateSelectInput(session, "graphType", choices= plotChoices[[input$graphWhat]])
    })
  })
  
  ## Update max nb superclasses
  observe({
    som <- ok.som()
    updateNumericInput(session, "kohSuperclass", max= som$grid$xdim * som$grid$ydim)
  })

  ## Update variable selection for graphs
  output$plotVarOne <- renderUI({
    if (is.null(ok.som())) return(NULL)
    isolate({
      fluidRow(column(4, p("Plot variable:")), 
               column(8, selectInput("plotVarOne", NULL, choices= colnames(ok.data()), 
                                     selected= ok.trainvars()[1])))
    })
  })
  output$plotVarMult <- renderUI({
    if (is.null(ok.som())) return(NULL)
    isolate({
      tmp.numeric <- sapply(ok.data(), is.numeric)
      fluidRow(column(4, p("Plot variables:")), 
               column(8, selectInput("plotVarMult", NULL, multiple= T,
                                     choices= colnames(ok.data())[tmp.numeric],
                                     selected= ok.trainvars()[tmp.numeric[ok.trainvars()]])))
    })
  })
  output$plotNames <- renderUI({
    if (is.null(ok.som())) return(NULL)
    isolate({
      tmp.numeric <- sapply(ok.data(), is.numeric)
      fluidRow(column(4, p("Observation names:")), 
               column(8, selectInput("plotNames", NULL,
                                     choices= c("(rownames)", colnames(ok.data())),
                                     selected= "(rownames)")))
    })
  })
    
  ## Dendrogram
  output$plotDendrogram <- renderPlot({
    if (is.null(ok.som())) return()
    plot(ok.hclust(), xlab= "", main= "")
    if (input$kohSuperclass > 1)
      rect.hclust(ok.hclust(), k= input$kohSuperclass)
  })
  ## Scree plot
  output$plotScreeplot <- renderPlot({
    if (is.null(ok.som())) return()
    ncells <- nrow(ok.som()$codes)
    nvalues <- max(input$kohSuperclass, min(ncells, max(ceiling(sqrt(ncells)), 10)))
    clust.var <- sapply(1:nvalues, function(k) {
      clust <- cutree(ok.hclust(), k)
      clust.means <- do.call(rbind, by(ok.som()$codes, clust, colMeans))[clust, ]
      mean(rowSums((ok.som()$codes - clust.means)^2))
    })
    unexpl <- 100 * round(clust.var / 
                            (sum(apply(ok.som()$codes, 2, var)) * (ncells - 1) / ncells), 3)
    plot(unexpl, t= "b", ylim= c(0, 100),
         xlab= "Nb. Superclasses", ylab= "% Unexpl. Variance")
    grid()                      
    abline(h= unexpl[input$kohSuperclass], col= 2)
  })
  ## Smooth distance plot
  output$plotSmoothDist <- renderPlot({
    if (is.null(ok.som())) return()
    values <- matrix(rowMeans(ok.dist()$proto.data.dist.neigh, na.rm= T), 
                     ok.som()$grid$ydim, ok.som()$grid$xdim)
    filled.contour(1:ok.som()$grid$ydim, 1:ok.som()$grid$xdim, 
                   values[, ok.som()$grid$xdim:1], 
                   color.palette= function(x) paste0(getPalette(input$palplot, x, input$plotRevPal), "FF"))
  })
  
  ## Fancy JS Plots
  output$thePlot <- reactive({
    if (is.null(ok.som()) | !(input$graphType %in% c("Radar", 
                                                     "Camembert", "CatBarplot",
                                                     "Barplot", "Boxplot", 
                                                     "Color", "Star", 
                                                     "Hitmap", "Line", 
                                                     "Names", "UMatrix")))
      return(NULL) # si on n'a pas calculé, on donne NULL à JS
    
    plot.data <- isolate(ok.data()[ok.trainrows(), ])
    if(is.null(plot.data)) return(NULL)
    # Obs names per cell for message box
    if (is.null(input$plotNames)) return()
    if (input$plotNames == "(rownames)") {
      plotNames.var <- rownames(plot.data)
    } else 
      plotNames.var <- as.character(plot.data[, input$plotNames])
    cellNames <- unname(lapply(split(plotNames.var, ok.clust()), 
                        function(x) paste(x, collapse= ", "))) # "&#13;&#10;" "<br />"

    if (input$graphType %in% c("Radar", "Star", "Barplot", "Boxplot", "Line")) {
      if (is.null(input$plotVarMult)) return()
      plotVar <- input$plotVarMult
      data <- plot.data[, plotVar]
    } else if (input$graphType %in% c("Color", "Camembert", "CatBarplot")) {
      if (is.null(input$plotVarOne)) return()
      plotVar <- input$plotVarOne
      data <- plot.data[, plotVar]
    } else if (input$graphType %in% c("Hitmap")) {
      plotVar <- NULL
      data <- NULL
    } else if (input$graphType %in% c("Names")) {
      plotVar <- NULL
      data <- as.character(plot.data[, input$plotVarOne])
    } else if (input$graphType == "UMatrix") {
      plotVar <- NULL
      proto.gridspace.dist <- as.matrix(dist(ok.som()$grid$pts))
      proto.dataspace.dist <- as.matrix(dist(ok.som()$codes))
      proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
      proto.dataspace.dist[proto.gridspace.dist == 0] <- NA
      data <- rowMeans(proto.dataspace.dist, na.rm= T)[ok.clust()]
      plotVar <- "Mean distance to neighbours"
    }
    
    options <- list(equalSize= input$plotEqualSize)
    contrast <- ifelse(input$contrast, "contrast", "range")
    graphType <- ifelse(input$graphType == "UMatrix", "Color", input$graphType)
    getPlotParams(graphType, ok.som(), ok.sc(), 
                  data, input$plotSize, plotVar, contrast, 
                  input$palsc, input$palplot, cellNames, 
                  input$plotOutliers, input$plotRevPal, options)
  })
  
  ## Plot warning
  output$plotWarning <- renderText({
    if ( ! input$palsc %in% c("viridis", "grey", "rainbow", "heat", "terrain", "topo", "cm")) {
      if (input$kohSuperclass > RColorBrewer::brewer.pal.info[input$palsc, "maxcolors"]) {
        return(paste0("WARNING: Palette ", input$palsc, 
                      " does not support more than ", 
                      RColorBrewer::brewer.pal.info[input$palsc, "maxcolors"], " colors."))
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
  
  # Update choices for rownames column on button clicks
  observe({
    input$clustSelectNone
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", selected= c("rownames", "Superclass", "SOM.cell"))
  })
  observe({
    input$clustSelectTrain
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", 
                      selected= c("rownames", "Superclass", "SOM.cell", isolate(ok.trainvars())))
  })
  observe({
    input$clustSelectAll
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", 
                      selected= c("rownames", "Superclass", "SOM.cell", isolate(colnames(ok.data()))))
  })
  

  # Current clustered data table
  ok.clustTable <- reactive({
    if (is.null(ok.sc()) | is.null(input$clustVariables)) return()
    res <- data.frame(isolate(ok.data()), SOM.cell= NA, Superclass= NA)
    res$rownames <- rownames(isolate(ok.data()))
    isolate({
      traindat <- ok.traindat()$dat
      res[rownames(traindat), "traindat"] <- rownames(traindat)
      res[rownames(traindat), "SOM.cell"] <- ok.clust()
      res[rownames(traindat), "Superclass"] <- ok.sc()[ok.clust()]
    })
    res[, input$clustVariables]
  })

  # Display clustered data  
  output$clustTable <- renderDataTable(ok.clustTable())

  # Download clustered data
  output$clustDownload <- 
    downloadHandler(filename= paste0("aweSOM-clust-", Sys.Date(), ".csv"), 
                    content= function(con) write.csv(ok.clustTable()[, colnames(ok.clustTable()) != "rownames"], con)) 
})
