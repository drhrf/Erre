server <- function(input, output, session) {

# shinyServer(function(input, output) {
  
  data <- reactive({
    
    req(input$tbl)
    
    ext <- tools::file_ext(input$tbl$datapath)
    
    if (ext == "xlsx") {
      d <- input$tbl
      df <- readxl::read_excel(path = d$datapath, sheet = input$sheet)
    } else if (ext == "csv") {
      d <- input$tbl
      df <- vroom::vroom(d$datapath, delim = ",")
    } else if (ext == "tsv") {
      d <- input$tbl
      df <- vroom::vroom(d$datapath, delim = "\t")
    } else {
      return('WARNING: wrong file format. Please try again.')
    }

  })
  
  datashowow <- reactive({
    
    if (is.null(input$tbl)) {
      df <- as.data.frame(iris[,1:4])
      df <- as.data.frame(df)
      return(df)
    } else {
      df <- as.data.frame(data())
      return(df)}
    
  })
    
  datahead <- reactive({
    
    if (is.null(input$tbl)) {
      dt <- as.data.frame(iris[,1:4])
      return(head(dt, input$n))
    } else {
      dt <- data()
      return(head(dt, input$n))}
    
  })
  
  sats <- reactive({
    
    df <- datashowow()
    descr <- describe(df)
    descr <- descr[,2:13]
    columns <- colnames(df)
    descr <- cbind(columns, descr)
    dmelt <- melt(df, id.vars = 0)
    dt <- dmelt %>% 
      dplyr::group_by(variable) %>% 
      identify_outliers(value)
    shapiro <- shapiro.test(dmelt$value)
    aov <- stats::aov(data = dmelt, formula = value ~ variable)
    aovv <- summary(aov)
    aovv <- unlist(aovv)['Pr(>F)1']
    tuk <- stats::TukeyHSD(x = aov, conf.level = input$conflvl)
    tuk <- as.data.frame(tuk$variable)
    comparison <- rownames(tuk)
    tuk <- cbind.data.frame(comparison, tuk)
    ttst <- t.test(df[,input$ttstga], df[,input$ttstgb], 
                   paired = FALSE, conf.level = input$conflvl)
    krus <- kruskal_test(data = dmelt, formula = value ~ variable)
    dun <- dunn_test(formula = value ~ variable,
                     data = dmelt,
                     p.adjust.method = "bonferroni",
                     detailed = FALSE)
    dun <- dun[,c(2,3,4,5,8)]
    dlt <- df[,c(input$ttstga, input$ttstgb)]
    dlt <- melt(dlt, id.vars = 0)
    wil <- wilcox_test(data = dlt, 
                       formula = value ~ variable, 
                       p.adjust.method = 'bonferroni', 
                       paired = FALSE, 
                       exact = TRUE, 
                       conf.level = input$conflvl, 
                       detailed = FALSE)
    
    dataframe <- cbind.data.frame(shapiro$p.value, 
                                  aovv, 
                                  krus$p, 
                                  ttst$p.value, 
                                  wil$p)
    rw <- 'p-value'
    cl <- c('Shapiro-Wilk', 
            'Anova', 
            'Kruskal-Wallis', 
            'Welch t-test', 
            'Wilcoxon')
    colnames(dataframe) <- cl
    rownames(dataframe) <- rw
    aa <- list(dataframe, tuk, dun, dt, descr)
    return(aa)
    
  })
  
  output$text <- renderText({
    
    a <- ' '
    
    df <- datashowow()
    
    if (is.null(input$tbl)) {
      a <- 'WELCOME! Fell free to explore the model dataset or upload your own data.'
    } else {
      for (i in 1:ncol(df)) {
        if (is.character(df[[i]])) {
          a <- 'WARNING: non-numeric data in the file. Please try again.'}
    }}
    
    if (nchar(a) > 1) {
      print(a)
    } else {return()}
    
  })
  
  output$currtest <- renderText({
    
    df <- datashowow()
    
    A <- names(df)[input$ttstga]
    
    B <- names(df)[input$ttstgb]
    
    paste("[ Current comparison: ", A, " x ", B, "]")
    
  })
  
  output$xy <- renderText({
    
    df <- datashowow()
    
    A <- names(df)[input$ttstga]
    
    B <- names(df)[input$ttstgb]
    
    paste("[ Current plot: {X}", A, " x {Y}", B, "]")
    
  })
  
  output$histplot <- renderPlotly({
    
    hist()
    
  })  
  
  hist <- reactive({  
    
    validate(
      need(input$fill != '', 'Please provide a valid FILL COLOR input.'),
      need(input$border != '', 'Please provide a valid BORDER COLOR input.'),
      need(input$brks != '', 'Please provide a valid NUMBER OF BREAKS input.'),
      need(input$density != '', 'Please provide a valid DENSITY LINE COLOR input.'),
      need(input$nbr != '', 'Please provide a valid COLUMN NUMBER input.'),
      need(input$xannt != '', 'Please provide a valid TEXT POSITION IN X AXIS input.'),
      need(input$yannt != '', 'Please provide a valid TEXT POSITION IN y AXIS input.'),
      need(input$txt != '', 'Please provide a valid ANNOTATION input.'),
      need(input$txtcol != '', 'Please provide a valid ANNOTATION COLOR input.'),
      need(input$txtsize != '', 'Please provide a valid ANNOTATION SIZE input.')
    )
    
    df <- datashowow()
    dt <- as.data.frame(df[,input$nbr])
    dt <- drop_na(dt)
    colnames(dt) <- 'col1'
    
    if (input$xannt == 0) {
      xpos <- median(dt$col1)
    } else {
      xpos <- input$xannt
    }
    
    if (input$yannt == 0) {
      ypos <- 0
    } else {
      ypos <- input$yannt
    }
    
    a<-ggplot(data = dt, mapping = aes(x = col1)) + 
      geom_histogram(mapping = aes(y = ..density..),
                     fill = input$fill,
                     color = input$border, 
                     bins = input$brks) + 
      geom_density(color = input$density) +
      theme(panel.background = element_blank(),
            axis.line.y = element_line("black", size = .25), 
            axis.line.x = element_line("black", size = .25)) + 
      labs(title = input$title, x = input$xlab, y = input$ylab) +
      annotate('text', 
               x=xpos, y=ypos, 
               label=input$txt, col=input$txtcol, cex=input$txtsize)
    
    a
    
  })
  
  output$downhist <- downloadHandler(
    filename = function() {"Histogram.tiff"},
    content = function(file) {
      ggsave(file, 
             plot = hist(), 
             device = "tiff", 
             dpi = input$resol, 
             width = input$width, 
             height = input$height)
    }
  )
  
  output$citukey <- renderPlotly({
    
    tukey()
    
  })
  
  tukey <- reactive({
    
    validate(
      need(input$fill != '', 'Please provide a valid FILL COLOR input.'), 
      need(input$xannttu != '', 'Please provide a valid TEXT POSITION IN X AXIS input.'),
      need(input$yannttu != '', 'Please provide a valid TEXT POSITION IN y AXIS input.'),
      need(input$txttu != '', 'Please provide a valid ANNOTATION input.'),
      need(input$txtcoltu != '', 'Please provide a valid ANNOTATION COLOR input.'),
      need(input$txtsizetu != '', 'Please provide a valid ANNOTATION SIZE input.')
      )
    
    tkk <- sats()
    tkk <- as.data.frame(tkk[2])
    
    if (input$xannttu == 0) {
      xpos <- 1
    } else {
      xpos <- input$xannttu
    }
    
    if (input$yannttu == 0) {
      ypos <- 0
    } else {
      ypos <- input$yannttu
    }
    
    d<-ggplot(data = tkk) +
      geom_errorbar(aes(x = reorder(x = comparison, -diff), y = diff,
                        ymin = lwr, 
                        ymax = upr), width = 0.4) +
      geom_point(aes(x = reorder(x = comparison, -diff), y = diff), 
                 color = input$fill) +
      geom_hline(yintercept = c(0,0), linetype = 'dotted') +
      theme(panel.background = element_blank(),
            axis.line.y = element_line("black", size = .25),
            axis.line.x = element_line("black", size = .25)) +
      labs(y = 'Difference (upper/lower CI)',
           x = '',
           title = 'Confidence intervals (Tukey test)') + 
      coord_flip() +
      annotate('text', 
               x=xpos, y=ypos, 
               label=input$txttu, 
               col=input$txtcoltu, cex=input$txtsizetu)
    
    d
  
  }) 
  
  output$downtukey <- downloadHandler(
    filename = function() {"Tukey_plot.tiff"},
    content = function(file) {
      ggsave(file, 
             plot = tukey(), 
             device = "tiff", 
             dpi = input$resol, 
             width = input$width, 
             height = input$height)
    }
  )
  
  output$boxplot <- renderPlotly({
    
    box()
    
  })
  
  box <- reactive({
    
    validate(
      need(input$fill != '', 'Please provide a valid FILL COLOR input.'),
      need(input$border != '', 'Please provide a valid BORDER COLOR input.'),
      need(input$jitter != '', 'Please provide a valid JITTER DOT COLOR input.'),
      need(input$viol != '', 'Please provide a valid VIOLIN PLOT COLOR input.'),
      need(input$point != '', 'Please provide a valid MEAN DOT COLOR input.'),
      need(input$xanntbx != '', 'Please provide a valid TEXT POSITION IN X AXIS input.'),
      need(input$yanntbx != '', 'Please provide a valid TEXT POSITION IN y AXIS input.'),
      need(input$txtbx != '', 'Please provide a valid ANNOTATION input.'),
      need(input$txtcolbx != '', 'Please provide a valid ANNOTATION COLOR input.'),
      need(input$txtsizebx != '', 'Please provide a valid ANNOTATION SIZE input.')
    )
    
    dmelt <- melt(datashowow(), id.vars = 0)
    dmelt <- drop_na(dmelt)
    
    dt <- dmelt %>% group_by(variable) %>% 
      summarise(mean = mean(value), sd = sd(value))
    
    if (input$xanntbx == 0) {
      xpos <- 1
    } else {
      xpos <- input$xanntbx
    }
    
    if (input$yanntbx == 0) {
      ypos <- 0
    } else {
      ypos <- input$yanntbx
    }
    
    if (input$ord == 'y') {
      
      b<-ggplot(data = dmelt, 
             aes(x = reorder(x = variable, -value), y = value)) + 
        geom_violin(fill = NA, col = input$viol) +
        geom_boxplot(fill = input$fill, outlier.shape = NA, 
                     col = input$border) +
        stat_boxplot(geom = "errorbar",
                     width = 0.1) +
        geom_jitter(position = position_jitter(0.2),
                    color = input$jitter, 
                    size = .5, 
                    shape = 20) +
        geom_point(data = dt, 
                   aes(x = reorder(x = variable, -mean), y = mean), 
                   color = input$point, size = 1) +
        theme(panel.background = element_blank(),
              axis.line.y = element_line("black", size = .25), 
              axis.line.x = element_line("black", size = .25)) +
        labs(y = input$ylab,
             x = input$xlab, 
             title = input$title) +
        annotate('text', 
                 x=xpos, y=ypos, 
                 label=input$txtbx, 
                 col=input$txtcolbx, cex=input$txtsizebx)
        
      b
      
    } else {
      
      b<-ggplot(data = dmelt,
             aes(x = variable, y = value)) + 
        geom_violin(fill = NA, col = input$viol) +
        geom_boxplot(fill = input$fill, outlier.shape = NA,
                     col = input$border) +
        geom_jitter(position = position_jitter(0.2),
                    color = input$jitter, 
                    size = .5, 
                    shape = 20) +
        geom_point(data = dt, 
                   aes(x = variable, y = mean), 
                   color = input$point, size = 1) +
        theme(panel.background = element_blank(),
              axis.line.y = element_line("black", size = .25), 
              axis.line.x = element_line("black", size = .25)) +
        labs(y = input$ylab,
             x = input$xlab, 
             title = input$title) + 
        annotate('text', 
                 x=xpos, y=ypos, 
                 label=input$txtbx, 
                 col=input$txtcolbx, cex=input$txtsizebx)
      
      b
    }
    
  })
  
  output$downbox <- downloadHandler(
    filename = function() {"Boxplot.tiff"},
    content = function(file) {
      ggsave(file, 
             plot = box(), 
             device = "tiff", 
             dpi = input$resol, 
             width = input$width, 
             height = input$height)
    }
  )
  
  output$barplot <- renderPlotly({
    
    bar()
    
  })
  
  bar <- reactive({
    
    validate(
      need(input$ord != '', 'Please provide a valid ORDERED COLUMNS input.'),
      need(input$funbar != '', 'Please provide a valid BARPLOT STATISTICS input.'),
      need(input$fill != '', 'Please provide a valid FILL COLOR input.'),
      need(input$border != '', 'Please provide a valid BORDER COLOR input.'),
      need(input$errcol != '', 'Please provide a valid ERROR BAR COLOR input.'),
      need(input$jitter != '', 'Please provide a valid JITTER DOT COLOR input.'),
      need(input$point != '', 'Please provide a valid MEAN DOT COLOR input.'), 
      need(input$xanntbr != '', 'Please provide a valid TEXT POSITION IN X AXIS input.'),
      need(input$yanntbr != '', 'Please provide a valid TEXT POSITION IN y AXIS input.'),
      need(input$txtbr != '', 'Please provide a valid ANNOTATION input.'),
      need(input$txtcolbr != '', 'Please provide a valid ANNOTATION COLOR input.'),
      need(input$txtsizebr != '', 'Please provide a valid ANNOTATION SIZE input.')
    )
    
    dmelt <- melt(datashowow(), id.vars = 0)
    dmelt <- drop_na(dmelt)
    
    dt <- dmelt %>% group_by(variable) %>% 
      summarise(mean = mean(value), sd = sd(value), median = median(value))
    
    if (input$xanntbr == 0) {
      xpos <- 1
    } else {
      xpos <- input$xanntbr
    }
    
    if (input$yanntbr == 0) {
      ypos <- 0
    } else {
      ypos <- input$yanntbr
    }
    
    if (input$ord == 'y') {
      
      c<-ggplot() +
        theme(panel.background = element_blank(),
              axis.line.y = element_line("black", size = .25),
              axis.line.x = element_line("black", size = .25)) +
        labs(y = input$ylab,
             x = input$xlab,
             title = input$title) +
        geom_bar(data = dmelt, 
                 mapping = aes(x = reorder(x = variable, -value), y = value), 
                 fill = input$fill, 
                 stat = 'summary', 
                 fun = input$funbar, 
                 color = input$border) +
        geom_errorbar(data = dt, 
                      mapping = aes(x = reorder(x = variable, -mean),
                                    y = mean,
                                    ymin = mean - sd,
                                    ymax = mean + sd), 
                      col = input$errcol, 
                      width = 0.4) +
        geom_point(data = dt, 
                   mapping = aes(x = reorder(x = variable, -mean), y = mean),
                   color = input$point, size = 1) + 
        geom_jitter(data = dmelt, 
                    mapping = aes(x = reorder(x = variable, -value), y = value),
                    position = position_jitter(0.2),
                    color = input$jitter,
                    size = .5,
                    shape = 20) +
        annotate('text', 
                 x=xpos, y=ypos, 
                 label=input$txtbr, 
                 col=input$txtcolbr, cex=input$txtsizebr)
      
      c
      
    } else {
      
      c<-ggplot() +
        theme(panel.background = element_blank(),
              axis.line.y = element_line("black", size = .25),
              axis.line.x = element_line("black", size = .25)) +
        labs(y = input$ylab,
             x = input$xlab,
             title = input$title) + 
        geom_bar(data = dmelt,
                 mapping = aes(x = variable, y = value), 
                 fill = input$fill, 
                 stat = 'summary',
                 fun = input$funbar, color = input$border) +
        geom_errorbar(data = dt, 
                      mapping = aes(x = variable,
                                    y = mean,
                                    ymin = mean - sd,
                                    ymax = mean + sd),
                      col = input$errcol, 
                      width = 0.4) +
        geom_point(data = dt, 
                   mapping = aes(x = variable, y = mean),
                   color = input$point, size = 1) + 
        geom_jitter(data = dmelt, 
                    mapping = aes(x = variable, y = value),
                    position = position_jitter(0.2),
                    color = input$jitter,
                    size = .5,
                    shape = 20) +
        annotate('text', 
                 x=xpos, y=ypos, 
                 label=input$txtbr, 
                 col=input$txtcolbr, cex=input$txtsizebr)
        
      c
    }
    
  })
  
  output$downbar <- downloadHandler(
    filename = function() {"Barplot.tiff"},
    content = function(file) {
      ggsave(file, 
             plot = bar(), 
             device = "tiff", 
             dpi = input$resol, 
             width = input$width, 
             height = input$height)
    }
  )
  
  output$regres <- renderPlotly({
    
    reg()
    
  })
  
  reg <- reactive({
    
    validate(
      need(input$ttstga != '', 'Please provide a valid GROUP A input.'),
      need(input$ttstgb != '', 'Please provide a valid GROUP B input.'),
      need(input$setrue != '', 'Please provide a valid STANDARD ERROR input.'),
      need(input$regcolor != '', 'Please provide a valid REGRESSION LINE COLOR input.'),
      need(input$regtype != '', 'Please provide a valid REGRESSION MODEL input.'),
      need(input$jitter != '', 'Please provide a valid JITTER DOT COLOR input.'), 
      need(input$xanntre != '', 'Please provide a valid TEXT POSITION IN X AXIS input.'),
      need(input$yanntre != '', 'Please provide a valid TEXT POSITION IN y AXIS input.'),
      need(input$txtre != '', 'Please provide a valid ANNOTATION input.'),
      need(input$txtcolre != '', 'Please provide a valid ANNOTATION COLOR input.'),
      need(input$txtsizere != '', 'Please provide a valid ANNOTATION SIZE input.')
      )
    
    df <- datashowow()
    x <- df[,input$ttstga]
    y <- df[,input$ttstgb]
    dt <- cbind.data.frame(x, y)
    colnames(dt) <- c('x', 'y')
    
    setrue <- as.logical(input$setrue)
    
    if (input$xanntre == 0) {
      xpos <- min(dt$x)
    } else {
      xpos <- input$xanntre
    }
    
    if (input$yanntre == 0) {
      ypos <- min(dt$y)
    } else {
      ypos <- input$yanntre
    }
    
    e <- ggplot(data = dt) +
      geom_point(mapping = aes(x = x,
                               y = y),
                 col = input$jitter, 
                 size = input$pointsz) +
      geom_smooth(mapping = aes(x = x,
                                y = y),
                  color = input$regcolor,
                  lwd = .5,
                  method = input$regtype, 
                  formula = y ~ x, 
                  se = setrue) +
      theme(panel.background = element_blank(),
            axis.line.y = element_line("black", size = .25),
            axis.line.x = element_line("black", size = .25)) +
      labs(title = input$title, x = input$xlab, y = input$ylab) + 
      annotate('text', 
               x=xpos, y=ypos, 
               label=input$txtre, 
               col=input$txtcolre, cex=input$txtsizere)
    
    e
    
  })
  
  output$downreg <- downloadHandler(
    filename = function() {"Regression_plot.tiff"},
    content = function(file) {
      ggsave(file, 
             plot = reg(), 
             device = "tiff", 
             dpi = input$resol, 
             width = input$width, 
             height = input$height)
    }
  )
  
  output$resgre <- renderTable({
    
    regtable()
    
  })
  
  regtable <- reactive({
    
    df <- datashowow()
    x <- df[,input$ttstga]
    y <- df[,input$ttstgb]
    dt <- cbind.data.frame(x, y)
    colnames(dt) <- c('x', 'y')
    
    if (input$regtype == 'loess') {
      j<-summary(loess(data = dt, y ~ x))
      N <- j$n 
      Equivalent_N_of_parameters <- j$enp 
      Residual_standard_error <- j$s 
      Trace_of_smoother_matrix <- j$trace.hat
      loe <- rbind.data.frame(N,
                              Equivalent_N_of_parameters,
                              Residual_standard_error, 
                              Trace_of_smoother_matrix)
      rws <- c("N", 
               "Equivalent_number_of_parameters", 
               "Residual_standard_error", 
               "Trace_of_smoother_matrix")
      cls <- c("Parameter", "Results")
      loe <- round(loe, 2)
      loe <- cbind.data.frame(rws, loe)
      colnames(loe) <- cls
      return(loe)
    } else if (input$regtype == 'lm') {
      j<-summary(lm(data = dt, y ~ x))
      R_squared <- j$r.squared 
      Adj_R_squared <- j$adj.r.squared 
      P <- as.data.frame(j$coefficients)
      T_value_intercept <- P[1,3]
      P_value_intercept <- P[1,4]
      T_value_variable <- P[2,3]
      P_value_variable <- P[2,4]
      Residual_standard_error <- j$sigma
      lem <- rbind.data.frame(R_squared, 
                              Adj_R_squared, 
                              T_value_intercept, 
                              P_value_intercept, 
                              T_value_variable, 
                              P_value_variable, 
                              Residual_standard_error)
      rws <- c("R_squared", 
               "Adj_R_squared", 
               "T_value_intercept", 
               "P_value_intercept", 
               "T_value_variable", 
               "P_value_variable", 
               "Residual_standard_error")
      cls <- c("Parameter", "Results")
      lem <- round(lem, 2)
      lem <- cbind.data.frame(rws, lem)
      colnames(lem) <- cls
      return(lem)
    } else if (input$regtype == 'glm') {
      j<-summary(glm(data = iris, Petal.Length ~ Petal.Width))
      P <- as.data.frame(j$coefficients)
      T_value_intercept <- P[1,3]
      P_value_intercept <- P[1,4]
      T_value_variable <- P[2,3]
      P_value_variable <- P[2,4]
      Residual_standard_error <- j$deviance
      glem <- rbind.data.frame(T_value_intercept, 
                               P_value_intercept, 
                               T_value_variable, 
                               P_value_variable, 
                               Residual_standard_error)
      rws <- c("T_value_intercept", 
               "P_value_intercept", 
               "T_value_variable", 
               "P_value_variable", 
               "Residual_standard_error")
      cls <- c("Parameter", "Results")
      glem <- round(glem, 2)
      glem <- cbind.data.frame(rws, glem)
      colnames(glem) <- cls
      return(glem) 
    } else {
      print("Please use 'loess', 'lm', or 'glm' as regression model.")
    }
    
  })
  
  output$downregt <- downloadHandler(
    filename = function() {"Regression_model.xlsx"},
    content = function(file) {
      write.xlsx(file, x = regtable())
    }
  )

  output$tablehead <- renderTable({
    
    datahead()
    
  })
  
  output$satstest5 <- renderTable({
    
    stats5()
    
  })
  
  stats5 <- reactive({
  
    aa <- sats()
    a5 <- aa[5]
    
    a5
    
  })
  
  output$downdescr <- downloadHandler(
    filename = function() {"Descriptive.xlsx"},
    content = function(file) {
      write.xlsx(file, x = stats5())
    }
  )
  
  output$satstest4 <- renderTable({
    
    stats4()
    
  })
  
  stats4 <- reactive({
    
    aa <- sats()
    a4 <- aa[4]
    
    a4
    
  })
  
  output$downout <- downloadHandler(
    filename = function() {"Outliers.xlsx"},
    content = function(file) {
      write.xlsx(file, x = stats4())
    }
  )
  
  output$satstest <- renderTable({
    
    stats1()
    
  })
  
  stats1 <- reactive({
  
    aa <- sats()
    a1 <- aa[1]
    
    a1
    
  })
  
  output$downmulti <- downloadHandler(
    filename = function() {"Multiple_tests.xlsx"},
    content = function(file) {
      write.xlsx(file, x = stats1())
    }
  )
  
  output$satstest2 <- renderTable({
    
    stats2()
    
  })
  
  stats2 <- reactive({
  
    aa <- sats()
    a2 <- aa[2]
    
    a2
    
  })
  
  output$downttukey <- downloadHandler(
    filename = function() {"Tukey_test.xlsx"},
    content = function(file) {
      write.xlsx(file, x = stats2())
    }
  )
  
  output$satstest3 <- renderTable({
    
    stats3()
    
  })
  
  stats3 <- reactive({
  
    aa <- sats()
    a3 <- aa[3]
    
    a3
    
  })
  
  output$downtdunnett <- downloadHandler(
    filename = function() {"Dunnetts_test.xlsx"},
    content = function(file) {
      write.xlsx(file, x = stats3())
    }
  )
    
# })


  output$user_out <- renderPrint({
    session$userData$user()
  })
  
  observeEvent(input$sign_out, {
    sign_out_from_shiny()
    session$reload()
  })
}

secure_server(server)