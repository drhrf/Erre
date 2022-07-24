library(shiny)
library(tidyverse)
library(gplots)
library(bslib)
library(plotly)
library(reshape2)
library(stats)
library(psych)
library(kableExtra)
library(rstatix)
library(DescTools)
library(xlsx)

if(!require(shiny)) {install.packages("shiny"); library(shiny)}
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(gplots)) {install.packages("gplots"); library(gplots)}
if(!require(bslib)) {install.packages("bslib"); library(bslib)}
if(!require(plotly)) {install.packages("plotly"); library(plotly)}
if(!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if(!require(stats)) {install.packages("stats"); library(stats)}
if(!require(psych)) {install.packages("psych"); library(psych)}
if(!require(kableExtra)) {install.packages("kableExtra"); library(kableExtra)}
if(!require(rstatix)) {install.packages("rstatix"); library(rstatix)}
if(!require(DescTools)) {install.packages("DescTools"); library(DescTools)}
if(!require(xlsx)) {install.packages("xlsx"); library(xlsx)}

shinyUI(fluidPage(

  titlePanel(" "), 
  theme = bslib::bs_theme(bootswatch = "minty",  
                          secondary = 'lightblue', base_font = 'Calibri'),

    sidebarLayout(
        sidebarPanel(
          h4("Settings"),
          p(""),
          tabsetPanel(
            tabPanel("File upload", 
                     p(""),
              fileInput(inputId = 'tbl',
                        label = 'Upload file here (.xlsx/.csv/.tsv):', 
                        accept = c(".csv", ".tsv", "xlsx", "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"),
                        buttonLabel = 'Upload',
                        placeholder = 'No file'), 
              numericInput(inputId = "n", 
                           label = "Number of rows", 
                           value = 3, 
                           min = 1, 
                           step = 1)),
        
            tabPanel("General", 
                     p(""),
                     textInput(inputId = 'title', 
                               label = 'Plot title (empty for none)', 
                               value = 'Default plot'), 
                     textInput(inputId = 'xlab', 
                               label = 'X axis label (empty for none)', 
                               value = 'X'), 
                     textInput(inputId = 'ylab', 
                               label = 'Y axis label (empty for none)', 
                               value = 'Y'), 
                     textInput(inputId = 'border', 
                               label = 'Border color (NA for none)', 
                               value = 'black'), 
                     textInput(inputId = 'fill', 
                               label = 'Fill color (NA for none)', 
                               value = 'gray50'), 
                     textInput(inputId = 'point', 
                               label = 'Mean dot color (NA for none)', 
                               value = 'red'), 
                     textInput(inputId = 'jitter', 
                               label = 'Jitter dot color (NA for none)', 
                               value = 'black'),
                     textInput(inputId = 'ord', 
                               label = 'Ordered columns (y/n)', 
                               value = 'y'), 
                     numericInput(inputId = 'width',
                                  label = 'Plot width',
                                  value = 12, 
                                  step = 0.1), 
                     numericInput(inputId = 'height',
                                  label = 'Plot height',
                                  value = 8, 
                                  step = 0.1), 
                     numericInput(inputId = 'resol',
                                  label = 'Plot resolution',
                                  value = 300, 
                                  step = 10)), 
            
            tabPanel("Histogram",
                     p(""),
                     numericInput(inputId = 'nbr',
                                  label = 'Column number',
                                  value = 1,
                                  min = 1,
                                  step = 1),
                     numericInput(inputId = 'brks',
                                  label = 'Number of breaks',
                                  value = 30,
                                  min = 5,
                                  step = 5), 
                     textInput(inputId = 'density', 
                               label = 'Density line color (NA for none)', 
                               value = 'black'), 
                     downloadButton(outputId = "downhist", 
                                    label = 'Download plot')),
            
            tabPanel("Barplot",
                     p(""),
                     textInput(inputId = 'funbar', 
                               label = 'Barplot statistics', 
                               value = 'mean'),
                     textInput(inputId = 'errcol', 
                               label = 'Error bar color (NA for none)', 
                               value = 'black'), 
                     downloadButton(outputId = "downbar", 
                                    label = 'Download plot')),
            
            tabPanel("Boxplot",
                     p(""),
                     textInput(inputId = 'viol', 
                               label = 'Violin color (NA for none)', 
                               value = 'gray10'), 
                     downloadButton(outputId = "downbox", 
                                    label = 'Download plot')),
            
            tabPanel("Regression",
                     p(""),
                     textInput(inputId = 'regtype', 
                               label = 'Regression model', 
                               value = 'loess'),
                     textInput(inputId = 'regcolor', 
                               label = 'Regression line color (NA for none)', 
                               value = 'black'),
                     numericInput(inputId = 'setrue', 
                                  label = 'Standard error (1/0)', 
                                  value = 1, 
                                  min = 0, 
                                  max = 1, 
                                  step = 1),
                     numericInput(inputId = 'pointsz', 
                                  label = 'Point size', 
                                  value = 2, 
                                  step = .1),
                     downloadButton(outputId = "downreg", 
                                    label = 'Download plot')),
            
            tabPanel("Statistics",
                     p(""),
                     textInput(inputId = 'ttstga', 
                               label = 'Group A name (required)', 
                               value = 'Petal.Length'), 
                     textInput(inputId = 'ttstgb', 
                               label = 'Group B name (required)', 
                               value = 'Petal.Width'),
                     numericInput(inputId = 'conflvl',
                                  label = 'Confidence level',
                                  value = .95, 
                                  min = .01, 
                                  max = .99, 
                                  step = .01), 
                     downloadButton(outputId = "downtukey", 
                                    label = 'Download plot')),
            
            tabPanel("Help", 
                     p(""), 
                     helpText("Most settings are available in the 'General' section."),
                     p(""), 
                     helpText("Regression models take 'loess', 'glm', and 'lm' inputs."),
                     p(""),
                     helpText("Barplot statistics take 'mean', 
                              'median', 'sd', and 'var' inputs."),
                     p(""),
                     helpText("Before saving a plot, 
                              please provide the desired resolution, 
                              height, and width in the 'General' section.")))),
        
        mainPanel(img(src = "fig.png", height = 120, width = 90),
                  h3("Data analyzer - Hercules Freitas, Ph.D."),
                  p("."),
                  textOutput('text'),
                  p("."),
          
          tabsetPanel(
            tabPanel("Dataframe",
                     p(""),
                     tableOutput('tablehead'),
                     p(".")),
            
            tabPanel("Histogram", 
                     p(""),
                     plotlyOutput('histplot'),
                     p(".")),
            
            tabPanel("Boxplot", 
                     p(""),
                     plotlyOutput('boxplot'),
                     p(".")),
            
            tabPanel("Barplot", 
                     p(""),
                     plotlyOutput('barplot'),
                     p(".")),
            
            tabPanel("Regression plot", 
                     p(""),
                     plotlyOutput('regres'),
                     p(""),
                     tableOutput('resgre'),
                     p(".")),
            
            tabPanel("Statistics",
                     p("."),
                     textOutput('currtest'),
                     p("."),
                     ("Descriptive statistics"),
                     p(""),
                     tableOutput('satstest5'),
                     downloadButton(outputId = "downdescr", 
                                    label = 'Download table'),
                     p(""),
                     ("Outliers and extreme values"),
                     p(""),
                     tableOutput('satstest4'),
                     downloadButton(outputId = "downout", 
                                    label = 'Download table'),
                     p(""),
                     ("Normality test, analisys of variance, t-test"),
                     p(""),
                     tableOutput('satstest'),
                     downloadButton(outputId = "downmulti", 
                                    label = 'Download table'),
                     p(""),
                     ("Tukey test"),
                     p(""),
                     tableOutput('satstest2'),
                     downloadButton(outputId = "downttukey", 
                                    label = 'Download table'),
                     p(""),
                     plotlyOutput('citukey'),
                     p(""),
                     ("Dunnett's test"),
                     p(""),
                     tableOutput('satstest3'),
                     downloadButton(outputId = "downtdunnett", 
                                    label = 'Download table'),
                     p(".")))
        ))
    )
)
