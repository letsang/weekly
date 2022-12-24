#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Shiny
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
# Web
library(shinyjs)
library(shinyscreenshot)
library(htmltools)
# Data Wrangling
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(aweek)
library(stringr)
# Data Visualization
library(ggplot2)
library(plotly)
# Table
library(DT)
library(formattable)
library(RCurl)
# Office365
library(Microsoft365R)

### NETWORK ###
### https://frankzheng.me/en/2022/06/run-shiny-apps-locally/ ###
x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
port <- 8888

### ONE DRIVE ###
odb <- get_business_onedrive()
odb$download_file(src = "DB LIFESTYLE_2022.xlsx", "db\\DB LIFESTYLE_2022.xlsx", overwrite = TRUE)

### DB ###
inv <- read_excel("db\\DB LIFESTYLE_2022.xlsx", "INV", skip = 8)
sales <- read_excel("db\\DB LIFESTYLE_2022.xlsx", "SALES", skip = 7)
lifestyle_sales <- sales %>% filter(str_detect(Department...4,'LIFESTYLE'))
lifestyle_stock <- inv %>% filter(str_detect(Department...20, 'LIFESTYLE'))

### FUNCTION TO APPEND MIN 10 COLUMNS FOR BESTSELLERS TABLES ###
append <- function(x){
  while (nrow(x) < 10){
    x <- x %>% add_row(Picture = '<img src="https://getmedia.pdi.keringapps.com/image/pijqmiaszv/000000000000000" height="200"></img>', SMC = "-")
  }
  return(x)
}

############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = htmltools::tags$img(src = "https://www.ysl.com/on/demandware.static/-/Library-Sites-Library-SLP/default/dw86be354f/images/logo.svg", height = "20px")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
             htmltools::tags$style(HTML(".display.dataTable.no-footer > thead {
                                            background-color: black;
                                            color: white;
                                            }
                              
                                        .h2, h2 {
                                            font-family: Helvetica;
                                            font-size: 24px;
                                            font-weight: 600;
                                            letter-spacing: -1px;
                                            margin-top: 0px;
                                            margin-bottom: 0px;
                                            }
                              
                                        .h3, h3 {
                                            font-family: Helvetica;
                                            font-size: 16px;
                                            font-weight: 600;
                                            letter-spacing: -1px;
                                            margin-top: 0px;
                                            margin-bottom: 0px;
                                            }
                              
                                        .h5, h5 {
                                            font-family: Helvetica;
                                            font-size: 8px;
                                            font-weight: bold;
                                            letter-spacing: -1px;
                                            margin-top: 0px;
                                            margin-bottom: 0px;
                                        }
                                        
                                        p {
                                            font-family: Helvetica;
                                            font-size: 14px;
                                            font-weight: bold;
                                            margin: 0;
                                            display: block;
                                            margin-block-start: 0px;
                                            margin-block-end: 0px;
                                            margin-inline-start: 0px;
                                            margin-inline-end: 0px;
                                        }    
                                        
                                        .skin-black .main-header .logo {
                                            background-color: #fff;
                                            color: #fff;
                                            border-bottom: 0 solid transparent;
                                            border-right: 0 solid transparent;
                                        }
                              
                                        .skin-black .main-header .navbar {
                                        background-color: #fff;
                                        }
                              
                                        .skin-black .main-header .navbar .sidebar-toggle {
                                        color: #fff;
                                        display: none;
                                        }
                              
                                        .content-wrapper, .right-side {
                                        min-height: 100%;
                                        background-color: #fff;
                                        }")),
             fluidRow(
               column(12, dateInput("date", label = uiOutput("weeknum"), value = as.Date(paste(max(lifestyle_sales$YEAR), max(lifestyle_sales[lifestyle_sales$YEAR==max(lifestyle_sales$YEAR),]$`WEEK #`),1), format="%Y %U %u"), min = "2019-06-01", max = as.Date(paste(max(lifestyle_sales$YEAR), max(lifestyle_sales[lifestyle_sales$YEAR==max(lifestyle_sales$YEAR),]$`WEEK #`),1), format="%Y %U %u"), weekstart = 1, daysofweekdisabled = as.integer(c(0,2,3,4,5,6)))),
               column(6, checkboxGroupInput("status", h3("LIFESTYLE"), choices = unique(lifestyle_sales$COV), selected = unique(lifestyle_sales$COV), inline = TRUE)),
               column(6, checkboxGroupInput("cat", h3("CATALOGUE"), choices = unique(lifestyle_sales$`RELEASE YEAR`), selected = unique(lifestyle_sales$`RELEASE YEAR`), inline = TRUE)),
               column(12, checkboxGroupInput("categ", h3("CATEGORY"), choices = unique(lifestyle_sales$`FINAL SUB DEB`), selected = unique(lifestyle_sales$`FINAL SUB DEB`), inline = TRUE)),
               
               ### LIFESTYLE I + LIFESTYLE II + LIFESTYLE III
               ############################# VALUE BOXES #############################
               column(2, box(h3("TOTAL"), width = NULL, height = 100, background = "black", uiOutput("total_lifestyle"))),
               column(2, box(h3("RD PARIS"), width = NULL, height = 100, background = "black", uiOutput("paris_lifestyle"))), 
               column(2, box(h3("RD LA"), width = NULL, height = 100, background = "black", uiOutput("la_lifestyle"))), 
               column(2, box(h3("RD.COM"), width = NULL, height = 100, background = "black", uiOutput("ecom_lifestyle"))), 
               column(2, box(h3("WECHAT"), width = NULL, height = 100, background = "black", uiOutput("wechat_lifestyle"))), 
               column(2, box(h3("OTHER"), width = NULL, height = 100, background = "black", uiOutput("other_lifestyle"))), 
               
               ############################# BEST SELLERS #############################
               column(12, align="center", h2("TOTAL")),
               column(12, DT::dataTableOutput("best_total_lifestyle")),
               column(12, htmltools::tags$br()),
               column(12, align="center", h2("RD PARIS")),
               column(12, DT::dataTableOutput("best_paris_lifestyle")),
               column(12, htmltools::tags$br()),
               column(12, align="center", h2("RD LA")),
               column(12, DT::dataTableOutput("best_la_lifestyle")),
               column(12, htmltools::tags$br()),
               column(12, align="center", h2("RD.COM")),
               column(12, DT::dataTableOutput("best_ecom_lifestyle")),
               column(12, htmltools::tags$br()),
               column(12, align="center", h2("WECHAT")),
               column(12, DT::dataTableOutput("best_wechat_lifestyle")),
               column(12, htmltools::tags$br()),
               
               ############################# WHAT'S NEW #############################
               # column(12, align="left", h3("WHAT'S NEW ?")),
               # HTML("<iframe height='100%' width='100%' frameborder='no' src='https://www.ysl.com/fr-fr/displayname-archives-rivedroite?archivesPage=2'> </iframe>")
             )
           )
  )
)

############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

server <- function(input, output) {
  
  values <- reactiveValues(
    week0 = max(lifestyle_sales[lifestyle_sales$YEAR==max(lifestyle_sales$YEAR),]$`WEEK #`),
    year0 = max(lifestyle_sales$YEAR),
  )
  
  observe({
    values$week0 <- week(input$date)
  })
  
  output$weeknum <- renderUI({
    h2(paste0("WEEK ", values$week0))
  })
  
  lifestyle_by_region <- reactive({
    tmp <- lifestyle_sales %>%
      filter(`COV` %in% input$status) %>%
      filter(`RELEASE YEAR` %in% input$cat) %>%
      filter(`WEEK #` %in% week(input$date)) %>%
      filter(`FINAL SUB DEB` %in% input$categ) %>%
      group_by(`YEAR`,`WEEK #`,`RD REGION`) %>%
      summarise(Sales=round(sum(`Sales FP (Value)`,na.rm=TRUE), digits = 0)) %>%
      spread(`YEAR`,Sales)
    
    tmp[is.na(tmp)] <- 0
    tmp %>% mutate(
      "vs.LY" = round(((`2022`/`2021`)-1)*100, digits = 0),
      "Diff" = round((`2022`-`2021`)/1000, digits = 0)
    )
  })
  
  lifestyle_total <- reactive({
    tmp <- lifestyle_sales %>%
      filter(`COV` %in% input$status) %>%
      filter(`RELEASE YEAR` %in% input$cat) %>%
      filter(`WEEK #` %in% week(input$date)) %>%
      filter(`FINAL SUB DEB` %in% input$categ) %>%
      group_by(`YEAR`,`WEEK #`) %>%
      summarise(Sales=round(sum(`Sales FP (Value)`,na.rm=TRUE), digits = 0)) %>%
      spread(`YEAR`,Sales)
    
    tmp[is.na(tmp)] <- 0
    tmp %>% mutate(
      "vs.LY" = round(((`2022`/`2021`)-1)*100, digits = 0),
      "Diff" = round((`2022`-`2021`)/1000, digits = 0)
    )
  })
  
  lifestyle_other <- reactive({
    tmp <- lifestyle_sales %>%
      filter(!(`RD REGION` %in% c("RD PARIS", "RD LA", "RD.COM", "WECHAT"))) %>%
      filter(`COV` %in% input$status) %>%
      filter(`RELEASE YEAR` %in% input$cat) %>%
      filter(`WEEK #` %in% week(input$date)) %>%
      filter(`FINAL SUB DEB` %in% input$categ) %>%
      group_by(`YEAR`,`WEEK #`) %>%
      summarise(Sales=round(sum(`Sales FP (Value)`,na.rm=TRUE), digits = 0)) %>%
      spread(`YEAR`,Sales)
    
    tmp[is.na(tmp)] <- 0
    tmp %>% mutate(
      "vs.LY" = round(((`2022`/`2021`)-1)*100, digits = 0),
      "Diff" = round((`2022`-`2021`)/1000, digits = 0)
    )
  })
  
  ############################# VALUE BOXES #############################
  
  output$total_lifestyle <- renderUI({
    tmp <- lifestyle_total()
    tagList(
      if (length(tmp$'2022') > 0) {
        p(paste0(format(round(tmp$'2022', digits = 0), big.mark=" "), " €"), style = "text-align: right")
      } else {""},
      if (tmp$'vs.LY' >= 0) {
        p(paste0("+", tmp$'vs.LY',"% vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'vs.LY',"% vs LY"), style = "color:red; text-align: right")},
      if (tmp$'Diff' >= 0) {
        p(paste0("+", tmp$'Diff'," K€ vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'Diff'," K€ vs LY"), style = "color:red; text-align: right")}
    )
  })
  
  output$paris_lifestyle <- renderUI({
    tmp <- lifestyle_by_region() %>% filter(`RD REGION` == "RD PARIS")
    tagList(
      if (length(tmp$'2022') > 0) {
        p(paste0(format(round(tmp$'2022', digits = 0), big.mark=" "), " €"), style = "text-align: right")
      } else {""},
      if (tmp$'vs.LY' >= 0) {
        p(paste0("+", tmp$'vs.LY',"% vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'vs.LY',"% vs LY"), style = "color:red; text-align: right")},
      if (tmp$'Diff' >= 0) {
        p(paste0("+", tmp$'Diff'," K€ vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'Diff'," K€ vs LY"), style = "color:red; text-align: right")}
    )
  })
  
  output$la_lifestyle <- renderUI({
    tmp <- lifestyle_by_region() %>% filter(`RD REGION` == "RD LA")
    tagList(
      if (length(tmp$'2022') > 0) {
        p(paste0(format(round(tmp$'2022', digits = 0), big.mark=" "), " €"), style = "text-align: right")
      } else {""},
      if (tmp$'vs.LY' >= 0) {
        p(paste0("+", tmp$'vs.LY',"% vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'vs.LY',"% vs LY"), style = "color:red; text-align: right")},
      if (tmp$'Diff' >= 0) {
        p(paste0("+", tmp$'Diff'," K€ vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'Diff'," K€ vs LY"), style = "color:red; text-align: right")}
    )
  })
  
  output$ecom_lifestyle <- renderUI({
    tmp <- lifestyle_by_region() %>% filter(`RD REGION` == "RD.COM")
    tagList(
      if (length(tmp$'2022') > 0) {
        p(paste0(format(round(tmp$'2022', digits = 0), big.mark=" "), " €"), style = "text-align: right")
      } else {""},
      if (tmp$'vs.LY' >= 0) {
        p(paste0("+", tmp$'vs.LY',"% vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'vs.LY',"% vs LY"), style = "color:red; text-align: right")},
      if (tmp$'Diff' >= 0) {
        p(paste0("+", tmp$'Diff'," K€ vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'Diff'," K€ vs LY"), style = "color:red; text-align: right")}
    )
  })
  
  output$wechat_lifestyle <- renderUI({
    tmp <- lifestyle_by_region() %>% filter(`RD REGION` == "WECHAT")
    tagList(
      if (length(tmp$'2022') > 0) {
        p(paste0(format(round(tmp$'2022', digits = 0), big.mark=" "), " €"), style = "text-align: right")
      } else {""},
      if (tmp$'vs.LY' >= 0) {
        p(paste0("+", tmp$'vs.LY',"% vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'vs.LY',"% vs LY"), style = "color:red; text-align: right")},
      if (tmp$'Diff' >= 0) {
        p(paste0("+", tmp$'Diff'," K€ vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'Diff'," K€ vs LY"), style = "color:red; text-align: right")}
    )
  })
  
  output$other_lifestyle <- renderUI({
    tmp <- lifestyle_other()
    tagList(
      if (length(tmp$'2022') > 0) {
        p(paste0(format(round(tmp$'2022', digits = 0), big.mark=" "), " €"), style = "text-align: right")
      } else {""},
      if (tmp$'vs.LY' >= 0) {
        p(paste0("+", tmp$'vs.LY',"% vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'vs.LY',"% vs LY"), style = "color:red; text-align: right")},
      if (tmp$'Diff' >= 0) {
        p(paste0("+", tmp$'Diff'," K€ vs LY"), style = "color:lime; text-align: right")
      } else {p(paste0(tmp$'Diff'," K€ vs LY"), style = "color:red; text-align: right")}
    )
  })
  
  ############################# BEST SELLERS #############################
  
  best <- reactive({
    tmp <- lifestyle_sales %>%
      filter(`COV` %in% input$status) %>%
      filter(`RELEASE YEAR` %in% input$cat) %>%
      filter(`YEAR` %in% year(input$date)) %>%
      filter(`WEEK #` %in% week(input$date)) %>%
      filter(`FINAL SUB DEB` %in% input$categ)
  })
  
  output$best_total_lifestyle <- DT::renderDataTable({
    tmp <- best() %>%
      group_by(SMC) %>% summarise(Qty = sum(`Sales FP (Qty)`, na.rm = TRUE), Value = sum(`Sales FP (Value)`, na.rm = TRUE)) %>%
      mutate(Picture = paste('<img src="https://getmedia.pdi.keringapps.com/image/pijqmiaszv/',SMC,'" height="200"></img>',sep=""),
             Value = currency(round(as.numeric(Value),0),"EUR",digits=0,big.mark=" ",sep=" "),
             Qty = as.numeric(Qty),
             Catalogue = 0,
             Status = 0,
             AWS = 0,
             WOC = 0,
             SOH = 0) %>%
      arrange(desc(Value)) %>%
      append() %>%
      head(10)
    
    for (i in 1:nrow(tmp)) {
      tmp$Catalogue[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$`RELEASE YEAR`[1]
      tmp$Status[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$COV[1]
      tmp$AWS[i] <- sum(lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i] &
                                          lifestyle_sales$`WEEK #` %in% c(values$week0, values$week0-1, values$week0-2, values$week0-3) &
                                          lifestyle_sales$`YEAR` == values$year0, "Sales FP (Qty)"])/4
      tmp$SOH[i] <- sum(filter(inv, SMC == tmp$SMC[i])$`OH W/ PFP (QTY)`)
      tmp$WOC[i] <- max(round(tmp$SOH[i]/tmp$AWS[i],0), 0)
    }
    
    tmp <- select(tmp, Status, Catalogue, Picture, SMC, Qty, Value, AWS, SOH, WOC)
    datatable(t(tmp), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, info = FALSE, dom='t', ordering=F, columnDefs = list(list(className = 'dt-center', targets = "_all"))), escape = FALSE, colnames = c("1","2","3","4","5","6","7","8","9","10"))
  })
  
  output$best_paris_lifestyle <- DT::renderDataTable({
    tmp <- best() %>% filter(`RD REGION` == "RD PARIS") %>%
      group_by(SMC) %>% summarise(Qty = sum(`Sales FP (Qty)`, na.rm = TRUE), Value = sum(`Sales FP (Value)`, na.rm = TRUE)) %>%
      mutate(Picture = paste('<img src="https://getmedia.pdi.keringapps.com/image/pijqmiaszv/',SMC,'" height="200"></img>',sep=""),
             Value = currency(round(as.numeric(Value),0),"EUR",digits=0,big.mark=" ",sep=" "),
             Qty = as.numeric(Qty),
             Catalogue = 0,
             Status = 0,
             AWS = 0,
             WOC = 0,
             SOH = 0) %>%
      arrange(desc(Value)) %>%
      append() %>%
      head(10)

    for (i in 1:nrow(tmp)) {
      tmp$Catalogue[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$`RELEASE YEAR`[1]
      tmp$Status[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$COV[1]
      tmp$AWS[i] <- sum(lifestyle_sales[lifestyle_sales$`RD REGION` == "RD PARIS" &
                                          lifestyle_sales$SMC == tmp$SMC[i] &
                                          lifestyle_sales$`WEEK #` %in% c(values$week0, values$week0-1, values$week0-2, values$week0-3) &
                                          lifestyle_sales$`YEAR` == values$year0, "Sales FP (Qty)"])/4
      tmp$SOH[i] <- sum(filter(inv, `Location...10` %in% c(30343,30344) & SMC == tmp$SMC[i])$`OH W/ PFP (QTY)`)
      tmp$WOC[i] <- max(round(tmp$SOH[i]/tmp$AWS[i],0), 0)
    }
    
    tmp <- select(tmp, Status, Catalogue, Picture, SMC, Qty, Value, AWS, SOH, WOC)
    datatable(t(tmp), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, info = FALSE, dom='t', ordering=F, columnDefs = list(list(className = 'dt-center', targets = "_all"))), escape = FALSE, colnames = c("1","2","3","4","5","6","7","8","9","10"))
  })
  
  output$best_la_lifestyle <- DT::renderDataTable({
    tmp <- best() %>% filter(`RD REGION` == "RD LA") %>%
      group_by(SMC) %>% summarise(Qty = sum(`Sales FP (Qty)`, na.rm = TRUE), Value = sum(`Sales FP (Value)`, na.rm = TRUE)) %>%
      mutate(Picture = paste('<img src="https://getmedia.pdi.keringapps.com/image/pijqmiaszv/',SMC,'" height="200"></img>',sep=""),
             Value = currency(round(as.numeric(Value),0),"EUR",digits=0,big.mark=" ",sep=" "),
             Qty = as.numeric(Qty),
             Catalogue = 0,
             Status = 0,
             AWS = 0,
             WOC = 0,
             SOH = 0) %>%
      arrange(desc(Value)) %>%
      append() %>%
      head(10)
    
    for (i in 1:nrow(tmp)) {
      tmp$Catalogue[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$`RELEASE YEAR`[1]
      tmp$Status[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$COV[1]
      tmp$AWS[i] <- sum(lifestyle_sales[lifestyle_sales$`RD REGION` == "RD LA" &
                                          lifestyle_sales$SMC == tmp$SMC[i] &
                                          lifestyle_sales$`WEEK #` %in% c(values$week0, values$week0-1, values$week0-2, values$week0-3) &
                                          lifestyle_sales$`YEAR` == values$year0, "Sales FP (Qty)"])/4
      tmp$SOH[i] <- sum(filter(inv, `Location...10` %in% c(33014) & SMC == tmp$SMC[i])$`OH W/ PFP (QTY)`)
      tmp$WOC[i] <- max(round(tmp$SOH[i]/tmp$AWS[i],0), 0)
    }
    
    tmp <- select(tmp, Status, Catalogue, Picture, SMC, Qty, Value, AWS, SOH, WOC)
    datatable(t(tmp), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, info = FALSE, dom='t', ordering=F, columnDefs = list(list(className = 'dt-center', targets = "_all"))), escape = FALSE, colnames = c("1","2","3","4","5","6","7","8","9","10"))
  })
  
  output$best_ecom_lifestyle <- DT::renderDataTable({
    tmp <- best() %>% filter(`RD REGION` == "RD.COM") %>%
      group_by(SMC) %>% summarise(Qty = sum(`Sales FP (Qty)`, na.rm = TRUE), Value = sum(`Sales FP (Value)`, na.rm = TRUE)) %>%
      mutate(Picture = paste('<img src="https://getmedia.pdi.keringapps.com/image/pijqmiaszv/',SMC,'" height="200"></img>',sep=""),
             Value = currency(round(as.numeric(Value),0),"EUR",digits=0,big.mark=" ",sep=" "),
             Qty = as.numeric(Qty),
             Catalogue = 0,
             Status = 0,
             AWS = 0,
             WOC = 0,
             SOH = 0) %>%
      arrange(desc(Value)) %>%
      append() %>%
      head(10)
    
    for (i in 1:nrow(tmp)) {
      tmp$Catalogue[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$`RELEASE YEAR`[1]
      tmp$Status[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$COV[1]
      tmp$AWS[i] <- sum(lifestyle_sales[lifestyle_sales$`RD REGION` == "RD.COM" &
                                          lifestyle_sales$SMC == tmp$SMC[i] &
                                          lifestyle_sales$`WEEK #` %in% c(values$week0, values$week0-1, values$week0-2, values$week0-3) &
                                          lifestyle_sales$`YEAR` == values$year0, "Sales FP (Qty)"])/4
      tmp$SOH[i] <- sum(filter(inv, `RD REGION` %in% "RD.COM" & SMC == tmp$SMC[i])$`OH W/ PFP (QTY)`)
      tmp$WOC[i] <- max(round(tmp$SOH[i]/tmp$AWS[i],0), 0)
    }
    
    tmp <- select(tmp, Status, Catalogue, Picture, SMC, Qty, Value, AWS, SOH, WOC)
    datatable(t(tmp), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, info = FALSE, dom='t', ordering=F, columnDefs = list(list(className = 'dt-center', targets = "_all"))), escape = FALSE, colnames = c("1","2","3","4","5","6","7","8","9","10"))
  })
  
  output$best_wechat_lifestyle <- DT::renderDataTable({
    tmp <- best() %>% filter(`RD REGION` == "WECHAT") %>%
      group_by(SMC) %>% summarise(Qty = sum(`Sales FP (Qty)`, na.rm = TRUE), Value = sum(`Sales FP (Value)`, na.rm = TRUE)) %>%
      mutate(Picture = paste('<img src="https://getmedia.pdi.keringapps.com/image/pijqmiaszv/',SMC,'" height="200"></img>',sep=""),
             Value = currency(round(as.numeric(Value),0),"EUR",digits=0,big.mark=" ",sep=" "),
             Qty = as.numeric(Qty),
             Catalogue = 0,
             Status = 0,
             AWS = 0,
             WOC = 0,
             SOH = 0) %>%
      arrange(desc(Value)) %>%
      append() %>%
      head(10)
    
    for (i in 1:nrow(tmp)) {
      tmp$Catalogue[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$`RELEASE YEAR`[1]
      tmp$Status[i] <- lifestyle_sales[lifestyle_sales$SMC == tmp$SMC[i],]$COV[1]
      tmp$AWS[i] <- sum(lifestyle_sales[lifestyle_sales$`RD REGION` == "WECHAT" &
                                          lifestyle_sales$SMC == tmp$SMC[i] &
                                          lifestyle_sales$`WEEK #` %in% c(values$week0, values$week0-1, values$week0-2, values$week0-3) &
                                          lifestyle_sales$`YEAR` == values$year0, "Sales FP (Qty)"])/4
      tmp$SOH[i] <- sum(filter(inv, `RD REGION` %in% "WECHAT" & SMC == tmp$SMC[i])$`OH W/ PFP (QTY)`)
      tmp$WOC[i] <- max(round(tmp$SOH[i]/tmp$AWS[i],0), 0)
    }
    
    tmp <- select(tmp, Status, Catalogue, Picture, SMC, Qty, Value, AWS, SOH, WOC)
    datatable(t(tmp), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, info = FALSE, dom='t', ordering=F, columnDefs = list(list(className = 'dt-center', targets = "_all"))), escape = FALSE, colnames = c("1","2","3","4","5","6","7","8","9","10"))
  })

}

############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

# Run the application 
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = FALSE, port = port, host = ip)
