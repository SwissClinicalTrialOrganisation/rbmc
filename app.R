#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# options(shiny.reactlog=TRUE) 

library(shiny)
library(flexdashboard)
library(shinydashboard)
library(gt)
library(dplyr)
library(magrittr)
library(tidyr)
library(shinybusy)
library(shinyBS)

texttab <- read.csv("texttable.csv")


# pages ----
instpage <- tabItem(tabName = "inst",
                    h2("General instructions"),
                    "This", tags$b("Risk-Based Monitoring (RBM) Score Calculator"), "was developed by the Monitoring Platform of the Swiss Clinical
Trial Organisation (SCTO) and first released in June 2019. These user instructions enable you to calculate and determine
the recommended monitoring strategy for a particular clinical trial you are planning, by completing a spreadsheet-based
questionnaire. Once you have completed the information related to each risk score, the RBM Score Calculator provides
you with an overall answer. Your user feedback is welcome to help us to improve our calculator.")

studpage <- tabItem(tabName = "stud",
                    h2("General Study Information"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    textInput("studyname", "Study title/identifier"),
                    radioButtons("clino_cat", "ClinO risk category", 
                                 c("A", "B", "C"), inline = TRUE),
                    textInput("au", "Your name")
)

partpage <- tabItem(tabName = "part",
                    h3("I. Participant"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    tags$br(),
                    tags$br(),
                    uiOutput("I_vuln_fullcontrol"),
                    uiOutput("I_emsit_fullcontrol"),
                    uiOutput("I_comp_fullcontrol")
)

desipage <- tabItem(tabName = "desi",
                    h3("II. Design"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    tags$br(),
                    tags$br(),
                    uiOutput("II_comp_fullcontrol"),
                    uiOutput("II_descomp_fullcontrol"),
                    uiOutput("II_primcomp_fullcontrol"),
                    uiOutput("II_primbias_fullcontrol"),
                    uiOutput("II_trtconcom_fullcontrol"),
                    uiOutput("II_proccomp_fullcontrol"),
                    uiOutput("II_withdraw_fullcontrol")
    )
safepage <- tabItem(tabName = "safe",
                    h3("III. Safety"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    tags$br(),
                    tags$br(),
                    uiOutput("III_reaction_fullcontrol"),
                    uiOutput("III_interaction_fullcontrol"),
                    uiOutput("III_cond_fullcontrol")                
                    )
intepage <- tabItem(tabName = "inte",
                    h3("IV. Intervention (IMP, IMD, surgery, etc.)"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    tags$br(),
                    tags$br(),
                    uiOutput("IV_knowledge_fullcontrol"),
                    uiOutput("IV_admin_fullcontrol"),
                    uiOutput("IV_logistics_fullcontrol"),
                    uiOutput("IV_unblind_fullcontrol")
                    )
manapage <- tabItem(tabName = "mana",
                    h3("V. Management"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    tags$br(),
                    tags$br(),
                    uiOutput("V_sites_fullcontrol"),
                    uiOutput("V_tech_fullcontrol"),
                    uiOutput("V_staff_fullcontrol")
                    )
datapage <- tabItem(tabName = "data",
                    h3("VI. Data"),
                    "For each of the following risk factors, indicate whether it is applicable, and if so, it's impact, occurrence and detectability.",
                    tags$br(),
                    tags$br(),
                    uiOutput("VI_datavol_fullcontrol"),
                    uiOutput("VI_crfqual_fullcontrol")
                    )
othepage <- tabItem(tabName = "othe",
                    h3("VII. Other"),
                    "Enter other more trial specific risks here",
                    uiOutput("otherui"),
                    actionButton("addOtherInput","Add a risk"),
                    actionButton("removeOtherInput","Remove a risk")
                    )
repopage <- tabItem(tabName = "repo",
                    h4("Overview of the data entered"),
                    gt_output("report_table"),
                    h4("Risk summary"),
                    gt_output("report_summ"),
                    
                    h4("Risk matrix"),
                    "The outlined box indicates the recommended Risk Based Monitoring strategy for your trial, based on the information you have entered.",
                    gt_output("report_matrix"),
                    
                    "Download a PDF report of your results by clicking the 'Generate report' button below",
                    tags$br(),
                    downloadButton("report", "Generate report"),
                    use_busy_spinner(spin = "fading-circle")
                    )

# dashboard UI ----
ui <- dashboardPage(skin = "red",
    dashboardHeader(title = "SCTO Risk Based Monitoring Score Calculator", titleWidth = 500),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Instructions", tabName = "inst", icon = icon("home")),
            menuItem("Study information", tabName = "stud"),
            menuItem("Participants", tabName = "part"),
            menuItem("Design", tabName = "desi"),
            menuItem("Safety", tabName = "safe"),
            menuItem("Intervention", tabName = "inte"),
            menuItem("Management", tabName = "mana"),
            menuItem("Data", tabName = "data"),
            menuItem("Other", tabName = "othe"),
            menuItem("Report", tabName = "repo")
        ),
        sidebarPanel(
            width = 12,
            tags$style(HTML(".well {
                                 background-color: #222d32;
                                 border-color: #222d32;
                                 }")),
            br(),
            br(),
            br(),
            img(src = "logo.png", align = "center")
        )    
        
    ),
    dashboardBody(
        tabItems(instpage,
                 studpage,
                 partpage,
                 desipage,
                 safepage,
                 intepage,
                 manapage,
                 datapage,
                 othepage,
                 repopage)
    )
)

# server ----
server <- function(input, output, session) {
    
    refs <- texttab$ref
    
    lapply(refs, function(x){
        tmp <- texttab[texttab$ref == x, ]
        uiname <- paste0(x, "_fullcontrol")
        output[[uiname]] <- renderUI({
            fluidPage(
                box(title = tmp$Risk,
                    width = 12,
                    solidHeader = TRUE,
                    tmp$txt,
                    # tags$div(tags$ul(
                    #     tags$li(tmp$bullet1),
                    #     tags$li(tmp$bullet2),
                    #     tags$li(tmp$bullet3)
                    # )),
                    radioButtons(paste0(x, "_appl"), "Applicable", 
                                 c("Yes" = 1, "No" = 0),
                                 selected = 1, inline = TRUE),
                    uiOutput(paste0(x, "_control")),
                    bsTooltip(paste0(x, "_imp"), tmp$bullet1, options = list(container = "body")),
                    bsTooltip(paste0(x, "_occ"), tmp$bullet2),
                    bsTooltip(paste0(x, "_det"), tmp$bullet3),
                    radioButtons(paste0(x, "_note_yn"), "Do you want to add a note?", 
                                 c("Yes" = 1, "No" = 0),
                                 selected = 0, inline = TRUE),
                    uiOutput(paste0(x, "_noteUI"))
                )
            )
        })
    })
    
    
    # inputs
    lapply(refs, function(x){
        uiname <- paste0(x, "_control")
        output[[uiname]] <- renderUI({
            if(input[[paste0(x, "_appl")]] == 1){
                # remove white space after fluidRow - works in RStudio, not firefox
                div(# style = "margin-bottom:-6em; padding: 0px 0px;", 
                    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}", 
                               ".html-widget.gauge {margin-bottom: -100px}"),
                    fluidRow(
                        column(3,
                               sliderInput(paste0(x, "_imp"),
                                           label = "Impact", min = 1,
                                           max = 3, value = 1, step = 1)
                        ),
                        column(3,
                               sliderInput(paste0(x, "_occ"),
                                           label = "Occurance", min = 1,
                                           max = 3, value = 1, step = 1)
                        ),
                        column(3,
                               sliderInput(paste0(x, "_det"),
                                           label = "Detectability", min = 1,
                                           max = 3, value = 1, step = 1)
                        )
                        , column(3,
                               gaugeOutput(paste0(x, "_gauge")))
                    ))
                
            }
        })
    })
    
    # notes box
    lapply(refs, function(x){
        uiname <- paste0(x, "_noteUI")
        output[[uiname]] <- renderUI({
            if(input[[paste0(x, "_note_yn")]] == 1){
                textInput(paste0(x, "_note"), "Note regarding this risk (optional)")
            }
        })
    })
    
    # gauge output
    lapply(refs, function(x){
        uiname <- paste0(x, "_gauge")
        output[[uiname]] <- renderGauge({
            gauge(input[[paste0(x, "_imp")]] * input[[paste0(x, "_occ")]] * input[[paste0(x, "_det")]],
                  min = 1, max = 27, label = "Risk score",
                  sectors = gaugeSectors(c(1,3), c(4,9), c(10,27)))
        })
    })
    
    # other risks ----
    ids <- reactive({
        if (input$addOtherInput == 0) return(NULL)
        
        if (input$addOtherInput == 1){
            output <- 1
        } else {
            if(input$addOtherInput > input$removeOtherInput) {
                output <- 1:(input$addOtherInput-input$removeOtherInput)
            } else return(NULL)
            
        }
        return(output)
    })
    
    tmp <- observeEvent(ids(), 
                        lapply(1:length(ids()), function(x){
                                print(paste("reactive", x))
                                uiname <- paste0("other", x, "_gauge")
                                check_input_imp <- paste0("other", x, "_imp")
                                check_input_occ <- paste0("other", x, "_occ")
                                check_input_det <- paste0("other", x, "_det")
                                print(check_input_imp)
                                print(input[[check_input_imp]])
                                print(uiname)
                                output[[uiname]] <- renderGauge({
                                    gauge(input[[check_input_imp]] * input[[check_input_occ]] * input[[check_input_det]],
                                          min = 1, max = 27, label = "Risk score",
                                          sectors = gaugeSectors(c(1,3), c(4,9), c(10,27)))
                                })
                                # output[[uiname]] <- renderText("FOOOBAR")

                                return(output)
                            }))
    
    
    output$otherui <- renderUI({
        if (is.null(ids())) return(NULL)
        tagList(
            lapply(1:length(ids()),function(i){
                # print(i)
                check_input_imp <- paste0("other", ids()[i], "_imp")
                check_input_occ <- paste0("other", ids()[i], "_occ")
                check_input_det <- paste0("other", ids()[i], "_det")
                check_input_gauge <- paste0("other", ids()[i], "_gauge")
                # print(check_input_imp)
                input_txt <- paste0("other", ids()[i], "_tx")
                if(is.null(input[[input_txt]])){
                    # Create a div that contains 3 new sub divs
                    div(
                        textInput(input_txt, label = "Describe the risk:", 
                                  value = input[[input_txt]]),
                        fluidRow(
                            column(3,
                                   sliderInput(check_input_imp,
                                               label = "Impact", min = 1,
                                               max = 3, value = 1, step = 1),
                            ),
                            column(3,
                                   sliderInput(check_input_occ,
                                               label = "Occurance", min = 1,
                                               max = 3, value = 1, step = 1),
                            ),
                            column(3,
                                   sliderInput(check_input_det,
                                               label = "Detectability", min = 1,
                                               max = 3, value = 1, step = 1)
                            )
                            , column(3,
                                     gaugeOutput(check_input_gauge))
                                     # textOutput(check_input_gauge))
                        )
                    )
                } else {
                    # Create a div that contains 3 existing sub divs
                    div(
                        textInput(input_txt, label = "Describe the risk:",
                                  value = input[[input_txt]]),
                        fluidRow(
                            column(3,
                                   sliderInput(check_input_imp,
                                               label = "Impact", min = 1,
                                               max = 3, value = input[[check_input_imp]], step = 1),
                            ),
                            column(3,
                                   sliderInput(check_input_occ,
                                               label = "Occurrence", min = 1,
                                               max = 3, value = input[[check_input_occ]], step = 1),
                            ),
                            column(3,
                                   sliderInput(check_input_det,
                                               label = "Detectability", min = 1,
                                               max = 3, value = input[[check_input_det]], step = 1)
                            )
                            , column(3,
                                     gaugeOutput(check_input_gauge))
                        )
                    )
                }
                
            })
        )
    })
    
    
    # report page ----
    inputtab <- reactive({
        inputs <- reactiveValuesToList(input)
        inputs <- inputs[!names(inputs) == "sidebarItemExpanded"]
        tmp <- as.data.frame(inputs) %>%
            mutate(across(everything(), as.character)) %>%
            pivot_longer(cols = everything(), 
                         names_to = c("i", "j"), 
                         names_pattern = "(.*)_(imp|occ|det|tx|appl)$") %>%
            filter(!is.na(i)) %>%
            pivot_wider(id_cols = "i", names_from = "j", values_from = "value") %>%
            mutate(across(c("imp", "occ", "appl", "det"), as.numeric),
                   Score = imp*occ*det,
                   appl = case_when(grepl("other", .data$i) ~ 1,
                                    TRUE ~ appl)) %>% 
            filter(grepl(paste0("other(", paste0(ids(), collapse = "|"), ")"), .data$i) | !grepl("other", .data$i))
    })
    
    output$report_table <- render_gt({
        
        tmp <- inputtab()
        # browser()
        tmp2 <- texttab %>%
            full_join(tmp, by = c("ref" = "i")) %>%
            filter(appl > 0)
        
        if("tx" %in% names(tmp)) tmp2 <- tmp2 %>% 
            mutate(Risk = case_when(!is.na(Risk) ~ Risk,
                                    is.na(Risk) ~ tx))
        
        tmp3 <- tmp2 %>%
            mutate(category = case_when(!is.na(category) ~ category,
                                        is.na(category) ~ "VII. Other Risks")) %>%
            select(category, Risk, imp, occ, det, Score) %>%
            rename(Impact = imp,
                   Occurrence = occ,
                   Detectability = det)
         
        tmp3 %>% 
            group_by(category) %>%
            gt() %>%
            data_color(columns = "Score",
                       colors = scales::col_bin(palette = c("green", "orange", "red"),
                                                 bins = c(1, 4, 10, 27)),
                       apply_to = "text") %>% 
            cols_align(columns = 2:5, 
                       align = "center")
    })
    
    summtab <- reactive({
        tmp <- inputtab()
        table(cut(tmp$Score, breaks = c(0, 3, 9, 27), labels = c("Low", "Medium", "High"))) %>% 
            as.data.frame()
        
    })
    
    overall <- reactive({
        tmp <- summtab() %>%
            mutate(x = 1) %>%
            pivot_wider(id_cols = x, 
                        values_from = Freq, names_from = Var1) %>%
            mutate(row = case_when(High > 1 | Medium > 12 ~ 3,
                                   High == 1 | (Medium > 5 & Medium < 13) ~ 2,
                                   Medium < 6 ~ 1))
    })
    
    output$report_summ <- render_gt({
        summtab() %>% 
            rename('Risk Level' = Var1,
                   'Risks in Level' = Freq) %>%
            gt() 
    })
    
    
    output$report_matrix <- render_gt({
        
        tibble::tribble(~'Number of risks', ~'ClinO A', ~'ClinO B', ~'ClinO C',
                        'Less than 6 medium risks, no high risks', 'low-risk', 'low-risk', 'medium-risk',
                        '6 to 12 medium risks or 1 high risk', 'low-risk', 'medium-risk', 'high-risk',
                        'More than 12 medium risks, more than 1 high risk', 'medium-risk', 'high-risk', 'high-risk'
                        ) %>%
            gt() %>%
            cols_align(align = "center") %>% 
            data_color(columns = c('ClinO A', 'ClinO B', 'ClinO C'),
                       colors = scales::col_factor(palette = c("green", "yellow", "orange"),
                                                   levels = c('low-risk', 'medium-risk', 'high-risk'))) %>%
            tab_style(locations = cells_body(rows = overall()$row,
                                             columns = paste("ClinO", input$clino_cat)),
                      style = list(cell_borders(weight = px(5), color = "black"), 
                                   cell_text(weight = "bolder"))
                      )
        
        
    })

    
    output$report <- downloadHandler(
        filename = "report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            
            tempdir <- tempdir()
            tempReport <- file.path(tempdir, "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            file.copy("www/logo.png", file.path(tempdir, "logo.png"), overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(input = input,
                           texttab = texttab)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            
            show_modal_spinner(text = "Compiling PDF",
                               spin = "folding-cube")
            rmarkdown::render(input = tempReport, 
                              output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            remove_modal_spinner()
        }
    )
}

# Run the application 
# shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
shinyApp(ui = ui, server = server)

