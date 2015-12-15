library(shiny)
library(shinydashboard)
library(shinyFiles)
library(xtable)
require(xlsx)
require(dplyr)
require(stringr)
require(DiagrammeR)
require(networkD3)

get_list_nodes <- function(x, ds){
    endpts <- ds$End[tolower(ds$Start) == tolower(x)]
    
    if(length(endpts) == 1 & gsub("#[0-9]+", "", tolower(endpts[1])) == "eof"){
        list(name=x)
    } else {
        endpts <- endpts[!gsub("#[0-9]+", "", tolower(endpts)) == "eof"]
        list(name=x, children=lapply(endpts, function(y) get_list_nodes(y, ds)))
    }
}

ui <- dashboardPage(
    dashboardHeader(
        title="Logic Helper"
    ),
    dashboardSidebar(
        tags$head(tags$script(src = "enter_button.js")), 
        fileInput('files', 'Choose csv file with columns in order End, Start, Logic', multiple=F,
                  accept='.csv'),
        tags$hr(), 
        tags$h4("Interactive Section"),
        textInput("s_term", "Element Name", value=""),
        actionButton("updateButton", "Update"),
        verbatimTextOutput('directory_note'),
        tags$hr(),
        tags$h4("Automated Section"),
        fileInput('outfile', 'Choose csv with column of elements', multiple=F,
                  accept='.csv')
    ),
    dashboardBody(
        uiOutput("logic_boxes")#,
        # grVizOutput('diagram', width = "100%", height = "1200px")
    )
)

server <- function(input, output, session) {
    library(igraph)
    source("logic_mk_uniq.R")
    
    volumes <- getVolumes() 
    vals <- reactiveValues(logic = data.frame(Start = character(), End = character(), Logic = character()),
                           printed = c(),
                           ig=NULL,
                           logic_boxes = list(box(title="Instructions", solidHeader = TRUE, status = "info", width=12, HTML("Here be instructions"))))
    
    shinyDirChoose(input, 'directory', roots=volumes, session=session, restrictions=system.file(package='base'))
    
    update_boxes <- function(s_term){
        if(nrow(vals$logic) > 0 & !s_term %in% vals$printed & s_term != ""){
            vals$printed <- c(vals$printed, s_term)
            # get_logic(s_term, vals$logic)
            all_paths <- get_path(vals$ig, vals$logic, end = s_term)
            if(length(all_paths) == 1){
                vals$logic_boxes <- c(list(box(title=s_term, solidHeader = TRUE, status = "info", width=12, 
                                               HTML(sprintf("'%s' is not an element in the algorithm", s_term)))),
                                      vals$logic_boxes)
            } else {
                vals$logic_boxes <- c(list(box(title=s_term, solidHeader = TRUE, status = "info", width=12, 
                                               HTML(gsub("\\|\\|","<font color=\"green\"><strong>||</strong></font>", all_paths$logic))),
                                           box(title=sprintf("All Pathways - %s", s_term), solidHeader = TRUE, status = "info", width=12, HTML(all_paths$paths))), vals$logic_boxes)
            }
        }
        vals$logic_boxes
    }
    
    sterm <- eventReactive(input$updateButton, {
        input$s_term
    })
    
    observe({
        s_term_strp <- gsub("#[0-9]+", "", tolower(sterm()))
        isolate(output$logic_boxes <- renderUI(update_boxes(s_term_strp)))
    })
    
    observeEvent(input$files, {
        inFile <- input$files
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".csv", sep=""))
        tmp <- read.csv(paste0(inFile$datapath, ".csv"), stringsAsFactors=F)[1:3]
        
        vals$printed <- c()
        if(nrow(tmp) == 0) {
            vals$logic <- data.frame(Start = character(), End = character(), Logic = character())
            output$directory_note <- renderText("Logic spreadsheet is empty")
            vals$ig <- NULL
        } else {
            ## spreadsheet needs to be laid out as below
            colnames(tmp) <- c("End", "Start", "Logic")
            tmp[] <- sapply(tmp, str_trim)
            not_comp <- which(!tmp$End %in% tmp$Start & !tolower(gsub("#[0-9]+", "", tmp$End)) %in% "eof")
            not_comp <- not_comp[not_comp != 1]
            if(length(not_comp) > 0){
                ## if there are hanging elements throw error
                vals$logic_boxes <- list(box(title="Data not valid", solidHeader = TRUE, status = "info", width=12, 
                                             HTML(sprintf("All elements that appear in the first column should appear in the second column at some point (other than EOF). The elements below appear in the first column but not the second <ul>%s</ul>", 
                                                          paste("<li>", tmp$End[not_comp], "</li>", collapse="\n")))))
                vals$logic <- data.frame(Start = character(), End = character(), Logic = character())
                output$directory_note <- renderText("Logic spreadsheet is empty")
                vals$ig <- NULL
            } else {
                eofs <- tmp$End[tolower(tmp$End) == "eof"]
                tmp$End[tolower(tmp$End) == "eof"] <- paste0(eofs, sprintf("#%06d", 1:length(eofs)))
                vals$logic <- tmp
                vals$ig <- inspect_graph(vals$logic %>% select(Start, End))
                
                nods <- unique(unlist(vals$logic[1:2]))
                nodes_df <- data_frame(nodes=nods, label=nods, style="empty", color="darkgray", shape="diamond")
                edges_df <- vals$logic %>% select(from=Start, to=End, label=Logic) %>% 
                    mutate(label=gsub("\"|\'|\\(|\\)|\\.", "", gsub("\"\"|\'\'", "NA", label)))
                
                test <- create_graph(nodes_df = nodes_df, edges_df = edges_df,
                                     node_attrs = c("fontname = Helvetica"),
                                     edge_attrs = c("color = gray", "penwidth = 4"),
                                     graph_attrs = c("overlap = true",
                                                     "ranksep = 1",
                                                     "outputorder = edgesfirst"))
                # edges_df$label[31] <- ""
                # grViz(test$dot_code)
                # grViz(create_graph(nodes_df = nodes_df, edges_df = edges_df %>% select(-label))$dot_code, engine="twopi")
                
                # diagonalNetwork(List = get_list_nodes("sof", vals$logic), fontSize = 10, opacity = 0.9)
                # list_nodes <- get_list_nodes("sof", vals$logic)
                
                # grVizOutput('diagram', width = "100%", height = "1000px")
                vals$logic$Logic[vals$logic$Logic == ""] <- "delete"
                # output$diagram <- renderGrViz({grViz(test$dot_code)})
                output$sn <- renderSimpleNetwork(simpleNetwork(vals$logic[1:2], fontSize=12, zoom=T, linkDistance=75))
                ## This is giving all of the pathways not the algorithm
                # output$dn <- renderDiagonalNetwork(diagonalNetwork(List = get_list_nodes("sof", vals$logic), fontSize = 10, opacity = 0.9))
                # diagonalNetworkOutput("dn")
                vals$logic_boxes <- c(list(box(title=sprintf("There are %g unique vertices", length(get.vertex.attribute(vals$ig)$name)),
                                               soldHeader=T, status="info", width=12, height=800,
                                               simpleNetworkOutput("sn", height=800))),
                                      vals$logic_boxes)
            }
        }
        output$logic_boxes <- renderUI(vals$logic_boxes)
    })
}

shinyApp(ui, server)
