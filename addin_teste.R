



exemplo  = function(){
  
  ui<- miniUI::miniPage(
    miniTabstripPanel(
      
      # Tab 1 - choose any colour
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          
          fluidRow(
          column(4,
                 textInput("description", 
                           "Description:", c("Search")
                 ) 
          ),
          
          
          column(2,
                 selectInput("periodicity",
                             "Periodicity:",
                             c("All","M","A","Q","W","D")
                             
                 )       
          ),
          column(3,
                 textInput("source",
                           "Source:",
                           c("All")
                 )       
          ),
          # Create a new row for the table.
          
          DT::dataTableOutput("table")
          
          
        )
        )
      ),
      
      # Tab 2 - choose an R colour similar to a colour you choose
      miniTabPanel(
        "Visualitation",
        icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput("table_full")
        )
      ),
      
      miniTabPanel(
        "Export",
        icon = icon("save"),
        miniContentPanel(
         fluidRow(
          column(3, strong(span("Code of TS")),
                span(verbatimTextOutput('code'))
                ),
          column(3,
                textInput("name","file name","Dados")
                ), 
         column(6,
                textInput("local","Save file",c(getwd()))
                )
          ),
         
         fluidRow(
           column(4,span("")),
           column(4,span("")),
           column(4,span(""))
           
           
         ),
         fluidRow(
           column(4,span("")),
           column(4,span("")),
           column(4,span(""))
         )
        )
      ) 
    
    
  )
  )  
   
  
server <- function(input, output, session) {
    
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    req(input$description) # tratamento para o input da descricao
    req(input$source)      # tratamento para o input da fonte
    
    
    
    nomes = c("Code","Description","Unit","Periodicity","Start","Last Value","Source")
    
    data.addin <- BETS.search(description ="*",view=F)
    names(data.addin) = nomes
    
    
    
    if(input$description != "Search"){
      print(input$description)
      req(input$description) # tratamento para o input da descricao
      data.addin <- BETS.search(description = input$description,view=F)
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
        
      }else{
        data.addin 
      }
      
    }else{
      data.addin <- BETS.search(description ="*",view=F)
    }
    
    
    if(input$periodicity!= "All"){
      req(input$periodicity)      # tratamento para o input da fonte
      #data.addin <- BETS.search(description = input$description,view=F)
      data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
      }else{
        data.addin 
      }
    }
    
    if(input$source!= "All"){
      req(input$source)      # tratamento para o input da fonte
      #data.addin <- BETS.search(description = input$description,view=F)
      data.addin <- data.addin[data.addin$source == input$source,]
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
      }else{
        data.addin 
      }
    }
    
    print(data.addin)
    
    if(is.character(data.addin)){
      data.addin = BETS.search(description="*",view=F)
    }else{
      data.addin 
    }
    
    names(data.addin) = nomes
    data.addin  
    
    
    
  },options = list(pageLength = 5, dom = 'tip'),selection = 'single'))
  
  
  
  output$table_full<- DT::renderDataTable(DT::datatable({
    
    req(input$description) # tratamento para o input da descricao
    req(input$source)      # tratamento para o input da fonte
    
    
    
    nomes = c("Code","Description","Unit","Periodicity","Start","Last Value","Source")
    
    data.addin <- BETS.search(description ="*",view=F)
    names(data.addin) = nomes
    
    
    
    if(input$description != "Search"){
      print(input$description)
      req(input$description) # tratamento para o input da descricao
      data.addin <- BETS.search(description = input$description,view=F)
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
        
      }else{
        data.addin 
      }
      
    }else{
      data.addin <- BETS.search(description ="*",view=F)
    }
    
    
    if(input$periodicity!= "All"){
      req(input$periodicity)      # tratamento para o input da fonte
      #data.addin <- BETS.search(description = input$description,view=F)
      data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
      }else{
        data.addin 
      }
    }
    
    if(input$source!= "All"){
      req(input$source)      # tratamento para o input da fonte
      #data.addin <- BETS.search(description = input$description,view=F)
      data.addin <- data.addin[data.addin$source == input$source,]
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
      }else{
        data.addin 
      }
    }
    
    print(data.addin)
    
    if(is.character(data.addin)){
      data.addin = BETS.search(description="*",view=F)
    }else{
      data.addin 
    }
    
    names(data.addin) = nomes
    data.addin  
    
    
    
  },options = list(pageLength = 8),selection = 'single'))
  
  
  
  output$code = req(renderPrint(input$table_cell_clicked$value))
  
    
}
  
  
  
  
  
  
    
    
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}

exemplo()

