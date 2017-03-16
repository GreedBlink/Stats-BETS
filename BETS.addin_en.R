#' BETS search 
#' 
#' An interface for searching time series with possibility 
#' to extract the data in different extensions.
#' @import shiny DT
#' @import miniUI
#' @import rstudioapi
#' 
#' @export


BETS.addin_en <- function(){
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    gadgetTitleBar("BETS Search Addin"),
    
    miniTabstripPanel(
    
      miniTabPanel(  
      icon = icon("search"),
      miniContentPanel(
    
      # Create a new Row in the UI for selectInputs
      
    
      
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
      
      #para exportacao dos dados
     
      ), uiOutput("allColsSection") 
    
    
    ),#fechando a primeira tab (pesquisa de series)
    #fechando o conteudo da minitab
    miniTabPanel(
      icon = icon("save"),
      miniContentPanel(
        
        
      )
      
    )
    
    
    
    ) # minitabpanel
  )  
  
  server <- function(input, output, session) {
    
    # Filter data based on selections
   
   
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
        
        
        
    },options = list(pageLength = 5, dom = 'tip')))
    
    
  }
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}


BETS.addin_en()
