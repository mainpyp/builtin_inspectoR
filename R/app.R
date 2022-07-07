source("global.R")
source("interactive_table_functions.R")

all_datasets <- as.list(as.data.table(data()$results)$Item)
all_datasets <- sapply(as.list(as.data.table(data()$results)$Item), str_split, pattern=" ")
all_datasets <- sapply(all_datasets,"[[", 1)

ui <- fluidPage(
    selectInput("Data", label = "Select Datatable",
              choices = all_datasets, selected = "mtcars"),
    selectInput("n_cont", label = "Select Continous Threshold",
                choices = 3:35, selected = 19),
    checkboxInput("verbose", "Show if data not list"),
    uiOutput("modals"),
    DTOutput("table")
)

server <- function(input, output, session){
      observeEvent(input$Data, {

        data_id <- input$Data
        threshold <- input$n_cont

        dat <- tryCatch(
          {
            scraped <- get( data_id  )
            type_scraped <- typeof(scraped)
            
            as.data.table(scraped)
            
          },
          warning= function(cond) {
            message(paste0(data_id, " not found."))
            message(cond)
            return()
          })
        
        type_of_dat <- typeof(get(data_id))
        
        tryCatch(
          {
            as.data.table(dat)
          },
          warning= function(cond) {
            shinyalert(sprintf("%s not convertable to data.table", data_id))
            message(cond)
            return()
          })
        
        if(type_of_dat != "list" && input$verbose) {
          shinyalert(sprintf("%s not a list, but of type %s", data_id, type_of_dat))
        }

        if(length(dat) == 0) {
          shinyalert(sprintf("%s is empty", data_id))
          return()
        }
        if(length( colnames(dat) ) > 1) {
          colnames(dat) <- gsub("-", "_", colnames(dat))
        }

        buttons <- create_buttons(dat, file = data_id)

        output[["table"]] <- DT::renderDataTable({
          create_datatable(dat, buttons)
        })

        # modals ####
        output[["modals"]] <- renderUI({
          create_modals(dat, file = data_id, threshold)
        })


        # plots in modals ####
        for(i in 1:ncol(dat)){
          local({
            ii <- i
            output[[paste0("plot",ii)]] <- renderPlot({
              create_modal_plots(input, dat, file=data_id, index=ii, threshold)
            })
          })
        }
      })
}

shinyApp(ui, server)
