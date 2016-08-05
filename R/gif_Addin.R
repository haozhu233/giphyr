gif_Addin <- function() {
  ui <- miniPage(
    gadgetTitleBar("Find GIFs"),
    miniContentPanel(
      fillRow(flex = c(7, 3), height = "45px",
              uiOutput("gif_search_box"), uiOutput("gif_search_button")),
      htmlOutput("preview")
    )
  )

  server <- function(input, output, session) {
    output$gif_search_box <- renderUI({
      textInput("search_text", label = NULL,
                placeholder = "Search All the GIFs", width = "98%")
    })

    output$gif_search_button <- renderUI({
      actionButton("search_button", label = "Search",
                   icon = icon("search"),
                   width = "100%", class="btn-primary")
    })

    preview_gifs <- reactive({
      req(input$search_text)
      out <- gif_search(input$search_text, img_format = "fixed_height_small")
      out <- out[, c("id", "fixed_height_small")]
      return(out)
    })

    output$preview <- function(){
      req(input$search_text)
      HTML(
        paste0(paste0("<img src='", preview_gifs()$fixed_height_small, "'>"),
               collapse = "\n")
      )
    }


    # observeEvent(input$search_button, {
    #   bib_to_write <- suppressWarnings(try(cr_cn(dois = input$entered_dois),
    #                                        silent = TRUE))
    #   if(class(bib_to_write) != "try-error"){
    #     if(!"crossref.bib" %in% list.files()){file.create("crossref.bib")}
    #     bib_to_write <- correct_bib(bib_to_write)
    #     write(paste0(bib_to_write, "\n"), "crossref.bib", append = T)
    #     updateTextInput(session, "entered_dois", value = "")
    #     preview_message$added <- 1
    #     preview_message$error <- 0
    #   }else{
    #     updateTextInput(session, "entered_dois", value = "")
    #     preview_message$added <- 0
    #     preview_message$error <- 1
    #   }
    # })

    observeEvent(input$done, {
      invisible(stopApp())
    })
  }

  viewer <- dialogViewer("Add Crossref Citations", width = 600, height = 800)
  runGadget(ui, server, viewer = viewer)
}
