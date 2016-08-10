#' @importFrom shiny h4 fillRow htmlOutput renderUI textInput actionButton req
#' HTML observeEvent stopApp dialogViewer runGadget uiOutput actionLink icon
#' reactiveValues reactive includeCSS includeScript tags img addResourcePath
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom rstudioapi insertText
#' @importFrom utils download.file
gif_Addin <- function() {
  addResourcePath("rsc",
                  system.file("gadgets/gif_Addin", package = "giphyr"))
  ui <- miniPage(
    tags$head(
      includeCSS(system.file("gadgets/gif_Addin/app.css", package = "giphyr"))
    ),
    gadgetTitleBar("Add GIFs"),
    miniContentPanel(
      fillRow(flex = c(1, 1, 7, 3, 3), height = "45px",
              uiOutput("back_button"), uiOutput("forward_button"),
              uiOutput("gif_search_box"),
              uiOutput("download_gif_button"), uiOutput("insert_gif_button")),
      uiOutput("preview"),
      htmlOutput("space_holder"),
      tags$div(class="giphylogo",
               img(src=file.path("rsc/giphy.png"), width = 100))
    ),
    includeScript(system.file("gadgets/gif_Addin/app.js", package = "giphyr"))
  )

  server <- function(input, output, session) {
    output$gif_search_box <- renderUI({
      textInput("search_text", label = NULL,
                placeholder = "Search all the GIFs",
                width = "98%")
    })

    output$download_gif_button <- renderUI({
      actionButton("download_gif", label = "Download",
                   icon = icon("download"),
                   width = "95%", class="btn-primary")
    })

    output$insert_gif_button <- renderUI({
      actionButton("insert_gif", label = "Insert to Rmd",
                   icon = icon("file-text-o"),
                   width = "95%", class="btn-success")
    })

    output$back_button <- renderUI({
      actionButton("pagination_back", label = NULL, width = "95%",
                   icon = icon("angle-double-left"))
    })

    output$forward_button <- renderUI({
      actionButton("pagination_forward", label = NULL, width = "95%",
                   icon = icon("angle-double-right"))
    })

    values <- reactiveValues(offset = 0)

    observeEvent(input$pagination_back, {
      if (values$offset >= 10){
        values$offset = values$offset - 10
      }else{
        if(values$offset > 0){
          values$offset = 0
        }
      }
    })

    observeEvent(input$pagination_forward, {
        values$offset = values$offset + 10
    })

    observeEvent(input$search_text, {
      values$offset = 0
    })

    preview_gifs <- reactive({
      req(input$search_text)
      out <- suppressWarnings(
        gif_search(input$search_text,
                   img_format = c("fixed_height_small", "original"),
                   offset = values$offset))
      if(is.null(out)){return(NULL)}
      out <- out[, c("id", "slug", "fixed_height_small", "original")]
      return(out)
    })

    output$preview <- renderUI({
      req(preview_gifs())
      apply(preview_gifs(), 1, function(x){
        actionLink(x[1], title=x[2], label = NULL, class = "gifpreview",
                   tags$img(src=x[3]))
      })
    })

    output$space_holder <- function(){
      if(is.null(input$search_text))return(NULL)
      if(input$search_text != "")return(NULL)
      HTML(
        '<div class="spaceHolder"><p>Step 1. Search any keywords</p>',
        '<p>Step 2. Select a GIF you like</p><p>Step 3. Download it or directly ',
        'insert that to your Rmarkdown.</p></div>'
      )
    }

    observeEvent(input$done, {
      invisible(stopApp())
    })

    observeEvent(input$download_gif, {
      if(!is.null(input$clickedgif) & !is.null(input$search_text)){
        dir.create("img", showWarnings = F)
        dl_file_name <- paste0(
          "img/", search_query_parser(input$search_text),
          "_", input$clickedgif, ".gif"
        )
        status <- try(download.file(
          preview_gifs()$original[preview_gifs()$id == input$clickedgif],
          dl_file_name
        ), silent = T)
      }
    })

    observeEvent(input$insert_gif, {
      if(!is.null(input$clickedgif) & !is.null(input$search_text)){
        dir.create("img", showWarnings = F)
        dl_file_name <- paste0(
          "img/", search_query_parser(input$search_text),
          "_", input$clickedgif, ".gif"
        )
        status <- try(download.file(
          preview_gifs()$original[preview_gifs()$id == input$clickedgif],
          dl_file_name
        ), silent = T)
        rstudioapi::insertText(
          paste0('![', search_query_parser(input$search_text), "](",
                 dl_file_name, ")\n")
        )
      }
    })
  }

  viewer <- dialogViewer("Add Crossref Citations", width = 800, height = 800)
  runGadget(ui, server, viewer = viewer)
}
