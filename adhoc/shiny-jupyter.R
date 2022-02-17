# installation
# rpymat::configure_conda()
# rpymat::add_jupyter()

# Launch jupyter instance
ip <- '127.0.0.1'
token <- raveio::raveio_getopt(key = "jupyter_token", default = "asdasda")
url <- sprintf("http://%s:8888/jupyter/lab?token=%s", ip, token)
rpymat::jupyter_launch(open_browser = F, host = ip, token = token, async = TRUE)

# Launch shiny, embed jupyter in shiny
library(shiny)
ui <- fluidPage(
  shiny::tags$iframe(src = url, width = "100%", height = '800px'),
  shiny::actionButton("asd", "asdasdads")
)

server <- function(input, output, session) {

}

shinyApp(ui, server, options = list(launch.browser = TRUE, host = ip))

rstudioapi::viewer(url)
