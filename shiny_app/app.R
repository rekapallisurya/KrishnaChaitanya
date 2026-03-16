app_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
	dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE))
} else if (basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)) == "shiny_app") {
	normalizePath(getwd(), winslash = "/", mustWork = FALSE)
} else {
	normalizePath(file.path(getwd(), "shiny_app"), winslash = "/", mustWork = FALSE)
}

project_root <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE)

source(file.path(app_dir, "ui.R"))
source(file.path(app_dir, "server.R"))

shinyApp(ui = ui, server = server)
