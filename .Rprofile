# Source the user's own .Rprofile if it exists
if (file.exists("~/.Rprofile")) {
    source("~/.Rprofile")
}

# Formatting for styler at the project level:
# Use VS Code config for indent spaces;
# Use = instead of <-
options(languageserver.formatting_style = function(options) {
    style <- styler::tidyverse_style(indent_by = options$tabSize)
    style$token$force_assignment_op <- NULL
    style
})
