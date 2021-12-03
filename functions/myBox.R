
# add function for box format
myBox <- function(..., title = NULL, footer = NULL, status = NULL,
                  solidHeader = FALSE, color, background, width = 6,
                  height = NULL, collapsible = FALSE, collapsed = FALSE) {
    
    boxClass <- "box"
    if (solidHeader || !is.null(background)) {
        boxClass <- paste(boxClass, "box-solid")
    }
    if (!is.null(status)) {
        validateStatus(status)
        boxClass <- paste0(boxClass, " box-", status)
    }
    if (collapsible && collapsed) {
        boxClass <- paste(boxClass, "collapsed-box")
    }
    
    style <-paste0("color: ", color, "; background-color: ", background, ";")
    if (!is.null(height)) {
        style <- paste0(style, " height: ", validateCssUnit(height))
    }
    
    titleTag <- NULL
    if (!is.null(title)) {
        titleTag <- h3(class = "box-title", title)
    }
    
    collapseTag <- NULL
    if (collapsible) {
        buttonStatus <- status %OR% "default"
        
        collapseIcon <- if (collapsed) "plus" else "minus"
        
        collapseTag <- div(class = "box-tools pull-right",
                           tags$button(class = paste0("btn btn-box-tool"),
                                       `data-widget` = "collapse",
                                       shiny::icon(collapseIcon)
                           )
        )
    }
    
    headerTag <- NULL
    if (!is.null(titleTag) || !is.null(collapseTag)) {
        headerTag <- div(class = "box-header", 
                         style = paste0("color: ", color, ";"),
                         titleTag,
                         collapseTag
        )
    }
    
    div(class = if (!is.null(width)) paste0("col-sm-", width),
        div(style = style,
            headerTag,
            div(class = "box-body", ...),
            if (!is.null(footer)) div(class = "box-footer", footer)
        )
    )
}