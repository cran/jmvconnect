
#' @importFrom utils capture.output
#' @importFrom evaluate evaluate new_output_handler
#' @importFrom jmvcore Options Group Image Preformatted
eval <- function(script, data, root) {
    
    eval.env <- new.env()
    
    if ( ! missing(data))
        eval.env$data <- data
    
    env <- new.env()
    env$count <- 1
    options <- Options$new()
    
    if (missing(root))
        root <- Group$new(options, title="Results")
    
    text_handler <- function(object) {
        
        results <- Preformatted$new(options, paste(env$count))
        env$count <- env$count + 1
        env$last <- NULL
        root$add(results)
        
        if (inherits(object, 'ResultsElement')) {
            object$print()
            value <- object$asString()
        }
        else {
            value <- capture.output(object)
        }
        
        results$setContent(value)
        
        object
    }
    
    source_handler <- function(value) {
        value <- trimws(value$src)
        if (value == '')
            return()
        
        if (is.null(env$last) || ! inherits(env$last, 'Preformatted')) {
            results <- Preformatted$new(options, paste(env$count))
            root$add(results)
            env$count <- env$count + 1
        }
        else {
            results <- env$last
        }
        
        value <- paste0('> ', value)
        
        content <- results$content
        if (content != '')
            content <- paste0(content, '\n', value)
        else
            content <- value
        
        results$setContent(content)
        env$last <- results
    }
    
    graphics_handler <- function(plot) {
        results <- Image$new(
            options=options,
            name=paste(env$count),
            renderFun='.render')
        root$add(results)
        results$setState(plot)
        env$count <- env$count + 1
        env$last <- NULL
    }
    
    handler <- new_output_handler(
        source=source_handler,
        value=text_handler,
        graphics=graphics_handler)
    
    evaluate(
        input=script,
        envir=eval.env,
        output_handler=handler,
        stop_on_error=2)
    
    root
}

