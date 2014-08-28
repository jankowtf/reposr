#' Signal Condition
#'
#' @description 
#' Signals custom conditions.   
#' 
#' @details
#' The returned object will always inherit from class \code{condition}.
#' The value of \code{condition} will always be the first entry/entries of the  
#' object's class table.  
#' Depending on the setting of \code{type}, the it also inherits from 
#' either \code{message}, \code{warning}, \code{error} or \code{custom}.
#' If no value for \code{condition} is specified, then depending on the setting
#' of \code{type} respective default values are used: \code{"DefaultMessage"},
#' \code{"DefaultWarning"}, \code{"DefaultError"} and \code{"DefaultCondition"}.
#' 
#' @param condition \strong{Signature argument}.
#' @param msg \code{\link{character}}. Dimension: any.
#' 		Condition message. Named components are transformed into the following
#' 		structure: \code{* <item>: <item-value>}, (e.g. "Item 1"="some message"
#' 		will become "* Item 1: some message" in the actual condition message.
#' 		The first element will be treated as the message's header.
#' 		Line numbers are automatically added beginning from the second line
#' 		(\code{[<number>] some message} or 
#' 		\code{[<number>] * <item>: <item-value>}).
#' @param call \code{\link{call}}. Dimension: 1.
#' 		System call one frame up the calling stack. This will correspond to 
#' 		the closure that is spanned by the function/method that calls this 
#' 		method when a condition is to be signaled.
#' 		Default: \code{sys.call(-1)}. 
#' 		There should be no need to change this default value.
#' @param type \code{\link{character}}. Dimension: 1.
#' 		Selector for the condition type.
#' 		Choices: \code{"message"}, \code{"warning"}, \code{"error"} or 
#' 		\code{"condition"}.  
#' 		Default: \code{"message"}.
#' @param signal \code{\link{logical}}. Dimension: 1.
#' 		\code{TRUE} (default) means the condition is signaled right away, 
#' 		\code{FALSE} means the condition object is returned only.
#' @return Object of the class(es) that have been provided by \code{condition} and
#' 		also inheriting from \code{\link{condition}} and depending on
#' 		\code{type} also from \code{\link{message}}, \code{\link{warning}} or
#' 		\code{\link{error}}.
#' @author Janko Thyson \email{janko.thyson@@rappster.de}
#' @references \url{http://www.rappster.de/}
#' @example inst/examples/signalCondition.R
#' @seealso \code{\link[base]{condition}}
#' @export
setGeneric(
    name="signalCondition",
    signature=c("condition"),
    def=function(
        condition,
        msg=NULL,
        call=sys.call(-2),
        type=c("message", "warning", "error", "condition"),
        signal=TRUE
    ) {
    standardGeneric("signalCondition")       
	}
)

#' @param condition \code{\link{character}}. Dimension: any.
#' 		Name(s) of the condition(es) that will be passed to the condition object's
#' 		class table.  
#' @describeIn signalCondition
#' @export
setMethod(
    f="signalCondition", 
    signature=signature(
        condition="character"
    ), 
    definition=function(
        condition,
        msg,
        call,
        type,
        signal
    ) {

    ## Match arg //
    type <- match.arg(type, 
        c("message", "warning", "error", "condition")
    )

    pkg <- "rapp2"
    
    ## Message line break at end //
    if (length(msg)) {
        msg <- sapply(msg, function(ii) {
            if (!length(grep("\\n$", ii, perl=TRUE))) {
                ii <- paste0(ii, "\n")    
            }     
        })
        if (length(msg) > 1) {
            msg[1] <- gsub("\\n", " //\n", msg[1])
            msg[2:length(msg)] <- paste0(
                "[", seq(along=msg[2:length(msg)]), "] ", 
                "* ", names(msg[2:length(msg)]), ": ", msg[2:length(msg)])
        }
    } else {
        msg <- ""
    }

    ## Add call to message //
    msg <- c(
        paste0(Sys.time(), "/"),
        ifelse(!is.null(pkg), paste0(pkg, "/"), ""),
        ifelse(!is.null(call), paste0(call, "/"), ""),
        condition, "> \n", msg
    )
    msg <- paste(msg, collapse="")
    msg <- gsub("\\* : ", "", msg)
    
    ## Sub sub condition class //
    subsub <- switch(
        type,
        message=c(type, "condition"),
        warning=c(type, "condition"),
        error=c(type, "condition"),
        condition="condition"
    )
  
    ## Create condition object //
    out <- structure(
        class=unique(c(condition, subsub)),
        list(message=msg, call=call)
    )
    if (signal) {
        switch(
            type,
            message=message(out),
            warning=warning(out),
            error=stop(out),
            condition=signalCondition(out)
        )    
    }
    
    ## Return //
    return(out)
    
    }
)

#' @param condition \code{\link{missing}}.
#' @describeIn signalCondition
#' @export
setMethod(
    f="signalCondition", 
    signature=signature(
        condition="missing"
    ), 
    definition=function(
        condition,
        msg,
        call,
        type,
        signal
    ) {

    ## Match arg //
    type <- match.arg(type, 
        c("message", "warning", "error", "condition")
    )

    ## Default condition classes //
    if (missing(condition)) {
        condition <- switch(
            type,
            message="DefaultMessage",
            warning="DefaultWarning",
            error="DefaultError",
            condition="DefaultCondition"
        )    
    }

    ## Dispatch //
    out <- signalCondition(
        condition=condition,
        call=call,
        type=type,
        signal=signal
    )
    
    ## Return //
    return(out)
    
    }
)
