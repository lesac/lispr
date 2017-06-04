# Implements Peter Norvig's scheme interpreter lispy in R
# For his python interpreter see http://www.norvig.com/lispy.html

# to mimic pythons pop on lists we create our own stack
create_twosided_stack <- function (data) {
    stack <- new.env()
    stack$.Data <- data
    stack$pop <- function () {
        elt <- .Data[length(.Data)]
        .Data <<- .Data[-length(.Data)]
        elt
    }
    stack$rpop <- function () {
        elt <- .Data[1]
        .Data <<- .Data[-1]
        elt
    }
    stack$rpeek <- function () {
        .Data[1]
    }
    stack$len <- function () {
        length(.Data)
    }
    environment(stack$pop) <- as.environment(stack)
    environment(stack$rpop) <- as.environment(stack)
    environment(stack$rpeek) <- as.environment(stack)
    environment(stack$len) <- as.environment(stack)
    stack
}

create_env <- function (lst) {
    envir <- new.env()
    envir$.env <- lst
    envir$set <- function (name, value) {
        envir$.env[name] <<- value
    }
    envir$get <- function (name) {
        envir$.env[name]
    }
    envir$list <- function () {
        envir$.env
    }
    environment(envir$set) <- as.environment(envir)
    environment(envir$get) <- as.environment(envir)
    environment(envir$list) <- as.environment(envir)
    envir
}

tokenize <- function(chars) {
    tokens <- Filter(function (x) nchar(x) > 0,
                     strsplit(gsub('(', ' ( ', gsub(')', ' ) ', chars, fixed = TRUE), fixed = TRUE), " ", fixed = TRUE)[[1]])
    create_twosided_stack(tokens)
}

atom <- function(token) {
    if (suppressWarnings(!is.na(as.numeric(token)))) {
        return(as.numeric(token))
    } else {
        return(token)
    }
}

reader <- function(tokens) {
    if (tokens$len() == 0) {
        stop("Unexpected EOF while reading")
    }
    token <- tokens$rpop()
    if (token == '(') {
        L <- list()
        while (tokens$rpeek() != ')') {
            L <- c(L, reader(tokens))
        }
        tokens$rpop() # pop off ')'
        return(list(L))
    } else if (token == ')') {
        stop("Unexpected )")
    } else {
        return(atom(token))
    }
}

parse <- function(program) {
    # read scheme expression from string
    reader(tokenize(program))[[1]]
}

global_env <- create_env(list(
    '+' = `+`,
    '-' = `-`,
    '*' = `*`))

issymbol <- function(x) {
    is.character(x)
}

islist <- function(x) {
    is.list(x)
}

isnumber <- function(x) {
    is.numeric(x)
}

scheme_eval <- function(x, envir = global_env) {
    if (issymbol(x)) {
        envir$get(x)
    } else if(!islist(x)) {
        x
    } else if (x[[1]] == "define") {
        var <- x[[2]]
        expr <- x[[3]]
        envir$set(var, scheme_eval(expr, envir))
    } else {
        proc <- scheme_eval(x[[1]], envir)[[1]]
        args <- unlist(lapply(x[-1], scheme_eval), use.names = FALSE)
        do.call(proc, as.list(args))
    }
}

# finally our small repl
repl <- function () {
    while (TRUE) {
        input <- readline("lisp.R > ")
        val <- scheme_eval(parse(input))
        if (!is.null(val)) {
            print(val)
        }
    }
}
