# copied directly from rlang file compat-friendly-type.R, commit 962eb3f, on the
# advice of Lionel Henry

friendly_type_of <- function(x, length = FALSE) {
  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- paste(class(x), collapse = "/")
    }
    return(sprintf("a <%s> object", type))
  }

  friendly <- as_friendly_type(typeof(x))

  if (length && rlang::is_vector(x)) {
    friendly <- paste0(friendly, sprintf(" of length %s", length(x)))
  }

  friendly
}

as_friendly_type <- function(type) {
  switch(type,
         logical = "a logical vector",
         integer = "an integer vector",
         numeric = ,
         double = "a double vector",
         complex = "a complex vector",
         character = "a character vector",
         raw = "a raw vector",
         list = "a list",

         NULL = "NULL",
         environment = "an environment",
         externalptr = "a pointer",
         weakref = "a weak reference",
         S4 = "an S4 object",

         name = ,
         symbol = "a symbol",
         language = "a call",
         pairlist = "a pairlist node",
         expression = "an expression vector",

         char = "an internal string",
         promise = "an internal promise",
         ... = "an internal dots object",
         any = "an internal `any` object",
         bytecode = "an internal bytecode object",

         primitive = ,
         builtin = ,
         special = "a primitive function",
         closure = "a function",

         type
  )
}
