exception Soupault_error of string

let soupault_error s = raise (Soupault_error s)
