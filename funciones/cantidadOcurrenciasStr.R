cantidadComas <- function(s, char = ",") {
    s2 <- gsub(char,"",s)
    return (nchar(s) - nchar(s2))
}