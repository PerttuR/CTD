library(stringr)
library(dplyr)

read.cnv <- function(path) {
  lines = readLines(path)
  for(row in seq_along(lines)) {
    if(lines[row] == "*END*") {
      #TODO: check that this split works
      metadata <- na.omit(lines[1:(row - 1)])
      Encoding(metadata) <- "UTF-8"
      metadata <- str_replace_all(metadata, "\xe9", "Θ")
      data <- na.omit(lines[-(1:row)])
      data <- read.table(textConnection(paste(data, collapse="\n")))
      dataNames <- data.frame(str_match(metadata, "name (?<number>[0-9]+) = (?<name>.+): ?(?<comment>.*)"))
      dataNames <- dataNames %>% filter(!is.na(number)) %>% mutate(number = as.numeric(number) + 1) %>% arrange(number)
      names(data) <- dataNames$name
      return(setNames(list(data, metadata), c("data", "metadata")))
    }
  }
  return(setNames(list(lines, NULL), c("metadata", "data")))
}
