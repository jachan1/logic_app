# logic <- read.csv("P:/Documents/NS_Logic/ex_logic_uniq.csv", stringsAsFactors=F)
# vals <- list()
# vals$logic <- read.csv("C:/Users/ch178814/Desktop/c.csv", stringsAsFactors=F)

logic_rec <- function(row, ds, conn=" && "){
  # full_logic <- paste(full_logic, conn, )
  current_logic <- paste0("(", ds$Logic[row], ")")
  if(tolower(ds$Start[row]) == "sof"){
    current_logic
  } else {
    paste(current_logic, conn, get_logic_step(ds$Start[row], ds))
  }
}

get_logic_step <- function(elem, ds) {
  ## need to do the grep here
  logic <- paste0("(", sapply(which(ds$End == tolower(elem)), function(x) logic_rec(x, ds)), ")", collapse=" || ")
  gsub("#[0-9]+", "", logic)
}

# elem <- "rua_1yr"
# dput(vals$logic, "tmp_load")
# ds <- dget("tmp_load")

get_logic <- function(elem, ds) {
  ## need to do the grep here
  ds[] <- sapply(ds, tolower)
  logic <- paste0("(", sapply(which(gsub("#[0-9]+", "", ds$End) == tolower(elem)), function(x) logic_rec(x, ds)), ")", collapse=" || ")
  gsub("#[0-9]+", "", logic)
}

# logic$Logic[logic$Logic == ""] <- T
# 
# get_logic("das28", logic)
# cat(get_logic("ccs", logic))
# cat(get_logic("EOF", logic))
# cat(get_logic("meth", logic))

inspect_graph <- function(ds){
  ig <- graph.data.frame(sapply(ds[1:2], tolower), directed=TRUE, vertices = NULL)
  ig
}

cfrac <- function(x) Reduce(function(u, v) u + 1 / v, x, right = TRUE)
## Continued fraction approximation for pi:
cfrac(c(3, 7, 15, 1, 292))

get_path <- function(ig, ds, start="sof", end){
  nms <- get.vertex.attribute(ig)$name
  if(!end %in% nms){
    allends <- nms[which(gsub("#[0-9]+", "", nms) == end)]
    if(length(allends) == 0){
      return("")
    }
  } else allends <- end
  all_nums <- lapply(allends, function(x) {
    tmp <- all_simple_paths(ig, start, x)
    one_num <- lapply(tmp, function(z) {
      steps <- nms[as.numeric(z)]
      if(tolower(nms[1]) != "sof") print("there's a hanging element")
      logic <- unlist(lapply(2:(length(steps) - 1), function(t) ds$Logic[ds$Start == steps[t] & ds$End == steps[t+1]]))
      list(logic = paste0("(", logic, ")", collapse=" && "),
           paths = paste(steps, collapse=" &#10143; "))
    })
    list(logic=paste0("(", lapply(one_num, "[[", "logic"), ")", collapse=" || "),
         paths=paste0(lapply(one_num, "[[", "paths"), collapse="</br>"))
  })
  list(logic=paste0("(", gsub("#[0-9]+", "", lapply(all_nums, "[[", "logic")), ")", collapse=" || "),
       paths=paste0(lapply(all_nums, "[[", "paths"), collapse="</br>"))
  # vtcs <- get.vertex.attribute(ig)
}