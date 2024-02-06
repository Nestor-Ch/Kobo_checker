visualise.survey <- function(tool.survey, left.question, right.question, excluded_questions = c("calculate", "note")) {
  questions <- get.relevanse.question(tool.survey, c("note"))
  questions <- questions[!(questions$type %in% excluded_questions),]
  
  if (!left.question %in% questions$ref.name) return("left question not found")
  if (!right.question %in% questions$ref.name) return("right question not found")
  
  left_row_id <- which(questions$ref.name == left.question)
  right_row_id <- which(questions$ref.name == right.question)
  if (left_row_id >= right_row_id) return("left question must be before right question")
  
  group.questions <- questions[left_row_id:right_row_id, ]
  
  nodes <- data.frame()
  links <- data.frame()
  
  for (i in 1:nrow(group.questions)) {
    df <- data.frame(
      id = group.questions$ref.name[i],
      question = group.questions$ref.name[i],
      stringsAsFactors = FALSE
    )
    nodes <- bind_rows(nodes, df)
    
    parents <- parents.names(group.questions, group.questions$ref.name[i])
    
    if (!is.logical(parents) & !is.null(parents)) {
      for (j in 1:length(parents)) {
        links <- bind_rows(links, data.frame(
          from = parents[j],
          to = group.questions$ref.name[i],
          relevant = group.questions$relevant[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  links <- tibble(links)
  
  vis.nodes <- nodes
  vis.links <- links
  
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- FALSE
  vis.nodes$title  <- vis.nodes$question
  vis.nodes$label  <- vis.nodes$question
  vis.nodes$size   <- 20
  vis.nodes$borderWidth <- 1
  
  # vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  vis.links$width <- 5
  vis.links$color <- "gray"
  vis.links$arrows <- "middle"
  vis.links$smooth <- FALSE
  vis.links$shadow <- FALSE
  vis.links$title <- vis.links$relevant
  
  visnet <- visNetwork(vis.nodes, vis.links)
  
  visnet <- visnet %>%
    visInteraction(dragNodes = FALSE) %>%
    visEdges(
      length = 300
    )
  
  return(visOptions(visnet, highlightNearest = TRUE, nodesIdSelection = TRUE))
}
