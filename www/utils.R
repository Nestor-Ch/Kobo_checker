get.relevanse.question <- function(tool.survey) {
  questions <- data.frame(ref.name = ifelse(!grepl("group", tool.survey$type), tool.survey$name, NA))
  questions <- na.omit(questions)
  
  questions <- questions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(parent.survey = sapply(ref.name, function(x) {
      relevance.condition <- filter(tool.survey, name == x)$relevant %>%
        ifelse(length(.) == 0, NA, .)
      pattern <- "\\$\\{([^\\}]+)\\}"
      result <- gsub("\\$\\{|\\}", "", str_extract_all(relevance.condition, pattern)[[1]])
      relevance_str <-paste(result, collapse = ' ')
      return(ifelse(relevance_str == "", NA, relevance_str))
    })) %>%
    dplyr::mutate(relevant = sapply(ref.name, function(x) {
      relevance.condition <- filter(tool.survey, name == x)$relevant %>%
        ifelse(length(.) == 0, NA, .)}))
  
  get_relevant_surveys <- function(ref_name, df) {
    if (is.na(ref_name)) {
      return(NA)
    } else {
      parent_surveys <- unlist(strsplit(ref_name, " "))
      relevant_surveys <- df$ref.name[df$parent.survey %in% parent_surveys]
      result <- stringr::str_trim(paste(relevant_surveys, collapse = ' '))
      if (result == "") {
        return(NA)
      } else {
        return(result)
      }
    }
  }
  
  questions <- questions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(relevant.survey = get_relevant_surveys(ref.name, questions))
  
  return(questions)
}


children.names <- function(questions, question.name) {
  parse.parents <- function(questions.row) {
    if (is.na(questions.row$parent.survey)) {
      return(NA)
    }
    parents <- unlist(strsplit(questions.row$parent.survey, " ")[[1]])
    parents <- unique(parents)
    return(parents)
  }
  if (nrow(questions[questions$ref.name == question.name, ]) == 0) {
    return(c())
  }
  parents <- parse.parents(questions[questions$ref.name == question.name, ])
  return(parents)
}

build_tree <- function(questions, question.name) {
  children.list <- list()
  
  child_names <- children.names(questions, question.name)
  relevant.formula <- questions[questions$ref.name == question.name, ]$relevant
  
  if (length(child_names) == 0 || (length(child_names) == 1 && child_names[1] == "NA")) {
    return(NULL)
  }
  formulas.list <- list()
  for (child_name in child_names) {
    child_tree <- build_tree(questions, child_name)
    children.list <- append(children.list, list(child_tree))
    formulas.list <- append(formulas.list, list(relevant.formula))
    
  }
  if (length(children.list) > 0) {
    main.df <- tibble(
      name = child_names,
      children = children.list,
      formulas = formulas.list
    )
  } else {
    main.df <- tibble(
      name = child_names
    )
  }
  
  return(main.df)
}

parse.formula_full <- function(formula) {
  pattern_names <- "\\$\\{([^\\}]+)\\}"
  pattern_values <- "'(.*?)'"
  conditions <- gregexpr("\\b(and|or)\\b", formula, ignore.case = TRUE)
  conditions <- regmatches(formula, conditions)[[1]]
  questions_names <- gsub("\\$\\{|\\}", "", str_extract_all(formula, pattern_names)[[1]])
  questions_values <- gsub("'", "", str_extract_all(formula, pattern_values)[[1]])
  
  result <- list()
  for (i in 1:length(questions_names)) {
    result <- append(result, list(questions_names[i], "==", questions_values[i]))
    if (i != length(questions_names)) {
      result <- append(result, conditions[i])
    }
  }
  
  result <- paste(result, collapse = ' ')
  return(result)
}

build_matrix <- function(questions, question.name, depth) {
  child.names <- children.names(questions, question.name)
  res.df <- data.frame()
  for (child in child.names) {
    formula <- questions[questions$ref.name == question.name, ]$relevant
    if (!is.na(formula)) {
      df <- data.frame(child = question.name, parent = child, formula=parse.formula_full(formula), depth=depth)
      res.df <- rbind(res.df, df)
      df <- build_matrix(questions, child, depth + 1)
      res.df <- rbind(res.df, df)
    }
  }
  row.names(res.df) <- NULL
  res.df <- res.df %>%
    arrange(depth)
  
  res.df$depth <- as.integer(res.df$depth)
  res.df <- res.df[!duplicated(res.df), ]
  return(res.df)
}


parse.formula <- function(input_string,return='value') {
  pattern_names <- "\\$\\{([^\\}]+)\\}"
  pattern_values <- "'(.*?)'"
  conditions <- gregexpr("\\b(and|or)\\b", input_string, ignore.case = TRUE)
  conditions <- regmatches(input_string, conditions)[[1]]
  questions_values <- gsub("'", "", str_extract_all(input_string, pattern_values)[[1]])
  questions_names <- gsub("\\$\\{|\\}", "", str_extract_all(input_string, pattern_names)[[1]])
  if(return=='name'){
    return(questions_names)
  }else{
    return(questions_values)
  }
}


