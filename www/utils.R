get.relevanse.question <- function(tool.survey, excluded_questions = c("calculate", "note")) {
  questions <- data.frame(ref.name = ifelse(!grepl("group", tool.survey$type), tool.survey$name, NA), type=tool.survey$type)
  questions <- na.omit(questions)
  
  questions <- questions[!(questions$type %in% excluded_questions),]
  
  questions <- questions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(parent.survey = sapply(ref.name, function(x) {
      relevance.condition <- filter(tool.survey, name == x)$relevant %>%
        ifelse(length(.) == 0, NA, .)
      pattern <- "\\$\\{([^\\}]+)\\}"
      result <- gsub("\\$\\{|\\}", "", str_extract_all(relevance.condition, pattern)[[1]])
      result <- result[result %in% questions$ref.name]
      relevance_str <-paste(result, collapse = ' ')
      return(ifelse(relevance_str == "", NA, relevance_str))
    })) %>%
    dplyr::mutate(relevant = sapply(ref.name, function(x) {
      relevance.condition <- filter(tool.survey, name == x)$relevant %>%
        ifelse(length(.) == 0, NA, .)}))
  
  get_relevant_surveys <- function(ref_name, df) {
    if (is.na(ref_name) || ref_name == "") {
      return(NA)
    } else {
      parent_surveys <- unlist(strsplit(ref_name, " "))
      relevant_surveys <- df[sapply(strsplit(df$parent.survey, " "), function(x) any(x %in% parent_surveys)), "ref.name", drop = FALSE]
      result <- stringr::str_trim(paste(relevant_surveys$ref.name, collapse = ' '))
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

get.constraint.question <- function(tool.survey) {
  questions <- data.frame(ref.name = ifelse(!grepl("group", tool.survey$type), tool.survey$name, NA))
  questions <- na.omit(questions)
  
  questions <- questions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(parent.survey = sapply(ref.name, function(x) {
      constraint.condition <- filter(tool.survey, name == x)$constraint %>%
        ifelse(length(.) == 0, NA, .)
      pattern <- "\\$\\{([^\\}]+)\\}"
      result <- gsub("\\$\\{|\\}", "", str_extract_all(constraint.condition, pattern)[[1]])
      constraint_str <- paste(result, collapse = ' ')
      return(ifelse(constraint_str == "", NA, constraint_str))
    })) %>%
    dplyr::mutate(relevant = sapply(ref.name, function(x) {
      constraint.condition <- filter(tool.survey, name == x)$constraint %>%
        ifelse(length(.) == 0, NA, .)}))
  
  get_constraint_surveys <- function(ref_name, df) {
    if (is.na(ref_name) || ref_name == "") {
      return(NA)
    } else {
      parent_surveys <- unlist(strsplit(ref_name, " "))
      relevant_surveys <- df[sapply(strsplit(df$parent.survey, " "), function(x) any(x %in% parent_surveys)), "ref.name", drop = FALSE]
      result <- stringr::str_trim(paste(relevant_surveys$ref.name, collapse = ' '))
      if (result == "") {
        return(NA)
      } else {
        return(result)
      }
    }
  }
  
  questions <- questions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(relevant.survey = get_constraint_surveys(ref.name, questions))
  
  return(questions)
}

parents.names <- function(questions, question.name) {
  parse.parents <- function(questions.row) {
    if (is.na(questions.row$parent.survey)) {
      return(c())
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

children.names <- function(questions, question.name) {
  parse.children <- function(questions.row) {
    if (is.na(questions.row$relevant.survey)) {
      return(NA)
    }
    children <- unlist(strsplit(questions.row$relevant.survey, " ")[[1]])
    children <- unique(children)
    return(children)
  }
  if (nrow(questions[questions$ref.name == question.name, ]) == 0) {
    return(c())
  }
  children <- parse.children(questions[questions$ref.name == question.name, ])
  return(children)
}

build_tree_parents <- function(questions, question.name) {
  parent.list <- list()
  
  parent_names <- parents.names(questions, question.name)
  if (is.logical(parent_names)) {
    return(NULL)
  }
  if (length(parent_names) == 0 || (length(parent_names) == 1 && parent_names[1] == "NA")) {
    return(NULL)
  }
  if (question.name %in% parent_names) {
    parent_names <- parent_names[!parent_names %in% question.name]
  }
  for (parent_name in parent_names) {
    parent_tree <- build_tree_parents(questions, parent_name)
    parent.list <- append(parent.list, list(parent_tree))
  }
  if (length(parent.list) > 0) {
    main.df <- tibble(
      name = parent_names,
      children = parent.list
    )
  } else {
    main.df <- tibble(
      name = parent_names
    )
  }
  
  return(main.df)
}

build_tree_children <- function(questions, question.name) {
  children.list <- list()
  
  child_names <- children.names(questions, question.name)
  if (is.logical(child_names)) {
    return(NULL)
  }
  if (length(child_names) == 0 || (length(child_names) == 1 && child_names[1] == "NA")) {
    return(NULL)
  }
  if (question.name %in% child_names) {
    child_names <- child_names[!child_names %in% question.name]
  }
  formulas.list <- list()
  for (child_name in child_names) {
    child_tree <- build_tree_children(questions, child_name)
    children.list <- append(children.list, list(child_tree))
  }
  if (length(children.list) > 0) {
    main.df <- tibble(
      name = child_names,
      children = children.list
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
    value <- ifelse(questions_values[i] == '', '""', questions_values[i])
    result <- append(result, list(questions_names[i], "==", value))
    if (i != length(questions_names)) {
      result <- append(result, conditions[i])
    }
  }
  
  result <- paste(result, collapse = ' ')
  return(result)
}

build_matrix_parents <- function(questions, question.name, depth) {
  parents <- parents.names(questions, question.name)
  res.df <- data.frame()
  
  if (is.logical(parents)) {
    return(NULL)
  }
  if (length(parents) == 0 || (length(parents) == 1 && parents[1] == "NA")) {
    return(NULL)
  }
  if (question.name %in% parents) {
    parents <- parents[!parents %in% question.name]
  }
  
  for (parent in parents) {
    formula <- questions[questions$ref.name == question.name, ]$relevant
    print(formula)
    if (!is.na(formula)) {
      df <- data.frame(child = question.name, parent = parent, formula=formula, depth=depth)
      res.df <- rbind(res.df, df)
      df <- build_matrix_parents(questions, parent, depth + 1)
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

build_matrix_children <- function(questions, question.name, depth) {
  children <- children.names(questions, question.name)
  res.df <- data.frame()
  
  if (is.logical(children)) {
    return(NULL)
  }
  if (length(children) == 0 || (length(children) == 1 && children[1] == "NA")) {
    return(NULL)
  }
  if (question.name %in% children) {
    children <- children[!children %in% question.name]
  }
  
  for (child in children) {
    formula <- questions[questions$ref.name == child, ]$relevant
    if (!is.na(formula)) {
      df <- data.frame(child = child, parent = question.name, formula=formula, depth=depth)
      res.df <- rbind(res.df, df)
      df <- build_matrix_children(questions, child, depth + 1)
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
