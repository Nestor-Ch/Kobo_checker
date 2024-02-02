library(DT)
library(shiny)
library(echarts4r)
library(tidyverse)
library(utilityR)
library(readxl)
library(data.table)
library(stringdist)

source("www/utils.R")


ui <- fluidPage(
  titlePanel("Kobo Checker App"),
  tags$head(
    tags$style(HTML(
      "
            #table-container {
              overflow: visible !important;
            }
          "
    )),
    HTML(
      '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'
    ),
    includeCSS("www/style.css"),
    HTML(
      '<a style="padding-left:10px;" class="app-title" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="app-description" style="font-size: 16px; color: #FFFFFF"><strong>Database_test</strong></span>'
    ),
  ),
  hr(),
  tabsetPanel(
    tabPanel("Data Checker",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose your kobo tool", accept = ".xlsx"),
                 actionButton("processBtn", "Process Data"),
                 width = 3
               ),
               mainPanel(
                 DTOutput("resultTable"),
                 width=9
               )
             )
    ),
    tabPanel("Question Inspection",
             titlePanel("Survey Question Inspection"),
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("question_name",'Select the question name',choices=NULL),
                 actionButton("submit_btn", "Submit"),
                 checkboxInput("calculate", "Include calculate questions"),
                 width=3
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Parents Tree View", echarts4rOutput("tree_chart_parents")),
                   tabPanel("Children Tree View", echarts4rOutput("tree_chart_children")),
                   tabPanel("Parents Relationship Matrix", tableOutput("parents_matrix_table")),
                   tabPanel("Children Relationship Matrix", tableOutput("children_matrix_table")),
                   tabPanel("Constraint Parents Tree View", echarts4rOutput("constraint_tree_chart_parents")),
                   tabPanel("Constraint Children Tree View", echarts4rOutput("constraint_tree_chart_children"))
                 ),
                 width = 9
               )
             )
    )
  )
)


# Server
server <- function(input, output, session) {
  # Reactive values for storing data
  data.tool <- reactiveVal(NULL)
  data.choices <- reactiveVal(NULL)
  
  label <- reactiveVal(NULL)
  
  
  # Load data from file
  observeEvent(input$file, {
    res <- readxl::read_xlsx(input$file$datapath, sheet = "survey", 
                             col_types = "text") %>% names()
    
    label(res[grepl('english',tolower(res))][1])
    
    labels <-label() 
    data.tool(load.tool.survey(input$file$datapath,labels,keep_cols = T))
    data.choices(load.tool.choices(filename_tool = input$file$datapath,
                                   label_colname = labels))
  })
  
  # Process data and update table
  observeEvent(input$processBtn, {
    if (!is.null(data.tool())) {
      kobo_data.t <- data.tool()
      kobo_data.c <- data.choices()
      labels <- label()
      
      kobo_data.t <- kobo_data.t %>% 
        filter(!grepl('\\bsettlement|\\brectangle\\b|\\brectangles\\b|geo_location|\\bpoint\\b|\\bhub\\b|raion|hromada|oblast|center_idp',list_name))
      
      kobo_data.c <- kobo_data.c %>% 
        filter(!grepl('\\bsettlement|\\brectangle\\b|\\brectangles\\b|geo_location|\\bpoint\\b|\\bhub\\b|raion|hromada|oblast|center_idp',list_name),
               !grepl("^UKRs\\d+$", name),
               !grepl("^UA\\d+$", name))
      
      
      # test for non eng in tool survey
      non_eng.t <- kobo_data.t %>% 
        select(name, relevant,type) %>% 
        rownames_to_column(var='rownames') %>% 
        pivot_longer(cols= name:type, names_to = 'column', values_to = 'value') %>% 
        filter(!is.na(value),
               !value==' ',
               !grepl('\xc2\xa0+',value),
               grepl("[^ -~]",value, perl=T)) %>% 
        mutate(cyrillic_char = regmatches(value, 
                                          gregexpr("[^ -~]", 
                                                   value, perl = TRUE)) %>%unlist  %>%  paste(collapse = '')) 
      
      if(nrow(non_eng.t)>0){non_eng.t <- non_eng.t %>% mutate(issue = 'cyrillic',
                                                              file = 'tool.survey')}
      
      # test for non eng in tool survey
      non_eng.c <- kobo_data.c %>% 
        select(list_name,!!sym(labels),name) %>% 
        rownames_to_column(var='rownames') %>% 
        pivot_longer(cols= list_name:name, names_to = 'column', values_to = 'value') %>% 
        filter(!is.na(value),
               !value==' ',
               !grepl('\xc2\xa0+',value),
               grepl("[^ -~]",value, perl=T)) %>% 
        rowwise() %>% 
        mutate(cyrillic_char = regmatches(value, 
                                          gregexpr("[^ -~]", 
                                                   value, perl = TRUE)) %>%unlist  %>%  paste(collapse = '')) %>% 
        filter(!grepl('\\`|\\’|\\–|\\´',cyrillic_char ))
      
      if(nrow(non_eng.c)>0){non_eng.c <- non_eng.c %>% mutate(issue = 'cyrillic',
                                                              file = 'tool.choices')}
      
      non_eng <- bind_rows(non_eng.c,non_eng.t)%>% 
        mutate(priority = 'Second priority')
      
      # test for leading/trailing spaces in tool survey
      add.space.t <- kobo_data.t %>% 
        select(name) %>% 
        rownames_to_column(var='rownames') %>% 
        rename(value=name) %>% 
        filter(!is.na(value),
               !value==' ',
               grepl("^\\s+|\\s+$|\\s{2,}",value, perl=T))
      
      if(nrow(add.space.t)>0){add.space.t <- add.space.t %>% mutate(issue = 'double/leading/trailing space',
                                                                    file = 'tool.survey')}
      
      
      # test for leading/trailing spaces in tool survey
      add.space.c <- kobo_data.c %>% 
        filter(!grepl('geo',list_name)) %>% 
        select(name,!!sym(labels)) %>% 
        rownames_to_column(var='rownames') %>% 
        pivot_longer(cols= name:!!sym(labels), names_to = 'column', values_to = 'value') %>% 
        filter(!is.na(value),
               !value==' ',
               grepl("^\\s+|\\s+$|\\s{2,}",value, perl=T))
      
      if(nrow(add.space.c)>0){add.space.c <- add.space.c %>%
        mutate(issue = 'double/leading/trailing space',
               file = 'tool.choices')
      }else{add.space.c <- data.frame()}
      
      add.space <- bind_rows(add.space.c,add.space.t)%>% 
        mutate(priority = 'Second priority')
      
      
      # check for _other variables
      other_check <- kobo_data.t %>% 
        filter(type=='text') %>% 
        filter(!is.na(relevant)) %>% 
        select(name,relevant) %>% 
        rownames_to_column(var='rownames') %>% 
        mutate(single_rel = relevant) %>% 
        tidyr::separate_rows(single_rel,sep='\\bor|\\band') %>% 
        mutate(single_rel=str_squish(single_rel)) %>% 
        filter(!grepl('not\\(selected',single_rel)) %>% 
        mutate(n_relevancies=str_count(relevant,'\\{'),
               questions_values = sapply(single_rel, parse.formula),
               question_names = sapply(single_rel, function(x){parse.formula(x,return='name')})) %>% 
        filter(grepl('other',questions_values))
      
      # check number of others per question
      other_check_n_others <- other_check %>% 
        group_by(question_names) %>% 
        mutate(count_others = sum(n())) %>% 
        filter(count_others>1) %>% 
        ungroup()
      if(nrow(other_check_n_others)>0){
        other_check_n_others <- other_check_n_others %>% 
          select(rownames,name,relevant)%>% 
          rename(value = relevant,
                 column = name) %>% 
          mutate(issue = 'Two others per 1 question. Pls fix',
                 file = 'tool.survey')%>% 
          mutate(priority = 'First priority')
      }else(other_check_n_others <- data.frame())
      
      
      other_n_rel <- other_check %>% filter(n_relevancies>1) %>% 
        select(rownames,name,relevant) %>% 
        rename(value = relevant,
               column = name) %>% 
        mutate(issue = 'Double relevancy',
               file = 'tool.survey')%>% 
        mutate(priority = 'First priority')
      
      other_wrong_choice <- other_check %>% filter(!questions_values=='other') %>% 
        select(rownames,name,relevant) %>% 
        rename(value = relevant,
               column = name) %>% 
        mutate(issue = 'Other choice is not called "other"',
               file = 'tool.survey')%>% 
        mutate(priority = 'First priority')
      
      other_wrong_name <- other_check %>% filter(!grepl('_other$',name)) %>% 
        select(rownames,name,relevant) %>% 
        rename(value = relevant,
               column = name) %>% 
        mutate(issue = "The name of this 'other' value doesn't end with '_other' this will break the cleaning script",
               file = 'tool.survey')%>% 
        mutate(priority = 'First priority')
      
      other_checks <- bind_rows(other_n_rel,other_wrong_choice,other_wrong_name,other_check_n_others)
      
      
      # check if all relevances match the available choices
      
      check_rel <- kobo_data.t %>% 
        rownames_to_column(var='rownames') %>% 
        select(rownames,name, type, relevant) %>% 
        filter(!is.na(relevant),
               grepl('select_multiple|select_one',type)) %>% 
        mutate(single_rel = relevant) %>% 
        tidyr::separate_rows(single_rel,sep='\\bor|\\band') %>% 
        filter(grepl('selected',single_rel))
      if(nrow(check_rel)>0){
        check_rel <- check_rel %>% 
          mutate(questions_values = sapply(single_rel, parse.formula),
                 question_names = sapply(single_rel, function(x){parse.formula(x,return='name')})) %>% 
          left_join(kobo_data.t %>% select(name,list_name) %>% rename(question_names=name)) %>% 
          rowwise() %>% 
          mutate(check = 
                   questions_values %in% kobo_data.c[kobo_data.c$list_name==list_name,]$name
          ) %>% 
          filter(check==F & !questions_values=='') %>% 
          mutate(issue = paste0('Relevance value ',questions_values, ' is not present in the list ',list_name),
                 column = 'relevant',
                 file = 'tool.survey') %>% 
          rename(value=relevant) %>% 
          select(rownames,column,value,issue,file)%>% 
          mutate(priority = 'First priority')
        
      }else{check_rel <- data.frame()}
      
      # check constraints for having existing values
      check_con <- kobo_data.t %>% 
        rownames_to_column(var='rownames') %>% 
        select(rownames,name, type, constraint) %>% 
        filter(!is.na(constraint),
               grepl('select_multiple|select_one',type)) %>% 
        mutate(single_const = constraint) %>% 
        tidyr::separate_rows(single_const,sep='\\bor|\\band') %>% 
        filter(grepl('selected\\(\\$',single_const))
      
      if(nrow(check_con)>0){
        check_con <- check_con %>% 
          mutate(questions_values = sapply(single_const, parse.formula),
                 question_names = sapply(single_const, function(x){parse.formula(x,return='name')})) %>% 
          left_join(kobo_data.t %>% select(name,list_name) %>% rename(question_names=name)) %>% 
          rowwise() %>% 
          mutate(check = 
                   questions_values %in% kobo_data.c[kobo_data.c$list_name==list_name,]$name
          ) %>% 
          filter(check==F & !questions_values=='') %>% 
          mutate(issue = paste0('Constraint value ',questions_values, ' is not present in the list ',list_name),
                 column = 'relevant',
                 file = 'tool.survey') %>% 
          rename(value=constraint) %>% 
          select(rownames,column,value,issue,file)%>% 
          mutate(priority = 'First priority')
      }else{check_con <- data.frame()}
      
      # Duplicate choices in list_choices
      
      dupl_choices <- kobo_data.c %>%
        rownames_to_column(var='rownames') %>% 
        group_by(list_name,name) %>%
        mutate(dupobs = n()) %>%
        filter(dupobs>1) %>% 
        ungroup() 
      if(nrow(dupl_choices)>0){
        dupl_choices <- dupl_choices %>% 
          group_by(list_name) %>% 
          summarise(issue = paste0('duplicate name in list_name ',unique(list_name)),
                    rownames = min(rownames),
                    value = unique(name)) %>% 
          ungroup() %>% 
          select(-list_name) %>% 
          mutate(column = 'name',
                 file = 'tool.choices')%>% 
          mutate(priority = 'First priority')
      }else{dupl_choices <- data.frame()}
      
      
      # Check if None is in constraint if it's present in the list of choices
      non_check <- kobo_data.t %>% 
        rownames_to_column(var='rownames') %>% 
        select(rownames,list_name,constraint) %>% 
        rowwise() %>% 
        mutate(check = any(grepl('none', kobo_data.c[kobo_data.c$list_name %in% list_name,]$name))) %>%
        ungroup() %>% 
        mutate(check2 = grepl('none',constraint)) %>% 
        filter(check==T & check2==F)
      if(nrow(non_check)>0){
        non_check <- non_check %>% 
          select(rownames,constraint) %>% 
          rename(value=constraint) %>% 
          mutate(column = 'constraint',
                 issue = 'None is present in the list_name for the variable, but not in the constraint',
                 file = 'tool.survey')%>% 
          mutate(priority = 'Second priority')
      }else{non_check <- data.frame()}
      
      
      # Check if the names match the labels (at least somewhat)
      
      label_issues <- kobo_data.c %>% 
        tibble() %>% 
        rownames_to_column(var='rownames') %>% 
        select(list_name,rownames,name,!!sym(labels)) %>% 
        mutate(name = tolower(gsub('_',' ',name)),
               label = tolower(gsub('_',' ',!!sym(labels))),
               label = gsub("\\(.*?\\)", "", label),
               label = gsub("[[:punct:]]", "", label),
               similarity = stringsim(
                 name,label,
                 method='jaccard',
                 q=2
               )) %>% 
        rowwise() %>% 
        filter(similarity<0.2 & ! grepl(name,label) & !name=='idk') %>% ungroup()
      if(nrow(label_issues)>0){
        label_issues <- label_issues %>% 
          mutate(issue = paste0('The name "',name,'" is dissimlar to the label "',!!sym(labels),'". Please double check'),
                 column = labels,
                 value = !!sym(labels),
                 file = 'tool.choices') %>% 
          select(rownames,column,value,issue,file) %>% 
          mutate(priority = 'Second priority')
      }else{label_issues <- data.frame()}
      
      
      processed_data <- bind_rows(other_checks,add.space,non_eng,check_rel,check_con,
                                  dupl_choices,check_con,check_rel,non_check,label_issues) %>% 
        relocate(file) %>% 
        mutate(rownames = as.numeric(rownames)+1)
      
      if(all(is.na(processed_data$cyrillic_char))){
        processed_data <- processed_data %>% select(-cyrillic_char)
      }
      
      output$resultTable <- renderDT({
        DT::datatable(
          processed_data,
          filter = "top",
          extensions = 'Buttons',
          options = list(
            dom = 'lfrtipB',
            buttons = c("copy", "excel"),
            pageLength = 100,
            scrollX=TRUE
          )
        )
      })
      
    }
  })
  
  
  observeEvent(input$processBtn, {
    req(!is.null(data.tool()))
    
    names_list <-  data.tool() %>% filter(grepl('select_',type)) %>% pull(name)
    
    updateSelectizeInput(session, 'question_name', choices = names_list, server = TRUE, selected="")
  })

    
  observeEvent(input$calculate, {
    if (!is.null(data.tool())) {
      if (input$calculate == T) {
        names_list <- data.tool() %>%
          filter(grepl('^(select_|calculate)', type)) %>%
          pull(name)
        
      } else {
        names_list <- data.tool() %>% filter(grepl('select_', type)) %>% pull(name)
      }
      
      updateSelectizeInput(session, 'question_name', choices = names_list, server = TRUE, selected="")
    }
  })  
  
  observeEvent({
    input$question_name
    input$submit_btn
  }, {
    if (!is.null(data.tool())) {
      
      if (input$calculate) {
        questions <- get.relevanse.question(data.tool(), c("note"))
      } else {
        questions <- get.relevanse.question(data.tool())
      }
      
      questions_constraints <- get.constraint.question(data.tool())
      question_name <- input$question_name
      if (question_name %in% questions$ref.name) {
        tree_data_parents <- build_tree_parents(questions, question_name)
        tree_data_parents <- tibble(
          name = input$question_name, 
          children = list(tree_data_parents)
        )
        
        tree_data_children <- build_tree_children(questions, question_name)
        tree_data_children <- tibble(
          name = input$question_name, 
          children = list(tree_data_children)
        )
        
        if (question_name %in% questions_constraints$ref.name) {
          constraint_tree_data_parents <- build_tree_parents(questions_constraints, question_name)
          constraint_tree_data_parents <- tibble(
            name = input$question_name, 
            children = list(constraint_tree_data_parents)
          )
          
          constraint_tree_data_children <- build_tree_children(questions_constraints, question_name)
          constraint_tree_data_children <- tibble(
            name = input$question_name, 
            children = list(constraint_tree_data_children)
          )
        } else {
          constraint_tree_data_parents <- tibble(
            name = input$question_name, 
            children = list()
          )
          
          constraint_tree_data_children <- tibble(
            name = input$question_name, 
            children = list()
          )
        }
        
        
        matrix_data_parents <- build_matrix_parents(questions, question_name, 0)
        matrix_data_children <- build_matrix_children(questions, question_name, 0)
        
        output$tree_chart_parents <- renderEcharts4r({
          tree_data_parents %>%
            e_charts() %>% 
            e_tree(orient = "RL", label = list(normal = list(position = "outside")), initialTreeDepth = 5)
        })
        
        output$tree_chart_children <- renderEcharts4r({
          tree_data_children %>%
            e_charts() %>% 
            e_tree(orient = "LR", label = list(normal = list(position = "outside")), initialTreeDepth = 5)
        })
        
        output$constraint_tree_chart_parents <- renderEcharts4r({
          constraint_tree_data_parents %>%
            e_charts() %>% 
            e_tree(orient = "RL", label = list(normal = list(position = "outside")), initialTreeDepth = 5)
        })
        
        output$constraint_tree_chart_children <- renderEcharts4r({
          constraint_tree_data_children %>%
            e_charts() %>% 
            e_tree(orient = "LR", label = list(normal = list(position = "outside")), initialTreeDepth = 5)
        })
        
        output$parents_matrix_table <- renderTable({
          matrix_data_parents
        })
        
        output$children_matrix_table <- renderTable({
          matrix_data_children
        })
        
      } else {
        tree_data <- tibble(
          name = "question wasn't found in the survey",
        )
        output$constraint_tree_chart_parents <- renderEcharts4r({
          tree_data %>%
            e_charts() %>% 
            e_tree(orient = "RL", label = list(normal = list(position = "outside")), initialTreeDepth = 5)
        })
        
        output$constraint_tree_chart_children <- renderEcharts4r({
          tree_data %>%
            e_charts() %>% 
            e_tree(orient = "LR", label = list(normal = list(position = "outside")), initialTreeDepth = 5)
        })
      }
      
    }
  })
}

# Run the app
shinyApp(ui, server)
