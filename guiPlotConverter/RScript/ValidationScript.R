################################################################################
#--------------GPT Placeholder2plotConnector Validation Script-----------------#
#------------------------------------------------------------------------------#
# Author: SK ------------------------------------------------------------------#
# Date: 02.11.2025 ------------------------------------------------------------#
# Version: 001 ----------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Author: SK ------------------------------------------------------------------#
# Date: 17.12.2025 ------------------------------------------------------------#
# Version: 002 ----------------------------------------------------------------#
################################################################################


rm(list = ls())
path.prog <- suppressWarnings(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/"))
setwd(path.prog) 
path.software <- paste0("C:/Program Files/Lixoft/MonolixSuite2024R1")
install.packages(paste0(path.software,"/connectors/lixoftConnectors.tar.gz"), repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)

initializeLixoftConnectors(software = "monolix", path = path.software)

library(ggplot2)
library(gridExtra)
library(patchwork)
library(waldo) 
library(dplyr)
library(readr)
library(stringr) 
library(tools) 


# ------------------------------------------------------------------------------
# HELPER: Wechsel zw. Softwares (initialisieren & laden von Projekten 
smartLoadProject <- function(project_path, lixoft_path) {
  
  # Leite Software von Extension ab
  ext <- tolower(tools::file_ext(project_path))
  
  target_software <- switch(ext,
                            "pkx"     = "pkanalix",
                            "mlxtran" = "monolix",
                            "monolix") # Default fallback
  
  # 2. Check Current Connector State
  # We use tryCatch in case connectors aren't initialized yet
  current_state <- tryCatch(getLixoftConnectorsState(), error = function(e) NULL)
  current_software <- if(!is.null(current_state)) current_state$software else ""
  
  # 3. Switch Software Environment if necessary
  if (!identical(tolower(current_software), tolower(target_software))) {
    cat(sprintf("   -> Switching connectors from '%s' to '%s'...\n", current_software, target_software))
    
    # Set option to force switch as per your snippet
    op <- options()
    options(lixoft_lixoftConnectors_forceSoftwareSwitch = TRUE)
    on.exit(options(op)) # Ensure options are reset even if error occurs
    
    initializeLixoftConnectors(software = target_software, path = lixoft_path)
  }
  
  # 4. Load the Project
  loadProject(project_path)
}
# ------------------------------------------------------------------------------
get_call_args_robust <- function(code_str) {
  tryCatch({
    # Parse den String
    p        <- parse(text = code_str)
    call_obj <- p[[length(p)]]
    
    # Hole die Funktionsdefinition aus der geladenen Umgebung
    func_name <- as.character(call_obj[[1]])
    func_def  <- getAnywhere(func_name)$objs[[1]]
    
    standardized_call <- rlang::call_standardise(call_obj, env = parent.frame())
    
    # Convert to list
    args_list <- as.list(standardized_call)[-1]
    
    evaluated_args <- lapply(args_list, function(x) {
      if (is.call(x)) return(eval(x))
      return(x)
    })
    
    return(evaluated_args)
  }, error = function(e) {
    return(list(error = paste("Parsing error:", e$message)))
  })
}
# ------------------------------------------------------------------------------
normalize_plot_object <- function(p) {
  # Für ggplot objects
  if (inherits(p, "ggplot")) {
    built <- ggplot_build(p)
    return(list(
      type   = "ggplot",
      data   = built$data,      # z.B. berechneten Daten (Punkte, Linien)
      labels = p$labels,        # z.B. Achsenbeschriftungen, Titel
      layers = sapply(p$layers, function(x) class(x$geom)[1]))) # Geoms: (Points, Lines)
  }
  
  # TableGrob (Lixoft Output bei Faceting/Grid)
  if (inherits(p, "gtable")) {
    # Vergleich Struktur der Grobs z.B. extrahier' Texte (Titel, Achsen) aus den Grobs
    return(list(
      type   = "gtable",
      layout = p$layout,
      grobs_names = lapply(p$grobs, function(x) x$name)))
  }
  
  # Liste von Plots
  if (is.list(p) && !inherits(p, "ggplot")) {
    return(lapply(p, normalize_plot_object))
  }
  
  return(p)
}

# ------------------------------------------------------------------------------
# Säubert Code von Markdown "Wrappers"
clean_code_string <- function(code_str) {
  if (is.na(code_str)) return("")
  c <- gsub("^```[rR]?\\s*", "", code_str) # Entfernt start block ```r or ```R or just ```
  c <- gsub("```\\s*$", "", c)             # Entfernt end block ```
  
  c <- trimws(c)
  
  if (grepl('^".*"$', c) || grepl("^'.*'$", c)) { # check ob plot mit "" anfängt und aufhört
    c <- substring(c, 2, nchar(c) - 1)            # Entferne 1. und letzten character
  }
  c <- gsub('\\\\"', '"', c)
  c <- gsub('\\\\n', '\n', c)
  return(c)
}
# ---------------------------------------------------------
# VALIDATIONS SCHLEIFE

validate_ai_plots <- function(df_cases, base_path, lixoft_path) {
  df_cases$validation_status <- NA_character_
  df_cases$mismatch_severity <- NA_character_
  df_cases$error_message     <- NA_character_
  
  current_loaded_project <- ""
  
  for (i in 1:nrow(df_cases)) {
    project_name <- df_cases$project[i]
    model_name   <- df_cases$AI_model[i]
    cat(sprintf("\n--- Row %d | Model: %s | Project: '%s' ---\n", i, model_name, project_name))
    
    # 1. Projekt-Pfad & Loading
    project_file <- if(tools::file_ext(project_name) == "") paste0(project_name, ".mlxtran") else project_name
    full_project_path <- file.path(base_path, project_file)
    
    if (full_project_path != current_loaded_project) {
      load_result <- try(smartLoadProject(full_project_path, lixoft_path), silent = TRUE)
      if (inherits(load_result, "try-error")) {
        df_cases$validation_status[i] <- "PROJECT_LOAD_ERROR"
        next 
      }
      current_loaded_project <- full_project_path
    }
    
    # 2. Code Reinigung und Ausführung (Original-Objekt Vergleich)
    clean_ref  <- clean_code_string(df_cases$ref_code[i])
    clean_test <- clean_code_string(df_cases$test_code[i])
    
    ref_obj  <- tryCatch({ eval(parse(text = clean_ref)) },  error = function(e) e)
    test_obj <- tryCatch({ eval(parse(text = clean_test)) }, error = function(e) e)
    
    # 3. Check auf Execution Errors
    if (inherits(ref_obj, "error") || inherits(test_obj, "error")) {
      df_cases$validation_status[i] <- "EXECUTION_ERROR"
      df_cases$mismatch_severity[i] <- "N/A"
      df_cases$error_message[i] <- paste0(
        if(inherits(ref_obj, "error"))  paste("Ref  Error:", ref_obj$message)  else "",
        if(inherits(test_obj, "error")) paste("Test Error:", test_obj$message) else ""
      )
      cat("FAIL: Execution Error.\n")
      next
    }
    
    # 4. BESTIMMUNG STATUS (Originaler Ansatz: Vergleich der Plot-Daten)
    norm_ref  <- normalize_plot_object(ref_obj)
    norm_test <- normalize_plot_object(test_obj)
    
    # Der waldo Vergleich auf Objekten bestimmt PASS/MISMATCH
    obj_diffs <- waldo::compare(norm_ref, norm_test, x_arg = "ref", y_arg = "gpt")
    
    if (length(obj_diffs) == 0) {
      cat("PASS: Objects match.\n")
      df_cases$validation_status[i] <- "PASS"
      df_cases$mismatch_severity[i] <- "0 NO MISMATCH"
    } else {
      cat("FAIL: Objects mismatch. Calculating severity via arguments...\n")
      df_cases$validation_status[i] <- "MISMATCH"
      
      # 5. BESTIMMUNG SEVERITY (Neuer Ansatz: Vergleich der Argumente)
      args_ref  <- get_call_args_robust(clean_ref)
      args_test <- get_call_args_robust(clean_gpt)
      
      # Wir zählen die logischen Unterschiede in den Settings
      arg_diffs   <- waldo::compare(args_ref, args_test)
      n_arg_diffs <- length(arg_diffs)
      
      df_cases$error_message[i] <- paste("Object mismatch. Logic diffs:", n_arg_diffs)
      
      if(n_arg_diffs <= 2) {
        df_cases$mismatch_severity[i] <- paste0("1 MINOR MISMATCH (<=2 logic diffs)")
      } else if (n_arg_diffs <= 4) {
        df_cases$mismatch_severity[i] <- paste0("2 MODERATE MISMATCH (<=4 logic diffs)")
      } else {
        df_cases$mismatch_severity[i] <- paste0("3 SEVERE MISMATCH (>=5 logic diffs)")
      }
    }
  }
  return(df_cases)
}

# ---------------------------------------------------------
# AUSFÜHRUNG:

test_cases_openai   <- read_csv(file.path(path.prog,"..","ValidationDataSets/openai_populate_plots_prevalidation.csv"), show_col_types = FALSE) %>%
  rename(test_code = gpt_code,
         AI_model  = gpt_model)  

test_cases_claude   <- read_csv(file.path(path.prog,"..","ValidationDataSets/claude_populate_plots_prevalidation.csv"), show_col_types = FALSE) %>%
  rename(test_code = claude_code,
         AI_model  = claude_model)

test_cases <- rbind(test_cases_openai, test_cases_claude)

# Repair Invalid Strings
test_cases <- test_cases %>% 
  mutate(
    # iconv to convert and repair corrupted characters
    # "//TRANSLIT" option attempts to find substitution
    # "//IGNORE" option discards invalid characters if translation fails
    ref_code  = iconv(ref_code,  from = "", to = "UTF-8", sub = ""),
    test_code = iconv(test_code, from = "", to = "UTF-8", sub = ""))

path.example <- file.path(path.prog,"..","ExampleBase")
# Run Validation
results_df <- validate_ai_plots(test_cases, path.example, path.software)
error_execution <- results_df %>% filter(validation_status=="EXECUTION_ERROR")
mismatch        <- results_df %>% filter(validation_status!="PASS")

write.csv(results_df,      file = file.path(path.prog,"..","/Results/results_postvalidation.csv"), row.names = F)
write.csv(error_execution, file = file.path(path.prog,"..","/Results/results_error_execution.csv"),row.names = F)
write.csv(mismatch,        file = file.path(path.prog,"..","/Results/results_mismatch.csv"),       row.names = F)

################################################################################
# ANALYSIS OF THE RESULTS
results_df <- read.csv(file.path(path.prog,"..","/Results/results_postvalidation.csv"))

time_openAI <- read.csv(file.path(path.prog,"..","/ValidationDataSets/openai_time_per_iteration.csv"))
time_claude <- read.csv(file.path(path.prog,"..","/ValidationDataSets/claude_time_per_iteration.csv"))

time_spent <- rbind(time_openAI, time_claude) %>%
  mutate(s_per_it          = as.numeric(sub(".*,\\s*([0-9.]+)s/it\\]", "\\1", time_per_iteration)),
         elapsed_time      = str_extract(time_per_iteration, "(?<=\\[)[0-9:]+(?=<)"),
         elapsed_mm        = as.numeric(str_extract(elapsed_time, "^[0-9]+")),
         elapsed_ss        = as.numeric(str_extract(elapsed_time, "[0-9]+$")),
         total_elapsed_sec = elapsed_mm * 60 + elapsed_ss) %>%
  select(-elapsed_mm, -elapsed_ss)

# Normalize labels to an alphanumeric-only key so ANY punctuation, whitespace,
# or non-breaking-space differences ("[temp: 0.0]" vs "[temp:0.0]", trailing
# U+00A0, etc.) don't break the join. Distinct labels stay distinct
# (e.g. "...effortlow" vs "...effortmedium", "temp00" vs "temp05").
results_df$join_key <- tolower(gsub("[^[:alnum:]]", "", results_df$AI_model))
time_spent$join_key <- tolower(gsub("[^[:alnum:]]", "", time_spent$AI_model))

# Drop time_spent's AI_model so the readable label comes only from results_df.
results_df <- merge(results_df, time_spent[, setdiff(names(time_spent), "AI_model")], by = "join_key")

# Order models for plotting AFTER the merge, on the clean results_df label.
# All 19 levels are listed so no row becomes NA.
model_levels <- c("gpt-5.6-sol [effort: none]", 
                  "gpt-5.6-sol [effort: low]", 
                  "gpt-5.6-sol [effort: medium]", 
                  "gpt-5.6-terra [effort: none]", 
                  "gpt-5.6-terra [effort: low]", 
                  "gpt-5.6-terra [effort: medium]",
                  "claude-opus-5 [effort: low]", 
                  "claude-opus-5 [effort: medium]", 
                  "claude-sonnet-5 [effort: low]", 
                  "claude-sonnet-5 [effort: medium]")
results_df$AI_model <- factor(results_df$AI_model, levels = model_levels)

# ------------------------------------------------------------------------------
# HELPER: summary 
summarize_validation <- function(data) {
  data %>%
    summarise(
      Total_Cases           = n(),
      Count_PASS            = sum(validation_status        == "PASS",            na.rm = TRUE),
      Pass_Rate_Pct         = round((sum(validation_status == "PASS",            na.rm = TRUE) / n()) * 100, 2),
      Count_MISMATCH        = sum(validation_status        == "MISMATCH",        na.rm = TRUE), 
      Mismatch_Rate_Pct     = round((sum(validation_status == "MISMATCH",        na.rm = TRUE) / n()) * 100, 2),
      Count_EXECUTION_ERROR = sum(validation_status        == "EXECUTION_ERROR", na.rm = TRUE),
      Error_Rate_Pct        = round((sum(validation_status == "EXECUTION_ERROR", na.rm = TRUE) / n()) * 100, 2),
      .groups = "drop") %>%
    mutate(Pass     = paste0("N=",Count_PASS,           " (",Pass_Rate_Pct,    "%)"),
           Mismatch = paste0("N=",Count_MISMATCH,       " (",Mismatch_Rate_Pct,"%)"),
           Error    = paste0("N=",Count_EXECUTION_ERROR," (",Error_Rate_Pct,   "%)")) 
}

# ------------------------------------------------------------------------------
# HELPER: Visualisierung der Ergebnisse
# df <- results_df
# 
plot_validation_split <- function(df, split_by = NULL, sort_by = "pass") {
  
  plot_data <- df %>%
    group_by(across(all_of(c("AI_model", split_by, "validation_status")))) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(total_in_group = sum(n), 
           pct            = round((n / total_in_group) * 100, 0),
           label_text     = paste0("N=", n, "\n(", pct, "%)")) %>%
    ungroup() %>%
    mutate(validation_status = factor(validation_status, levels = c("EXECUTION_ERROR", "MISMATCH", "PASS")))
  
  
  if (sort_by == "speed") {
    # Use original df (has s_per_it column)
    model_order <- df %>%
      group_by(AI_model) %>%
      summarize(metric = mean(s_per_it, na.rm = TRUE)) %>%
      arrange(metric) %>%
      pull(AI_model)
  } else {
    
    model_order <- plot_data %>%
      filter(validation_status == "PASS") %>%
      arrange(desc(pct)) %>%
      pull(AI_model) %>%
      unique()
  }
  # Convert AI_model to factor
  plot_data <- plot_data %>%
    mutate(AI_model = factor(AI_model, levels = model_order))
  
  p <- ggplot(plot_data, aes(x = AI_model, y = n, fill = validation_status)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = ifelse(n > 0, label_text, "")), 
              position = position_stack(vjust = 0.5), 
              color = "white", fontface = "bold", size = 2.3) +
    scale_fill_manual(values = c("PASS" = "#077B44", "MISMATCH" = "#ED9609", "EXECUTION_ERROR" = "#D50A27")) +
    labs(x = NULL, y = "Number of Cases", fill = "Status") +
    theme_minimal() +
    theme(legend.position = "top", panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold", size = 10)) +
    coord_flip() +
    scale_x_discrete(limits = rev)  # Add this back for top-to-bottom ordering
  
  if (!is.null(split_by)) {
    facet_formula <- as.formula(paste("~", paste(split_by, collapse = "+")))
    p <- p + facet_wrap(facet_formula, scales = "free_x") 
  }
  
  return(p)
  
}

#-------------------------------------------------------------------------------
# Nb of plots in validation data set
{
  nbplots <- results_df %>%
    filter(AI_model == unique(AI_model)[1]) %>%
    count(plot, name = "count") %>%
    bind_rows(summarise(., plot = "Total", count = sum(count))) %>%
    arrange(count)
  print(nbplots)
}
#-------------------------------------------------------------------------------
# By gpt model
{

  stats_by_model <- results_df %>%
    group_by(AI_model) %>%
    summarize_validation() %>%
    rename(`Used Model` = AI_model)%>%
    select(-Total_Cases, -Count_PASS, -Pass_Rate_Pct, -Count_MISMATCH, -Mismatch_Rate_Pct, -Count_EXECUTION_ERROR, -Error_Rate_Pct) %>%
    arrange(desc(Pass))
  print(stats_by_model)
  pp_by_model <- plot_validation_split(results_df)
}

#-------------------------------------------------------------------------------
# Success rate vs. speed (Pareto view)
{
  # Visual encoding that carries meaning (colleague's comment):
  family_shapes <- c("GPT 5.6 Sol"     = 16,
                     "GPT 5.6 Terra"   = 17,
                     "Claude Opus 5"   = 15,
                     "Claude Sonnet 5" = 18)                       # all filled
  effort_colors <- c("none" = "#4285f4", "low" = "#db4437", "medium" = "#f4b400")

  # Per-model stats: success rate + mean time per iteration, then split the
  # AI_model label into the family (shape) and effort (color) aesthetics.
  stats_speed <- results_df %>%
    group_by(AI_model) %>%
    summarise(
      n_total     = n(),
      n_pass      = sum(validation_status == "PASS", na.rm = TRUE),
      success_pct = round(n_pass / n_total * 100, 1),
      mean_time_s = mean(s_per_it, na.rm = TRUE),
      .groups = "drop") %>%
    mutate(
      ModelFamily = factor(case_when(
        grepl("sol",    AI_model) ~ "GPT 5.6 Sol",
        grepl("terra",  AI_model) ~ "GPT 5.6 Terra",
        grepl("opus",   AI_model) ~ "Claude Opus 5",
        grepl("sonnet", AI_model) ~ "Claude Sonnet 5"), levels = names(family_shapes)),
      Effort = factor(sub(".*\\[effort:\\s*([a-z]+)\\].*", "\\1", as.character(AI_model)),
                      levels = names(effort_colors)))

  x_max <- ceiling(max(stats_speed$mean_time_s, na.rm = TRUE) / 5) * 5

  pp_success_vs_time <- ggplot(stats_speed,
      aes(x = mean_time_s, y = success_pct, color = Effort, shape = ModelFamily)) +
    geom_hline(aes(yintercept = 90, linetype = "90% Threshold"),
               color = "black", linewidth = 1) +                  # regulatory threshold
    geom_point(size = 4, alpha = 0.8) +
    scale_color_manual(values = effort_colors, drop = FALSE) +
    scale_shape_manual(values = family_shapes, drop = FALSE) +
    scale_linetype_manual(values = "dashed", name = NULL) +
    scale_x_continuous(breaks = seq(0, x_max, 5), minor_breaks = seq(0, x_max, 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5)) +
    labs(x = "Average Time per Iteration (s)",
         y = "Success Rate (%)",
         color = "Effort",
         shape = "Model") +
    theme_minimal() +
    theme(
      legend.position  = "right",
      legend.box       = "vertical",
      legend.text      = element_text(size = 9),
      panel.grid.minor = element_line(linewidth = 0.25, color = "grey90")) +
    guides(
      shape    = guide_legend(order = 1, override.aes = list(size = 4)),
      color    = guide_legend(order = 2, override.aes = list(size = 4)),
      linetype = guide_legend(order = 3)) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, 100))

  print(pp_success_vs_time)
}


#-------------------------------------------------------------------------------
# Fastest & best models combined: Efficiency Score (Pass / time)
{
  stats_combined1 <- results_df %>%
    group_by(AI_model) %>%
    summarize_validation() %>%
    left_join(
      results_df %>%
        group_by(AI_model) %>%
        summarize(avg_s_per_it = mean(s_per_it, na.rm = TRUE)),
      by = "AI_model") %>%
    mutate(Efficiency = round((Pass_Rate_Pct / avg_s_per_it),3)) %>%
    arrange(desc(Efficiency)) %>%
    select(`Used model` = AI_model,  `Pass %`=Pass_Rate_Pct, `Speed (s/it)`=avg_s_per_it, Efficiency)
  print(stats_combined1)
  
  
}

#-------------------------------------------------------------------------------
# Fastest & best models combined: Weighted score 0.5 * quality + 0.5 * speed = equal balance
{
  stats_combined2 <- results_df %>%
    group_by(AI_model) %>%
    summarize_validation() %>%
    left_join(
      results_df %>%
        group_by(AI_model) %>%
        summarize(avg_s_per_it = mean(s_per_it, na.rm = TRUE)),
      by = "AI_model") %>%
    mutate(# Normalize to 0-1 scale
      norm_quality = Pass_Rate_Pct / 100,
      norm_speed = 1 - ((avg_s_per_it - min(avg_s_per_it)) / (max(avg_s_per_it) - min(avg_s_per_it))),
      # Weighted score (adjust weights as needed)
      Combined_Score = round((0.5 * norm_quality + 0.5 * norm_speed),2)) %>%  # 70% quality, 30% speed
    arrange(desc(Combined_Score)) %>%
    select(`Used model` = AI_model,  `Pass %`=Pass_Rate_Pct, `Speed (s/it)`=avg_s_per_it, Score = Combined_Score)
  print(stats_combined2)

}

#-------------------------------------------------------------------------------
# By gpt model & software
{
  stats_by_software_model <- results_df %>%
    group_by(software, AI_model) %>%
    summarize_validation() %>%
    select(-Total_Cases, -Count_PASS, -Pass_Rate_Pct, -Count_MISMATCH, -Mismatch_Rate_Pct, -Count_EXECUTION_ERROR, -Error_Rate_Pct) %>%
    arrange(desc(Pass))
    # filter(AI_model %in% model_filter)
  # print(stats_by_software_model)
  print(stats_by_software_model)
  
  pp_by_model_software <- plot_validation_split(results_df, split_by = c("software"))
  print(pp_by_model_software)
}

#-------------------------------------------------------------------------------
# By gpt model plot complexity
{
  stats_by_model_complexity <- results_df %>%
    group_by(AI_model, plot_complexity) %>%
    summarize_validation() %>%
    select(-Total_Cases, -Count_PASS, -Pass_Rate_Pct, -Count_MISMATCH, -Mismatch_Rate_Pct, -Count_EXECUTION_ERROR, -Error_Rate_Pct) %>%
    arrange(desc(Pass))
  
  total_model_complexity <- results_df %>%
    group_by(AI_model, plot_complexity) %>%
    summarise(Total = n()) 
  
  stats_by_model_complexity <- merge(stats_by_model_complexity, total_model_complexity, by=c("AI_model", "plot_complexity"))
  
  print(stats_by_model_complexity)
  pp_by_model_complexity <- plot_validation_split(results_df, split_by = c("plot_complexity"))
  # pp_by_model_complexity <- plot_validation_split(results_df1 %>% filter(gpt_model %in% model_filter), split_by = c("plot_complexity"))
}

#-------------------------------------------------------------------------------
# By gpt model & software & plot complexity
{
  stats_by_software_model_complexity <- results_df %>%
    group_by(software, AI_model, plot_complexity) %>%
    summarize_validation() %>%
  select(-Total_Cases, -Count_PASS, -Pass_Rate_Pct, -Count_MISMATCH, -Mismatch_Rate_Pct, -Count_EXECUTION_ERROR, -Error_Rate_Pct) %>%
    arrange(desc(Pass))
    
  print(stats_by_software_model_complexity)
  pp_by_model_softwarem_complexity <- plot_validation_split(results_df %>% filter(software == "m"), split_by = c("software", "plot_complexity"))
  print(pp_by_model_softwarem_complexity)
  
  pp_by_model_softwarep_complexity <- plot_validation_split(results_df %>% filter(software == "p"), split_by = c("software", "plot_complexity"))
  print(pp_by_model_softwarep_complexity)
}

#-------------------------------------------------------------------------------
# By gpt model & software & mismatch severity
{
  stats_by_software_model_mismatch_severity <- results_df %>%
    group_by(software, AI_model, mismatch_severity) %>%
    summarize_validation() %>%
    select(-Total_Cases, -Count_PASS, -Pass_Rate_Pct, -Count_MISMATCH, -Mismatch_Rate_Pct, -Count_EXECUTION_ERROR, -Error_Rate_Pct) %>%
    arrange(desc(Pass))
    
  print(stats_by_software_model_mismatch_severity)
  
}

#-------------------------------------------------------------------------------
# By gpt model & software & plot complexity
{
  stats_by_model_plot <- results_df %>%
    group_by(AI_model, plot) %>%
    summarize_validation()
  print(stats_by_model_plot)
}

#-------------------------------------------------------------------------------
