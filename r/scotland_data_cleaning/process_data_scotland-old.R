library(readxl)
library(snakecase)
library(data.table)


dem_filename <- "data/raw_data/sc/panel_a/wave_1/Panel A Demographics - Raw Data (EMPTY DATASET).xlsx"
dem_dt <- readxl::read_xlsx(dem_filename)
nrow(dem_dt)
cols <- names(dem_dt)
cols <- data.table(var_short = gsub(":.*|\\/", "", cols))
cols[, var := ifelse(!grepl("\\.|[a-z]|[A-Z]", var_short), paste0(var_short, ".", rowid(var_short)), var_short)]
cols[, var_original := names(dem_dt)]
cols

base_qid <- NA
i <- 0
tables_dt <- NA
for (q in unique(seq_questions$var_original)[1:1000]) {

  varname <- snakecase::to_snake_case(q)
  question_id <- as.numeric(gsub('.*\\[question id="|\\".*', "", q))

  if (is.na(question_id) & is.na(base_qid)) {
    message(paste("Skipping:", q))
  } else if(is.na(question_id) & !is.na(base_qid)) {
    qid_seq <- base_qid
  } else if (!is.na(question_id)) {
    # check if the question_id has an associated qid_seq
    question_id_ <- question_id
    qid_seq <- q_ids[question_id == question_id_]$qid_seq
    if (is.null(qid_seq) | length(qid_seq) == 0) {
      # create qid_seq
      i <- i + 1
      x <- data.table(question_id = question_id)
      x[, qid_seq := i]
      q_ids <- rbind(q_ids, x, fill = T)
      qid_seq <- q_ids[question_id == question_id_]$qid_seq
    }
  } else {
    print("else")
    browser()
  }

  if (!is.na(qid_seq)) {
    cs <- grep(varname, names(match_rd_dt), value = T)
    c
    q_wide <- match_rd_dt[,c(id_vars, cs), with = F]
    q_long <- melt(q_wide, id.cols = id_vars,
                   measure.vars = cs,
                   value.name = "value")
    col_name <- sub("^.*[0-9]_{1}","" , as.character(q_long$variable[1]), perl = T)
    if(!is.na(question_id)) {
      col_name <- gsub(as.character(question_id) ,"" ,col_name, perl = T)
    }
    setnames(q_long, "value", col_name)
    q_long[, variable := NULL]
    q_long[, table_row := qid_seq]
    base_qid <- qid_seq
    if(is.na(tables_dt)) {
      tables_dt <- q_long
    } else {
      # lapply(tables_dt, class)
      # lapply(q_long, class)
      tables_dt <- merge_tables(tables_dt, q_long, all = T)
    }

  }
}



View(tables_dt)

# potential for later
# data_dt <- merge(loop_questions_dt, seq_questions_dt, by = match_vars, all.x  = T)
