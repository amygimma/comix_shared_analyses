library(readxl)
library(snakecase)
library(data.table)

#
# dem_dt <- readxl::read_xlsx(dem_filename)
# nrow(dem_dt)
# cols <- names(dem_dt)
# cols <- data.table(var_short = gsub(":.*|\\/", "", cols))
# cols[, var := ifelse(!grepl("\\.|[a-z]|[A-Z]", var_short), paste0(var_short, ".", rowid(var_short)), var_short)]
# cols[, var_original := names(dem_dt)]
# cols

rd_filename <- "data/raw_data/sc/panel_a/wave_1/Panel A Wave - Raw Data (EMPTY DATASET).xlsx"
rd_dt <- readxl::read_xlsx(rd_filename)
#Make one row of data
rd_dt[1,] <- 1
rd_dt <- as.data.table(rd_dt)
rd_dt[, Respondent := 1:nrow(rd_dt)]
rd_dt[1,]

# standardize names
match_rd_dt <- rd_dt
names(match_rd_dt) <- snakecase::to_snake_case(names(rd_dt))


names(rd_dt)
rd <- data.table(var_original = names(rd_dt))
rd[, var_short := gsub(":.*|\\/", "", var_original)]
rd[, var := ifelse(!grepl("\\.|[a-z]|[A-Z]", var_short), paste0(var_short, ".", rowid(var_short)), var_short)]
rd[, var_question := gsub(".*:|\\/|[0-9]", "", var_original)]
rd[, var_number := gsub("\\..*|\\/", "", var_original)]
rd[grepl("\\.", var_short), cnt_id := gsub(".*\\.", "", var_short)]
rd[, var2 := paste0(var_short, ".", rowid(var_short)), by = var]
rd[grepl("Contact ", var_original), contact_id := gsub("*Contact ", "", var_original, ignore.case = F)]
rd[grepl("!HM", var_original), hhm_id := gsub("*!HM|:*","", var_original)]
rd[, loop_question := grepl("[0-9]_[A-Z]", var_original) ]
# rd[loop_question == FALSE, loop_question := grepl("\\.", var_short) ]
#
# rd[var_original == "5.0: How many new people have joined your household?",
#    loop_question := FALSE ]
rd[(!grepl("\\.|[a-z]|[A-Z]", var_short)) & loop_question == F, var_number := gsub("\\:.*|\\/", "", var_original)]

# rd[grepl(var_original )]
rdu <- unique(rd, by = "var2")
rdu <- unique(rd, by = "var_question")

# View(rdu)
fwrite(rdu, "data/contact_questions.csv")



#


loop_questions <- rd[var_short %in% c(28.3)]

loop_questions
table_list <- list()
id_vars <- "respondent"
for (q in unique(loop_questions$var_question)) {
  # browser()
  varname <- snakecase::to_snake_case(q)

  cs <- grep(varname, names(match_rd_dt), value = T)

  q_wide <- match_rd_dt[,c(id_vars, cs), with = F]
  q_long <- melt(q_wide, id.cols = id_vars,
                 measure.vars = cs,
                 value.name = varname)
  q_long[grepl("[0-9]_[0-9]", variable),
         table_row := as.numeric(sub("^[^_]*_([^_]*).*", "\\1", variable))]
  q_long <- q_long[!grepl("[0-9]_[0-9]", variable), table_row := 0]
  q_long[, variable := NULL]
  table_list[[paste0("table_", varname)]] <- q_long
}

match_vars <- c(id_vars, "table_row")
loop_questions_dt <- Reduce(function(...) merge(..., by = match_vars), table_list)
# PROBLEM: table_row == 0 and respondent == 1 is not merging into one row but several (??)



# Problem: not all var are numbers
rd[, varn := as.numeric(var)]


rd[grepl("\\[question id=", var_original), question_id := as.numeric(gsub('.*\\[question id="|\\".*', "", var_original)) ]
rd <- rd[order(varn)][!is.na(question_id), seq_id := .GRP, by = question_id]
rd[order(varn), seq_id := seq_id[1], .(cumsum(!is.na(seq_id)))]

seq_questions <- rd[!is.na(seq_id) & loop_question == FALSE]

# PROBLEM: seq_id == 4 are data for several contacts
seq_questions <- seq_questions[seq_id != 4]

# table_list <- list()
# id_vars <- "respondent"
# for (q in unique(seq_questions$var_original)) {
#   # browser()
#   varname <- snakecase::to_snake_case(q)
#   # if (varname == "157_before_the_coronavirus_epidemic_started_how_often_did_you_usually_have_direct_contact_with_question_id_80984179_output_answer_type_text_encoding_utf_8_select_only_one") browser()
#   cs <- grep(varname, names(match_rd_dt), value = T)
#
#   q_wide <- match_rd_dt[,c(id_vars, cs), with = F]
#   q_long <- melt(q_wide, id.cols = id_vars,
#                  measure.vars = cs,
#                  value.name = varname)
#   table_row <- rd[varname == snakecase::to_snake_case(var_original)]$seq_id
#   q_long[, table_row := table_row]
#   q_long[, variable := NULL]
#   table_list[[paste0("table_", varname)]] <- q_long
# }
#
# match_vars <- c(id_vars, "table_row")
# # PROBLEM: Seems to work but I can't get them to merge together in groups of more than 6
# seq_questions_dt <-
#   Reduce(function(...) merge(..., by = match_vars), table_list[1:4])



# potential solution: merge in groups of 5 (works i think), then merge the resulting dts (doesnt work)
seq_questions_dt_list <- list()
i <- 1
for (n in seq(1,length(table_list), 4)) {
  # print(n)
  l <- n+4

  seq_questions_dt_list[[i]] <-
    Reduce(function(...) merge(..., by = match_vars), table_list[n:l])
  i <- i + 1
}

seq_questions_dt <-  Reduce(function(...) merge(..., by = match_vars), seq_questions_dt_list[1:2])



# eventually:

data_dt <- merge(loop_questions_dt, seq_questions_dt, by = match_vars, all.x  = T)





#Approach 2
#
table_list <- list()
id_vars <- "respondent"
q_ids <- data.table()
base_qid <- NA
i <- 0
tables_dt <- NA
for (q in unique(seq_questions$var_original)[1:100]) {

  varname <- snakecase::to_snake_case(q)
  question_id <- as.numeric(gsub('.*\\[question id="|\\".*', "", q))

  if (is.na(question_id) & is.na(base_qid)) {
    message(paste("Skipping:", q))
  } else if(is.na(question_id) & !is.na(base_qid)) {
    # browser()
    qid_seq <- base_qid
  } else if (!is.na(question_id)) {
    # browser()
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
    # browser()
    col_name <- sub("^.*[0-9]_{1}","" , as.character(q_long$variable[1]), perl = T)
    if(!is.na(question_id)) {
      # browser()
      col_name <- gsub(as.character(question_id) ,"" ,col_name, perl = T)
    }
    setnames(q_long, "value", col_name)
    # browser()
    q_long[, variable := NULL]
    q_long[, table_row := qid_seq]
    q
    base_qid <- qid_seq
    # browser()
    # table_list[[paste0("table_", varname)]] <- q_long
    if(is.na(tables_dt)) {
      tables_dt <- q_long
    } else {

      tables_dt <- merge_tables(tables_dt, q_long, all = T)
    }

  }
}

View(  tables_dt)

merge_tables <- function(t1, t2, all) {
  mergeby <- intersect(names(t1), names(t2))
  merge(t1, t2, by = mergeby, all = all)
  # merge(t1,t2, by = mergeby, all = all)
}
#
#
# seq_questions_dt <-
#   Reduce(function(...) merge_tables(...), table_list)
# x <- 1
# if (x == 0) {
#   print(x)
# } else if(x == 1) {
#   print(x+1)
# } else {
#   print(x+2)
# }
