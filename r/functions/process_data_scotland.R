library(readxl)
library(snakecase)
library(data.table)


# dem_filename <- "data/raw_data/sc/panel_a/wave_1/Panel A Demographics - Raw Data (EMPTY DATASET).xlsx"
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
# rd_dt[1,]

# standardize names
match_rd_dt <- rd_dt
names(match_rd_dt) <- snakecase::to_snake_case(names(rd_dt))


# names(rd_dt)
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

merge_tables <- function(t1, t2, all) {
  # mergeby <- intersect(names(t1), names(t2))
  mergeby <- c("respondent", "table_row")
  # merge(t1, t2, by = mergeby)
  # if (length(t2$table_row) > 1) browser()
  if (nrow(t1[table_row %in% t2$table_row]) == 0) {
    t3 <- rbind(t1,t2, fill = T)
  } else {
    tr <- t2$table_row
    t2a <- t2[, -mergeby, with = F]
    nam <- names(t2a)
    t1[table_row == tr, c(nam) := t2a[, get(nam)]]
    t3 <- t1
  }
  t3
}


#


loop_questions <- rd[var_short %in% c(28.3)]

loop_questions
table_list <- list()
id_vars <- "respondent"
tables_dt_symptoms  <- NA
for (q in unique(loop_questions$var_question)) {
  varname <- snakecase::to_snake_case(q)
  cs <- grep(varname, names(match_rd_dt), value = T)
  cs <- grep("^28_", names(match_rd_dt), value = T)
  # browser()
  #
  q_wide <- match_rd_dt[,c(id_vars, cs), with = F]
  q_long <- melt(q_wide, id.cols = id_vars,
                 measure.vars = cs,
                 value.name = varname)
  q_long[grepl("[0-9]_[0-9]", variable),
         table_row := as.numeric(sub("^[^_]*_([^_]*).*", "\\1", variable))]
  q_long <- q_long[!grepl("[0-9]_[0-9]", variable), table_row := 0]
  q_long[, variable := NULL]
  # browser()
  if (is.na(tables_dt_symptoms) | length(tables_dt_symptoms) == 0) {
    tables_dt_symptoms <- q_long
  } else{
    # if (q == " _None of these") browser()
    tables_dt_symptoms <- merge_tables(tables_dt_symptoms, q_long, all = T)
  }
}

# View(tables_dt_symptoms)

match_vars <- c(id_vars, "table_row")
loop_questions_dt <- Reduce(function(...) merge(..., by = match_vars), table_list)
# PROBLEM: table_row == 0 and respondent == 1 is not merging into one row but several (??)



# # Problem: not all var are numbers
rd[, varn := as.numeric(var)]
#
#
rd[grepl("\\[question id=", var_original), question_id := as.numeric(gsub('.*\\[question id="|\\".*', "", var_original)) ]
rd <- rd[order(varn)][!is.na(question_id), seq_id := .GRP, by = question_id]
rd[order(varn), seq_id := seq_id[1], .(cumsum(!is.na(seq_id)))]
#
seq_questions <- rd[!is.na(seq_id) & loop_question == FALSE]
# seq_questions <- seq_questions[seq_id != 4]
# PROBLEM: seq_id == 4 are data for several contacts
# seq_questions <- seq_questions[var_short != 27]
# seq_questions <- seq_questions[!grep("^33\\.|34\\.", var_original)]



#Approach 2
#
table_list <- list()
id_vars <- "respondent"
q_ids <- data.table()
base_qid <- NA
i <- 0
tables_dt <- NA
for (q in unique(seq_questions$var_original)) {
  skip <- F
  varname <- snakecase::to_snake_case(q)
  num <- gsub("_.*", "", varname)
  # if (grepl("age_group", varname)) browser()

  if (num %in% c("27")) {
    skip <- T
    base_qid <- NA
  }
  print(varname)
  new_seq_id <- FALSE
  # if (grepl("extent", varname)) browser()
  question_id <- as.numeric(gsub('.*\\[question id="|\\".*', "", q))
  if(!is.na(question_id)) new_seq_id <- TRUE
  if (is.na(question_id)) {
    question_id <- as.numeric(gsub(".*HM|!.*", "", q))
    if (!is.na(question_id)) base_qid <- question_id
  }
  if (is.na(question_id)) {
    # browser()
    question_id <- as.numeric(gsub(".*new_person_|_who.*", "", snakecase::to_snake_case(q)))
    if (!is.na(question_id)) base_qid <- question_id

  }
  if (is.na(question_id)) {
    question_id <- as.numeric(strsplit(varname, "_")[[1]][2])
    if (!is.na(question_id)) base_qid <- question_id
  }
  # if (grepl("hm_[9]", varname)) browser()

  if (is.na(question_id) & is.na(base_qid)) {
    message(paste("Skipping:", q))
    skip <- TRUE
  } else if(is.na(question_id) & !is.na(base_qid)) {
    qid_seq <- base_qid
  } else if (!is.na(question_id)) {
    # check if the question_id has an associated qid_seq
    question_id_ <- question_id
    qid_seq <- q_ids[question_id == question_id_]$qid_seq
    if (is.null(qid_seq) | length(qid_seq) == 0) {
      # create qid_seq
      # if(new_seq_id) i <- i + 1
      if (nchar(question_id) > 3) {
        i <- i + 1
        x <- data.table(question_id = question_id)
        x[, qid_seq := i]
        q_ids <- rbind(q_ids, x, fill = T)
        qid_seq <- q_ids[question_id == question_id_]$qid_seq
        base_qid <- qid_seq
      } else {
        # browser()
        qid_seq <- question_id
      }
    }
  } else {
    print("else")
    browser()
  }



  if(skip) browser()
  if (!is.na(qid_seq) & !skip) {
    cs <- grep(varname, names(match_rd_dt), value = T)
    # if(varname == "61_outside") cs <- "61_outside"
    num <- gsub("_.*", "", varname)
    # if (num == "134") browser()
    # cs <- grep(paste0("^", num), cs, value = T)
    # 61 and 73
    if (varname  == "61_outside")  cs <- "61_outside"
    if (num %in% c("73", "74", "76", "81", "93", "94", "96")) {
      cs <- grep(paste0("^", num), cs, value = T)
    }

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
    # brow
    if(is.na(tables_dt)) {
      tables_dt <- q_long
    } else {
      # lapply(tables_dt, class)
      # lapply(q_long, class)
      if (length(q_long$table_row) > 1) browser()

      tables_dt <- merge_tables(tables_dt, q_long, all = T)
    }

  }
  if (num == "373")  return()
}



View(tables_dt)

# potential for later
# data_dt <- merge(loop_questions_dt, seq_questions_dt, by = match_vars, all.x  = T)
