# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1")
        utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())

#' Processes input CSV/TSV files
#' 
#' This file would contain the timeseries to be labeled. A typical file will contain a minimum of 3 columns
#' (ds, grp, value)
#' 
#' The ds column may be date (%Y-%m-%d) or date-time (%Y-%m-%d %H:%M:%S). `UTC` timezone is assumed
#'
#' @param input_file 
#' @param sep 
#' @param quote 
#' @param ANOMALY_TAGS 
#' @param GROUPS 
#' @param date_coltype 
#'
#' @return
process_input_file <- function(input_file,
                               sep,
                               quote,
                               ANOMALY_TAGS,
                               GROUPS,
                               date_coltype) {
        # Read CSV
        out <- data.table::fread(file = input_file,
                                 header = TRUE,
                                 sep = sep,
                                 quote = quote)
        
        # Check columns, quality check data
        if(ANOMALY_TAGS)
                if(!all(c("anomaly","tag") %in% names(out)))
                        stop("You have stated Anomaly and Tag columns are present. However, `anomaly` or `tag` were not found in the header.")
        
        if(all(c("anomaly","tag") %in% names(out)))
                ANOMALY_TAGS <- TRUE
        
        if(GROUPS)
                if(!all(c("grp") %in% names(out)))
                        stop("You have stated Group column is present. However, `grp` was not found in the header.")
        if("grp" %in% names(out))
                GROUPS <- TRUE
        
        if(!all(c("ds","value") %in% names(out)))
                stop("`ds` or `value` was not found in the header.")
        
        if(!GROUPS)
                out[,grp:="No Groups"]
        
        if(!ANOMALY_TAGS){
                out[,anomaly:=0]
                out[,tag:=""]
        }
        
        if(!all(out[,unique(anomaly)] %in% c(0,1)))
                stop("anomaly column contains values not [0,1]")
        
        # Change col order to standard order
        out <- out[,.(ds, grp, value, anomaly, tag)]
        
        # Test for NA values in tag. If present, replace by ""
        if((count_ <- out[is.na(tag),.N])!=0){
                warning("Found ", count_, " NA values in tag column. Replacing by empty string ('')")
                out[is.na(tag), tag := ""]
        }
        
        # Col data types
        if(date_coltype=="date")
                out[,ds:=lubridate::ymd(ds, quiet = T, tz = "UTC")]
        if(date_coltype=="date_time")
                out[,ds:=lubridate::ymd_hms(ds, quiet = T, tz = "UTC")]
        if (any(is.na(out[, ds]))) {
                stop("Could not parse date-time column. Format expected - For date-time: %Y-%m-%dT%H:%M:%SZ. For date: %Y-%m-%d")
        }
        out[, value := as.numeric(value)]
        
        # Set order by date-time
        data.table::setkeyv(out, "ds")
        
        # Any anomaly=1 where tag = ""?
        count_ <- out[anomaly==1 & tag=="", .N]
        if(count_!=0)
                warning("Found ",count_," rows where anomaly is 1, yet tag is empty ('')")
        
        # Any anomaly=0 where tag != ""?
        count_ <- out[anomaly==0 & tag!="", .N]
        if(count_!=0)
                warning("Found ",count_," rows where anomaly is 0, yet tag is not empty")
        
        return_list <- list()
        
        # Process tag values
        return_list$tag_values <- c("spike",
                               "trend-change",
                               "level-shift",
                               "variance-shift",
                               "")
        tag_choices <- return_list$tag_values
        tag_choices[tag_choices == ""] <- "remove tag"
        return_list$tag_choices <- tag_choices
        
        return_list$total_pts <- out[, .N]
        return_list$total_grps <- out[, length(unique(grp))]
        
        tags_in_file <- out[anomaly == 1, unique(tag)]
        
        custom_tags <- tags_in_file[!(tags_in_file %in% return_list$tag_values)]
        
        if (length(custom_tags) > 0) {
                return_list$tag_values <- c(return_list$tag_values[return_list$tag_values != ""],
                                       custom_tags, "")
                return_list$tag_choices <- c(return_list$tag_choices[return_list$tag_choices != "remove tag"], custom_tags, "remove tag")
        }
        
        return_list$count_existing_anomalies <- out[anomaly == 1, .N]
        
        return_list$original <- out
        
        return_list
        
}