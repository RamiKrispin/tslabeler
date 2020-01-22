test_that("CSV input function works", {
        tmpfile <- tempfile(fileext = ".csv")
        dat <- data.table::data.table(
                ds = as.POSIXct(Sys.time(), tz = "UTC"),
                grp = "one",
                value = runif(1),
                anomaly = 0,
                tag = "tag1"
        )
        data.table::fwrite(
                x = dat,
                file = tmpfile,
                sep = ",",
                col.names = T
        )
        
        expect_length(
                object = process_input_file(
                        input_file = tmpfile,
                        sep = ",",
                        quote = '"',
                        ANOMALY_TAGS = TRUE,
                        GROUPS = TRUE,
                        date_coltype = "date_time"
                ),
                n = 6
        )
})
test_that("CSV input function fails for missing columns", {
        tmpfile <- tempfile(fileext = ".csv")
        dat <- data.table::data.table(
                ds = as.POSIXct(Sys.time(), tz = "UTC"),
                grp = "one",
                value = runif(1),
                anomaly = 0,
                tag = "tag1"
        )
        
        # GROUP set to TRUE, but no grp col in CSV
        data.table::fwrite(
                x = dat[,.(ds,value,anomaly,tag)],
                file = tmpfile,
                sep = ",",
                col.names = T
        )
        expect_error(
                object = process_input_file(
                        input_file = tmpfile,
                        sep = ",",
                        quote = '"',
                        ANOMALY_TAGS = TRUE,
                        GROUPS = TRUE,
                        date_coltype = "date_time"
                )
        )
        
        # ANOMALY_TAG set to TRUE, but no tag col in CSV
        data.table::fwrite(
                x = dat[,.(ds,grp,value,anomaly)],
                file = tmpfile,
                sep = ",",
                col.names = T
        )
        expect_error(
                object = process_input_file(
                        input_file = tmpfile,
                        sep = ",",
                        quote = '"',
                        ANOMALY_TAGS = TRUE,
                        GROUPS = TRUE,
                        date_coltype = "date_time"
                )
        )
        
        # ANOMALY_TAG set to TRUE, but no anomaly col in CSV
        data.table::fwrite(
                x = dat[,.(ds,grp,value,tag)],
                file = tmpfile,
                sep = ",",
                col.names = T
        )
        expect_error(
                object = process_input_file(
                        input_file = tmpfile,
                        sep = ",",
                        quote = '"',
                        ANOMALY_TAGS = TRUE,
                        GROUPS = TRUE,
                        date_coltype = "date_time"
                )
        )
})
test_that("CSV input function fails for incorrect date-time formats", {
        tmpfile <- tempfile(fileext = ".csv")
        dat <- data.table::data.table(
                ds = "ABCDEF",
                grp = "one",
                value = runif(1),
                anomaly = 0,
                tag = "tag1"
        )
        
        # GROUP set to TRUE, but no grp col in CSV
        data.table::fwrite(
                x = dat[,.(ds,value,grp,anomaly,tag)],
                file = tmpfile,
                sep = ",",
                col.names = T
        )
        expect_error(
                object = process_input_file(
                        input_file = tmpfile,
                        sep = ",",
                        quote = '"',
                        ANOMALY_TAGS = TRUE,
                        GROUPS = TRUE,
                        date_coltype = "date_time"
                )
        )
        expect_error(
                object = process_input_file(
                        input_file = tmpfile,
                        sep = ",",
                        quote = '"',
                        ANOMALY_TAGS = TRUE,
                        GROUPS = TRUE,
                        date_coltype = "date"
                )
        )
})