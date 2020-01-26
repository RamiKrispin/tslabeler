test_that("df input function handles columns correctly", {
        # A minimum of ds, value columns are present
        expect_error(process_dt_from_env(out = data.table::data.table(value = runif(n = 10))))
        expect_error(process_dt_from_env(out = data.table::data.table(ts = as.POSIXct(
                Sys.time(), tz = "UTC"
        ))))
        
        # If grp not present, grp col is added
        processed <-
                process_dt_from_env(out = data.table::data.table(
                        ds = as.POSIXct(Sys.time(), tz = "UTC"),
                        value = runif(1)
                ))
        expect_true("grp" %in% names(processed))
        
        # If anomaly, tag not present, anomaly, tag col is added
        processed <-
                process_dt_from_env(out = data.table::data.table(
                        ds = as.POSIXct(Sys.time(), tz = "UTC"),
                        value = runif(1),
                        grp = "one"
                ))
        expect_true(all(c("tag", "anomaly") %in% names(processed)))
        
        
})

test_that("ds col is expected to be Date or POSIXct format", {
        # Fail for a NA or number or character
        expect_error(process_dt_from_env(
                out = data.table::data.table(
                        ds = NA,
                        value = runif(1),
                        grp = "one",
                        tag = "tag1",
                        anomaly = 1
                )
        ))
        expect_error(process_dt_from_env(
                out = data.table::data.table(
                        ds = "abcdef",
                        value = runif(1),
                        grp = "one",
                        tag = "tag1",
                        anomaly = 1
                )
        ))
        expect_error(process_dt_from_env(
                out = data.table::data.table(
                        ds = 12345,
                        value = runif(1),
                        grp = "one",
                        tag = "tag1",
                        anomaly = 1
                )
        ))
        
        # Given a Date character, get back a Date
        expect_identical(
                object = process_dt_from_env(
                        out = data.table::data.table(
                                ds = "2020-01-03",
                                value = 3.1415,
                                grp = "one",
                                tag = "tag1",
                                anomaly = 1
                        )
                ),
                expected = data.table::data.table(
                        ds = lubridate::ymd("2020-01-03", tz = "UTC"),
                        value = 3.1415,
                        grp = "one",
                        tag = "tag1",
                        anomaly = 1,
                        key = "ds"
                )
        )
        
        # Given a Date, get back a Date
        expect_identical(
                object = process_dt_from_env(
                        out = data.table::data.table(
                                ds = as.Date("2020-01-01"),
                                value = 3.1415,
                                grp = "one",
                                tag = "tag1",
                                anomaly = 1
                        )
                ),
                expected = data.table::data.table(
                        ds = lubridate::ymd("2020-01-01"),
                        value = 3.1415,
                        grp = "one",
                        tag = "tag1",
                        anomaly = 1,
                        key = "ds"
                )
        )
        
        # Given a POSIXct, get back a POSIXct
        expect_identical(
                object = process_dt_from_env(
                        out = data.table::data.table(
                                ds = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                                value = 3.1415,
                                grp = "one",
                                tag = "tag1",
                                anomaly = 1
                        )
                ),
                expected = data.table::data.table(
                        ds = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                        value = 3.1415,
                        grp = "one",
                        tag = "tag1",
                        anomaly = 1,
                        key = "ds"
                )
        )
        
})

test_that("df input function correctly parses and appends new tags to values$tag_values",
          {
                  
          })

test_that("df input function passes warning when anomaly and tag are not consistant",
          {
                  # CASE 1 - Anomaly = 1, but no Tag is present
                  
                  # CASE 2 - Anomaly = 0, but a Tag is present
                  
          })

test_that("df input function fails when anomaly col has values other than [0,1]",
          {
                  
          })