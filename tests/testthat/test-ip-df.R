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
        expect_true("grp" %in% names(processed$original))
        
        # If anomaly, tag not present, anomaly, tag col is added
        processed <-
                process_dt_from_env(out = data.table::data.table(
                        ds = as.POSIXct(Sys.time(), tz = "UTC"),
                        value = runif(1),
                        grp = "one"
                ))
        expect_true(all(c("tag", "anomaly") %in% names(processed$original)))
        
        
})

test_that("ds col is expected to be Date or POSIXct format", {
        # Fail for a NA or number or character
        expect_error(process_dt_from_env(
                out = data.table::data.table(
                        ds = NA,
                        grp = "one",
                        value = runif(1),
                        anomaly = 1,
                        tag = "tag1"
                )
        ))
        expect_error(process_dt_from_env(
                out = data.table::data.table(
                        ds = "abcdef",
                        grp = "one",
                        value = runif(1),
                        anomaly = 1,
                        tag = "tag1"
                )
        ))
        expect_error(process_dt_from_env(
                out = data.table::data.table(
                        ds = 12345,
                        grp = "one",
                        value = runif(1),
                        anomaly = 1,
                        tag = "tag1"
                )
        ))
        
        # Given a Date, get back a Date
        processed <- process_dt_from_env(
                out = data.table::data.table(
                        ds = as.Date("2020-01-01"),
                        grp = "one",
                        value = 3.1415,
                        anomaly = 1,
                        tag = "tag1"
                ))$original
        expect_s3_class(processed$ds, "Date")
        
        # Given a Date character, get back a POSIXct
        processed <- process_dt_from_env(
                out = data.table::data.table(
                        ds = "2020-01-03",
                        grp = "one",
                        value = 3.1415,
                        anomaly = 1,
                        tag = "tag1"
                ))$original
        expect_s3_class(processed$ds, "POSIXct")
        
        # Given a POSIXct, get back a POSIXct
        processed <- process_dt_from_env(
                out = data.table::data.table(
                        ds = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                        grp = "one",
                        value = 3.1415,
                        anomaly = 1,
                        tag = "tag1"
                ))$original
        expect_s3_class(processed$ds, "POSIXct")
        
})

test_that("df input function correctly parses and appends new tags to values$tag_values",
          {
                  dat <- data.table::data.table(
                          ds = as.POSIXct(Sys.time(), tz = "UTC"),
                          grp = "one",
                          value = runif(1),
                          anomaly = 1,
                          tag = "tag1"
                  )
                  values <- process_dt_from_env(dat)
                  expect_identical(values$tag_values,
                                   c("spike",
                                     "trend-change",
                                     "level-shift",
                                     "variance-shift",
                                     "tag1",
                                     ""))
          })

test_that("df input function passes warning when anomaly and tag are not consistant",
          {
                  # CASE 1 - Anomaly = 1, but no Tag is present
                  dat <- data.table::data.table(
                          ds = as.POSIXct(Sys.time(), tz = "UTC"),
                          grp = "one",
                          value = runif(1),
                          anomaly = 1,
                          tag = ""
                  )
                  expect_warning(process_dt_from_env(dat))
                  
                  # CASE 2 - Anomaly = 0, but a Tag is present
                  dat <- data.table::data.table(
                          ds = as.POSIXct(Sys.time(), tz = "UTC"),
                          grp = "one",
                          value = runif(1),
                          anomaly = 0,
                          tag = "tag1"
                  )
                  expect_warning(process_dt_from_env(dat))
                  
          })

test_that("df input function fails when anomaly col has values other than [0,1]",
          {
                  dat <- data.table::data.table(
                          ds = c(
                                  as.POSIXct(Sys.time(), tz = "UTC"),
                                  as.POSIXct(Sys.time(), tz = "UTC")
                          ),
                          grp = c("one", "one"),
                          value = runif(2),
                          anomaly = c(0, 2),
                          tag = c("", "abc")
                  )
                  expect_error(process_dt_from_env(dat))
                  
                  dat <- data.table::data.table(
                          ds = c(
                                  as.POSIXct(Sys.time(), tz = "UTC"),
                                  as.POSIXct(Sys.time(), tz = "UTC")
                          ),
                          grp = c("one", "one"),
                          value = runif(2),
                          anomaly = c(NA, "a"),
                          tag = c("", "abc")
                  )
                  expect_error(process_dt_from_env(dat))
                  
          })