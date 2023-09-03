# check_inherits()
check_inherits(iris, "data.frame")
expect_error(check_inherits(iris, "data.table"))


