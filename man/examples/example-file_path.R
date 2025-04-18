# Set up example
temp <- tempdir()
sink <- file.path(temp, "patter")
dir.create(sink, recursive = TRUE)
write.table("", file = file.path(sink, "1.csv"))
write.table("", file = file.path(sink, "2.csv"))

# Use `file_path()` to construct & validate file paths
file_path(temp, "patter")

# Use `file_list()` to list files
file_list(temp, "patter", pattern = "*.csv$")

# Use `file_size()` to summarise file sizes
file_size(temp, "patter")
file_size(temp, "patter", .unit = "GB")
file_size(temp, "patter", .unit = "TB")

file_cleanup(sink)
