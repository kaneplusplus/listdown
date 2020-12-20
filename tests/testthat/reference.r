make_reference <- FALSE

write_if_make_reference <-
  function(object, file_name, reference_data_dir = "reference-data",
           write_reference = make_reference) {

  if (!dir.exists(reference_data_dir)) {
    dir.create(reference_data_dir)
  }

  if (write_reference) {
    saveRDS(object, file.path(reference_data_dir, file_name))
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

read_reference <- function(file_name, reference_data_dir = "reference-data") {
  readRDS(file.path(reference_data_dir, file_name))
}
