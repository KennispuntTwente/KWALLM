library(shinytest2)

test_that("{shinytest2} recording: file upload error handling", {
  app <- AppDriver$new(
    name = "file upload error handling",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  # Test 1: Upload normal working Excel file
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat", 
      "test_files",
      "normal_working.xlsx"
    )
  )
  
  # Wait for file to be processed
  Sys.sleep(3)
  
  # Check that sheet selector appears
  sheet_selector <- app$get_html("#text_upload-sheet_selector")
  expect_true(nchar(sheet_selector) > 0)
  
  # Select the first sheet
  app$set_inputs(`text_upload-sheet` = "Sheet1")
  Sys.sleep(2)
  
  # Check that column selector appears 
  column_selector <- app$get_html("#text_upload-column_selector")
  expect_true(nchar(column_selector) > 0)
  
  # Select text column
  app$set_inputs(`text_upload-column` = "text")
  Sys.sleep(2)
  
  # Test 2: Try to upload corrupted Excel file
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_files", 
      "corrupted.xlsx"
    )
  )
  
  # Wait for error message
  Sys.sleep(3)
  
  # Test 3: Upload empty Excel file
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_files",
      "empty.xlsx"
    )
  )
  
  # Wait for processing
  Sys.sleep(3)
  
  # Test 4: Upload file with special characters
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_files",
      "special_chars.xlsx"
    )
  )
  
  # Wait for processing
  Sys.sleep(3)
  
  # Select sheet and column
  app$set_inputs(`text_upload-sheet` = "Sheet1")
  Sys.sleep(2)
  app$set_inputs(`text_upload-column` = "text")
  Sys.sleep(2)
  
  # Test 5: Upload large file (should show warning about size)
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_files",
      "large_file.xlsx"
    )
  )
  
  # Wait for processing
  Sys.sleep(5) # Large file may take longer
  
  app$stop()
})

test_that("{shinytest2} recording: text file upload with encoding", {
  # Create a test text file with special characters
  test_text_file <- tempfile(fileext = ".txt")
  writeLines(
    c("Normal text", "Special chars: áéíóú", "Chinese: 中文", "Arabic: العربية"),
    test_text_file,
    useBytes = TRUE
  )
  
  app <- AppDriver$new(
    name = "text file encoding test",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  # Upload the text file
  app$upload_file(
    `text_upload-text_file` = test_text_file
  )
  
  # Wait for processing
  Sys.sleep(3)
  
  # Clean up
  unlink(test_text_file)
  app$stop()
})

test_that("{shinytest2} recording: CSV file upload", {
  # Create a test CSV file
  test_csv_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      text = c("CSV text 1", "CSV text 2", "CSV text 3"),
      other_col = c("A", "B", "C")
    ),
    test_csv_file,
    row.names = FALSE
  )
  
  app <- AppDriver$new(
    name = "CSV file upload test",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  # Upload the CSV file
  app$upload_file(
    `text_upload-text_file` = test_csv_file
  )
  
  # Wait for processing
  Sys.sleep(3)
  
  # Select text column
  app$set_inputs(`text_upload-column` = "text")
  Sys.sleep(2)
  
  # Clean up
  unlink(test_csv_file)
  app$stop()
})