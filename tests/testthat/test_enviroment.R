test_that("Check for existence of API key environment variable when calling read_airtable", {
  test_that("Environment variable at_pa_tkn not found", {
    expect_error(read_airtable("Contacts"),
                 "API key not found. Make sure you have your API key in a hidden variable named `at_pa_tkn`.")
  })

  test_that("Environment variable at_pa_tkn found", {
    Sys.setenv(at_pa_tkn = "your_api_key_here")
    expect_no_error(read_airtable("Contacts"))
    Sys.unsetenv("at_pa_tkn")  # Clean up the environment variable after the test
  })
})
