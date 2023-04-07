test_that("Get application", {

  mock_server <- function(protocol,
                          host,
                          port,
                          method,
                          path,
                          headers,
                          contentType = NULL,
                          contentFile = NULL,
                          certificate = NULL,
                          timeout = NULL) {

    if (startsWith(path, "/outputs/")) {
      output_id = as.integer(strsplit(path, "/")[[1]][3])
      body = list(
        "id"=output_id,
        "source_id"=1,
        "url"="http://fake-url.test.me/"
      )
    } else if (startsWith(path, "/applications/")) {
      application_id = as.integer(strsplit(path, "/")[[1]][3])
      body = list(
        "id"=application_id,
        "content_id"=5
      )
    } else {
      stop(paste("No mocked response defined for", path))
    }

    request <- list(
      protocol = protocol,
      host = host,
      port = port,
      method = method,
      path = path
    )

    list(
      req = request,
      status = 200,
      location = "",
      contentType = "application/json",
      content = jsonlite::toJSON(body, auto_unbox=TRUE)
    )
  }

  restore_opt <- options(rsconnect.http = mock_server)
  withr::defer(options(restore_opt))

  fake_service <- list(
    protocol="test",
    host="unit-test",
    port=42
  )
  client <- cloudClient(fake_service, NULL)

  app <- client$getApplication(10)

  expect_equal(app$id, 10)
  expect_equal(app$content_id, 5)
  expect_equal(app$url, "http://fake-url.test.me/")
})
