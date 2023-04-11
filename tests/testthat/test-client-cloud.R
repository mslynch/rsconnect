mockServerFactory <- function(responses) {
  mockServer <- function(protocol,
                          host,
                          port,
                          method,
                          path,
                          headers,
                          contentType = NULL,
                          contentFile = NULL,
                          certificate = NULL,
                          timeout = NULL) {

    methodAndPath = paste(method, path)

    request <- list(
      protocol = protocol,
      host = host,
      port = port,
      method = method,
      path = path
    )

    response = list(
      req = request,
      status = 200,
      location = "",
      contentType = "application/json"
    )

    # Stock responses for certain endpoints. Can still be overridden by tests.
    if (is.null(responses$"^GET /v1/users/current")) {
      responses$"GET /v1/users/current" = list(
        content = list(
          id=100
        )
      )
    }

    if (is.null(responses$"^GET /v1/accounts/?")) {
      responses$"^GET /v1/accounts/?" = list(
        content = list(
          count=1,
          total=1,
          offset=0,
          accounts=list(
            list(
              id=50,
              name="testthat-account",
              account="testthat-account"
            )
          )
        )
      )
    }

    found <- FALSE

    for (pathRegex in names(responses)) {
      match <- regexec(pathRegex, methodAndPath)[[1]]
      if (match[1] != -1) {
        found <- TRUE
        responseSupplement <- responses[[pathRegex]]

        for (respProperty in names(responseSupplement)) {
          if (is.function(responseSupplement[[respProperty]])) {
            responseSupplement[[respProperty]] <- responseSupplement[[respProperty]](
              methodAndPath,
              match,
              headers=headers,
              contentType=contentType,
              contentFile=contentFile)
          }

          response[[respProperty]] = responseSupplement[[respProperty]]
        }

        if (is.list(response$content)) {
          response$content <- jsonlite::toJSON(response$content, auto_unbox=TRUE)
        }

        break
      }
    }

    if (!found) {
      stop(paste("No mocked response defined for", methodAndPath))
    }

    response
  }

  mockServer
}

configureTestAccount <- function(server = 'posit.cloud', name = NULL) {
  if (is.null(name)) {
    name = 'testthat-account'
  }

  existingAccount <- NULL
  tryCatch(
    existingAccount <- accountInfo(name, server),
    error = function(e) { existingAccount = NULL }
  )

  if (is.null(existingAccount)) {
    setAccountInfo(
      name = name,
      token = 'foo',
      secret = 'bar',
      server = server
    )
  }

  name
}

test_that("Get application", {
  mockServer = mockServerFactory(list(
    "^GET /outputs/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, 'match.length')[2] + match[2]
        output_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id"=output_id,
          "source_id"=1,
          "url"="http://fake-url.test.me/"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, 'match.length')[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id"=application_id,
          "content_id"=5
        )
      }
    )
  ))

  restoreOpt <- options(rsconnect.http = mockServer)
  withr::defer(options(restoreOpt))

  fakeService <- list(
    protocol="test",
    host="unit-test",
    port=42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$getApplication(10)

  expect_equal(app$id, 10)
  expect_equal(app$content_id, 5)
  expect_equal(app$url, "http://fake-url.test.me/")

  app <- client$getApplication("lucid:content:5")

  expect_equal(app$id, 1)
  expect_equal(app$content_id, 5)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create application", {
  mockServer = mockServerFactory(list(
    "^POST /outputs" = list(
      content = list(
        "id"=1,
        "source_id"=2,
        "url"="http://fake-url.test.me/"
      )
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, 'match.length')[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id"=application_id,
          "content_id"=1
        )
      })
  ))

  restoreOpt <- options(rsconnect.http = mockServer)
  withr::defer(options(restoreOpt))

  fakeService <- list(
    protocol="test",
    host="unit-test",
    port=42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "static")

  expect_equal(app$id, 2)
  expect_equal(app$content_id, 1)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create application with linked source project", {
  mockServer = mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPAth, match, contentFile, ...) {
        content = jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))

        expect_equal(content$project, 41)
        expect_equal(content$space, 99)

        list(
        "id"=1,
        "source_id"=2,
        "url"="http://fake-url.test.me/"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, 'match.length')[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))
        list(
          "id"=application_id,
          "content_id"=application_id-1
        )
      }
    ),
    "^GET /content/41" = list(
      content = list(
        "id"=41,
        "space_id"=99
      )
    )
  ))

  restoreOpt <- options(rsconnect.http = mockServer)
  withr::defer(options(restoreOpt))

  Sys.setenv(LUCID_APPLICATION_ID="42")
  withr::defer(Sys.unsetenv("LUCID_APPLICATION_ID"))

  fakeService <- list(
    protocol="test",
    host="unit-test",
    port=42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "static")

  expect_equal(app$id, 2)
  expect_equal(app$content_id, 1)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("deploymentTargetForApp() results in correct Cloud API calls", {
  mockServer = mockServerFactory(list(
    "^GET /v1/applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, 'match.length')[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))
        list(
          "id"=application_id,
          "content_id"=application_id-1,
          "name"=paste("testthat app", application_id)
        )
      }
    ),
    "^GET /v1/outputs/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, 'match.length')[2] + match[2]
        output_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id"=output_id,
          "source_id"=output_id + 1,
          "url"="http://fake-url.test.me/"
        )
      }
    )
  ))

  restoreOpt <- options(rsconnect.http = mockServer)
  withr::defer(options(restoreOpt))

  testAccount <- configureTestAccount()
  withr::defer(removeAccount(testAccount))

  target <- deploymentTargetForApp(
    appId = 3,
    account = testAccount,
    server = 'posit.cloud',
  )

  expect_equal(target$appName, "testthat app 3")
  expect_equal(target$account, testAccount)
  expect_equal(target$server, 'posit.cloud')
  expect_equal(target$appId, 3)
})

test_that("deployApp() results in correct Cloud API calls", {
  mockServer = mockServerFactory(list())

  restoreOpt <- options(rsconnect.http = mockServer)
  withr::defer(options(restoreOpt))

  deployApp(
    appDir = test_path('shinyapp-simple'),
    server = 'posit.cloud'
  )

  # TODO: add another posit.cloud account and test again with that environment
})
