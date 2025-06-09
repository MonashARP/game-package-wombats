test_that("get_home_dir returns HOME if set", {
  withr::local_envvar(HOME = "/fake/home")
  expected <- normalizePath("/fake/home", mustWork = FALSE)
  actual   <- normalizePath(get_home_dir(), mustWork = FALSE)
  expect_equal(actual, expected)
})

test_that("get_home_dir falls back to USERPROFILE", {
  withr::local_envvar(HOME = "", USERPROFILE = "C:/Users/FakeUser")
  expect_equal(
    normalizePath(get_home_dir(), mustWork = FALSE),
    normalizePath("C:/Users/FakeUser", mustWork = FALSE)
  )
})

test_that("get_home_dir uses tempdir if no env vars set", {
  withr::local_envvar(HOME = "", USERPROFILE = "")
  expect_equal(
    normalizePath(get_home_dir(), mustWork = FALSE),
    normalizePath(tempdir(), mustWork = FALSE)
  )
})

