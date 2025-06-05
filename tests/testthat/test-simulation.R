test_that("simulate_blackjack_with_split runs without error", {
  res <- simulate_blackjack(num_rounds = 10)
  expect_true(is.list(res))
  expect_true("win_rate" %in% names(res))
})
test_that("simulate works for different split/buy_insurance strategies", {
  res1 <- simulate_blackjack(num_rounds = 10, split = TRUE, buy_insurance = FALSE)
  res2 <- simulate_blackjack(num_rounds = 10, split = FALSE, buy_insurance = TRUE)
  expect_true(is.numeric(res1$win_rate))
  expect_true(is.numeric(res2$lose_rate))
})

test_that("simulate handles custom split function", {
  res <- simulate_blackjack(
    num_rounds = 10,
    split = function(hand) { FALSE }
  )
  expect_true(is.list(res))
})
test_that("simulate_blackjack fails on bad split", {
  expect_error(simulate_blackjack(num_rounds = 5, split = "xxx"))
})
test_that("simulation returns correct structure", {
  res <- simulate_blackjack(num_rounds = 10)
  expect_true(all(c("win_rate", "lose_rate", "tie_rate", "final_coins") %in% names(res)))
})
