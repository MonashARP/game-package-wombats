test_that("card_value returns correct values", {
  cards <- card(rank = c("A", "10", "Q", "2"), suit = c("♠", "♦", "♥", "♣"))
  expect_equal(unname(card_value(cards)), c(11L, 10L, 10L, 2L))
})

test_that("card_is_face correctly identifies face cards", {
  cards <- card(c("J", "7", "Q", "K"), c("♠", "♦", "♥", "♣"))
  expect_equal(card_is_face(cards), c(TRUE, FALSE, TRUE, TRUE))
})

test_that("card_is_ace correctly identifies aces", {
  cards <- card(c("A", "3", "A"), c("♠", "♦", "♥"))
  expect_equal(card_is_ace(cards), c(TRUE, FALSE, TRUE))
})

test_that("card_suit extracts suits correctly", {
  cards <- card(c("A", "10", "Q"), c("♠", "♦", "♥"))
  expect_equal(card_suit(cards), c("♠", "♦", "♥"))
})

test_that("card_rank.card extracts ranks from card object", {
  cards <- card(c("A", "2", "K"), c("♠", "♣", "♦"))
  expect_equal(card_rank(cards), c("A", "2", "K"))
})

test_that("card_rank.character extracts ranks from string", {
  card_strs <- c("10♠", "Q♦", "2♥", "A♣")
  expect_equal(card_rank(card_strs), c("10", "Q", "2", "A"))
})

test_that("card_rank.default returns NULL with warning", {
  expect_warning(res <- card_rank(42))
  expect_null(res)
})

test_that("card_rank.blackjack_hand extracts ranks from hand", {
  cards <- card(c("10", "J", "A"), c("♠", "♣", "♦"))
  hand <- new_blackjack_hand(cards)
  expect_equal(card_rank(hand), c("10", "J", "A"))
})
