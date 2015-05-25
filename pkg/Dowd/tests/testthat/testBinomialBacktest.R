test_that("Binomial Backtest Works.",{
  # Success
  expect_equal(0.7358, BinomialBacktest(1, 100, 0.99), tolerance=0.001)
  expect_equal(0.2529, BinomialBacktest(55, 1000, 0.95), tolerance=0.001)
  
  # Warnings
  expect_warning(val <- BinomialBacktest(35,30,0.95))
  expect_true(is.nan(val))
  expect_warning(val <- BinomialBacktest(5,30,1.5))
  expect_true(is.nan(val))
})