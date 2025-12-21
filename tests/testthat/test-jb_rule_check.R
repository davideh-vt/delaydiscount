test_that("rule check results", {
  subj = c(rep(1, 5), rep(2, 5), rep(1, 5))
  group = c(rep("Baseline", 10), rep("EFT", 5))
  delay = 1:5
  indiff = c(0.75, 0.54, 0.75, 0.6, 0.64,
             0.75, 0.54, 0.75, 0.96, 0.64,
             0.99, 0.99, 0.99, 0.99, 0.90)

  # There can be issues with floating point precision here
  # 0.75-0.55 >= 0.2 evaluates to FALSE

  dd_data = data.frame(subj, group, delay, indiff)
  rc_res = jb_rule_check(dd_data)

  expect_equal(rc_res$C1[1], TRUE)
  expect_equal(rc_res$C2[1], TRUE)
  expect_equal(rc_res$C1[2], FALSE)
  expect_equal(rc_res$C2[2], TRUE)
  expect_equal(rc_res$C1[3], TRUE)
  expect_equal(rc_res$C2[3], FALSE)

  expect_equal(dim(rc_res), c(3,4))


  # Test that changing order of observations does not affect output
  dd_data <- rbind(dd_data[15:1,])
  rc_res = jb_rule_check(dd_data)

  expect_equal(rc_res$C1[1], TRUE)
  expect_equal(rc_res$C2[1], TRUE)
  expect_equal(rc_res$C1[2], FALSE)
  expect_equal(rc_res$C2[2], TRUE)
  expect_equal(rc_res$C1[3], TRUE)
  expect_equal(rc_res$C2[3], FALSE)

  expect_equal(dim(rc_res), c(3,4))

})
