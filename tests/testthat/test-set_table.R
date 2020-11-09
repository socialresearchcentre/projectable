testthat::test_that("`.rows` uses expressions only", {
  chk <- set_table(
    .data = mtcars,
    four_cyl = cyl %in% 4,
    six_cyl = cyl %in% 6,
    eight_cyl = cyl %in% 8,
    .cols = list(
      vshaped_all = col_freq(vs %in% 1, nrow(mtcars)),
      vshaped_rbr = col_freq(vs %in% 1, vs %in% 0:1),
      vshaped_col = col_freq(vs %in% 1, mtcars$vs %in% 1),

      nvshaped_all = col_freq(vs %in% 0, nrow(mtcars)),
      nvshaped_rbr = col_freq(vs %in% 0, vs %in% 0:1),
      nvshaped_col = col_freq(vs %in% 0, mtcars$vs %in% 0),

      little_n = sum(vs %in% 1),
      big_n = length(vs),
      bigger_n = nrow(mtcars)
    )
  )

  # Check calculations against one another
  testthat::expect_true(all(face_value(chk$vshaped_all) == chk$little_n / chk$bigger_n))
  testthat::expect_true(all(face_value(chk$vshaped_rbr) == chk$little_n / chk$big_n))

  # Check calculations against alternative
  testthat::expect_equal(
    chk$little_n,
    vapply(split(mtcars$vs, mtcars$cyl), sum, double(1), USE.NAMES = FALSE)
  )
  testthat::expect_equal(
    chk$big_n,
    vapply(split(mtcars$vs, mtcars$cyl), length, double(1), USE.NAMES = FALSE)
  )

  # Summing overall MECE proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk$vshaped_all, "p")) + sum(vctrs::field(chk$nvshaped_all, "p")),
    1
  )
  # Summing column proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk$vshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk$nvshaped_col, "p")),
    1
  )
  # Summing row proportions should give 1
  testthat::expect_equal(
    vctrs::field(chk$vshaped_rbr, "p") + vctrs::field(chk$nvshaped_rbr, "p"),
    c(1, 1, 1)
  )

})

testthat::test_that("`.rows` uses one symbol only", {
  chk <- set_table(
    .data = mtcars,
    cyl,
    .cols = list(
      vshaped_all = col_freq(vs %in% 1, nrow(mtcars)),
      vshaped_rbr = col_freq(vs %in% 1, vs %in% 0:1),
      vshaped_col = col_freq(vs %in% 1, mtcars$vs %in% 1),

      nvshaped_all = col_freq(vs %in% 0, nrow(mtcars)),
      nvshaped_rbr = col_freq(vs %in% 0, vs %in% 0:1),
      nvshaped_col = col_freq(vs %in% 0, mtcars$vs %in% 0),

      little_n = sum(vs %in% 1),
      big_n = length(vs),
      bigger_n = nrow(mtcars)
    )
  )

  # Check calculations against one another
  testthat::expect_true(all(face_value(chk$vshaped_all) == chk$little_n / chk$bigger_n))
  testthat::expect_true(all(face_value(chk$vshaped_rbr) == chk$little_n / chk$big_n))

  # Check calculations against alternative
  testthat::expect_equal(
    chk$little_n,
    vapply(split(mtcars$vs, mtcars$cyl), sum, double(1), USE.NAMES = FALSE)
  )
  testthat::expect_equal(
    chk$big_n,
    vapply(split(mtcars$vs, mtcars$cyl), length, double(1), USE.NAMES = FALSE)
  )

  # Summing overall MECE proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk$vshaped_all, "p")) + sum(vctrs::field(chk$nvshaped_all, "p")),
    1
  )
  # Summing column proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk$vshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk$nvshaped_col, "p")),
    1
  )
  # Summing row proportions should give 1
  testthat::expect_equal(
    vctrs::field(chk$vshaped_rbr, "p") + vctrs::field(chk$nvshaped_rbr, "p"),
    c(1, 1, 1)
  )

})

testthat::test_that("`.rows` uses symbols only", {
  chk <- set_table(
    .data = mtcars,
    cylinders = cyl, transmission = am,
    .cols = list(
      vshaped_all = col_freq(vs %in% 1, nrow(mtcars)),
      vshaped_rbr = col_freq(vs %in% 1, vs %in% 0:1),
      vshaped_col = col_freq(vs %in% 1, mtcars$vs %in% 1),

      nvshaped_all = col_freq(vs %in% 0, nrow(mtcars)),
      nvshaped_rbr = col_freq(vs %in% 0, vs %in% 0:1),
      nvshaped_col = col_freq(vs %in% 0, mtcars$vs %in% 0),

      little_n = sum(vs %in% 1),
      big_n = length(vs),
      bigger_n = nrow(mtcars)
    )
  )

  # Check calculations against one another
  testthat::expect_true(all(face_value(chk$vshaped_all) == chk$little_n / chk$bigger_n))
  testthat::expect_true(all(face_value(chk$vshaped_rbr) == chk$little_n / chk$big_n))

  # Check calculations against alternative
  testthat::expect_equal(
    chk$little_n,
    c(
      vapply(split(mtcars$vs, mtcars$cyl), sum, double(1), USE.NAMES = FALSE),
      vapply(split(mtcars$vs, mtcars$am), sum, double(1), USE.NAMES = FALSE)
    )
  )
  testthat::expect_equal(
    chk$big_n,
    c(
      vapply(split(mtcars$vs, mtcars$cyl), length, double(1), USE.NAMES = FALSE),
      vapply(split(mtcars$vs, mtcars$am), length, double(1), USE.NAMES = FALSE)
    )
  )

  chk_split <- split(chk, chk$row_spanner)
  # Summing overall MECE proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[1]]$vshaped_all, "p")) + sum(vctrs::field(chk_split[[1]]$nvshaped_all, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[2]]$vshaped_all, "p")) + sum(vctrs::field(chk_split[[2]]$nvshaped_all, "p")),
    1
  )

  # Summing column proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[1]]$vshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[1]]$nvshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[2]]$vshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[2]]$nvshaped_col, "p")),
    1
  )
  # Summing row proportions should give 1
  testthat::expect_equal(
    vctrs::field(chk$vshaped_rbr, "p") + vctrs::field(chk$nvshaped_rbr, "p"),
    rep(1, nrow(chk))
  )

})

testthat::test_that("`.rows` uses symbols and expressions", {
  chk <- set_table(
    .data = mtcars,
    Cylinders = list(
      four_cyl = cyl %in% 4,
      six_cyl = cyl %in% 6,
      eight_cyl = cyl %in% 8
    ),
    Transmission = am,
    .cols = list(
      vshaped_all = col_freq(vs %in% 1, nrow(mtcars)),
      vshaped_rbr = col_freq(vs %in% 1, vs %in% 0:1),
      vshaped_col = col_freq(vs %in% 1, mtcars$vs %in% 1),

      nvshaped_all = col_freq(vs %in% 0, nrow(mtcars)),
      nvshaped_rbr = col_freq(vs %in% 0, vs %in% 0:1),
      nvshaped_col = col_freq(vs %in% 0, mtcars$vs %in% 0),

      little_n = sum(vs %in% 1),
      big_n = length(vs),
      bigger_n = nrow(mtcars)
    )
  )

  # Check calculations against one another
  testthat::expect_true(all(face_value(chk$vshaped_all) == chk$little_n / chk$bigger_n))
  testthat::expect_true(all(face_value(chk$vshaped_rbr) == chk$little_n / chk$big_n))

  # Check calculations against alternative
  testthat::expect_equal(
    chk$little_n,
    c(
      vapply(split(mtcars$vs, mtcars$cyl), sum, double(1), USE.NAMES = FALSE),
      vapply(split(mtcars$vs, mtcars$am), sum, double(1), USE.NAMES = FALSE)
    )
  )
  testthat::expect_equal(
    chk$big_n,
    c(
      vapply(split(mtcars$vs, mtcars$cyl), length, double(1), USE.NAMES = FALSE),
      vapply(split(mtcars$vs, mtcars$am), length, double(1), USE.NAMES = FALSE)
    )
  )

  chk$row_spanner[chk$row_spanner %in% NA] <- "cylinders"
  chk_split <- split(chk, chk$row_spanner)
  # Summing overall MECE proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[1]]$vshaped_all, "p")) + sum(vctrs::field(chk_split[[1]]$nvshaped_all, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[2]]$vshaped_all, "p")) + sum(vctrs::field(chk_split[[2]]$nvshaped_all, "p")),
    1
  )

  # Summing column proportions should give 1
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[1]]$vshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[1]]$nvshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[2]]$vshaped_col, "p")),
    1
  )
  testthat::expect_equal(
    sum(vctrs::field(chk_split[[2]]$nvshaped_col, "p")),
    1
  )
  # Summing row proportions should give 1
  testthat::expect_equal(
    vctrs::field(chk$vshaped_rbr, "p") + vctrs::field(chk$nvshaped_rbr, "p"),
    rep(1, nrow(chk))
  )

})

testthat::test_that("NSE gotchas", {
  .rows <- 4 # .rows is defined inside set_table

  testthat::expect_identical(
    set_table(mtcars, Four = cyl %in% .rows, .cols = list(vshaped = col_freq(vs %in% 1, vs %in% 0:1))),
    set_table(mtcars, Four = cyl %in% 4, .cols = list(vshaped = col_freq(vs %in% 1, vs %in% 0:1)))
  )

  .rows <- 1 # .rows is defined inside set_table

  testthat::expect_identical(
    set_table(mtcars, cyl, .cols = list(vshaped = col_freq(vs %in% .rows, vs %in% 0:1))),
    set_table(mtcars, cyl, .cols = list(vshaped = col_freq(vs %in% 1, vs %in% 0:1)))
  )
})

testthat::test_that("Bad calls", {
  testthat::expect_error(
    set_table(mtcars, not_a_col, .cols = list(vshaped = col_freq(vs %in% 1, vs %in% 0:1)))
  )

  testthat::expect_error(
    set_table(mtcars, cyl, .cols = list(vshaped = col_freq(not_a_col %in% 1, vs %in% 0:1)))
  )
})
