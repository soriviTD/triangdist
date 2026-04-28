library(testthat)

# Parámetros base para las pruebas
min_val <- 0
max_val <- 10
mode_val <- 2

test_that("Validación de argumentos (Errores stop)", {
  # Errores de parámetros de la distribución
  expect_error(dtriang(5, min = 10, max = 0, mode = 5))
  expect_error(ptriang(5, min = 0, max = 10, mode = 11))
  expect_error(rtriang(5, min = 0, max = 10, mode = -1))

  # Error específico de qtriang (probabilidad fuera de rango)
  expect_error(qtriang(1.5, min = 0, max = 10, mode = 2))
  expect_error(qtriang(-0.1, min = 0, max = 10, mode = 2))

})

test_that("dtriang - Densidad", {
  # Caso x == mode
  expect_equal(dtriang(2, 0, 10, 2), 2 / (10 - 0))

  # Caso x fuera de límites
  expect_equal(dtriang(-1, 0, 10, 2), 0)
  expect_equal(dtriang(11, 0, 10, 2), 0)

  # Caso x < mode (subtriángulo izquierdo)
  expect_true(dtriang(1, 0, 10, 2) > 0)

  # Caso x > mode (subtriángulo derecho)
  expect_true(dtriang(5, 0, 10, 2) > 0)
})

test_that("ptriang - Distribución acumulada", {
  # Caso q <= min
  expect_equal(ptriang(0, 0, 10, 2), 0)

  # Caso q >= max
  expect_equal(ptriang(10, 0, 10, 2), 1)

  # Caso q == mode
  p_at_mode <- ptriang(2, 0, 10, 2)
  expect_true(p_at_mode > 0 && p_at_mode < 1)

  # Caso q < mode
  expect_true(ptriang(1, 0, 10, 2) < p_at_mode)

  # Caso q > mode
  expect_true(ptriang(5, 0, 10, 2) > p_at_mode)
})

test_that("qtriang - Cuantiles (Inversa)", {
  p_m <- ptriang(mode_val, min_val, max_val, mode_val)

  # Caso p == p_mode
  expect_equal(qtriang(p_m, min_val, max_val, mode_val), mode_val)

  # Caso p < p_mode (usa res_g2 rama x1 o x2)
  val_bajo <- qtriang(0.1, min_val, max_val, mode_val)
  expect_true(val_bajo < mode_val)

  # Caso p > p_mode
  val_alto <- qtriang(0.9, min_val, max_val, mode_val)
  expect_true(val_alto > mode_val)
})

test_that("rtriang - Generación aleatoria", {
  n <- 50
  muestras <- rtriang(n, min_val, max_val, mode_val)

  expect_length(muestras, n)
  expect_true(all(muestras >= min_val & muestras <= max_val))
})
