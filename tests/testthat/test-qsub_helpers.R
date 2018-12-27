context("qsub_helpers")

test_that("mem", {
  expect_equal(mem(2L), "s_vmem=2G,mem_req=2G")
  expect_equal(mem(2.3), "s_vmem=2.3G,mem_req=2.3G")
  expect_error(mem("a"))
})

test_that("resource", {
  expect_equal(resource("This", c("is", "a"), "test"), "#$ This is a test")
})

test_that("arrayjob_option", {
  expect_equal(arrayjob_option(2L), "#$ -t 1-2 -tc 100")
  expect_equal(arrayjob_option(4, tc = 25, stepsize = 2), "#$ -t 1-4:2 -tc 25")
  expect_error(arrayjob_option(14.3))
})

#FIXME
# test_that("files_array", {
#   expect_equal(files_array(tibble(x = c(1, 2), y = c("a", "b"))),
#                'x=([1]=\"1\" [2]=\"2\")\ny=([1]=\"a\" [2]=\"b\")')
# })

test_that("parallel_option", {
  expect_equal(parallel_option(),
               "#$ -pe def_slot 1\n#$ -l s_vmem=5.3G,mem_req=5.3G\n\n#$ -q '!mjobs_rerun.q'\n\n\n\n\n")
  expect_equal(parallel_option(slot = 2L, no_rerun = FALSE),
               "#$ -pe def_slot 2\n#$ -l s_vmem=5.3G,mem_req=5.3G\n\n\n\n#$ -masterl s_vmem=5.3G,mem_req=5.3G\n\n\n")
  expect_error(parallel_option(slot = 25L))
  expect_error(parallel_option(slot = 12L, memory = 200))
  expect_error(parallel_option(slot = 12L, memory = 10, master_memory = 1990))
  expect_error(parallel_option(env = "mpi_4", slot = 16, memory = 600))
  expect_error(parallel_option(env = "mpi_4", slot = 15, memory = 100))
  expect_equal(parallel_option(env = "mpi_4", slot = 16, memory = 100),
               "#$ -pe mpi_4 16\n#$ -l s_vmem=100G,mem_req=100G\n\n#$ -q '!mjobs_rerun.q'\n#$ -l lmem\n#$ -masterl s_vmem=100G,mem_req=100G\n\n#$ -masterq '!mjobs_rerun.q'\n#$ -masterl lmem")
  expect_equal(parallel_option(memory = 10L, special_que = "cp"),
               "#$ -pe def_slot 1\n#$ -l cp\n#$ -l s_vmem=10G,mem_req=10G\n")
  expect_warning(parallel_option(env = "mpi_4", slot = 16, ljob = TRUE, memory = 100))
  expect_warning(parallel_option(ljob = TRUE, special_que = "cp"))
  expect_equal(parallel_option(special_que = "docker", docker_images = "hoo:1"),
               "#$ -pe def_slot 1\n#$ -l docker,docker_images=\"hoo:1\"\n#$ -l s_vmem=5.3G,mem_req=5.3G\n")
})
