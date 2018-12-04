
#' @importFrom digest digest
#' @importFrom yaml write_yaml
#' @importFrom rstan stan_rdump
cmdstan <- function(file,
                    model_name = 'anon_model',
                    model_code = '',
                    data = list(),
                    data_file = NULL,
                    chains = 4,
                    iter = 2000,
                    warmup = floor(iter/2),
                    thin = 1,
                    init = 'random',
                    seed = sample.int(.Machine$integer.max, 1),
                    control = NULL,
                    cores = getOption("mc.cores", 1L),
                    working_dir = '.',
                    save_warmup = TRUE,
                    refresh = 1,
                    stannis_yml = file.path(working_dir, glue::glue('{model_name}.yml'))) {
  if (!dir.exists(working_dir))
    dir.create(working_dir)

  # save data to file, if provided as a list
  if (!is.null(data_file)) {
    # assume user provided full path to data file, rather than list
    if (length(data) > 0)
      stop('Both data_file & data are provided. Please pick one.', call. = T)
  } else {
    # generate unique filename for data provided, based on hash of the data
    data_hash <- digest::digest(data, algo = 'md5', skip = 'auto')
    data_file <- file.path(working_dir, paste(data_hash, 'standata', 'Rds', sep = '.'))
    rstan::stan_rdump(names(data), file = data_file, envir = list2env(data))
  }

  # save model code to file, if provided as stancode
  if (missing(file)) {
    model_hash <- digest::digest(model_code, algo = 'md5', skip = 'auto')
    file <- file.path(working_dir, paste(model_name, model_hash, 'stan', sep = '.'))
    if (!file.exists(file)) {
      model_file <- file(file, open = 'w')
      writeLines(model_code, con = model_file)
      close(model_file)
    }
  }

  # update project-based yml file with this run's details
  this_run <- create_run(model_file = file, data_file = data_file, warmup = warmup, iter = iter, chains = chains,
                         control = control, working_dir = working_dir)
    # write out yaml file in stannis format
  if (file.exists(stannis_yml)) {
    yml <- yaml::read_yaml(stannis_yml)
  } else {
    yml <- yaml::read_yaml(system.file("inst", "yaml-defaults.yml", package="rcmdstan"))
    yml$runs <- list()
  }
  yml$runs <- list(this_run) # replace yml file with only this run
  yaml::write_yaml(yml, stannis_yml)

  run_details <- stannis::run_cmdstan(stannis_yml)
  # stannis::read_run(run_details[[1]]$fit_path)
  output_files <- purrr::map_chr(run_details,
                                 ~ dir(path = .$fit_path, pattern = 'output.csv', full.names = T))
  fit <- rstan::read_stan_csv(output_files)
}

#' @importFrom tools file_path_sans_ext
create_run <- function(model_file, data_file, warmup, iter, chains, control = list(), working_dir) {
  data_dir <- dirname(data_file)
  model_dir <- dirname(model_file)
  model_name <- tools::file_path_sans_ext(basename(model_file))
  binary_dir <- file.path(model_dir, 'model-binaries')
  init_dir <- file.path(model_dir, 'model-inits')
  if (!dir.exists(binary_dir))
    dir.create(binary_dir)
  if (!dir.exists(init_dir))
    dir.create(init_dir)
  run <- list(model_name = model_name,
              sample = list(num_warmup = as.integer(warmup),
                            num_samples = as.integer(iter - warmup),
                            num_chains = as.integer(chains)),
              replicates = as.integer(chains),
              data = list(file = basename(data_file)),
              target_dir = working_dir,
              data_dir = data_dir,
              model_dir = model_dir,
              init_dir = init_dir,
              binary_dir = binary_dir)
  if (length(control) > 0 && 'max_treedepth' %in% names(control)) {
    run$sample$hmc <- list(nuts = list(max_depth = control$max_treedepth))
  }
  run
}
