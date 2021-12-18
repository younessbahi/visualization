 tryCatch(
  expr  = {
    source('dataCleaning.R')
    source('plots.R')
  },
  error = function(e) {
    print(
      cat(
        paste0(
        crayon::bgRed('Some packages are not installed!\n'),
        crayon::red('proceed with installing the missing packages in: ../global/dependencies.R\n')
      )
      )
    )
  }
)