complete_check <- function(input, output, session) {
  source('./checks/site_check.R')
  site_check_result <- site_check(input,output)
  if(!is.null(site_check_result)) {
    return(site_check_result)
  }
  
  source('./checks/dm_check.R')
  dm_check_result <- dm_check(input,output)
  if(!is.null(dm_check_result)){
    return(dm_check_result)
  }
  
  source('./checks/dm_meta_check.R')
  dm_meta_check_result <- dm_meta_check(input,output)
  if(!is.null(dm_meta_check_result)) {
    return(dm_meta_check_result)
  }
  
  source('./checks/tree_meta_check.R')
  tree_meta_check_result <- tree_meta_check(input, output)
  if(!is.null(tree_meta_check_result)) {
    return(tree_meta_check_result)
  }
  
  source('./checks/clim_check.R')
  clim_check_result <- clim_check(input, output)
  if(!is.null(clim_check_result)) {
    return(clim_check_result)
  }
  
  source('./checks/clim_meta_check.R')
  clim_meta_check_result <- clim_meta_check(input, output)
  if(!is.null(clim_meta_check_result)) {
    return(clim_meta_check_result)
  }
  
  source('./checks/contributor_check.R')
  contributor_check_result <- contributor_check(input, output)
  if(!is.null(contributor_check_result)) {
    return(contributor_check_result)
  }
  
  return(NULL)
}
