

# Functions

get_period_boxplot_by_var <- function(data, var, var_unit, plot_args, harScen) {
  vals <- data[eval(parse(text = plot_args))]
  
  p <- ggplot(vals, aes(x=resolution, y=value, fill=data_from)) +
    geom_boxplot() + 
    labs(title = var, subtitle = harScen, y = var_unit, x = "Resolution m²") +
    theme(axis.text.x = element_text(size = 8)) +
    facet_wrap(~variable)
  
  return(p)
}


get_png_filename <- function(var_names, sep="-"){
  return(str_flatten(var_names, collapse = sep))
}


ggsave_plot <- function(filename, plot, args) {
  do.call(ggsave, c(list(filename = filename, plot = plot), args))
  print(paste0(filename, " saved in ", args$path))
}



get_linePlot_by_var <- function(data, var, var_unit, plot_args, harScen) {
  vals <- data[eval(parse(text = plot_args))]
  
  p <- ggplot(vals, aes(x=variable, y=V1, color=data_from, group = data_from)) +
    geom_line() +
    labs(title = var, subtitle = harScen, y = var_unit, x = "Period")
  
  return(p)
}

get_plot_list <- function(fun, var_names, var_units, args) {
  plots <- lapply(seq_along(var_names), 
                  function(x) do.call(fun, c(list(var = var_names[x], var_unit = var_units[x]), args)))
  return(plots)
}


get_aggr_dt <- function(dt, aggr_fun, by = c("data_from", "resolution", "var_name", "variable")) {
  aggr_dt <-  dt[, eval(parse(text = aggr_fun)), by=by]
  return(aggr_dt)
}



get_aggr_plots_list <- function(dt, aggr_fun, by=c("data_from", "resolution", "var_name", "variable"), 
                                harScen, plot_args, fun, var_names, var_units) {
  aggr_dt <- get_aggr_dt(dt, aggr_fun)
  args <- list(data=aggr_dt, harScen=harScen, plot_args=plot_args)
  plots <- get_plot_list(fun=fun, var_names=var_names, var_units=var_units, args=args)
  return(plots)
}