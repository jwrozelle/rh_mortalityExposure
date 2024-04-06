

model_me <- function(model, variables) {
  
  require(lme4)
  require(marginaleffects)
  
  # get contrast summary
  contrasts.summary <- marginaleffects::marginaleffects(
    model = model,
    variables = variables
  ) |> summary()
  
  # Marginal Means
  mem.summary <- marginalmeans(fm_fbd_prevu5_bord2.model, variables = variables, type = "response") |> summary()
  
  return(list(
    contrasts = contrasts.summary,
    mmeans = mem.summary
  ))
}

