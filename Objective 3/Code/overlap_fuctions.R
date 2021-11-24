#####
## These are the overlap functions from supplemental material of Carroll et al. (2019) 'A review of methods for quantifying predator-prey overlap,' Global Ecology and Biogeography. please see accompanying paper for detailed descriptions of metrics and their ecological interpretations. Prep functions were also helped along by code from Owen Liu (https://github.com/owenrliu/VAST_cod_crab/blob/93a7c77db93d0b185c7967dcaccaf616476bf873/VAST%20models/VAST_cod_crab.Rmd). 
#####

vast_prep_overlap_data<- function(vast_fit, response){
  
  if(FALSE){
    vast_fit = mod_temp
    response = "R1_gct"
  }
  
  # First, going to need the area associated with each of the knots
  knot_areas<- vast_fit$spatial_list$a_gl[,'All'] %>%
    as_tibble() %>%
    set_names("area") %>%
    mutate(.,  "knot" = row_number())
  
  # Next, estimated presence/absence or biomass, depending on response
  resp_array<- vast_fit$Report[[{{response}}]]
  
  # Organize into a long format
  resp_df<- resp_array[, 1,] %>% 
    as_tibble() %>% 
    set_names(as.character(vast_fit$year_labels)) %>% 
    mutate(knot = row_number()) %>%
    pivot_longer(-knot, names_to = "year", values_to = response) %>% 
    mutate(year = as.numeric(year)) %>% 
    left_join(knot_areas)
  
  # Finishing up and calculating the presence/absence or density based on area
  if(response == "R1_gct"){
    # Presence/absence based on lower quartile across the entire domain
    quart_cut<- quantile(resp_df$R1_gct, probs = c(0.25))
    resp_df<- resp_df %>%
      mutate(Presence_Absence = ifelse(R1_gct < quart_cut, 0, 1))
  } else {
    resp_df<- resp_df %>%
      mutate(., Density = D_gct)
  }
  
  # Return it
  return(resp_df)
}

## area overlap
## for binary data
## measures proportion of an area where two species co-occur
area_overlapfn <- function(prey, pred, area){
  total_area <- sum(area, na.rm = T)
  sum(area[pred > 0 & prey > 0], na.rm = T)/total_area
}

## range overlap
## for binary data
## measures the proportion of one species range where the other co-occurs
range_overlapfn<-function(prey, pred, area){
  area_prey <- sum(area[prey > 0], na.rm = T)
  sum(area[pred > 0 & prey > 0], na.rm = T)/area_prey
}

## local index of collocation
## estimates correlation of predator and prey densities
loc_collocfn <- function(prey, pred) {
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  sum(p_prey*p_pred, na.rm = T)/(sqrt(sum(p_prey^2, na.rm = T)*sum(p_pred^2, na.rm = T)))
}

## asymmetrical alpha
## measures pressure of predator on prey relative to underlying prey density
asymmalpha_overlapfn <-function(prey, pred){
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  sum(p_pred*p_prey, na.rm = T)/sum(p_prey^2, na.rm = T)
}

## biomass-weighted overlap (scaled to max)
## measures amount of predator biomass interacting with prey relative to underlying prey biomass
biomass_overlapfn <- function(prey, pred) {
  sum((prey/max(prey, na.rm = T)) * (pred/max(pred, na.rm = T)), na.rm = T)/sum(prey/max(prey, na.rm = T), na.rm = T)
}

## Hurlbert's overlap
## measures interspecific encounter rate between predator and prey
hurlbert_overlapfn <- function(prey, pred, area) {
  if(FALSE)
  area_occupied <- sum(area[pred > 0 | prey > 0], na.rm = T)
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  sum((p_pred*p_prey)/(area/area_occupied), na.rm = T)
}

## Schoener's D
## density or probability of occurrence data
## measures how equally predator and prey share available resources
schoeners_overlapfn <- function(prey, pred) {
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  1 - 0.5 * (sum(abs(p_prey-p_pred), na.rm = T))
}

## Bhattacharyya's coefficient
## density or probability of occurrence data
## measures whether two species use space independently
bhatta_coeffn <- function(prey, pred) {
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  sum(sqrt(p_prey*p_pred), na.rm = T)
}

## global index of collocation
## measures geographic distinctness by comparing centres of gravity and dispersion of sampled individuals
glob_collocfn <- function(prey_x, prey_y, prey, pred_x, pred_y, pred){
  prey_cgx <- sum(prey_x*prey, na.rm = T)/sum(prey, na.rm = T)
  prey_cgy <- sum(prey_y*prey, na.rm = T)/sum(prey, na.rm = T)
  prey_ix <- prey_x - prey_cgx
  prey_iy <- prey_y - prey_cgy
  prey_i <- sqrt(prey_ix^2 + prey_iy^2)
  prey_inert <- sum(prey * (prey_i^2), na.rm = T)/sum(prey, na.rm = T)
  pred_cgx <- sum(pred_x*pred, na.rm = T)/sum(pred, na.rm = T)
  pred_cgy <- sum(pred_y*pred, na.rm = T)/sum(pred, na.rm = T)
  pred_ix <- pred_x - pred_cgx
  pred_iy <- pred_y - pred_cgy
  pred_i <- sqrt(pred_ix^2 + pred_iy^2)
  pred_inert <- sum(pred * (pred_i^2), na.rm = T)/sum(pred, na.rm = T)
  GIC <- (((prey_cgx - pred_cgx)^2+(prey_cgy - pred_cgy)^2)/ (((prey_cgx-pred_cgx)^2+(prey_cgy-pred_cgy)^2)+prey_inert + pred_inert))
  if(!is.na(GIC))
    GIC <- 1-GIC
  else GIC <- 1
  GIC
}

## AB ratio
## measures predator production that can be attributed to spatial overlap with prey
AB_overlapfn <- function(prey, pred) { 
  mean((pred - mean(pred, na.rm = T)) * (prey - mean(prey, na.rm = T)), na.rm = T)/(mean(pred, na.rm = T) * mean(prey, na.rm = T)) 
}




