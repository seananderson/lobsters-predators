# Created by:    Sean C. Anderson
# Created:       Feb 20, 2013
# Last modified: Apr 01, 2013
# Purpose:       conduct the meta-analysis

source("Autocorr.Pyper.r")

d.ri <- ddply(d, c("region", "variable", "lag"), function(x){
              ri= with(x, cor(prey_val, com_val))
              vp = var(x$prey_val)
              vc = var(x$com_val)
              #ri = with(x, ri.raw/sqrt(vp*(vp+0.01)*vc*(vc+0.01)))
              ni.raw = length(x$prey_val)
              ni = Autocorr.Pyper(nrow(x), x$prey_val, x$com_val)
              data.frame(var_prey = vp, var_comparison = vc, ri = ri, ni = ni, ni.raw = ni.raw)
})

d.ri.env.pred <- ddply(d.env.pred, c("region"), function(x){
              ri = with(x, cor(pred_val, env_val))
              ni.raw = length(x$prey_val)
              ni = Autocorr.Pyper(nrow(x), x$pred_val, x$env_val)
              data.frame(ri = ri, ni = ni, ni.raw = ni.raw)
})

d.rma <- dlply(d.ri, c("variable", "lag"), function(x) {
  m <- rma.uni(ri = ri, ni = ni, data = x, measure = "ZCOR", method = "EB", 
    slab = x$region)
  list(model = m, variable = unique(x$variable))
}) 

d.rma.env.pred <- rma.uni(ri = ri, ni = ni, data = d.ri.env.pred, 
  measure = "ZCOR", method = "REML", slab = d.ri.env.pred$region)


get_pred_env_rma_out <- function(x) {
  blups <- blup.rma.uni(x, transf = transf.ztor)
  out.df <- data.frame(pred = blups$pred, pi.lb = blups$pi.lb, 
    pi.ub = blups$pi.ub)
  avg <- predict(x, transf = transf.ztor)
  out.df$ma.pred <- avg$pred
  out.df$ma.ci.lb <- avg$ci.lb
  out.df$ma.ci.ub <- avg$ci.ub
  out.df$region <- x$slab
  out.df
}
out.pred.env <- get_pred_env_rma_out(d.rma.env.pred)

library(psychometric) # for CIr
d.ri.cis <- adply(d.ri, 1, function(x) {
      cis <- CIr(x$ri, n= x$ni) # does with z transformation
      data.frame(pi.lb = cis[1], pi.ub = cis[2])
})


out <- ldply(d.rma, function(x){
      blups <- blup.rma.uni(x$model, transf = transf.ztor)
      out.df <- data.frame(pred = blups$pred, pi.lb = blups$pi.lb, 
        pi.ub = blups$pi.ub)
      avg <- predict(x$model, transf = transf.ztor)
      out.df$ma.pred <- avg$pred
      out.df$ma.ci.lb <- avg$ci.lb
      out.df$ma.ci.ub <- avg$ci.ub
      out.df$region <- x$model$slab
      out.df$variable <- x$variable
      out.df
})

# meta analytic averages:
out.ma <- ddply(out, c("lag", "variable"), summarize, ma.pred = ma.pred[1], 
  ma.ci.lb = ma.ci.lb[1], ma.ci.ub = ma.ci.ub[1])

# now let's see about a polynomial relationship for mean.temp as a
# moderator:

d.ri <- join(d.ri, region.temp)
d.ri <- transform(d.ri, mean.temp.sq = mean.temp^2)
# x <- subset(d.ri, variable == "predator" & lag == 3)
# mods <- as.matrix(x[,c("mean.temp", "mean.temp.sq")])
# m <- rma.uni(ri = ri, ni = ni, data =x , measure = "ZCOR", method = "ML", slab = x$region, mods = mods)
# m2 <- rma.uni(ri = ri, ni = ni, data =x , measure = "ZCOR", method = "ML", slab = x$region)
 # AIC(m)
 # AIC(m2)

region_group_df <- data.frame(region = c("Connecticut", "Georges Bank", 
  "Gulf of Maine", "Massachusetts", "Newfoundland", "Nova Scotia", 
  "Rhode Island", "s Gulf St Lawrence", "s New England"), 
  region.group = c(1, 2, 3, 1, 4, 5, 1, 6, 1), stringsAsFactors = FALSE)

d.ri$region.group <- NULL
d.ri <- plyr::join(d.ri, region_group_df)

if(!exists(rma.type)) rma.type <- "RE"
if(!rma.type %in% c("RE", "FE", "MV")) {
  warning("rma.type must be one of RE, FE, or MV. Defaulting to RE.")
  rma.type <- "RE"
}

d.rma.mods <- dlply(d.ri, c("variable", "lag"), function(x) {
  mods <- as.matrix(x[,c("mean.temp", "mean.temp.sq")])
  m.re <- rma.uni(ri = ri, ni = ni, data = x, measure = "ZCOR", 
    method = "REML", slab = x$region, mods = mods)  
  temp <- escalc(ri = ri, ni = ni, data = x, measure = "ZCOR")
  
  m.fe <- rma.uni(ri = ri, ni = ni, data = x, measure = "ZCOR", 
    method = "FE", slab = x$region, mods = mods) 
  
#   m.mv0 <- rma.mv(yi = yi, V = vi, data = temp, method = "REML", 
#     slab = x$region, mods = mods)
#   
#   m.mv1 <- rma.mv(yi = yi, V = vi, data = temp, method = "REML", 
#     slab = x$region, mods = mods, random = ~ 1 | region)
  
  m.mv <- rma.mv(yi = yi, V = vi, data = temp, method = "REML", 
    slab = x$region, mods = mods, random = ~ region | region.group,
    struct = "CS") # Compound symmetry variance structure with SNE grouped
  
  if(rma.type == "RE") return(list(model = m.re, variable = unique(x$variable)))
  if(rma.type == "FE") return(list(model = m.fe, variable = unique(x$variable)))
  if(rma.type == "MV") return(list(model = m.mv, variable = unique(x$variable)))
}) 

out.mods <- ldply(d.rma.mods, function(x){
  if(rma.type != "MV") {
    blups <- blup.rma.uni(x$model, transf = transf.ztor)
    out.df <- data.frame(pred = blups$pred, pi.lb = blups$pi.lb, 
      pi.ub = blups$pi.ub)
  } else {
    # fake data... this doesn't get used anywhere that's important now:
    out.df <- data.frame(pred = rep(0, 9), pi.l = rep(-0.1, 9), 
      pi.ub = rep(0.1, 9))    
  }
  avg <- predict(x$model, transf = transf.ztor)
  out.df$ma.pred <- avg$pred
  out.df$ma.pred.ci.lb <- avg$ci.lb
  out.df$ma.pred.ci.ub <- avg$ci.ub
  out.df$region <- x$model$slab
  out.df$variable <- x$variable
  out.df$b.int <- x$model$b[1]
  out.df$b.ci.l.int <- x$model$ci.lb[1]
  out.df$b.ci.u.int <- x$model$ci.ub[1]
  out.df$b.mean.temp <- x$model$b[2]
  out.df$b.ci.l.mean.temp <- x$model$ci.lb[2]
  out.df$b.ci.u.mean.temp <- x$model$ci.ub[2]
  out.df$b.mean.temp.sq <- x$model$b[3]
  out.df$b.ci.l.mean.temp.sq <- x$model$ci.lb[3]
  out.df$b.ci.u.mean.temp.sq <- x$model$ci.ub[3]
  out.df
})

# by lag and variable:                    
out.mods.ma <- out.mods[,-c(2, 3, 4, 5, 6, 7,8)] # just stuff that doesn't vary within a given model
out.mods.ma <- out.mods.ma[!duplicated(out.mods.ma),]

temps <- seq(4, 16, length.out = 50)
#curve_lag3 <- data.frame(temps= temps, ri_pred = z2r(temps * x$b.mean.temp + temps^2 * x$b.mean.temp.sq + x$b.int))
curves <- ddply(out.mods.ma, c("lag", "variable"), function(x) {
  data.frame(temps= temps, ri_pred = z2r(temps * x$b.mean.temp + 
      temps^2 * x$b.mean.temp.sq + x$b.int)) 
})

out.mods <- join(out.mods, region.temp)
d.ri.cis <- join(d.ri.cis, region.temp)
out.mods <- transform(out.mods, region_temperature_ordered = reorder(region, mean.temp))
d.ri.cis <- transform(d.ri.cis, region_temperature_ordered = reorder(region, mean.temp))

