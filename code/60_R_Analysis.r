data_dir <- readLines("data_dir.txt")

setwd(data_dir)

package_list = c("IRkernel",'plm','MASS','stargazer',
                 "lfe","dplyr","caret","data.table","nlWaldTest",
                 "msm","car","rlist","stringr","stringi","fixest")

install.packages(package_list)

# Read in data
df <- read.csv("./clean_data/fcs_final.csv")

# Require packages
lapply(package_list, require, character.only = TRUE)

nomvars = c("expend_fert","expend_equip", "equip_value",
             "total_crop_val", "farm_value", "expend_labor",
             "expend_feed", "expend_labor", "total_livestock_val", "animal_revenue")

for(var in nomvars){
df[df$year == 1920,var] <- (df[df$year == 1920,var]/.2)
df[df$year == 1925,var] <- (df[df$year == 1925,var]/.175)
df[df$year == 1930,var] <- (df[df$year == 1930,var]/.167)
df[df$year == 1935,var] <- (df[df$year == 1935,var]/.137)
df[df$year == 1940,var] <- (df[df$year == 1940,var]/.14)
}

## Variable generation
df$avg_fsize            = df$farm_area/df$farm_number
df$county_farm          = df$farm_area/df$county_area
df$avg_fval             = df$farm_value/df$farm_number/1000
df$farmval_pacr         = df$farm_value/df$farm_area
df$fert_pacr            = df$expend_fert/df$farm_area
df$equip_pacr           = df$expend_equip/df$farm_area
df$eqval_pacr           = df$equip_value/df$farm_area
df$labor_pacr           = df$expend_labor/df$farm_area
df$popdens              = df$total_pop/df$county_area
df$fail_pacr            = df$crop_failure/df$farm_area
df$tract_pacr           = df$equip_tractors/df$farm_area
df$tractors_per_farm    = df$equip_tractors/df$farm_number
df$tractors_per_10farm  = 10*df$equip_tractors/df$farm_number
df$tractors_per_100farm = 100*df$equip_tractors/df$farm_number
df$bean_yield           = df$total_bean_bshls/df$total_bean_acres
df$cropval_pacr         = df$total_crop_val/df$farm_area
df$lstockval_pacr       = df$total_livestock_val/df$farm_area
df$feed_pacr            = df$expend_feed/df$farm_area
df$pct_beans            = df$total_bean_acres/df$farm_area
df$animal_rev_pacr      = df$animal_revenue/df$farm_area
df$dairy_val_percow     = df$dairy_value/df$dairy_cows
df$milk_yield           = df$milk_produced/df$dairy_cows
df$butter_yield         = df$butter_produced/df$dairy_cows

# Make squared variables
df$temp_mean_sq = df$temp_mean**2
df$rain_mean_sq = df$rain_mean**2

# More treatment variables
df$pca30 <- as.numeric(df$pca_km<30)
df$pca45 <- as.numeric(df$pca_km<45)
df$pca60 <- as.numeric(df$pca_km<60)
df$pca100 <- as.numeric(df$pca_km<100)

# Time variable
df$after <- as.numeric(df$year>1930)

# Relevel variables
df$year_f <- relevel(as.factor(df$year),ref="1930")
df$pca_km_cat <- relevel(cut(df$pca_km,c(0,30,45,60,100,1000)),ref="(0,30]")
df$after_f <- relevel(as.factor(df$after),ref=1)

# Outcomes
outcomes <- c("corn_yield7","wheat_yield", "cropval_pacr", "tractors_per_farm", "fert_pacr")
covars_timev <- c("temp_mean","temp_mean_sq","rain_mean","rain_mean_sq")
E=c("pct_deps_sus","cornmean","wheatmean","erosion_med","erosion_high")
E_iffy = c("FCA","AAA","total_relief","public_works","total_grants","total_loans")

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

# Log the outcomes
for(i in c(outcomes,"pca_km")) {
  df[[paste0("ln",i)]] = log(df[[i]])
}

# Convert the infinites to NA
is.na(df) <- sapply(df, is.infinite)


# Do IHS transform
for(i in c(outcomes,covars_timev,E,E_iffy,"pca_km")) {
  df[[paste0("ihs",i)]] = ihs(df[[i]])
}

# Panel Variables

ivar = "FIPS"

tvar_2y = "after"
tvar = "year"

dvar = "PCA"
dvar_m = "pca_km_cat"
dvar_c = "lnpca_km"

# Outcomes

df$lnY = log(df$cropval_pacr)
Y_level = c("corn_yield7", "wheat_yield", "cropval_pacr", "tractors_per_farm", "fert_pacr") 
Y_log = c("lncorn_yield7", "lnwheat_yield", "lncropval_pacr", "lntractors_per_farm", "lnfert_pacr") 
Y_ihs = c("ihscorn_yield7", "ihswheat_yield", "ihscropval_pacr", "ihstractors_per_farm", "ihsfert_pacr" )

y = "ihscropval_pacr"


# Covariates 

X = paste(covars_timev,collapse = " + ")

E_ihs = E=c("ihspct_deps_sus","ihscornmean","ihswheatmean","ihserosion_med","ihserosion_high")

E_2y = paste(E_ihs,collapse = "*as.factor(after) + ")
E_all = paste(E_ihs,collapse = "*as.factor(year) + ")

covars_2y <- paste(E_2y,X)
covars <- paste(E_all,X)


# Treatment

df$D <- df$PCA

Treatment_Dist_Cat_2y <- "pca_km_cat*after + "
Treatment_Dist_Cont_2y <- "lnpca_km*after + "
Treatment_Binary_2y <- "pca*after + "

Treatment_Dist_Cat <- "pca_km_cat*year + "
Treatment_Dist_Cont <- "lnpca_km*year + "
Treatment_Binary <- "pca*year + "


# Factors

Factors_2y <- "fips + after"
Factors_2y_all <- "fips + after + as."

Factors_TWFE <- "fips+year"
Factors_All <- "fips+year + as.factor(fips_state):as.factor(year)"


# Standard Errors
Cluster <- "fips"
Z <- "0"


# Make a two period dataframe
df_t2  <- df[df$year==1930|df$year==1940,]

run_model <- function(data=data,
                      Y="ihscropval_pacr",
                      Treat="D*year_f + ",
                      X = covars_timev,
                      E = covars_time_inv,
                      G="fips+year_f+as.factor(fips_state):year_f",
                      C="FIPS",
                      two_period=FALSE){
  
  if (two_period){
    E = paste(E,collapse = "*as.factor(after) + ")
    } else {
    E = paste(E,collapse = "*year_f + ")
    }
  
  X = paste("+",X,collapse = " + ")
  
  covars <- paste(E,X)
  
  Z <- "0"
  f = as.formula(paste(Y," ~ ",Treat,covars,"|",G,"|",Z,"|",C))
  return(felm(data = data,formula= f))
}

nl_pvalue <- function(m,loc) {
    p = nlWaldtest(m,paste0("(exp(b[",loc,"])-1)"))$p.val[[1]]
    return(p)
}

binary_exp <- function (m, varlist) 
{
    L = list()
    for (i in 1:length(varlist)) {
        var = stri_escape_unicode(varlist[[i]])
        
        var_r = gsub("\\(","\\\\\\(",var)
        var_r = gsub("\\]","\\\\\\]",var_r)
        var_r = gsub("\\+","\\\\\\+",var_r)
        
        all_covars = names(coef(m))
        
        loc = grep(var_r,lapply(all_covars,stri_escape_unicode))
        ex = as.data.frame(deltaMethod(m, paste0("exp(`", stri_escape_unicode(var),"`)-1")))
        ex["p"] = nl_pvalue(m, loc)
        
        L[[loc]] = ex
    }
    res = list.rbind(L)
    rownames(res) = varlist
    return(res)
}

ihs_elast_pvalue <- function(m,loc,cons) {
    p = nlWaldtest(m,paste0("b[",loc,"]*x[1]"),x=c(cons))$p.val[[1]]
    return(p)
}

ihs_elasticity <- function (m, varlist,comb) 
{
    L = list()
    
    d = model.frame(m)
    
    outcome = all.vars(m$formula)[[1]]
    
    ybar = mean(d[,outcome],na.rm = T)
    
    for (i in 1:length(varlist)) {
        var = stri_escape_unicode(varlist[[i]])
        
        xbar = mean(model.matrix(as.formula(paste0("~",comb)),d)[,var],na.rm=T)
        
        C = xbar*(sqrt(ybar**2+1))/(ybar*sqrt(xbar**2+1))
        
        var_r = gsub("\\(","\\\\\\(",var)
        var_r = gsub("\\]","\\\\\\]",var_r)
        var_r = gsub("\\+","\\\\\\+",var_r)
        
        all_covars = names(coef(m))
        
        loc = grep(var_r,lapply(all_covars,stri_escape_unicode))
        ex = as.data.frame(deltaMethod(m,paste0("`", stri_escape_unicode(var),"`*adj"),constants = c(adj = C)))
        ex["p"] = ihs_elast_pvalue(m, loc, C)
        
        L[[loc]] = ex
    }
    res = list.rbind(L)
    rownames(res) = varlist
    return(res)
}

ihs_correct_discrete <- function(results,grep_pattern){
    res_ests = list()
    res_se = list()
    res_p = list()
    
    for(i in 1:length(results)){
        coef_list = names(coef(results[[i]]))
    
        var_list = coef_list[grep(grep_pattern,coef_list)]
    
        temp = binary_exp(results[[i]],var_list)
    
        other_vars = setdiff(coef_list,var_list)
    
        for(var in other_vars){
        temp[var,] =NA
        }
        temp <- temp[coef_list,]
    
        res_ests[[i]] = temp[,"Estimate"]
        res_se[[i]] = temp[,"SE"]
        res_p[[i]] = temp[,"p"]
    }
    return(c("ests"=res_ests,"se"=res_se,"p"=res_p))
}

ihs_correct_cont <- function(results,grep_pattern,comb){
    res_ests = list()
    res_se = list()
    res_p = list()
    
    for(i in 1:length(results)){
        coef_list = names(coef(results[[i]]))
    
        var_list = coef_list[grep(grep_pattern,coef_list)]
    
        temp = ihs_elasticity(results[[i]],var_list,comb)
    
        other_vars = setdiff(coef_list,var_list)
    
        for(var in other_vars){
        temp[var,] =NA
        }
        temp <- temp[coef_list,]
    
        res_ests[[i]] = temp[,"Estimate"]
        res_se[[i]] = temp[,"SE"]
        res_p[[i]] = temp[,"p"]
    }
    return(c("ests"=res_ests,"se"=res_se,"p"=res_p))
}

ftest_pretrend = function(model,treat){
        K = length(coef(model))

        VCOV = model$vcv    
        VCOV = VCOV[rowSums(is.na(VCOV)) != K,colSums(is.na(VCOV)) != K ]
    
        locs = c(grep(paste0(treat,".*1920"),names(coef(model))),
                 grep(paste0(treat,".*1925"),names(coef(model))))

        lht = linearHypothesis(model,
                               paste0(names(coef(model))[locs],"=0"),
                               singular.ok = TRUE,
                               vcov=VCOV,
                               white.adjust=TRUE,
                               test="F")
    
    return(lht)
    
}

pre_trend <- function(res,treat="pca_km_cat"){
    chi = list()
    chi_pvals = list()

    for(i in 1:length(res)){
        chi[[i]] = ftest_pretrend(res[[i]],treat)[2,3]
        chi_pvals[[i]] = ftest_pretrend(res[[i]],treat)[2,4]
    }  
    return(list("chi"=chi,"chi_pvals"=chi_pvals))
}


X <- c("temp_mean","temp_mean_sq","rain_mean","rain_mean_sq")
E=c("ihspct_deps_sus","ihscornmean","ihswheatmean","ihserosion_med","ihserosion_high")
X_iffy = c("county_farm","avg_fval")
E_iffy =c("ihstotal_relief","ihstotal_grants","ihspublic_works")

res1 = list()
for(i in 1:5) {
  res1[[i]] <- run_model(data=df_t2,Y=Y_ihs[[i]],Treat = "after + D:after+",X="+0",E="0",G="FIPS")
}

ihs_res = ihs_correct_discrete(res1,"after:D")

stargazer(res1,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep="after:D",
          type="text")

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df_t2,Y=Y_ihs[[i]],Treat = "pca_km_cat*after+",X="+0",E="0",G="FIPS")
}

ihs_res = ihs_correct_discrete(res2,":after")

stargazer(res2,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = ":after",
          type = "text")

res3 = list()

for(i in 1:5) {
  res3[[i]] <- run_model(data=df_t2,Y=Y_ihs[[i]],Treat = "ihspca_km*after+",X="+0",E="0",G="FIPS")
}

ihs_res = ihs_correct_cont(res3,"ihspca_km:after","ihspca_km:after")

stargazer(res3,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":after",
          type="text")

res1 = list()
for(i in 1:5) {
  res1[[i]] <- run_model(data=df,Y=Y_ihs[[i]],Treat = "D*year_f+",X="+0",E="0",G="FIPS +year")
}

ihs_res = ihs_correct_discrete(res1,"D:year")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = "D:year",
          type = "text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS+year")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-Test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res3 = list()

for(i in 1:5) {
  res3[[i]] <- run_model(data=df,Y=Y_ihs[[i]],Treat = "ihspca_km*year_f+",X="+0",E="0",G="FIPS+year")
}

ihs_res = ihs_correct_cont(res3,"ihspca\\_km:year*","ihspca_km:year_f")

Fpre = pre_trend(res3,treat="ihspca_km")

stargazer(res3,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res1 = list()
for(i in 1:5) {
  res1[[i]] <- run_model(data=df,
                         Y=Y_ihs[[i]],
                         Treat = "D*year_f+",
                         X="+0",E="0",
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res1,"D:year")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = "D:year",
          type = "text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))


res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X="+0",E="0",
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res3 = list()
for(i in 1:5) {
  res3[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "ihspca_km*year_f+",
                         X="+0",E="0",
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_cont(res3,"ihspca\\_km:year*","ihspca_km:year_f")

Fpre = pre_trend(res3,treat="ihspca_km")

stargazer(res3,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res1 = list()
for(i in 1:5) {
  res1[[i]] <- run_model(data=df,
                         Y=Y_ihs[[i]],
                         Treat = "D*year_f+",
                         X=X,E="0",
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res1,"D:year")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = "D:year",
          type = "text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=X,E="0",
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res3 = list()
for(i in 1:5) {
  res3[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "ihspca_km*year_f+",
                         X=X,E="0",
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_cont(res3,"ihspca\\_km:year*","ihspca_km:year_f")

Fpre = pre_trend(res3,treat="ihspca_km")

stargazer(res3,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res1 = list()
for(i in 1:5) {
  res1[[i]] <- run_model(data=df,
                         Y=Y_ihs[[i]],
                         Treat = "D*year_f+",
                         X=X,E=c(E,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res1,"D:year")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = "D:year",
          type = "text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=X,E=c(E,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res3 = list()
for(i in 1:5) {
  res3[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "ihspca_km*year_f+",
                         X=X,E=c(E,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_cont(res3,"ihspca\\_km:year*","ihspca_km:year_f")

Fpre = pre_trend(res3,treat="ihspca_km")

stargazer(res3,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res1 = list()
for(i in 1:5) {
  res1[[i]] <- run_model(data=df,
                         Y=Y_ihs[[i]],
                         Treat = "D*year_f+",
                         X=X,E=c(E,E_iffy,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res1,"D:year")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = "D:year",
#           type = "text",
          covariate.labels = c("PCA $\\times$ 1920","PCA $\\times$ 1925","PCA $\\times$ 1935", "PCA $\\times$ 1940"),

          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=c(X),E=c(E,E_iffy,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res3 = list()
for(i in 1:5) {
  res3[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "ihspca_km*year_f+",
                         X=X,E=c(E,E_iffy,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_cont(res3,"ihspca\\_km:year*","ihspca_km:year_f")

Fpre = pre_trend(res3,treat="ihspca_km")

stargazer(res3,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

df[,"ihstemp_mean_sqrd"] = df$ihstemp_mean**2
df[,"ihsrain_mean_sqrd"] = df$ihsrain_mean**2

X <- c("ihstemp_mean","ihstemp_mean_sqrd","ihsrain_mean","ihsrain_mean_sqrd")

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=c(X),E=c(E,E_iffy),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}
stargazer(res2,
#           type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=c(X),E=c(E,E_iffy_limited,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}
stargazer(res2,
#           type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

KFoldCV = function(outcome,train_ixs,data){
        M1 = feols(data=data[-train_ixs,],as.formula(paste0(outcome,"~pca_km_cat*year_f|FIPS")))
        M2 = feols(data=data[-train_ixs,],as.formula(paste0(outcome,"~pca_km_cat*year_f|FIPS+FIPS_state^year")))
        M3 = feols(data=data[-train_ixs,],as.formula(paste0(outcome,"~pca_km_cat*year_f+ihspct_deps_sus*year_f+ ihscornmean*year_f + ihswheatmean*year_f + ihserosion_med*year_f + ihserosion_high*year_f|FIPS+FIPS_state^year")))

        M4 = feols(data=data[-train_ixs,],as.formula(paste0(outcome,"~pca_km_cat*year_f+ihspct_deps_sus*year_f+ ihscornmean*year_f + ihswheatmean*year_f + ihserosion_med*year_f + ihserosion_high*year_f+temp_mean + temp_mean_sq + rain_mean + rain_mean_sq|FIPS+FIPS_state^year")))

        M5 = feols(data=data[-train_ixs,],as.formula(paste0(outcome,"~pca_km_cat*year_f+ihspct_deps_sus*year_f+ ihscornmean*year_f + ihswheatmean*year_f + ihserosion_med*year_f + ihserosion_high*year_f + ihsAAA*year_f +temp_mean + temp_mean_sq + rain_mean + rain_mean_sq|FIPS+FIPS_state^year")))

        M6 = feols(data=data[-train_ixs,],as.formula(paste0(outcome,"~pca_km_cat*year_f+ihspct_deps_sus*year_f+ ihscornmean*year_f + ihswheatmean*year_f + ihserosion_med*year_f + ihserosion_high*year_f+ihstotal_relief*year_f + ihspublic_works*year_f + ihstotal_grants*year_f + ihstotal_loans*year_f+temp_mean + temp_mean_sq + rain_mean + rain_mean_sq|FIPS+FIPS_state^year")))

        return(c(M1=RMSE(predict(M1,newdata=d[train_ixs,]),d[train_ixs,outcome],na.rm=T),
             M2=RMSE(predict(M2,newdata=d[train_ixs,]),d[train_ixs,outcome],na.rm=T),
             M3=RMSE(predict(M3,newdata=d[train_ixs,]),d[train_ixs,outcome],na.rm=T),
             M4=RMSE(predict(M4,newdata=d[train_ixs,]),d[train_ixs,outcome],na.rm=T),
             M5=RMSE(predict(M5,newdata=d[train_ixs,]),d[train_ixs,outcome],na.rm=T),
             M6=RMSE(predict(M6,newdata=d[train_ixs,]),d[train_ixs,outcome],na.rm=T)))
}

complete_sample = complete.cases(df[,c(covars_timev,E,"ihscropval_pacr","pca_km","ihstotal_relief")])

d = df[complete_sample,]

res1 = list()

res1[[1]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                       Treat = "D*year_f+",X="+0",E="0",G="FIPS")
res1[[2]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                       Treat = "D*year_f+",X="+0",E="0",G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[3]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                         Treat = "D*year_f+",X="+0",E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[4]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                         Treat = "D*year_f+",X=X,E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[5]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                         Treat = "D*year_f+",X=X,E=c(E,E_iffy),G="FIPS +year_f+as.factor(FIPS_state):year_f")


ihs_res = ihs_correct_discrete(res1,"*:year*")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep="D",
          type="text",
          dep.var.labels="Crop Value per Acre ($/acre)",
          omit.stat = c("rsq","ser"),
#           covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Year by State FE","","X","X","X","X"),
                         c("Time-Invariant Controls","","","X","X","X"),
                         c("Time-Variant Controls","","","","X","X"),
                         c("New Deal Spending","","","","","X"),
                         c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

res2 = list()

res2[[1]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                       Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS")
res2[[2]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                       Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[3]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                         Treat = "pca_km_cat*year_f+",X="+0",E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[4]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                         Treat = "pca_km_cat*year_f+",X=X,E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[5]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[3]],
                         Treat = "pca_km_cat*year_f+",X=X,E=c(E,E_iffy),G="FIPS +year_f+as.factor(FIPS_state):year_f")


ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year_f1940",
#           type="text",
          dep.var.labels="Crop Value per Acre ($/acre)",
          omit.stat = c("rsq","ser"),
          covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Year by State FE","","X","X","X","X"),
                         c("Time-Invariant Controls","","","X","X","X"),
                         c("Time-Variant Controls","","","","X","X"),
                         c("New Deal Spending","","","","","X"),
                         c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

outcome = Y_ihs[[3]]
L_all = c()
for(i in 1:100){

L = c()
folds10 = createFolds(y = d[,outcome],k=10)
for(fold in folds10){
   L= cbind(L,KFoldCV(outcome,fold,d))
}

L = as.data.frame(L)

L_all = cbind(L_all,rowMeans(L))}

write.csv(L_all,"./clean_data/crop_rmses.csv")

complete_sample = complete.cases(df[,c(covars_timev,E,"ihscorn_yield7","pca_km","ihstotal_relief")])

d = df[complete_sample,]

res1 = list()

res1[[1]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                       Treat = "D*year_f+",X="+0",E="0",G="FIPS")
res1[[2]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                       Treat = "D*year_f+",X="+0",E="0",G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[3]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                         Treat = "D*year_f+",X="+0",E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[4]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                         Treat = "D*year_f+",X=X,E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[5]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                         Treat = "D*year_f+",X=X,E=c(E,E_iffy),G="FIPS +year_f+as.factor(FIPS_state):year_f")


ihs_res = ihs_correct_discrete(res1,"*:year*")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep="D",
          type="text",
          dep.var.labels="Corn Yield",
          omit.stat = c("rsq","ser"),
#           covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Year by State FE","","X","X","X","X"),
                         c("Time-Invariant Controls","","","X","X","X"),
                         c("Time-Variant Controls","","","","X","X"),
                         c("New Deal Spending","","","","","X"),
                         c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))


res2 = list()

res2[[1]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                       Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS")
res2[[2]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                       Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[3]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                         Treat = "pca_km_cat*year_f+",X="+0",E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[4]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                         Treat = "pca_km_cat*year_f+",X=X,E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[5]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[1]],
                         Treat = "pca_km_cat*year_f+",X=X,E=c(E,E_iffy),G="FIPS +year_f+as.factor(FIPS_state):year_f")

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year_f1940",
          type="text",
          dep.var.labels="Corn Yield",
          omit.stat = c("rsq","ser"),
          covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Year by State FE","","X","X","X","X"),
                         c("Time-Invariant Controls","","","X","X","X"),
                         c("Time-Variant Controls","","","","X","X"),
                         c("New Deal Spending","","","","","X"),
                         c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

outcome = Y_ihs[[1]]
L_all = c()
for(i in 1:100){

L = c()
folds10 = createFolds(y = d[,outcome],k=10)
for(fold in folds10){
   L= cbind(L,KFoldCV(outcome,fold,d))
}

L = as.data.frame(L)

L_all = cbind(L_all,rowMeans(L))}

write.csv(L_all,"./clean_data/corn_rmses.csv")

complete_sample = complete.cases(df[,c(covars_timev,E,"ihstractors_per_farm","pca_km","ihstotal_relief")])

d = df[complete_sample,]

res1 = list()

res1[[1]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                       Treat = "D*year_f+",X="+0",E="0",G="FIPS")
res1[[2]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                       Treat = "D*year_f+",X="+0",E="0",G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[3]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                         Treat = "D*year_f+",X="+0",E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[4]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                         Treat = "D*year_f+",X=X,E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res1[[5]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                         Treat = "D*year_f+",X=X,E=c(E,E_iffy),G="FIPS +year_f+as.factor(FIPS_state):year_f")


ihs_res = ihs_correct_discrete(res1,"*:year*")

Fpre = pre_trend(res1,treat="D")

stargazer(res1,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep="D",
          type="text",
          dep.var.labels="Corn Yield",
          omit.stat = c("rsq","ser"),
#           covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Year by State FE","","X","X","X","X"),
                         c("Time-Invariant Controls","","","X","X","X"),
                         c("Time-Variant Controls","","","","X","X"),
                         c("New Deal Spending","","","","","X"),
                         c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

complete_sample = complete.cases(df[,c(covars_timev,E,"ihstractors_per_farm","pca_km","ihstotal_relief")])

res2 = list()

res2[[1]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                       Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS")
res2[[2]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                       Treat = "pca_km_cat*year_f+",X="+0",E="0",G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[3]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                         Treat = "pca_km_cat*year_f+",X="+0",E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[4]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                         Treat = "pca_km_cat*year_f+",X=X,E=E,G="FIPS +year_f+as.factor(FIPS_state):year_f")
res2[[5]] <- run_model(data=df[complete_sample,],Y=Y_ihs[[4]],
                         Treat = "pca_km_cat*year_f+",X=X,E=c(E,E_iffy),G="FIPS +year_f+as.factor(FIPS_state):year_f")



ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year_f1940",
#           type="text",
          dep.var.labels="Tractors per Farm (#/farm)",
          omit.stat = c("rsq","ser"),
          covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Year by State FE","","X","X","X","X"),
                         c("Time-Invariant Controls","","","X","X","X"),
                         c("Time-Variant Controls","","","","X","X"),
                         c("New Deal Spending","","","","","X"),
                         c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

outcome = Y_ihs[[4]]
L_all = c()
for(i in 1:100){

L = c()
folds10 = createFolds(y = d[,outcome],k=10)
for(fold in folds10){
   L= cbind(L,KFoldCV(outcome,fold,d))
}
    
L = as.data.frame(L)

L_all = cbind(L_all,rowMeans(L))}

write.csv(L_all,"./clean_data/tractor_rmses.csv")

run_model <- function(data=data,
                      Y="ihscropval_pacr",
                      Treat="D*year_f + ",
                      X = covars_timev,
                      E = covars_time_inv,
                      G="fips+year_f+as.factor(fips_state):year_f",
                      C="FIPS",
                      two_period=F){
  
  if (two_period){
    E = paste(E,collapse = "*as.factor(after) + ")
    } else {
    E = paste(E,collapse = "*year_f + ")
    }
  
  X = paste("+",X,collapse = " + ")
  
  covars <- paste(E,X)
  
  Z <- "0"
  f = as.formula(paste(Y," ~ ",Treat,covars,"|",G,"|",Z,"|",C))
  return(felm(data = data,formula= f, keepCX = TRUE))
}

res2 = list()
for(i in 1:5) {
  res2[[i]] <- run_model(data=df,Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=X,E=c(E,E_iffy,""),
                         G="year + FIPS + as.factor(FIPS_state):year_f",
                         C="lat + lon")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text")

conleySE <- function(reg = res2[[1]],
                     start_ix=17,
                     unit="FIPS",
                     time="year",
                     lat="lat",
                     lon="lon",
                     kernel = "bartlett",
                     dist_fn = "Haversine",
                     dist_cutoff = 500,
                     lag_cutoff = 5,
                     lat_scale = 111,
                     verbose = FALSE,
                     cores = 1,
                     balanced_pnl = FALSE){

    Fac2Num <- function(x) {as.numeric(as.character(x))}

    pkgs <- c("data.table", "lfe", "geosphere", "Rcpp", "RcppArmadillo")
    invisible(sapply(pkgs, require, character.only = TRUE))



    sourceCpp("cpp-functions.cpp")

    iterateObs <- function(sub_index, type, cutoff, ...) {
    if(type == "spatial" & balanced_pnl) {

        sub_dt <- dt[time == sub_index]
        n1 <- nrow(sub_dt)
        if(n1 > 1000 & verbose){message(paste("Starting on sub index:", sub_index))}

        X <- as.matrix(sub_dt[, eval(Xvars), with = FALSE])
        e <- sub_dt[, e]

        XeeXhs <- Bal_XeeXhC(d, X, e, n1, k)

    } else if(type == "spatial" & !balanced_pnl) {

        sub_dt <- dt[time == sub_index]
        n1 <- nrow(sub_dt)
        if(n1 > 1000 & verbose){message(paste("Starting on sub index:", sub_index))}

        X <- as.matrix(sub_dt[, eval(Xvars), with = FALSE])
        e <- sub_dt[, e]
        lat <- sub_dt[, lat]; lon <- sub_dt[, lon]

        # If n1 >= 50k obs, then avoiding construction of distance matrix.
        # This requires more operations, but is less memory intensive.
        if(n1 < 5 * 10^4) {
            XeeXhs <- XeeXhC(cbind(lat, lon), cutoff, X, e, n1, k,
                kernel, dist_fn)
        } else {
            XeeXhs <- XeeXhC_Lg(cbind(lat, lon), cutoff, X, e, n1, k,
                kernel, dist_fn)
        }

    } else if(type == "serial") {
        sub_dt <- dt[unit == sub_index]
        n1 <- nrow(sub_dt)
        if(n1 > 1000 & verbose){message(paste("Starting on sub index:", sub_index))}

        X <- as.matrix(sub_dt[, eval(Xvars), with = FALSE] )
        e <- sub_dt[, e]
        times <- sub_dt[, time]

        XeeXhs <- TimeDist(times, cutoff, X, e, n1, k)
    }

    XeeXhs
}

    Xvars <- names(reg$coefficients[start_ix:length(reg$coefficients),])
    dt = data.table(reg$cY, reg$cX[,start_ix:length(reg$coefficients)],
                    fe1 = Fac2Num(reg$fe[[1]]),
                    fe2 = Fac2Num(reg$fe[[2]]),
                    fe3 = Fac2Num(reg$fe[[3]]),
                    coord1 = Fac2Num(reg$clustervar[[1]]),
                    coord2 = Fac2Num(reg$clustervar[[2]]))
            setnames(dt,
                c("fe1", "fe2", "fe3", "coord1", "coord2"),
                c(names(reg$fe), names(reg$clustervar)))
    dt = dt[, e := as.numeric(reg$residuals)]


    n <- nrow(dt)
    k <- length(Xvars)

    # Renaming variables:
    orig_names <- c(unit, time, lat, lon)
    new_names <- c("unit", "time", "lat", "lon")
    setnames(dt, orig_names, new_names)

    # Empty Matrix:
    XeeX <- matrix(nrow = k, ncol = k, 0)

    # Correct for spatial correlation:
    timeUnique <- unique(dt[, time])
    Ntime <- length(timeUnique)
    setkey(dt, time)



    if(verbose){message("Starting to loop over time periods...")}


    if(cores == 1) {
        XeeXhs <- lapply(timeUnique, function(t) iterateObs(sub_index = t,
            type = "spatial", cutoff = dist_cutoff))
    } else {
        XeeXhs <- mclapply(timeUnique, function(t) iterateObs(sub_index = t,
            type = "spatial", cutoff = dist_cutoff), mc.cores = cores)
    }

 if(balanced_pnl){rm(d)}

    # First Reduce:
	XeeX <- Reduce("+",  XeeXhs)

    # Generate VCE for only cross-sectional spatial correlation:
    X <- as.matrix(dt[, eval(Xvars), with = FALSE])
    invXX <- solve(t(X) %*% X) * n

    V_spatial <- invXX %*% (XeeX / n) %*% invXX / n

    V_spatial <- (V_spatial + t(V_spatial)) / 2

    if(verbose) {message("Computed Spatial VCOV.")}

    #================================================================
    # Correct for serial correlation:
    panelUnique <- unique(dt[, unit])
    Npanel <- length(panelUnique)
    setkey(dt, unit)

    if(verbose){message("Starting to loop over units...")}

    if(cores == 1) {
        XeeXhs <- lapply(panelUnique, function(t) iterateObs(sub_index = t,
            type = "serial", cutoff = lag_cutoff))
    } else {
        XeeXhs <- mclapply(panelUnique,function(t) iterateObs(sub_index = t,
            type = "serial", cutoff = lag_cutoff), mc.cores = cores)
    }

	XeeX_serial <- Reduce("+",  XeeXhs)

	XeeX <- XeeX + XeeX_serial

    V_spatial_HAC <- invXX %*% (XeeX / n) %*% invXX / n
    V_spatial_HAC <- (V_spatial_HAC + t(V_spatial_HAC)) / 2
                           
    return(V_spatial_HAC)}
                           
                           

corn_SE = conleySE()
wheat_SE = conleySE(reg=res2[[2]],start_ix=17)
crop_SE = conleySE(reg=res2[[3]],start_ix=16)
tractor_SE = conleySE(reg=res2[[4]],start_ix=15)
fert_SE = conleySE(reg=res2[[5]],start_ix=16)

corn_SEs = c(replicate(16,NaN),diag(corn_SE**.5))
wheat_SEs = c(replicate(17,NaN),diag(wheat_SE**.5))
crop_SEs = c(replicate(16,NaN),diag(crop_SE**.5))
tractor_SEs = c(replicate(15,NaN),diag(tractor_SE**.5))
fert_SEs = c(replicate(16,NaN),diag(fert_SE**.5))

L1 = replicate(length(res2[[1]]),NaN)
L1[grep("pca_km*",names(corn_SEs))] = corn_SEs[grep("pca_km*",names(corn_SEs))]

L2 = replicate(length(res2[[2]]),NaN)
L2[grep("pca_km*",names(wheat_SEs))] = wheat_SEs[grep("pca_km*",names(wheat_SEs))]

L3 = replicate(length(res2[[3]]),NaN)
L3[grep("pca_km*",names(crop_SEs))] = crop_SEs[grep("pca_km*",names(crop_SEs))]

L4 = replicate(length(res2[[4]]),NaN)
L4[grep("pca_km*",names(tractor_SEs))] = tractor_SEs[grep("pca_km*",names(tractor_SEs))]

L5 = replicate(length(res2[[5]]),NaN)
L5[grep("pca_km*",names(fert_SEs))] = fert_SEs[grep("pca_km*",names(fert_SEs))]

stargazer(list(res2[[1]],res2[[1]],res2[[2]],res2[[2]],res2[[3]],res2[[3]]),
          se  = list(NULL,L1,NULL,L2,NULL,L3),
          keep="p.*:*1940")

stargazer(list(res2[[4]],res2[[4]],res2[[5]],res2[[5]]),
          se  = list(NULL,L4,NULL,L5),
          keep="p.*:*1940")

beta = reg$coefficients[16:length(reg$coefficients),]
CSE = diag(V_spatial_HAC)**.5

cbind(beta,
      CSE,
      beta - CSE*1.96,
      beta + CSE*1.96)

df$great_plains <- (df$FIPS_state==8)|
                    (df$FIPS_state==19)|
                    (df$FIPS_state==20)|
                    (df$FIPS_state==27)|
                    (df$FIPS_state==31)|
                    (df$FIPS_state==35)|
                    (df$FIPS_state==38)|
                    (df$FIPS_state==40)|
                    (df$FIPS_state==46)|
                    (df$FIPS_state==48)

all_data = replicate(length(df),TRUE)

# These states only have one PCA
df$restricted = !(df$state_name =="Arizona" | 
               df$state_name=="Utah" | 
               df$state_name =="Nevada" | 
               df$state_name =="Wyoming")

restricted = !(df$state_name =="Arizona" | 
               df$state_name=="Utah" | 
               df$state_name =="Nevada" | 
               df$state_name =="Wyoming")

# Leave out the whole western states
df$no_west = df$pca_district_groups!="West"

no_west = df$pca_district_groups!="West"

# No counties in the Great Plains that suffered erosions
df$dust_bowl = (df$great_plains &
                 ((df$erosion_high>0)))

df$no_dust_bowl = !df$dust_bowl

dust_bowl = (df$great_plains &
                 ((df$erosion_high>0)))

no_dust_bowl = !df$dust_bowl



samples = list(all_data,restricted,no_west,no_dust_bowl,dust_bowl)

write.csv(df[,c("FIPS","restricted","no_west","no_dust_bowl","dust_bowl")],"./clean_data/samples.csv")

res2 = list()

res2[[1]] <- run_model(data=df,Y=Y_ihs[[3]],
                         Treat = "pca_km_cat*year_f+",
                         X=X,E=c(E,E_iffy),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")

for(i in 2:length(samples)) {
  res2[[i]] <- run_model(data=df[samples[[i]],],
                         Y=Y_ihs[[3]],
                         Treat = "pca_km_cat*year_f+",
                         X=X,E=c(E,E_iffy),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}


ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:3)],
          se  = ihs_res[paste0("se",1:3)],
          p   = ihs_res[paste0("p",1:3)],
          keep=":year_f1940",
#           type="text",
          dep.var.caption = "Crop Value per Acre, 1940 Coefficients",
          dep.var.labels= "",
          column.labels=c("All Data","Restricted","No West","No Dust Bowl","Only Dust Bowl"),
          omit.stat = c("rsq","ser"),
          covariate.labels = c("(30, 45] km","(45, 60] km","(60, 100] km", "> 100 km"),
          add.lines=list(c("Pre-Trend F Stat",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

df$east_midwest = ((!df$great_plains)&(df$restricted)&(df$no_west))

res2_east = list()
for(i in 1:5) {
  res2_east[[i]] <- run_model(data=df[df$east_midwest,],Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=c(X),E=c(E,E_iffy,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2_east,"*:year*")

Fpre = pre_trend(res2_east,treat="pca_km_cat")

stargazer(res2_east,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))



res2_plains = list()
for(i in 1:5) {
  res2_plains[[i]] <- run_model(data=df[df$great_plains,],Y=Y_ihs[[i]],
                         Treat = "pca_km_cat*year_f+",
                         X=c(X),E=c(E,E_iffy,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2_plains,"*:year*")

Fpre = pre_trend(res2_plains,treat="pca_km_cat")

stargazer(res2_plains,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year",
          type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))



df[is.na(df$corn_hybrid_pct),"corn_hybrid_pct"] <- 0

df$corn_hybrid_state <- df$corn_hybrid_pct!=0

df$hybrid_early <- (df$corn_hybrid_pct!=0 & df$year==1935)

early_adopters <-
df %>%
  group_by(FIPS) %>%
  summarise(hybrid_early_adopt = max(hybrid_early, na.rm=TRUE))

df$hybrid_adopter <- (df$corn_hybrid_pct!=0 & df$year==1940)

adopters <-
df %>%
  group_by(FIPS) %>%
  summarise(hybrid_adopt = max(hybrid_adopter, na.rm=TRUE))

adopt_df = merge(early_adopters,adopters,by="FIPS")

adopt_df$hybrid_late_adopt <- as.numeric((adopt_df$hybrid_early!=1)&(adopt_df$hybrid_adopt==1))

adopt_df$hybrid_never <- as.numeric((adopt_df$hybrid_early!=1)&(adopt_df$hybrid_adopt!=1))

df <- merge(df,adopt_df,by="FIPS",all.x=TRUE)

samples = list(df$FIPS>0,df$hybrid_early_adopt==1,df$hybrid_late_adopt==1,df$hybrid_never==1)

write.csv(df[,c("FIPS","hybrid_early_adopt","hybrid_late_adopt","hybrid_never")],"./clean_data/corn_samples.csv")

res2= list()
for(i in 1:4) {
  res2[[i]] <- run_model(data=df[samples[[i]],],Y=Y_ihs[[1]],
                         Treat = "pca_km_cat*year_f+",
                         X=c(X),E=c(E,E_iffy,""),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2,"*:year*")

Fpre = pre_trend(res2,treat="pca_km_cat")

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year_f1935",
#           type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

stargazer(res2,
          coef= ihs_res[paste0("ests",1:5)],
          se  = ihs_res[paste0("se",1:5)],
          p   = ihs_res[paste0("p",1:5)],
          keep=":year_f1940",
#           type="text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))

Treat = list("PCA","PCA_area_adj","PCA")
Samples = list(df$PCA_area_adj>-1,df$PCA_area_adj>-1,df$PCA_area_adj==1)

res2 = list()

for(i in 1:3) {
  res2[[i]] <- run_model(data=df[Samples[[i]],],
                         Y=Y_ihs[[3]],
                         Treat = paste(Treat[[i]],"*year_f+"),
                         X=X,E=c(E,E_iffy),
                         G="FIPS +year_f+as.factor(FIPS_state):year_f")
}

ihs_res = ihs_correct_discrete(res2,"PCA.*:year.*")

Fpre = pre_trend(res2,treat="PCA.*:")

stargazer(res2,
          coef = ihs_res[paste0("ests",1:5)],
          se   = ihs_res[paste0("se",1:5)],
          p    = ihs_res[paste0("p",1:5)],
          keep = "PCA.*",
#           type = "text",
          add.lines=list(c("Pre-Trend F-test",unlist(lapply(Fpre$chi,round,3))),
                         c("Pre-Trend P-Value",unlist(lapply(Fpre$chi_pvals,round,3)))))
