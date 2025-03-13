# see https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
# for details on using `.data[[]]` pronoun


# ---- contingency-table-views ----------------
# function to produce a frequency table between (upto) TWO categorical variables
make_bi_freq_table <- function(
    d
    , var1                # 1st categorical variable
    , var2=var1           # 2nd categorical variable
    , rows_ids = TRUE
){
  # browser()
  # d <- ds1 # load the data set before testing the function
  # var1 = "sex"
  # var2 = "t_stage"
  # to ensure only one variable is passed if univariate
  ls_var_names <- list(var1, var2)
  if( ls_var_names[1][[1]] == ls_var_names[2][[1]] ){
    ls_var_names <- ls_var_names[-2]
  }

  if(rows_ids!=TRUE & is.character(rows_ids)){
    rows_ids_name <- rows_ids
  }else{
    rows_ids_name <- "rows_ids_auto"
  }

  d1 <- d %>%
    mutate(rows_ids_auto = row_number()) %>%
    # dplyr::group_by(.dots = unique(c(var1, var2)))%>% # simpler but depricated
    dplyr::group_by(!!!rlang::syms(ls_var_names)) %>%
    dplyr::summarize(
      row_count = n()
      ,id_count = n_distinct(!!rlang::sym(rows_ids_name)) # counts UNIUQE persons
      ,.groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      id_count_total = sum(id_count, na.rm =T)
    ) %>%
    # dplyr::group_by(.dots = var1) %>%
    dplyr::group_by(!!rlang::sym(var1)) %>%
    dplyr::mutate(
      var1_count = sum(id_count, na.rm = T)

      ,var1_prop = (var1_count/id_count_total)
      ,var12_prop = (id_count/var1_count)
      #
      ,var1_pct = scales::label_percent(accuracy = .1)(var1_prop)
      ,var12_pct = scales::label_percent(accuracy=.1)(var12_prop)
    ) %>%
    ungroup()

  # ,pct_1 = scales::label_percent()(total_1/total)
  # ,pct_12 = scales::label_percent()(n_people/total_1)
  #
  n_total <-  d1 %>% pull(id_count_total) %>% unique()
  return(d1)
}
# ds2 %>% make_bi_freq_table(var1="employment_state", var2="sex")
# ds2 %>% make_bi_freq_table(var1="employment_state", var2="sex")
# ds2 %>% make_bi_freq_table(var1="employment_state")
# ds2 %>% make_bi_freq_table(var1="gender", var2="employment_state")
# ds2 %>% make_bi_freq_table("sex","employment_state")
# View relationship between two categorical variables as a set of bar graphs
make_bi_freq_graph <- function(
    d
    ,var1                # 1st categorical variable
    ,var2     = var1     # 2nd categorical variable
    ,voption  ="plasma"  # a palette from {viridis} package
    ,n_breaks = 10       # number of labeled values on horizontal axis
    ,rev      = FALSE    # reverse the color gradient
){
  # var1 = "gender"
  # var1 = "is_episode_count"
  # var2 = var1
  # voption="plasma"
  # n_breaks = 10
  # d <- ds2
  # browser()
  d1 <- d  %>%
    make_bi_freq_table(var1, var2) %>%
    mutate_at(
      .vars = all_of(var1)
      ,.funs = ~factor(.data[[var1]])
    ) %>%
    mutate_at(
      .vars = all_of(var2)
      ,.funs = ~factor(.data[[var2]])
    )
  if(rev){
    d1 <- d1 %>%
      mutate_at(
        .vars = all_of(var1)
        ,.funs = ~fct_rev(.data[[var1]])

      )
  }

  d2 <- d1 %>% filter(!is.na(.data[[var1]])) # drop empty factors

  n_total <-  d2 %>% pull(id_count) %>% unique()

  g1 <- d2 %>%
    # ggplot2::ggplot(ggplot2::aes_string(x =var1, y = "id_count", fill = var2 ))+
    ggplot2::ggplot(ggplot2::aes(x =!!rlang::sym(var1), y = id_count, fill = !!rlang::sym(var2) ))+
    ggplot2::geom_col(position = ggplot2::position_dodge(), alpha = .7, color = "black")+
    # ggplot2::geom_text(ggplot2::aes(label = n_people),position = ggplot2::position_dodge(.9), vjust = 1.5, color = "white", size = 5 )+
    ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = voption
                                  ,guide= guide_legend(reverse=T)
    )+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(
      expand=ggplot2::expansion(mult = c(0,.1))
      , labels = scales::comma_format()
      # ,breaks =pretty_breaks()
      ,breaks = scales::breaks_pretty(n=n_breaks)

    )
  g1
  if(var1 == var2){
    g1 <- g1 +
      # ggplot2::geom_text(ggplot2::aes_string(label = "var1_pct"),position = ggplot2::position_dodge(.9), hjust = -0.1, color = "black", size = 4)+
      ggplot2::geom_text(ggplot2::aes(label = var1_pct),position = ggplot2::position_dodge(.9), hjust = -0.1, color = "black", size = 4)+
      ggplot2::labs(y = "Count", title = paste0("Frequency distribution of (",var1,")"))
  }else{
    g1 <- g1 +
      ggplot2::geom_text(ggplot2::aes(label = var12_pct),position = ggplot2::position_dodge(.9)
                         # , vjust = -.5
                         ,hjust = -0.1
                         , color = "black", size = 4)
    g1 <- g1 + ggplot2::labs(y = "Number of respondents", title = paste0("Association between (",var1,") and (",var2,")"))
  }


  return(g1)
}
# How to use
# ds2 %>% make_bi_freq_graph(var1="sex", var2="employment_state")
# ds2 %>% make_bi_freq_graph(var1="employment_state", var2="sex")


# Input = two categorical variables, var2 - used as dependent
run_contingency <- function(d,var1, var2){
  # browser()
  # d <- ds1
  # var1 <- "sex"
  # var2 <- "ulcer"

  # d1 <- d %>% select(.data[[var1]], .data[[var2]]) %>% na.omit()
  # v1 <- d1 %>% select(.data[[var1]]) %>% mutate_all(., as.character) %>% pull()
  # v2 <- d1 %>% select(.data[[var2]]) %>% mutate_all(., as.character) %>% pull()
  #
  d1 <- d %>%
    # select(.data[[var1]], .data[[var2]]) %>%
    select(all_of(c(var1,var2))) %>%
    na.omit() %>%
    mutate_all(., as.character) # to ensure factors and integers are treated as factors

  # (crosstab <- table(v1,v2))
  (crosstab <- table(d1[[var1]], d1[[var2]]))

  # Interpretations (min, max)
  # contingency_c   - (0,1) - Misfit adjusted for sample size
  # cramer_v        - (0,1) - Strength of association (none to perfect)
  # uncertainty_c   - (0,1) - Entropy/ given Y, what fraction of the bits of X can we predict?
  # kruskal_lambda  - (0,1) - Proportional Reduction in Error

  chisq.test(crosstab) %>% print()

  # Contingency Coefficient Corrected - rescaled to be 0 to 1 (C_max is not bound by 1 and depends on dims(crosstab))
  # measures the degree of association (from none to perfect),
  contingency_coef_corrected <- DescTools::ContCoef(crosstab, correct = TRUE)
  # Cramer's V Unbiased - (bias = lower N tends to overestimate effect)
  cramer_v_unbiased <- rcompanion::cramerV(crosstab, bias.correct = TRUE)
  # Uncertainty Coefficient - Measure of Entropy
  # uncertainty_c  <- DescTools::UncertCoef(crosstab, direction = "column", conf.level = NA, p.zero.correction = 1/(sum(crosstab)^2) )
  uncertainty_c  <- DescTools::UncertCoef(crosstab, direction = "column" )
  # Proportional reduction in error - Goodman-Kruskal Lambda
  kruskal_lambda <- DescTools::Lambda(crosstab, direction = "column") # Count( var1) is independed. X(var1) predicts Y(var2)
  # for verification see https://rcompanion.org/handbook/H_10.html

  # crosstab %>% DescTools::Assocs() # view a useful set of indices

  lto <- list(
    "varnames"        = c("var1"=var1, "var2"=var2)
    ,"crosstab"       = crosstab
    ,"test_values" = list(
      "contingency_c"   = contingency_coef_corrected
      ,"cramer_v"       = cramer_v_unbiased
      ,"uncertainty_c"  = uncertainty_c
      ,"kruskal_lambda" = kruskal_lambda
    )

  )
  return(lto)
}
# ls_crosstab <- ds2 %>% run_contingency("sex", "employment_state")
# ls_crosstab$varnames      # names of the two dimension used to create crosstab
# ls_crosstab$crosstab      # cross-tabulation of counts (rows by default)
# ls_crosstab$test_values   # curated set of indicies obtained from crosstab
