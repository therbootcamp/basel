#' Simulate baselers dataset
#'
#' This function can be used to simulate nsim participants. It draws samples from
#' a mulitvariate normal distribution with a prespecified correlation matrix (corMat_f
#' for women and corMat_m for men). Most (but not all) of the means (but not the
#' correlations) are based on actual data of the swiss population. Details can be
#' found in the data helpfile you can call with ?baselers if the package is loaded.
#'
#' @param nsim integer. The number of participants to simulate (same number of men
#' and women). If an uneven number is specified, the returned dataset will contain
#' one additional row.
#' @param corMat_f double matrix. A symmetric correlation matrix. If not specified
#' the default matrix is used.
#' @param corMat_m double matrix. A symmetric correlation matrix. If not specified
#' the default matrix is used.
#' @param seed integer. Is passed to set.seed, specify the same number to replicate
#' results. If not provided, results will vary randomly.
#'
#' @export
#'
#' @return A tibble with nsim rows, containing continous and categorical simulated
#' data of inhabitants of Basel.
#' @import dplyr
#' @importFrom stats runif
simulate_basel <- function(nsim = 1000,
                              corMat_f = NULL,
                              corMat_m = NULL,
                              seed = NULL){

  # some sources:
  # age: https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.3202980.html
  # weight, height: https://www.laenderdaten.info/durchschnittliche-koerpergroessen.php
  # tattoo: https://www.migrosmagazin.ch/tattoos-ohne-grenzen
    # income: https://www.srf.ch/news/schweiz/mit-7100-franken-pro-monat-ueberleben-oder-etwa-doch-weniger
  # consultations: https://www.krankenkasse-vergleich.ch/news/das-schweizer-gesundheitssystem-in-der-oecd-statistik, https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.250255.html

  if (!is.null(seed) && is.numeric(seed)){
    set.seed(seed = seed)
  }

  # continuous variables:
  # age, income (per month), weight (kg), height (cm), children,
  # happiness (0 to 10), fitness (0 to 10), food, alcohol, tattoos,
  # rhine (no per month), datause (no of times phone is checked per day),
  # consultations, hiking

  # means and sds for females and males
  mu_f <- c(43.14, 7112, 64, 164, 1.54, 6, 5, 438, 25, 3, 4, 88, 4.5, 60)
  stddev_f <- c(20, 1200, 8, 10, 1.25, 2, 2, 80, 10, 2.8, 2.5, 20, 2, 20)
  var_f <- stddev_f ^ 2

  mu_m <- c(41.01, 7112, 84.9, 178, 1.54, 6, 5, 438, 40, 3.8, 4, 88, 3, 60)
  stddev_m <- c(20, 1200, 12, 11, 1.25, 2, 2, 80, 15, 2.8, 2.5, 20, 1.5, 25)
  var_m <- stddev_m ^ 2

  if (is.null(corMat_f)){
    # correlation matrix females
    corMat_f <- matrix(c(   var_f[1],   .3,  -.1, -.15,   .2,   .1, -.25,  -.1,    0, -.45, -.15, -.23,   .5,    0,
                           .3,    var_f[2],    0,    0,  -.1,  .15,    0,   .5,   .2, -.08,    0,    0, -.05,    0,
                          -.1,    0,    var_f[3],   .6,    0,    0,  -.3,    0,  .15,    0,    0,    0,  .15, -.15,
                         -.15,    0,   .6,    var_f[4],    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                           .2,  -.1,    0,    0,    var_f[5],    0,    0,   .4,    0,    0,  -.1,    0,    0,    0,
                           .1,  .15,    0,    0,    0,    var_f[6],  .15,   .1,    0,    0,   .2,  -.3, -.15,  .25,
                         -.25,    0,  -.3,    0,    0,  .15,    var_f[7],    0, -.05,  .15,   .2,    0,  -.1,   .3,
                          -.1,   .5,    0,    0,   .4,   .1,    0,    var_f[8],    0,    0,    0,    0,    0,    0,
                            0,   .2,  .15,    0,    0,    0, -.05,    0,    var_f[9],    0,    0,    0,  .15,    0,
                         -.45, -.08,    0,    0,    0,    0,  .15,    0,    0,    var_f[10],    0,    0,    0,    0,
                         -.15,    0,    0,    0,  -.1,   .2,   .2,    0,    0,    0,    var_f[11],    0,    0,   .1,
                         -.23,    0,    0,    0,    0,  -.3,    0,    0,    0,    0,    0,    var_f[12],    0,    0,
                           .5, -.05,  .15,    0,    0, -.15,  -.1,    0,  .15,    0,    0,    0,    var_f[13], -.15,
                            0,    0, -.15,    0,    0,  .25,   .3,    0,    0,    0,   .1,    0, -.15,    var_f[14]),
                     ncol = 14)
  }

  if (is.null(corMat_m)){
    # correlation matrix for males
    corMat_m <- matrix(c(   var_m[1],   .3,  -.1, -.15,   .2,   .1, -.25,  -.1,    0, -.25, -.15, -.23,   .5,    0,
                           .3,    var_m[2],    0,    0,  -.1,  .15,    0,   .5,   .2, -.08,    0,    0, -.05,    0,
                          -.1,    0,    var_m[3],   .6,    0,    0,  -.3,    0,  .15,    0,    0,    0,  .15, -.15,
                         -.15,    0,   .6,    var_m[4],    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                           .2,  -.1,    0,    0,    var_m[5],    0,    0,   .4,    0,    0,  -.1,    0,    0,    0,
                           .1,  .15,    0,    0,    0,    var_m[6],  .15,   .1,    0,    0,   .2,  -.3, -.15,  .25,
                         -.25,    0,  -.3,    0,    0,  .15,    var_m[7],    0, -.05,  .15,   .2,    0,  -.1,   .3,
                          -.1,   .5,    0,    0,   .4,   .1,    0,    var_m[8],    0,    0,    0,    0,    0,    0,
                            0,   .2,  .15,    0,    0,    0, -.05,    0,    var_m[9],    0,    0,    0,  .15,    0,
                         -.25, -.08,    0,    0,    0,    0,  .15,    0,    0,    var_m[10],    0,    0,    0,    0,
                         -.15,    0,    0,    0,  -.1,   .2,   .2,    0,    0,    0,    var_m[11],    0,    0,   .1,
                         -.23,    0,    0,    0,    0,  -.3,    0,    0,    0,    0,    0,    var_m[12],    0,    0,
                           .5, -.05,  .15,    0,    0, -.15,  -.1,    0,  .15,    0,    0,    0,    var_m[13], -.15,
                            0,    0, -.15,    0,    0,  .25,   .3,    0,    0,    0,   .1,    0, -.15,    var_m[14]),
                       ncol = 14)
  }


  # if matrices are not positive definite, force them to be
  if (!corpcor::is.positive.definite(corMat_f)){
    corMat_f <- corpcor::make.positive.definite(corMat_f, tol=1e-3)
  }

  if (!corpcor::is.positive.definite(corMat_m)){
    corMat_m <- corpcor::make.positive.definite(corMat_m, tol=1e-3)
  }

  # draw samples from multinormal distribution
  mat_f <- MASS::mvrnorm(n = round(nsim / 2), mu = mu_f, Sigma = corMat_f,
                         empirical = TRUE)
  mat_m <- MASS::mvrnorm(n = round(nsim / 2), mu = mu_m, Sigma = corMat_m,
                         empirical = TRUE)


  tib_f <- tibble::as_tibble(mat_f)
  names(tib_f) <- c("alter", "einkommen", "gewicht", "groesse", "kinder",
                    "glueck", "fitness", "essen", "alkohol",
                    "tattoos", "rhein", "datause", "arztbesuche",
                    "wandern")

  tib_m <- tibble::as_tibble(mat_m)
  names(tib_m) <- c("alter", "einkommen", "gewicht", "groesse", "kinder",
                    "glueck", "fitness", "essen", "alkohol",
                    "tattoos", "rhein", "datause", "arztbesuche",
                    "wandern")

  tib_f$geschlecht <- "f"
  tib_m$geschlecht <- "m"

  tib_f$bildung <- sample(c("obligatorisch", "lehre", "sek II",
                            "sek III"), size = nrow(tib_f), replace = TRUE,
                            prob = c(.146, .447, .093, .314))
  tib_m$bildung <- sample(c("obligatorisch", "lehre", "sek II",
                              "sek III"), size = nrow(tib_m), replace = TRUE,
                                 prob = c(.101, .38, .05, .469))

  tib_f$konfession <- ifelse(tib_f$bildung ==  "sek III",
                             sample(c("konfessionslos", "muslimisch", "andere", "katholisch",
                               "evangelisch-reformiert"), size = nrow(tib_f),
                             replace = TRUE, prob = c(.353, .026, .049, .347, .225)),
                             sample(c("konfessionslos", "muslimisch", "andere", "katholisch",
                                      "evangelisch-reformiert"), size = nrow(tib_f),
                                    replace = TRUE, prob = c(.253, .051, .074, .372, .25)))

  tib_m$konfession <- ifelse(tib_m$bildung ==  "sek III",
                             sample(c("konfessionslos", "muslimisch", "andere", "katholisch",
                                      "evangelisch-reformiert"), size = nrow(tib_m),
                                    replace = TRUE, prob = c(.353, .026, .049, .347, .225)),
                             sample(c("konfessionslos", "muslimisch", "andere", "katholisch",
                                      "evangelisch-reformiert"), size = nrow(tib_m),
                                    replace = TRUE, prob = c(.253, .051, .074, .372, .25)))

  tib_f$fasnacht <- sample(c("ja", "nein"), size = nrow(tib_f), replace = TRUE,
                                  prob = c(.02, .98))
  tib_m$fasnacht <- sample(c("ja", "nein"), size = nrow(tib_m), replace = TRUE,
                                  prob = c(.035, .965))

  tib <- rbind(tib_f, tib_m)

  tib$alkohol <- tib$alkohol + ifelse(tib$fasnacht == "ja",
                                                        runif(1, 0:40), 0)

  tib$sehhilfe <- sample(c("ja", "nein"), size = nsim, replace = TRUE,
                               prob = c(.66, .37))

  tib <- tib[sample(1:nsim),]


  # id_scramble <- paste0("bas_", sapply(1:nsim, FUN = function(x) {paste(sample(LETTERS, size = 5, replace = TRUE), collapse = "")}))

  id_scramble <- 1:nsim


  tib <- tib %>%
    mutate(id = id_scramble,
           alter = case_when(alter < 18 | alter > 105 ~ runif(1, 18, 85),
                           TRUE ~ alter),
           alter = round(alter, 0),

           groesse = round(groesse, 1),

           # weight
           gewicht = round(gewicht, 1),

           ## make 10% of cases NA

           gewicht = case_when(runif(nsim) < .15 ~ NA_real_,
                               TRUE ~ gewicht),


           ## make 15% of cases NA

           einkommen = case_when(runif(nsim) < .15 ~ NA_real_,
                              TRUE ~ einkommen),

           kinder = case_when(kinder < 0 ~ runif(1, 0, 3),
                                   TRUE ~ kinder),
           kinder = round(kinder),
           glueck = case_when(glueck > 10 ~ runif(1, 6, 9),
                              glueck < 5 & runif(1, 0, 1) < .35 ~ runif(1, 6, 9),
                                 TRUE ~ glueck),
           glueck = case_when(glueck < 1 ~ runif(1, 1, 10),
                                TRUE ~ glueck),
           glueck = round(glueck),

           fitness = case_when(fitness < 1 | fitness > 10 ~ runif(1, 1, 10),
                               TRUE ~ fitness),
           fitness = round(fitness),



           ### income

           # as a function of happiness, height, fitness, tattoos age
           einkommen = einkommen + -100 * glueck + 2 * groesse + 50 * fitness - 50 * tattoos + 150 * alter + rnorm(nsim, mean = -150 * 40, sd = 200),

           einkommen = round(einkommen / 100, 0) * 100,

           einkommen = case_when(einkommen < 1000 ~ runif(1, 1000, 10000),
                               TRUE ~ einkommen),

           ### alcohol

           alkohol = case_when(alkohol < 0 ~ runif(1, 5, 50),
                                        TRUE ~ alkohol),
           alkohol = round(alkohol),

           ## make 15% of cases 0

           alkohol = case_when(runif(nsim) < .15 ~ 0,
                               TRUE ~ alkohol),


           ## Food as a function of happiness income and alcohol

           essen = 50 * glueck + .1 * einkommen + -10 * alkohol + rnorm(nsim, mean = 0, sd = 200),

           essen = round(essen / 10) * 10,

           ## Tattoos

           tattoos = case_when(tattoos < 0 ~ 0,
                               TRUE ~ tattoos),
           tattoos = round(tattoos),

           ## make 50% of cases 0

           tattoos = case_when(runif(nsim) < .5 ~ 0,
                               TRUE ~ tattoos),


           rhein = case_when(rhein < 0 ~ 0,
                                      TRUE ~ rhein),
           rhein = round(rhein),
           datause = round(datause),
           arztbesuche = case_when(arztbesuche < 0 ~ runif(1, 0, 10),
                                        TRUE ~ arztbesuche),
           arztbesuche = round(arztbesuche),
           wandern = case_when(wandern < 0 ~ 0,
                                          TRUE ~ wandern),
           wandern = round(wandern),

           # confession
           ## make 5% of cases NA

           konfession = case_when(runif(nsim) < .15 ~ NA_character_,
                              TRUE ~ konfession)


           )


  # Change order a bit

  tib <- tib %>% select(id, geschlecht, alter, groesse, gewicht, einkommen, bildung, konfession, everything())


  tib
}

