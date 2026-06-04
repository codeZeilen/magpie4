#' @title NitrogenBudgetNonagland
#' @description calculates projections of Nitrogen Budgets for non-agricutlural land from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level, reg, glo or regglo
#' @author Benjamin Leon Bodirsky, Edna J. Molina Bacca
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom mstools toolHoldConstant
#' @examples
#'
#'   \dontrun{
#'     x <- NitrogenBudgetNonagland(gdx)
#'   }
#'

NitrogenBudgetNonagland <- function(gdx, level = "reg") {
  nonagland <- c("forestry", "primforest", "secdforest", "urban", "other")

  if (level != "grid") {
    message("for now the nonag nitrogen budgets have to be calculated from grid-level data. this can be computationaly expensive and requires cell input data.")
  }
  landarea <- land(gdx, level = "grid", types = nonagland)

  ### deposition
  dep_rate <- read.magpie(file.path(dirname(normalizePath(gdx)), "f50_AtmosphericDepositionRates_0.5.mz"))[, , nonagland]
  getCells(dep_rate) <- getCells(landarea)
  dep <- landarea * collapseNames(dep_rate[, getYears(landarea), ])

  ### fixation
  # > fix_rate=setYears(calcOutput("NitrogenFixationRateNatural",cellular=TRUE,aggregate=FALSE)[,"y2010",],NULL)
  fix_rate <- read.magpie(file.path(dirname(normalizePath(gdx)), "f50_NitrogenFixationRateNatural_0.5.mz"))
  message("fixation rate has only data until 2010. Held constant for the future for now.")
  fix_rate <- toolHoldConstant(fix_rate, years = findset("time"))
  getCells(fix_rate) <- getCells(landarea)
  fix <- landarea * fix_rate[, getYears(landarea), ]

  # > dep   <- collapseNames(readGDX(gdx,"ov50_nr_deposition")[,,nonagland][,,"level"])
  # > dep   <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,types=nonagland)
  # > fix_rate=readGDX(gdx,"f50_NitrogenFixationRateNatural")[,getYears(harvest),]
  # > fix_rate=readGDX(gdx,"")[,,nonagland]
  # > fix   <- land(gdx,level="reg")[,,c("past","crop"),invert=TRUE] * fix_rate
  # > fix   <- gdxAggregate(gdx = gdx,weight = 'land',x = fix,to = level,absolute = TRUE,types=nonagland)

  out <- mbind(
    addDim(fix, dim = 3.1, dimName = "budget", item = "fixation_freeliving"),
    addDim(dep, dim = 3.1, dimName = "budget", item = "deposition")
  )

  out <- mbind(
    out,
    addDim(dimSums(out, dim = 3.1), dim = 3.1, dimName = "budge", item = "surplus")
  )

  out <- gdxAggregate(gdx, out, to = level, absolute = TRUE)

  return(out)
}
