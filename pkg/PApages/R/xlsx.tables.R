xlsx.Performance <- function (R, title, outputdir, timestamp) {
  # Create a spreadsheet with performance and risk metrics
  try(rm(wb)) # delete old or outdated objects in the workspace
  
  # @TODO: code after this could be functionalized as xlsx.Performance()
  
  # Create an Excel workbook using xlsx
  wb <- createWorkbook() # Create a workbook object
  
  # Get attributes of the data used
  x.lastDate = tail(index(x.R), n=1)
  
  ## Calculate Ex Post Risk and Returns Statistics table
  # Create a sheet for Since Inception
  sheetname = paste(manager," Stats", sep="")
  title = paste(manager, "Ex-Post Returns and Risk")
  subtitle = paste("Since inception, updated through ", x.lastDate, sep="")
  wb = xlsx.RiskStats(R=x.R, wb=wb, sheetname=sheetname, title=title, subtitle=subtitle)
  
  # Trailing n-month view of the same table
  periods = c(60,36,24,12)
  for(period in periods){
    sheetname=paste(manager," Stats ", period,"m", sep="")
    title = paste(manager, "Ex-Post Returns and Risk")
    subtitle = paste("Trailing ", period, "-month period through ", x.lastDate, sep="")
    wb = xlsx.RiskStats(R=last(x.R, period), wb=wb, sheetname=sheetname, title=title, subtitle=subtitle)
  }
  
  ## Calculate Calendar Returns table
  sheetname = paste(manager," Returns", sep="")
  title = paste(manager, "Calendar Returns")
  subtitle = paste("Since inception, updated through ", x.lastDate, sep="")
  wb = xlsx.Calendar(R=x.R, wb=wb, sheetname=sheetname, title=title, subtitle=subtitle)
  
  ## Drawdowns table
  
  ## SFM table
  # Create a sheet for Since Inception
  sheetname = paste(manager," SFM", sep="")
  title = paste(manager, "SFM Regression")
  subtitle = paste("Since inception, updated through ", x.lastDate, sep="")
  wb = xlsx.CAPM.R(R=x.R, wb=wb, sheetname=sheetname, title=title, subtitle=subtitle)
  
  # Trailing n-month view of the same table
  periods = c(60,36,12)
  for(period in periods){
    sheetname=paste(manager," SFM ", period,"m", sep="")
    title = paste(manager, "SFM Regression")
    subtitle = paste("Trailing ", period, "-month period through ", x.lastDate, sep="")
    wb = xlsx.CAPM(R=last(x.R, period), wb=wb, sheetname=sheetname, title=title, subtitle=subtitle)
  }
  
  ## AC table
  
  ## 
  saveWorkbook(wb, file=paste(outputdir,"/",manager," Performance ",timestamp,".xlsx", sep=""))
}