# Add the formatted output of table.RiskStats to an Excel workbook
# 
# wb = xlsx.RiskStats.R(R=last(x.R,60), wb=wb)

# @TODO: Wrap each table in a similar function: Calendar, etc.

xlsx.RiskStats <- function(R, p=(1-(1/12)), Rf=.03/12, wb, sheetname="Risk Stats", title="Risk Statistics", subtitle="Since inception") {
  # Peter Carl
  require(xlsx)
  
  # Calculate the table
  x.RiskStats = as.data.frame(t(table.RiskStats(R=R, p=p, Rf=Rf)))
  
  ## Set style attributes for the spreadsheet
  # Create a named cell style to be used for columns of ratios or percentages
  csSheetTitle <- CellStyle(wb) + Font(wb, heightInPoints=14, isBold=TRUE)
  csSheetSubTitle <- CellStyle(wb) + Font(wb, heightInPoints=12, isItalic=TRUE, isBold=FALSE)
  csTableRowNames <- CellStyle(wb) + Font(wb, isBold=TRUE)
  csTableColNames <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK")) 
  csRatioColumn <- CellStyle(wb, dataFormat=DataFormat("0.0")) # ... for ratio results
  csPercColumn <- CellStyle(wb, dataFormat=DataFormat("0.0%")) # ... for percentage results
  
  # Which columns in the table should be formatted which way?
  RiskStats.colRatio = list(
    '3'=csRatioColumn,
    '5'=csRatioColumn,
    '8'=csRatioColumn,
    '15'=csRatioColumn)
  RiskStats.colPerc =list(
    '1'=csPercColumn,
    '2'=csPercColumn,
    '4'=csPercColumn,
    '6'=csPercColumn,
    '7'=csPercColumn,
    '9'=csPercColumn,
    '10'=csPercColumn,
    '13'=csPercColumn,
    '14'=csPercColumn)
  
  # Create a sheet in the workbook, add the table, and format it
#   wb = set.xlsxWBStyles(wb) # Establish formats in the wb in case it hasn't happened before
  sheet <- createSheet(wb, sheetName = sheetname)
  addDataFrame(x.RiskStats, sheet, startRow=3, startColumn=1, 
               colStyle=c(RiskStats.colPerc,RiskStats.colRatio), 
               colnamesStyle = csTableColNames, rownamesStyle=csTableRowNames)
  setColumnWidth(sheet,colIndex=c(2:15),colWidth=11)
  setColumnWidth(sheet,colIndex=16,colWidth=13)
  setColumnWidth(sheet,colIndex=17,colWidth=6)
  setColumnWidth(sheet,colIndex=1,colWidth=max(nchar(rownames(x.RiskStats))))
  
  # Create the Sheet title ...
  rows <- createRow(sheet,rowIndex=1)
  sheetTitle <- createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], csSheetTitle)
  # ... and subtitle
  rows <- createRow(sheet,rowIndex=2)
  sheetSubTitle <- createCell(rows,colIndex=1)
  setCellValue(sheetSubTitle[[1,1]], subtitle)
  setCellStyle(sheetSubTitle[[1,1]], csSheetSubTitle)
  
  # Return the whole (now modified) workbook object
  return(wb)
}

