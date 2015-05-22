# Add the formatted output of table.CAPM to an Excel workbook

xlsx.CAPM <- function(Ra, Rb, p=(1-(1/12)), Rf=.03/12, wb, sheetname="SFM", title="Regression Statistics", subtitle="Since inception") {
  # Peter Carl
  require(xlsx)
  
  # Calculate the table
  # @TODO: Do this from manager inception instead
  # @TODO: Get rid of leading 'X' in column names
  x.capm = t(table.CAPM(R, as.perc=FALSE, digits=4))
  x.dim = dim(x.capm)
  
  ## Set style attributes for the spreadsheet
  # Create a named cell style to be used for columns of ratios or percentages
  csSheetTitle <- CellStyle(wb) + Font(wb, heightInPoints=14, isBold=TRUE)
  csSheetSubTitle <- CellStyle(wb) + Font(wb, heightInPoints=12, isItalic=TRUE, isBold=FALSE)
  csTableRowNames <- CellStyle(wb) + Font(wb, isBold=TRUE)
  csTableColNames <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK")) 
  csRatioColumn <- CellStyle(wb, dataFormat=DataFormat("0.0")) # ... for ratio results
  csPercColumn <- CellStyle(wb, dataFormat=DataFormat("0.0%")) # ... for percentage results
  
  CAPM.colRatio = list(
    '3'=csRatioColumn,
    '5'=csRatioColumn,
    '8'=csRatioColumn,
    '15'=csRatioColumn)
  CAPM.colPerc =list(
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
  sheet <- createSheet(wb, sheetName = sheetname)
  addDataFrame(x.RiskStats, sheet, startRow=3, startColumn=1, 
               colStyle=c(CAPM.colPerc,CAPM.colRatio), 
               colnamesStyle = csTableColNames, rownamesStyle=csTableRowNames)

  setColumnWidth(sheet,colIndex=c(2:x.dim[1]),colWidth=8)
  setColumnWidth(sheet,colIndex=1,colWidth=max(nchar(rownames(x.calendar))))
  
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