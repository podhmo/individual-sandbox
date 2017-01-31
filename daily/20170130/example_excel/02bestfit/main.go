package main

import (
	"strings"

	"github.com/tealeg/xlsx"
)

func main() {
	var file *xlsx.File
	var sheet *xlsx.Sheet
	var row *xlsx.Row
	var cell *xlsx.Cell
	var err error

	file = xlsx.NewFile()
	sheet, err = file.AddSheet("Sheet1")
	if err != nil {
		panic(err)
	}

	row = sheet.AddRow()
	for i := 0; i <= 9; i++ {
		cell = row.AddCell()
		cell.Value = strings.Repeat("ab", i)
	}
	if err := sheet.SetStretchWidthByBestFit(0, 9); err != nil {
		panic(err)
	}
	err = file.Save("bestfit.xlsx")
	if err != nil {
		panic(err)
	}
}
