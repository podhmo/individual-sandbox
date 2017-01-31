package main

import "github.com/tealeg/xlsx"

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
		cell.Value = "I am a cell!"
	}
	for i := 0; i <= 9; i++ {
		if err := sheet.SetColWidth(i, i, float64(i)); err != nil {
			panic(err)
		}
	}
	err = file.Save("width.xlsx")
	if err != nil {
		panic(err)
	}
}
