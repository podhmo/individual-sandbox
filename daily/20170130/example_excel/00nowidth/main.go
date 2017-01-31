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
	{
		cell = row.AddCell()
		cell.Value = "I am a cell!"
	}
	{
		cell = row.AddCell()
		cell.Value = "I am a cell!"
	}
	{
		cell = row.AddCell()
		cell.Value = "I am a cell!"
	}

	err = file.Save("nowidth.xlsx")
	if err != nil {
		panic(err)
	}
}
