package main

import "github.com/tealeg/xlsx"

func main() {
	err := WriteFile("workbook.xlsx")
	if err != nil {
		panic(err)
	}
}

// WriteFile :
func WriteFile(name string) error {
	// todo: set font
	file := xlsx.NewFile()
	sheet0, err := file.AddSheet("sheet0")
	if err != nil {
		return err
	}
	// todo: arguments
	if err := WriteXS(sheet0); err != nil {
		return err
	}

	return file.Save(name)
}

// WriteXS :
func WriteXS(sheet *xlsx.Sheet) error {
	row0 := sheet.AddRow()
	cell00 := row0.AddCell()
	cell00.SetString("1")
	cell01 := row0.AddCell()
	cell01.SetString("2")
	cell02 := row0.AddCell()
	cell02.SetString("3")
	cell03 := row0.AddCell()
	cell03.SetString("4")
	cell04 := row0.AddCell()
	cell04.SetString("5")

	row1 := sheet.AddRow()
	cell10 := row1.AddCell()
	cell10.SetFloat(1 * 1)
	cell11 := row1.AddCell()
	cell11.SetFloat(2 * 2)
	cell12 := row1.AddCell()
	cell12.SetFloat(3 * 3)
	cell13 := row1.AddCell()
	cell13.SetFloat(4 * 4)
	cell14 := row1.AddCell()
	cell14.SetFloat(5 * 5)

	return nil
}
