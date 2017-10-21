package main

import (
	"fmt"
	"log"
	"os"

	"github.com/xuri/excelize"
)

func main() {
	xlsx, err := excelize.OpenFile("./test.xlsx")
	if err != nil {
		fmt.Println(err)
		return
	}
	// Get all the rows in the Sheet1.
	f, err := os.Create("04.csv")
	if err != nil {
		log.Fatal(err)
	}

	rows := xlsx.GetRows("Sheet1")
	for _, row := range rows {
		for _, colCell := range row {
			fmt.Fprint(f, colCell, "\t")
		}
		fmt.Fprintln(f)
	}
	defer f.Close()
}
