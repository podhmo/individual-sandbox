package main

import (
	"fmt"
	"log"

	"m/structgen"

	"github.com/kjk/notionapi"
)

// https://presstige.io/p/Using-Notion-API-Go-client-2567fcfa8f7a4ed4bdf6f6ec9298d34a

func main() {
	client := &notionapi.Client{}
	pageID := "c969c9455d7c4dd79c7f860f3ace6429"
	page, err := client.DownloadPage(pageID)
	if err != nil {
		log.Fatalf("DownloadPage() failed with %s\n", err)
	}

	// look at page.Page to see structured content
	c := structgen.Config{}
	if err := c.Gen(page); err != nil {
		log.Fatalf("!! %+v", err)
	}
	fmt.Println("OK")
}
