package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/kjk/notionapi"
	"github.com/podhmo/tenuki/capture"
)

// https://presstige.io/p/Using-Notion-API-Go-client-2567fcfa8f7a4ed4bdf6f6ec9298d34a

func main() {
	ct := &capture.CapturedTransport{
		Transport: http.DefaultTransport,
		Dumper:    &capture.FileDumper{BaseDir: capture.Dir("/tmp/zzz")},
	}
	client := &notionapi.Client{
		HTTPClient: &http.Client{Transport: ct},
	}
	pageID := "c969c9455d7c4dd79c7f860f3ace6429"
	page, err := client.DownloadPage(pageID)
	if err != nil {
		log.Fatalf("DownloadPage() failed with %s\n", err)
	}

	// look at page.Page to see structured content
	fmt.Printf("%[1]T	%+#[1]v\n", page, page)
}
