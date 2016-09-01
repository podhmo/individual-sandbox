package cmd

import (
	"github.com/spf13/cobra"
)

// RootCmd ...
var RootCmd = &cobra.Command{
	Use:   "appName",
	Short: "short description",
	Long:  `long description`,
}

func init() {
	cobra.OnInitialize()
}
