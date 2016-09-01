package cmd

import (
	"fmt"
	"github.com/spf13/cobra"
)

var subCmd = &cobra.Command{
	Use:   "sub",
	Short: "sub command",
	Long:  `sub command`,
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("sub")
	},
}

func init() {
	RootCmd.AddCommand(subCmd)
}
