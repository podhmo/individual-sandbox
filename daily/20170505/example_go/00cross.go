func cross5(vs0 []string, vs1 []string, vs2 []string, vs3 []string, vs4 []string) [][]string {
	var r [][]string
	for _, v0 := range vs0  {
		for _, v1 := range vs1  {
			for _, v2 := range vs2  {
				for _, v3 := range vs3  {
					for _, v4 := range vs4  {
						r = append(r, []string{v0, v1, v2, v3, v4})
					}
				}
			}
		}
	}
	return r
}
