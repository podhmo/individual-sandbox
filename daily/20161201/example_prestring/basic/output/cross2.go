package main

func cross2(vs0 []string, vs1 []string) [][]string {
	var r [][]string
	for _,  v0 := range vs0  {
		for _,  v1 := range vs1  {
			r = append(r, []string{v0, v1})
		}
	}
	return r
}
