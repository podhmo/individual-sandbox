package main

func cross5(vs0 []int, vs1 []int, vs2 []int, vs3 []int, vs4 []int) [][]int {
	var r [][]int
	for _, v0 := range vs0  {
		for _, v1 := range vs1  {
			for _, v2 := range vs2  {
				for _, v3 := range vs3  {
					for _, v4 := range vs4  {
						r = append(r, []int{v0, v1, v2, v3, v4})
					}
				}
			}
		}
	}
	return r
}
