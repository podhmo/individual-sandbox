package main

func main() {
	type W struct {
		Name      string
		Published bool
	}

	type S struct {
		Name string
	}

    // この辺だめなんだっけ？だいじょうぶ？
	publishedSuspectMap := map[string]map[string]map[string][]string{}
	workspaceMap := map[string]W{
		"w1": W{Name: "a", Published: true},
		"w2": W{Name: "b", Published: false},
		"w3": W{Name: "c", Published: false},
	}
	suspectMap := map[string]map[string]map[string][]string{
		"s1": map[string][]string{"w1": []string{"x","y"},"w2": []string{"z"}},
		"s2": map[string][]string{"w3": []string{"z","y"}},
	}
	for sid, s := range suspectMap {
		for wid := range s {
			if workspaceMap[wid].Published {
				publishedSuspectMap[sid] = s
			}
		}
	}
}
