default:
	jqfpy --squash -c 'L = get("students"); [{"name": s["name"], "age": s["age"], "gpa": s["cum_gpa"], "CSC101": get("grades/CSC101", x)} for s in L for x in s["semesters"] if get("grades/CSC101", x) == "A"]' data.json
