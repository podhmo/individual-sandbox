func isZero(v reflect.Value) bool {
	return v.Interface() == reflect.Zero(v.Type()).Interface()
}

type Walker struct {
	M map[[]string]bool
}

func (w *Walker)Walk(ob interface{}) {
	rv := reflect.ValueOf(ob)
    if rv.Can
	for i := 0; i < rv.NumField(); i++ {
		f := rv.Field(i)
		if f.CanAddr() {
            f := f.Elem()
		} else {
			fmt.Println(f.Kind(), f.String(), f.Interface(), isZero(f))
		}

	}
}

	noZeroValue(PERSON)
	fmt.Println("{}")
	noZeroValue(personT{})
