## go additionalProperties

- true
- false


## go validation

- type毎のvalidation
- fieldのvalidation
- slice,...のvalidation

自分自身へのvalidationってstructにいる？

- registerAttribute
- registerType (struct, type)
		switch action.Type {
		case TargetTypeString:
			fn := action.CreateStringValidation(value)
			validations = append(validations, fn)
		default:
			panic(fmt.Sprintf("unexpected attribute: %s", attribute))
		}

