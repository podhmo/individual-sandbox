package autogen

type Tree struct {
	Left struct {
		Left struct {
			Left struct {
				Total int `json:"total"`
			} `json:"left"`
			Right struct {
				Total int `json:"total"`
			} `json:"right"`
			Total int `json:"total"`
		} `json:"left"`
		Right struct {
			Left struct {
				Total int `json:"total"`
			} `json:"left"`
			Right struct {
				Total int `json:"total"`
			} `json:"right"`
			Total int `json:"total"`
		} `json:"right"`
		Total int `json:"total"`
	} `json:"left"`
	Right struct {
		Left struct {
			Left struct {
				Total int `json:"total"`
			} `json:"left"`
			Right struct {
				Total int `json:"total"`
			} `json:"right"`
			Total int `json:"total"`
		} `json:"left"`
		Right struct {
			Total int `json:"total"`
		} `json:"right"`
		Total int `json:"total"`
	} `json:"right"`
	Total int `json:"total"`
}
