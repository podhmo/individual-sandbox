package main

type Data struct {
	Data []Datum
}

type Datum struct {
	Analysis Analysis
	CandidateIndex int
	Components Component
	DeliveryLine1 string
	DeliveryPointBarcode int
	InputIndex int
	LastLine string
	Metadata Metadatum
}

type Analysis struct {
	Active string
	DpvCmra string
	DpvFootnotes string
	DpvMatchCode string
	DpvVacant string
}

type Component struct {
	CityName string
	DeliveryPoint int
	DeliveryPointCheckDigit int
	Plus4Code int
	PrimaryNumber int
	StateAbbreviation string
	StreetName string
	StreetPredirection string
	StreetSuffix string
	Zipcode int
}

type Metadatum struct {
	CarrierRoute string
	CongressionalDistrict int
	CountyFips int
	CountyName string
	Dst bool
	ElotSequence int
	ElotSort string
	Latitude float
	Longitude float
	Precision string
	Rdi string
	RecordType string
	TimeZone string
	UtcOffset int
	ZipType string
}
