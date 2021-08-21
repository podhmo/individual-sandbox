resource "google_bigquery_dataset" "default" {
  dataset_id                  = "foo"
  friendly_name               = "test"
  description                 = "This is a test description"
  location                    = "asia-northeast1"
  default_table_expiration_ms = 3600000

  labels = {
    env = "default"
  }
}

resource "google_bigquery_table" "default" {
  dataset_id = google_bigquery_dataset.default.dataset_id
  table_id   = "bar"

#   time_partitioning {
#     type = "DAY"
#   }

  labels = {
    env = "default"
  }

  schema = jsonencode(
[
  {
    "name":"id",
    "type": "STRING",
    "mode": "NULLABLE",
    "description": "object id"
  },
  {
    "name": "name",
    "type": "STRING",
    "mode": "NULLABLE"
  }
]
  )


}