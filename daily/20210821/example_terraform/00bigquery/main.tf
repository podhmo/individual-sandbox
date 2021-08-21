// need: $ gcloud auth application-default login

provider "google" {
  project     = var.google_project_id
  region      = "asia-northeast"
}
