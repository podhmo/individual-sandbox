provider "openstack" {
  user_name   = "admin"
  tenant_name = "admin"
  password    = "pwd"
  auth_url    = "http://myauthurl:5000/v2.0"
  region      = "RegionOne"
}

resource "openstack_compute_instance_v2" "instance_1" {
  name = "instance_1"
  network {
    uuid = "75db0490-ae35-4a9a-87cd-bfcfe4a7a445"
  }
}
