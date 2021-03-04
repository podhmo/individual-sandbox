// https://www.terraform.io/docs/language/functions/templatefile.html

locals {
 port = 8080
 ip_addrs = ["10.0.0.1", "10.0.0.2"] 
}

output "result" {
  value = {
    backends = templatefile("${path.module}/backends.tpl", { port = local.port, ip_addrs = local.ip_addrs})

    backend_config_json = jsonencode({
      // commentも書ける
      "backends": [for addr in local.ip_addrs : "${addr}:${local.port}"],
    })
  }
}
