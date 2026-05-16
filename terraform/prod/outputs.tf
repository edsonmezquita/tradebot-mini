output "prod_nanode_id" {
  description = "Linode id of this app's prod Nanode."
  value       = linode_instance.prod_nanode.id
}

output "prod_nanode_ip" {
  description = "Public IPv4 — deploy workflow SSHs to this IP."
  value       = [for ip in linode_instance.prod_nanode.ipv4 : ip if !startswith(ip, "192.168.")][0]
}

output "prod_nanode_label" {
  value = linode_instance.prod_nanode.label
}
