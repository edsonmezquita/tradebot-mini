provider "linode" {
  token = var.linode_token
}

data "linode_sshkeys" "shared" {
  filter {
    name   = "label"
    values = [var.ssh_key_label]
  }
}

# Dedicated prod Nanode for the tradebot. Attached to the shared VPC so it
# can reach managed Postgres over the private network.
resource "linode_instance" "prod_nanode" {
  label            = var.prod_nanode_label
  region           = var.region
  type             = var.prod_nanode_type
  image            = var.prod_nanode_image
  authorized_keys  = [for k in data.linode_sshkeys.shared.sshkeys : k.ssh_key]
  authorized_users = [var.linode_username]
  tags             = var.tags

  metadata {
    user_data = base64encode(file("${path.module}/cloud-init.yml"))
  }

  interface {
    purpose = "public"
    primary = true
  }

  interface {
    purpose   = "vpc"
    subnet_id = var.vpc_subnet_id
  }
}
