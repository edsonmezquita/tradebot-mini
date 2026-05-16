variable "linode_token" {
  description = "Linode API token."
  type        = string
  sensitive   = true
}

variable "region" {
  description = "Linode region. Must match VPC/PG region."
  type        = string
  default     = "us-ord"
}

variable "vpc_subnet_id" {
  description = "Subnet in the shared VPC — prod Nanode attaches so it can reach managed Postgres privately."
  type        = number
}

variable "ssh_key_label" {
  description = "Label of the SSH key registered on Linode for admin access."
  type        = string
  default     = "dereckscompany-mba"
}

variable "linode_username" {
  description = "Linode username used for authorized_users on the prod Nanode."
  type        = string
  default     = "dereckscompany"
}

variable "prod_nanode_label" {
  description = "Label for this app's prod Nanode."
  type        = string
  default     = "tradebot-mini-prod"
}

variable "prod_nanode_type" {
  # 2 GB / 1 vCPU. Chromium under crawl4ai is the main RAM consumer —
  # bump to g6-standard-2 (4 GB) if we start OOM-ing.
  description = "Linode plan for the prod Nanode."
  type        = string
  default     = "g6-standard-1"
}

variable "prod_nanode_image" {
  description = "Base image for the prod Nanode."
  type        = string
  default     = "linode/debian12"
}

variable "tags" {
  description = "Tags applied to resources created by this config."
  type        = list(string)
  default     = ["linode-saas", "tradebot-mini", "prod"]
}
