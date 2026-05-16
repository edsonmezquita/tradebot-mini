terraform {
  backend "s3" {
    # key passed via `terraform init -backend-config="key=apps/tradebot-mini/prod.tfstate"`.
    # Linode E3 Object Storage — virtual-hosted-style addressing, no checksums.
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    skip_region_validation      = true
    skip_requesting_account_id  = true
    skip_s3_checksum            = true
    use_path_style              = false
  }
}
