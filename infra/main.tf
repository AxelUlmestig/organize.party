# organize.party on Fogpipe Cloud. the platform terminates TLS, the managed
# database hands out its own pooled connection url and mounts the extensions,
# so there is no nginx, certbot, pgbouncer or custom postgres image here.

terraform {
  required_providers {
    fpcloud = {
      source  = "fogpipe/fpcloud"
      version = "~> 0.144"
    }
  }
}

provider "fpcloud" {
  # api_url and credentials come from FPCLOUD_API_URL / `fpcloud login`
}

variable "org" {
  type        = string
  description = "The Fogpipe organization's opaque id — the frozen one, not the readable name. It says which org the project lives in AND is what the registry path is built from, because nothing derives from the readable name any more: an image path spelled with the name resolves to no project and the push is refused. Frozen, so changing it is not something that happens to you."
}

variable "host_label" {
  type        = string
  default     = ""
  description = "Pins the label the site is served on, under fogpipe.cloud. Empty takes the one the platform derives. Set it only for a deployment whose host predates a change to that derivation — the app stores its label, so it keeps serving a name the current rule would not produce."
}

variable "host" {
  type        = string
  description = "Custom domain to serve the site on. Empty serves it on var.host_label."
  default     = ""
}

variable "image_tag" {
  type        = string
  description = "Tag of the webapi and worker images in the fpcloud registry."
}

variable "smtp" {
  type = object({
    server   = string
    port     = string
    login    = string
    password = string
  })
  description = <<-EOT
    Outbound mail is deliberately left unconfigured: the defaults let the
    worker start but every send fails at the TLS handshake. The worker
    validates the certificate of any server that isn't localhost, so only a
    real relay can serve it. Note that bounce handling only works with AWS SES,
    the app learns about dead addresses from SES notifications over SNS.
  EOT
  default = {
    server   = "smtp.invalid"
    port     = "587"
    login    = "unconfigured"
    password = "unconfigured"
  }
}

variable "offsite_backup" {
  description = <<-EOT
    Where to mirror database backups outside the platform. Unset, backups exist
    only on the cluster that runs the database.

    provider_type "aws" assumes an IAM role over OIDC and stores no key — set
    role_arn and region. provider_type "s3" signs with a static key against any
    S3 API — set endpoint, access_key_id and secret_access_key, and region where
    the provider wants one. schedule is a cron expression; unset means the mirror
    only runs when asked.
  EOT
  type = object({
    provider_type     = string
    bucket            = string
    region            = optional(string)
    prefix            = optional(string)
    schedule          = optional(string)
    role_arn          = optional(string)
    endpoint          = optional(string)
    access_key_id     = optional(string)
    secret_access_key = optional(string)
  })
  default   = null
  sensitive = true
}

# <app>-<project>-<org>-app is the label the platform derives for an app's
# default host, off the org's frozen id — not its readable name, which nothing
# derives from since it became renameable. A deployment older than that change
# still answers on the name-spelled label, because the app stores its own label
# rather than re-deriving it; var.host_label is how such a deployment says so.
locals {
  host_label   = var.host_label != "" ? var.host_label : "webapi-organizeparty-${var.org}-app"
  default_host = "${local.host_label}.fogpipe.cloud"
  host         = var.host != "" ? var.host : local.default_host
}

resource "fpcloud_project" "organizeparty" {
  org  = var.org
  name = "organizeparty"

  # smtp submission (port 587) is the only traffic that leaves the platform,
  # and the https policy only opens port 443
  egress = "all"
}

# Postgres 18 is required, extension images mount through
# `extension_control_path` which doesn't exist in earlier versions. citext and
# pgcrypto are not listed, the sqitch migrations `create extension` those
# themselves and would fail if they were pre-installed.
resource "fpcloud_database" "events" {
  project_id = fpcloud_project.organizeparty.id
  name       = "events"
  version    = "18"
  extensions = ["semver", "pg_statecharts"]

  cpu     = "500m"
  memory  = "1Gi"
  storage = "10Gi"

  # the database ships every change to the platform's archive continuously, but
  # replaying it needs a base backup to start from. without one there is nothing
  # to restore, however healthy archiving looks
  backup = {
    enabled   = true
    schedule  = "0 3 * * *"
    retention = "30d"
  }
}

# The backups above live on the same cluster as the database, so they cover a bad
# migration but not the loss of the machine. This mirrors them to a bucket you
# own, and is off until you say where.
resource "fpcloud_database_backup_destination" "offsite" {
  count = var.offsite_backup == null ? 0 : 1

  database_id   = fpcloud_database.events.id
  provider_type = var.offsite_backup.provider_type
  bucket        = var.offsite_backup.bucket
  region        = var.offsite_backup.region
  prefix        = var.offsite_backup.prefix
  schedule      = var.offsite_backup.schedule

  # aws assumes a role over OIDC and holds no key; s3 signs with one
  role_arn          = var.offsite_backup.role_arn
  endpoint          = var.offsite_backup.endpoint
  access_key_id     = var.offsite_backup.access_key_id
  secret_access_key = var.offsite_backup.secret_access_key
}

# Photo uploads. The browser PUTs directly to the S3 API with a presigned url,
# the objects are served back through the bucket website since the S3 API
# refuses anonymous reads.
resource "fpcloud_bucket" "photos" {
  project         = fpcloud_project.organizeparty.id
  name            = "photos"
  website_enabled = true
}

# the page is on local.host and the upload PUT goes to the object store, without
# this the browser refuses to send it. Presigning doesn't cover the preflight
resource "fpcloud_bucket_cors" "photos" {
  bucket_id = fpcloud_bucket.photos.id

  # both, not just local.host: a custom domain does not stop the app answering
  # on its default one, and an upload from there would fail the preflight alone
  rule = [{
    allowed_origins = distinct(["https://${local.host}", "https://${local.default_host}"])
    allowed_methods = ["GET", "PUT", "HEAD"]
    allowed_headers = ["*"]
    expose_headers  = ["ETag"]
    max_age_seconds = 3600
  }]
}

# the Elm frontend is baked into the image and served by the same binary, one
# origin just like with nginx
resource "fpcloud_app" "webapi" {
  project_id = fpcloud_project.organizeparty.id
  name       = "webapi"
  image      = "registry.cloud.fogpipe.com/${var.org}/organizeparty/webapi:${var.image_tag}"
  port       = 8081
  ingress    = "all"
  url_slug   = var.host_label != "" ? var.host_label : null

  env = {
    HOST_URL  = "https://${local.host}"
    LOG_LEVEL = "LevelInfo"

    # where the bucket's objects are publicly readable, the rest of the S3
    # config is injected by the bucket bindings below
    S3_PUBLIC_BASE = fpcloud_bucket.photos.website_url
  }

  release_command = ["sqitch --chdir /db deploy \"db:pg://$${DATABASE_URL#postgres://}\" --mode change --verify"]

  depends_on = [fpcloud_database.events]
}

# `type = worker`: no port, no ingress, no URL, it only holds a Postgres
# LISTEN connection open
resource "fpcloud_app" "worker" {
  project_id = fpcloud_project.organizeparty.id
  name       = "worker"
  image      = "registry.cloud.fogpipe.com/${var.org}/organizeparty/worker:${var.image_tag}"
  type       = "worker"

  env = {
    LOG_LEVEL      = "LevelInfo"
    SMTP_SERVER    = var.smtp.server
    SMTP_PORT      = var.smtp.port
    S3_PUBLIC_BASE = fpcloud_bucket.photos.website_url
  }

  secret = {
    SMTP_LOGIN    = var.smtp.login
    SMTP_PASSWORD = var.smtp.password
  }

  depends_on = [fpcloud_database.events]
}

# injects S3_ENDPOINT, S3_BUCKET and the AWS_* credentials
resource "fpcloud_app_bucket" "webapi_photos" {
  app_id    = fpcloud_app.webapi.id
  bucket_id = fpcloud_bucket.photos.id
}

# read only, the worker only checks whether uploads arrived
resource "fpcloud_app_bucket" "worker_photos" {
  app_id    = fpcloud_app.worker.id
  bucket_id = fpcloud_bucket.photos.id
  read_only = true
}

# on_demand: the host is a label in our own wildcard zone, DNS already points
# at the cluster and there is no external owner to verify against
resource "fpcloud_domain" "organizeparty" {
  count = var.host != "" ? 1 : 0

  app_id = fpcloud_app.webapi.id
  domain = var.host
  mode   = "on_demand"
}

output "url" {
  value = "https://${local.host}"
}

output "photos_url" {
  value = fpcloud_bucket.photos.website_url
}
