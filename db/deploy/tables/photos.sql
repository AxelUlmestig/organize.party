-- Deploy events:tables/photos to pg

BEGIN;

  create table if not exists photos (
    id uuid not null,
    uploaded_at timestamptz not null default now(),
    photo_url text not null,
    name text not null,
    file_base64_sha256 text not null,

    primary key (id)
  );

  create unique index if not exists idx_photos_file_base64_sha256_name
    on photos (file_base64_sha256, name);

COMMIT;
