-- Deploy events:tables/photos to pg

BEGIN;

  create table if not exists photos (
    id uuid not null,
    uploaded_at timestamptz not null default now(),
    photo_url text not null,
    name text not null,
    -- TODO: hash value to avoid storing duplicates

    primary key (id)
  );

COMMIT;
