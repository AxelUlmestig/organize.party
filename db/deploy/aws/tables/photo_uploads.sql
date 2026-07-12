-- Deploy events:aws/tables/photo_uploads to pg

BEGIN;

  create table if not exists aws.photo_uploads (
    id uuid not null default uuidv7(),
    photo_id uuid, -- TODO: add fk
    state_machine_id bigint not null,
    file_name text not null,
    upload_url text not null,
    materialized_status text not null default 'queued',

    primary key (id),

    foreign key (photo_id)
      references photos(id)
  );

  create unique index if not exists idx_aws_photo_uploads_state_machine_id
    on aws.photo_uploads (state_machine_id);

  create unique index if not exists idx_aws_photo_uploads_photo_id
    on aws.photo_uploads (photo_id);

COMMIT;
