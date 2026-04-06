-- Deploy events:email/tables/email_attachments to pg

BEGIN;

  create table if not exists email.email_attachments (
    id uuid not null default uuidv7(),
    email_id uuid not null,
    content_type text not null,
    file_name text not null,
    file_contents bytea not null,

    primary key (id),

    foreign key (email_id)
      references email.emails(id)
      on delete cascade
  );

  create index if not exists idx_email_attachments_email_id 
    on email.email_attachments(email_id);

COMMIT;
