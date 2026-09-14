-- Deploy events:migrations/add-list_unsubscribe_url-column-to-emails-table to pg

BEGIN;

  alter table email.emails
    add column if not exists list_unsubscribe_url text;

COMMIT;
