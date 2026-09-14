-- Revert events:migrations/add-list_unsubscribe_url-column-to-emails-table from pg

BEGIN;

  alter table email.emails
    drop column list_unsubscribe_url;

COMMIT;
