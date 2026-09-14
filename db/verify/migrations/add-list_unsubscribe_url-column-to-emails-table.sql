-- Verify events:migrations/add-list_unsubscribe_url-column-to-emails-table on pg

BEGIN;

  select list_unsubscribe_url from email.emails where false;

ROLLBACK;
