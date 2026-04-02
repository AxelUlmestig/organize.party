-- Revert events:migrations/add-ics_email_sent-column-to-attendees-table from pg

BEGIN;

  alter table attendees
    drop column ics_email_sent;

COMMIT;
