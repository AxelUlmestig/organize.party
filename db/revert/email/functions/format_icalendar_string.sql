-- Revert events:email/functions/format_icalendar_string from pg

BEGIN;

  drop function email.create_icalendar_string;

COMMIT;
