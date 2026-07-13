-- Revert events:migrations/add-event_data-photo_id-column from pg

BEGIN;

  alter table event_data
    drop column photo_id;

COMMIT;
