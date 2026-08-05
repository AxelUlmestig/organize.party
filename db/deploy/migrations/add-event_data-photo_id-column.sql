-- Deploy events:migrations/add-event_data-photo_id-column to pg

BEGIN;

  alter table event_data
    add column photo_id uuid,
    add constraint fk_event_data_photos foreign key (photo_id) references photos (id);

COMMIT;
