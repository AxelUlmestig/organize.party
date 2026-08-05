-- Revert events:aws/tables/photo_uploads from pg

BEGIN;

  drop table aws.photo_uploads;

COMMIT;
