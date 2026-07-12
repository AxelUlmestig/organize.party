-- Revert events:tables/photos from pg

BEGIN;

  drop table if exists photos;

COMMIT;
