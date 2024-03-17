-- Deploy events:tables/commenters to pg

BEGIN;

  drop table if exists commenters;

COMMIT;
