-- Revert events:tables/comments from pg

BEGIN;

  drop table if exists comments;

COMMIT;
