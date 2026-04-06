-- Revert events:tables/forgetme from pg

BEGIN;

  drop table forgetme_requests;

COMMIT;
