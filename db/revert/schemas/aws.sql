-- Revert events:schemas/aws from pg

BEGIN;

  drop schema aws;

COMMIT;
