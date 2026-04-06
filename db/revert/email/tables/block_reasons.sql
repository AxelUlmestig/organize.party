-- Revert events:email/tables/block_reasons from pg

BEGIN;

  drop table email.block_reasons;

COMMIT;
