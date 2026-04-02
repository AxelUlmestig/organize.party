-- Revert events:statechart_extension from pg

BEGIN;

  drop extension pg_statecharts;

COMMIT;
