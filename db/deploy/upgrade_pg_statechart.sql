-- Deploy events:upgrade_pg_statechart to pg

do $migrate$
begin
  if exists (select 1 from pg_namespace where nspname = 'fsm')
     and not exists (select 1 from pg_extension where extname = 'pg_statecharts') then
    -- deployed from deploy/ with sqitch: adopt the existing objects
    create extension pg_statecharts version 'sqitch';
  else
    -- fresh database, or the extension is already installed
    create extension if not exists pg_statecharts cascade;
  end if;
  alter extension pg_statecharts update;
end
$migrate$;
