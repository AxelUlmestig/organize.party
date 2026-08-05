-- Verify events:statechart/aws/s3_photo_upload_flow-1.0 on pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;

-- Verify that the statechart is added
select 1 / count(*)
from fsm.statechart
where
    name = 'aws.s3_photo_upload_flow'
    and version = to_semver('1.0');

-- Verify that the functions that the statechart depends on exist
do $$
declare
    missing_funcs_count_ int;
    missing_funcs_ text;
begin

select
  string_agg(distinct format('%s.%s', schema_name, function_name), ', '),
  count(*)
into
  missing_funcs_,
  missing_funcs_count_
from fsm.statechart
join fsm.state
    on state.statechart_id = statechart.id
, lateral unnest(on_entry || on_exit)
where
  statechart.name = 'aws.s3_photo_upload_flow'
  and statechart.version = to_semver('1.0')
  and not exists (
    select 1
    from pg_proc p
    join pg_namespace n
      on p.pronamespace = n.oid
    where
      n.nspname = schema_name
      and p.proname = function_name
      and p.pronargs = 1
      and p.proargtypes[0] = 'fsm_event_payload'::regtype::oid
  );

if missing_funcs_count_ > 0 then
  raise exception
    $err$

    One or more missing or invalid functions: %
    All functions must take exactly one argument of the type fsm_event_payload

    $err$, missing_funcs_;
end if;

end
$$;

ROLLBACK;
