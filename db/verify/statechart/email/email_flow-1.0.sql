-- Verify events:statechart/email/email_flow-1.0 on pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN


BEGIN;

select *
from fsm.statechart
where name = 'email.email_flow'
and version = 1.0::semver
limit 1;

ROLLBACK;
