-- Verify events:statechart/aws/sns_webhook_message_flow-1.0 on pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN


BEGIN;

select *
from fsm.statechart
where name = 'aws.sns_webhook_message_flow'
and version = 1.0::semver
limit 1;

ROLLBACK;
