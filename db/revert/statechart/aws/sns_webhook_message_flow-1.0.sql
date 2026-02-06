-- Revert events:statechart/aws/sns_webhook_message_flow-1.0 from pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN


BEGIN;

with chart as (
    delete from fsm.statechart
    where name = 'aws.sns_webhook_message_flow'
    and version = 1.0::semver
    returning id
)
delete from fsm.state
    where statechart_id = (select id from chart);

COMMIT;
