-- Revert events:statechart/aws/s3_photo_upload_flow-1.0 from pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN


BEGIN;

with chart as (
    delete from fsm.statechart
    where name = 'aws.s3_photo_upload_flow'
    and version = to_semver('1.0')
    returning id
)
delete from fsm.state
    where statechart_id = (select id from chart);

COMMIT;
