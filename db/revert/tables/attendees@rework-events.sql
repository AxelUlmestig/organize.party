-- Revert events:tables/attendees from pg

begin;

drop table if exists attendees;
drop if exists type attendee_status;

commit;
