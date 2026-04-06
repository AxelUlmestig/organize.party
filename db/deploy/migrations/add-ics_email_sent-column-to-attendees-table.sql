-- Deploy events:migrations/add-ics_email_sent-column-to-attendees-table to pg

BEGIN;

  alter table attendees add column ics_email_sent bool;
  
  update attendees set
    ics_email_sent = exists(
      select 1
      from attendee_data as ad
      where
        ad.attendee_id = attendees.id
        and ad.status in ('coming', 'maybe_coming')
    );
  
  alter table attendees
    alter column ics_email_sent set not null,
    alter column ics_email_sent set default false;

COMMIT;
