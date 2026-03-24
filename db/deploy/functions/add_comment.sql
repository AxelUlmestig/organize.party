-- Deploy events:functions/add_comment to pg

BEGIN;

  create or replace function add_comment(
    host_url_ text,
    event_id_ uuid,
    email_ text,
    name_ text,
    comment_ text,
    force_notification_on_comment_ bool
  )
  returns void
  as
  $$
    declare
    begin
      email_ := trim(lower(email_));

      -- save comment in comments table
      insert into comments (
        comment,
        force_notification_on_comment,
        attendee_id,
        event_id
      )
      select
        comment_,
        force_notification_on_comment_,
        id,
        event_id
      from
        add_attendee_data(
          host_url_ => host_url_,
          event_id_ => event_id_,
          email_ => email_,
          name_ => name_
        );

      -- notify other attendees by email
      with
        attendees_to_notify as (
          select
            *,
            case
              when get_notified_on_comments then
                'You can unsubscribe from these messages by unclicking the <i>get notified on comments?</i> checkbox and resubmitting your RSVP'
              else
                '<b>' || name_ || '</b> chose to notify you of their comment by clicking the <i>send email notification to everyone?</i> checkbox'
            end as unsubscribe_info
          from latest_attendee_data
          where
            event_id = event_id_
            and email <> email_
            and unsubscribed_at is null
            and status in ('coming', 'maybe_coming')
            and (
              get_notified_on_comments
              or force_notification_on_comment_
            )
        )

        insert into email.emails (
          recipient_email,
          recipient_name,
          subject,
          body
        )
        select
          attendees_to_notify.email,
          attendees_to_notify.name,
          name_ || ' has left a comment on ' || event_data.title as subject,
'<b>' || name_ || '</b> has left a comment on <a href="' || host_url_ || '/e/' || event_data.id || '">' || event_data.title || '</a>
<br>
<br>
<i>
  <pre>' || comment_ || '</pre>
</i>
<br>
<br>
' || attendees_to_notify.unsubscribe_info || '
<br>
<br>
<div style="font-size: x-small">
  If you never want to receive an email from this event again, <a href="' || host_url_ || '/unsubscribe/' || attendees_to_notify.unsubscribe_id || '">click here to unsubscribe</a>. Warning, this can not be undone
</div>' as body
        from attendees_to_notify
        join event_data
          on event_data.id = attendees_to_notify.event_id
          and event_data.superseded_at is null;

    end;
  $$ language plpgsql;

COMMIT;
