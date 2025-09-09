import { test, expect } from '@playwright/test'
import * as crypto from 'crypto'

const mailhogUrl = 'http://localhost:8025'
const newEventUrl = 'http://localhost:8081'

test('can create event', async ({ page, request }) => {
  const eventName = `Test event ${Math.floor(100 * Math.random()) + 1}`
  const firstEventDescription = 'Much party, such fun'
  const secondEventDescription = 'More party, such fun'
  const firstEventLocation = 'Super cool location'
  const secondEventLocation = 'Even cooler location'
  const eventPassword = 'correct password'
  const organizerEmail = `${crypto.randomUUID()}@organize.party`
  const organizerName = 'Orgo McNizer'
  const attendeeEmail = `${crypto.randomUUID()}@organize.party`
  const attendeeName = 'Atti McEndee'
  const attendeeSecondName = 'Alias McEndee'
  const comment = 'This seems like a nice party'
  const secondComment = 'I will definitely attend'
  const deletedCommentText = 'Comment deleted by user'

  await page.goto(newEventUrl)

  // create new event
  // TODO: figure out how to set time
  await page.getByTestId('event-editor-event-name').fill(eventName)
  await page.getByTestId('expanding-text-area').fill(firstEventDescription)
  await page.getByTestId('event-editor-event-location').fill(firstEventLocation)
  await page.getByTestId('event-editor-event-password').fill(eventPassword)
  await page.getByRole('button', { name: /submit/i }).click()

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click()

  // verify that the new event have the assigned properties
  await expect(page.getByTestId('view-event-title')).toHaveText(eventName)
  await expect(page.getByTestId('view-event-description')).toHaveText(firstEventDescription)
  await expect(page.getByTestId('view-event-location')).toHaveText(firstEventLocation)
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 0")

  // attend the event with the attendee
  await page.getByTestId('view-event-attendee-name').fill(attendeeName)
  await page.getByTestId('view-event-attendee-email').fill(attendeeEmail)
  await page.getByRole('button', { name: /submit/i }).click()
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 1")
  await expect(page.getByText(attendeeName)).toBeVisible()

  await getEmailContents(request, attendeeEmail, /\/e\/[0-9\-a-f]+/)

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click()

  // attend the event with the organizer
  await page.getByTestId('view-event-attendee-name').fill(organizerName)
  await page.getByTestId('view-event-attendee-email').fill(organizerEmail)
  await page.getByRole('button', { name: /submit/i }).click()
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 2")
  await expect(page.getByText(organizerName)).toBeVisible()

  const eventUrl = await getEmailContents(request, organizerEmail, /http(s?):\/\/[^ \/]+\/e\/[0-9\-a-f]+/)

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click()

  // update attendee status to 'Maybe Coming'
  await page.getByTestId('view-event-attendee-plus-one').click({force: true})
  await page.getByTestId('view-event-attendee-status').selectOption({ label: 'Maybe Coming' })
  await page.getByTestId('view-event-submit-attendee').click()

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click()

  // Verify that the attendee list is updated
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 1")
  // Clicking checkbox is super flaky...
  // await expect(page.getByTestId('view-attendees-maybe-attending-number')).toHaveText("Maybe Attending: 1 (+1)")
  await expect(page.getByTestId('view-attendees-maybe-attending-number')).toHaveText(/Maybe Attending: 1.*/i)

  // add a comment as organizer
  await page.getByPlaceholder('Leave a comment').fill(comment)
  await page.getByRole('button', { name: /comment/i }).click()

  // verify comment
  await expect(page.locator('.comment-bubble').getByText(comment)).toHaveText(comment)

  // add a comment as attendee
  await page.getByTestId('view-event-attendee-name').fill(attendeeSecondName)
  await page.getByTestId('view-event-attendee-email').fill(attendeeEmail)
  await page.getByPlaceholder('Leave a comment').fill(secondComment)
  await page.getByRole('button', { name: /comment/i }).click()

  // verify second comment
  await expect(page.locator('.comment-bubble').getByText(secondComment)).toHaveText(secondComment);

  // verify that attendee updated their name
  await expect(page.getByText(attendeeName)).toHaveCount(0)
  await expect(page.getByText(attendeeSecondName)).toHaveCount(2)

  // edit event
  await page.getByTestId('edit-event').click()

  // cancel first to see that you can get back, then go back to edit
  await page.getByRole('button', { name: /cancel/i }).click()
  await page.getByTestId('edit-event').click()

  await page.getByTestId('event-editor-event-location').fill(secondEventLocation)
  await page.getByTestId('expanding-text-area').fill(secondEventDescription)
  await page.getByRole('button', { name: /submit/i }).click()

  // submit wrong password first
  await expect(page.getByText('Error: incorrect password')).toHaveCount(1)
  await page.getByRole('button', { name: /ok/i }).click()

  // try with correct password
  await page.getByTestId('event-editor-event-password').fill(eventPassword)
  await page.getByRole('button', { name: /submit/i }).click()

  await expect(page.getByTestId('view-event-location')).toHaveText(secondEventLocation)
  await getEmailContents(request, organizerEmail, new RegExp(secondEventDescription))

  // forget me
  await page.getByRole('link', { name: 'Forget Me' }).click()
  await page.waitForTimeout(100); // wait a bit so we don't get the 'Your email' from the previous page
  await page.getByPlaceholder('Your email').fill(organizerEmail)
  await page.getByRole('button', { name: /submit/i }).click()

  // get email with forget-me url
  let remainingAttempts = 10
  const getEmailIntervalMs = 100

  const forgetMeUrl = await getEmailContents(request, organizerEmail, /http(s?):\/\/[^ \/]+\/forget-me\/[0-9\-a-f]+/)

  // execute delete request
  await page.goto(forgetMeUrl)
  await page.getByRole('button', { name: /yes, forget me/i }).click()

  // return to the event page and verify that the organizer is deleted
  await page.goto(eventUrl)
  await expect(page.getByText(deletedCommentText)).toHaveText(deletedCommentText)
  await expect(page.getByText(organizerName)).toHaveCount(0)
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 1")
  await expect(page.getByText('Maybe Attending:')).toHaveCount(0)

  // verify that the attendee is still present (One for the comment and one for the RSVP status)
  await expect(page.getByText(attendeeSecondName)).toHaveCount(2)

  // verify that only the first comment is deleted
  await expect(page.getByText(comment)).toHaveCount(0)
  await expect(page.getByText('Comment deleted by user')).toHaveCount(1)
  await expect(page.getByText(secondComment)).toHaveCount(1)
})

const getEmailContents = async (request, recipientEmail, regex) => {
  let remainingAttempts = 10
  const getEmailIntervalMs = 100

  while(remainingAttempts) {
    const response = await request.get(`${mailhogUrl}/api/v2/search?kind=to&query=${recipientEmail}`)
    expect(response.ok()).toBeTruthy()
    const rawResponseBody = await response.body()
    const responseBody = JSON.parse(rawResponseBody.toString())

    // get most recent email
    const mostRecentEmail =
      responseBody
        .items
        .sort((d1, d2) => d1 > d2)
        .at(0)

    if(!mostRecentEmail) {
      continue
    }

    // The email body contains weird line breaks prefixed with '='
    const trimmedBody = mostRecentEmail.Content.Body.replace(/=(?:\r\n|\r|\n)/g,'')
    const regexResults = regex.exec(trimmedBody)

    if(regexResults) {
      return regexResults[0]
    }

    await delay(getEmailIntervalMs)
    remainingAttempts--
  }

  throw console.error('Did not find expected email')
}

const delay = ms => new Promise(resolve => setTimeout(resolve, ms))
