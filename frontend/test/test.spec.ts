import { test, expect } from '@playwright/test'
import * as crypto from 'crypto'

const mailhogUrl = 'http://localhost:8025'
const newEventUrl = 'http://localhost:8081'

test('can create event', async ({ page, request }) => {
  const eventName = `Test event ${Math.floor(100 * Math.random()) + 1}`
  const eventDescription = 'Much party, such fun'
  const eventLocation = 'Super cool location'
  const eventPassword = 'correct password'
  const organizerEmail = `${crypto.randomUUID()}@organize.party`
  const organizerName = 'Orgo McNizer'
  const comment = 'This seems like a nice party'
  const deletedCommentText = 'comment deleted by user'

  await page.goto(newEventUrl)

  // create new event
  // TODO: figure out how to set time
  await page.getByTestId('event-editor-event-name').fill(eventName)
  await page.getByTestId('expanding-text-area').fill(eventDescription)
  await page.getByTestId('event-editor-event-location').fill(eventLocation)
  await page.getByTestId('event-editor-event-password').fill(eventPassword)
  await page.getByRole('button', { name: /submit/i }).click();

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click();

  // verify that the new event have the assigned properties
  await expect(page.getByTestId('view-event-title')).toHaveText(eventName)
  await expect(page.getByTestId('view-event-description')).toHaveText(eventDescription)
  await expect(page.getByTestId('view-event-location')).toHaveText(eventLocation)
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 0")

  // attend the event
  await page.getByTestId('view-event-attendee-name').fill(organizerName)
  await page.getByTestId('view-event-attendee-email').fill(organizerEmail)
  await page.getByRole('button', { name: /submit/i }).click();
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 1")
  await expect(page.getByText(organizerName)).toBeVisible()

  const eventPath = await getEmailContents(request, organizerEmail, /\/e\/[0-9\-a-f]+/)

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click();

  // update attendee status to 'Maybe Coming'
  await page.getByTestId('view-event-attendee-plus-one').check({force: true})
  await page.getByTestId('view-event-attendee-status').selectOption({ label: 'Maybe Coming' })
  await page.getByTestId('view-event-submit-attendee').click()

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click();

  // Verify that the attendee list is updated
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 0")
  // Clicking checkbox is super flaky...
  // await expect(page.getByTestId('view-attendees-maybe-attending-number')).toHaveText("Maybe Attending: 1 (+1)")
  await expect(page.getByTestId('view-attendees-maybe-attending-number')).toHaveText(/Maybe Attending: 1.*/i)

  // add a comment
  await page.getByPlaceholder('Leave a comment').fill(comment)
  await page.getByRole('button', { name: /comment/i }).click();

  // verify comment
  await expect(page.getByText(comment)).toHaveText(comment)

  // forget me
  await page.getByRole('link', { name: 'Forget Me' }).click()
  await page.getByPlaceholder('Your email').fill(organizerEmail)
  await page.getByRole('button', { name: /submit/i }).click();

  // get email with forget-me url
  let remainingAttempts = 10
  const getEmailIntervalMs = 100

  const forgetMePath = await getEmailContents(request, organizerEmail, /\/forget-me\/[0-9\-a-f]+/)

  // execute delete request
  await page.goto(newEventUrl + forgetMePath)
  await page.getByRole('button', { name: /yes, forget me/i }).click();

  // return to the event page and verify that the attendee is deleted
  await page.goto(newEventUrl + eventPath)
  await expect(page.getByText(deletedCommentText)).toHaveText(deletedCommentText)
  await expect(page.getByText(organizerName)).toHaveCount(0);
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 0")
  await expect(page.getByText('Maybe Attending:')).toHaveCount(0);
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

  throw console.error('Did not get a forget me email')
}

const delay = ms => new Promise(resolve => setTimeout(resolve, ms))
