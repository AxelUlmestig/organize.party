import { test, expect } from '@playwright/test'
import * as crypto from 'crypto'

test('can create event', async ({ page }) => {
  const eventName = `Test event ${Math.floor(100 * Math.random()) + 1}`
  const eventDescription = 'Much party, such fun'
  const eventLocation = 'Super cool location'
  const eventPassword = 'correct password'
  const organizerEmail = `${crypto.randomUUID()}@organize.party`
  const organizerName = 'Orgo McNizer'

  await page.goto('http://localhost:8081')

  // create new event
  // TODO: figure out how to set time
  await page.getByTestId('event-editor-event-name').fill(eventName)
  await page.getByTestId('expanding-text-area').fill(eventDescription)
  await page.getByTestId('event-editor-event-location').fill(eventLocation)
  await page.getByTestId('event-editor-event-password').fill(eventPassword)
  await page.getByTestId('event-editor-event-submit-button').click()

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
  await page.getByTestId('view-event-submit-attendee').click()
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 1")
  await expect(page.getByText(organizerName)).toBeVisible()

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click();

  // update attendee status to 'Maybe Coming'
  await page.getByTestId('view-event-attendee-plus-one').check()
  await page.getByTestId('view-event-attendee-status').selectOption({ label: 'Maybe Coming' })
  await page.getByTestId('view-event-submit-attendee').click()

  // Click Ok button on info modal
  await page.getByRole('button', { name: /ok/i }).click();

  // Verify that the attendee list is updated
  await expect(page.getByTestId('view-attendees-attending-number')).toHaveText("Attending: 0")
  await expect(page.getByTestId('view-attendees-maybe-attending-number')).toHaveText("Maybe Attending: 1 (+1)")
})

