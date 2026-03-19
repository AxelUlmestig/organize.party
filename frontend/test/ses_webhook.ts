import * as crypto from 'crypto';
import * as fs from 'fs';
import * as path from 'path';
import * as selfsigned from 'selfsigned';

export async function generateTestKeyPair() {
  const { private: privateKey, cert } = await selfsigned.generate(null, {})

  return { privateKey, cert };
}

export function signWebhook({ privateKey, webhook, certName }) {
  const stringToSign = [
    'Message',
    webhook.Message,
    'MessageId',
    webhook.MessageId,
    'Timestamp',
    webhook.Timestamp,
    'TopicArn',
    webhook.TopicArn,
    'Type',
    webhook.Type,
    ''
  ].join('\n');

  const signer = crypto.createSign('SHA1');
  signer.update(stringToSign);
  signer.end();

  const signature = signer.sign(privateKey, 'base64');

  let signed = {
    ...webhook,
    "Signature": signature,
    "SigningCertURL": `http://localhost:8888/${certName}`,
    "SignatureVersion": "1"
  };

  return signed;
}


function constructWebhook({ privateKey, emailId, certName, message }) {
  const webhook = {
    "Type": "Notification",
    "Message": message,
    "TopicArn": "arn:aws:sns:eu-north-1:589106082734:organize-party-email-feedback",
    "MessageId": crypto.randomUUID(),
    "Timestamp": "2026-03-18T12:31:56.972Z",
    "SigningCertURL": `http://localhost:8888/${certName}`
  }

  return signWebhook({ privateKey, webhook, certName })
}

export function constructEmailDeliveryWebhook({ privateKey, emailId, certName }) {
  return constructWebhook({ privateKey, emailId, certName, message: JSON.stringify(constructDeliveryMessage(emailId)) })
}

export function constructEmailBounceWebhook({ privateKey, emailId, certName }) {
  return constructWebhook({ privateKey, emailId, certName, message: JSON.stringify(constructBounceMessage(emailId)) })
}

export function constructEmailComplaintWebhook({ privateKey, emailId, certName }) {
  return constructWebhook({ privateKey, emailId, certName, message: JSON.stringify(constructComplaintMessage(emailId)) })
}

function constructDeliveryMessage(emailId) {
  return {
    "notificationType": "Delivery",
    "mail": {
      "timestamp": "2026-03-20T12:36:46.418Z",
      "source": "noreply@organize.party",
      "sourceArn": "arn:aws:ses:eu-north-1:589106082734:identity/organize.party",
      "sourceIp": "78.67.203.128",
      "callerIdentity": "ses-smtp-user.20260212-223633",
      "sendingAccountId": "589106082734",
      "messageId": "0110019d0b3f88d2-97d25f08-4143-4f37-8e78-1d7e72910e50-000000",
      "destination": [
        "complaint@simulator.amazonses.com"
      ],
      "headersTruncated": false,
      "headers": [
        {
          "name": "Received",
          "value": "from axel-ThinkPad (78-67-203-128-no600.tbcn.telia.com [78.67.203.128]) by email-smtp.amazonaws.com with SMTP (SimpleEmailService-d-YASZ6RLDG) id QHgkB64AZREfBmjNX8m4 for complaint@simulator.amazonses.com; Fri, 20 Mar 2026 12:36:46 +0000 (UTC)"
        },
        {
          "name": "From",
          "value": "noreply@organize.party"
        },
        {
          "name": "To",
          "value": "complaint simulator <complaint@simulator.amazonses.com>"
        },
        {
          "name": "Subject",
          "value": ""
        },
        {
          "name": "X-OP-EMAIL-ID",
          "value": emailId
        },
        {
          "name": "MIME-Version",
          "value": "1.0"
        },
        {
          "name": "Content-Type",
          "value": "multipart/alternative; boundary=\"l1cObGNnVW\""
        }
      ],
      "commonHeaders": {
        "from": [
          "noreply@organize.party"
        ],
        "to": [
          "complaint simulator <complaint@simulator.amazonses.com>"
        ],
        "subject": ""
      }
    },
    "delivery": {
      "timestamp": "2026-03-20T12:36:49.077Z",
      "processingTimeMillis": 2659,
      "recipients": [
        "complaint@simulator.amazonses.com"
      ],
      "smtpResponse": "250 Ok",
      "remoteMtaIp": "3.248.120.48",
      "reportingMTA": "e240-2.smtp-out.eu-north-1.amazonses.com"
    }
  }
}

function constructBounceMessage(emailId) {
  return {
    "notificationType": "Bounce",
    "bounce": {
      "feedbackId": "0110019d00ee6588-b8b206e9-eb86-4fa7-97b4-e7abff9057ca-000000",
      "bounceType": "Permanent",
      "bounceSubType": "General",
      "bouncedRecipients": [
        {
          "emailAddress": "bounce@simulator.amazonses.com",
          "action": "failed",
          "status": "5.1.1",
          "diagnosticCode": "smtp; 550 5.1.1 As requested: user unknown <bounce@simulator.amazonses.com>"
        }
      ],
      "timestamp": "2026-03-18T12:31:56.000Z",
      "remoteMtaIp": "34.241.25.178",
      "reportingMTA": "dns; e240-6.smtp-out.eu-north-1.amazonses.com"
    },
    "mail": {
      "timestamp": "2026-03-18T12:31:56.241Z",
      "source": "noreply@organize.party",
      "sourceArn": "arn:aws:ses:eu-north-1:589106082734:identity/organize.party",
      "sourceIp": "78.67.203.128",
      "callerIdentity": "ses-smtp-user.20260212-223633",
      "sendingAccountId": "589106082734",
      "messageId": "0110019d00ee6351-65af309a-088c-46e2-b9eb-33372fd7f2fb-000000",
      "destination": [
        "bounce@simulator.amazonses.com"
      ],
      "headersTruncated": false,
      "headers": [
        {
          "name": "Received",
          "value": "from axel-ThinkPad (78-67-203-128-no600.tbcn.telia.com [78.67.203.128]) by email-smtp.amazonaws.com with SMTP (SimpleEmailService-d-0BTSVK324) id D6YryOiR0vVKW2e4dw4e for bounce@simulator.amazonses.com; Wed, 18 Mar 2026 12:31:56 +0000 (UTC)"
        },
        {
          "name": "From",
          "value": "noreply@organize.party"
        },
        {
          "name": "To",
          "value": "bounce simulator <bounce@simulator.amazonses.com>"
        },
        {
          "name": "Subject",
          "value": ""
        },
        {
          "name": "X-OP-EMAIL-ID",
          "value": emailId
        },
        {
          "name": "MIME-Version",
          "value": "1.0"
        },
        {
          "name": "Content-Type",
          "value": "multipart/alternative; boundary=\"6myG93bWZj\""
        }
      ],
      "commonHeaders": {
        "from": [
          "noreply@organize.party"
        ],
        "to": [
          "bounce simulator <bounce@simulator.amazonses.com>"
        ],
        "subject": ""
      }
    }
  }
}


function constructComplaintMessage(emailId) {
  return {
    "notificationType": "Complaint",
    "complaint": {
      "feedbackId": "0110019d0b3f9253-57cb942c-bda2-410e-a013-fcf742343d80-000000",
      "complaintSubType": null,
      "complainedRecipients": [
        {
          "emailAddress": "complaint@simulator.amazonses.com"
        }
      ],
      "timestamp": "2026-03-20T12:36:48.000Z",
      "userAgent": "Amazon SES Mailbox Simulator",
      "complaintFeedbackType": "abuse",
      "arrivalDate": "2026-03-20T12:36:48.979Z"
    },
    "mail": {
      "timestamp": "2026-03-20T12:36:46.418Z",
      "source": "noreply@organize.party",
      "sourceArn": "arn:aws:ses:eu-north-1:589106082734:identity/organize.party",
      "sourceIp": "78.67.203.128",
      "callerIdentity": "ses-smtp-user.20260212-223633",
      "sendingAccountId": "589106082734",
      "messageId": "0110019d0b3f88d2-97d25f08-4143-4f37-8e78-1d7e72910e50-000000",
      "destination": [
        "complaint@simulator.amazonses.com"
      ],
      "headersTruncated": false,
      "headers": [
        {
          "name": "Received",
          "value": "from axel-ThinkPad (78-67-203-128-no600.tbcn.telia.com [78.67.203.128]) by email-smtp.amazonaws.com with SMTP (SimpleEmailService-d-YASZ6RLDG) id QHgkB64AZREfBmjNX8m4 for complaint@simulator.amazonses.com; Fri, 20 Mar 2026 12:36:46 +0000 (UTC)"
        },
        {
          "name": "From",
          "value": "noreply@organize.party"
        },
        {
          "name": "To",
          "value": "complaint simulator <complaint@simulator.amazonses.com>"
        },
        {
          "name": "Subject",
          "value": ""
        },
        {
          "name": "X-OP-EMAIL-ID",
          "value": emailId
        },
        {
          "name": "MIME-Version",
          "value": "1.0"
        },
        {
          "name": "Content-Type",
          "value": "multipart/alternative; boundary=\"l1cObGNnVW\""
        }
      ],
      "commonHeaders": {
        "from": [
          "noreply@organize.party"
        ],
        "to": [
          "complaint simulator <complaint@simulator.amazonses.com>"
        ],
        "subject": ""
      }
    }
  }
}
