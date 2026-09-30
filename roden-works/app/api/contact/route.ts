import { NextResponse } from 'next/server'
import { SITE_CONFIG } from '@/lib/constants'

/**
 * Contact form delivery via Resend's REST API (no SDK needed).
 *
 * Configure in the deployment environment:
 *   RESEND_API_KEY  — required to send
 *   CONTACT_FROM    — verified sender, e.g. "roden.works <contact@roden.works>"
 *                     (defaults to Resend's test sender, which only delivers to the account owner)
 *   CONTACT_TO      — recipient (defaults to SITE_CONFIG.email)
 *
 * Without RESEND_API_KEY the route answers 503 so the form can offer a direct
 * email fallback — it never reports a message as sent when it wasn't.
 */
const EMAIL_RE = /^[^\s@]+@[^\s@]+\.[^\s@]+$/

export async function POST(request: Request) {
  let body: Record<string, unknown>
  try {
    body = await request.json()
  } catch {
    return NextResponse.json({ error: 'invalid_request' }, { status: 400 })
  }

  const name = String(body.name ?? '').trim()
  const email = String(body.email ?? '').trim()
  const subject = String(body.subject ?? '').trim()
  const message = String(body.message ?? '').trim()

  // Honeypot: real visitors never fill the hidden "website" field
  if (body.website) return NextResponse.json({ success: true })

  if (!name || !email || !subject || !message) {
    return NextResponse.json({ error: 'All fields are required' }, { status: 400 })
  }
  if (!EMAIL_RE.test(email) || name.length > 200 || subject.length > 200 || message.length > 5000) {
    return NextResponse.json({ error: 'invalid_fields' }, { status: 400 })
  }

  const apiKey = process.env.RESEND_API_KEY
  if (!apiKey) {
    return NextResponse.json({ error: 'not_configured' }, { status: 503 })
  }

  try {
    const res = await fetch('https://api.resend.com/emails', {
      method: 'POST',
      headers: { Authorization: `Bearer ${apiKey}`, 'Content-Type': 'application/json' },
      body: JSON.stringify({
        from: process.env.CONTACT_FROM || 'roden.works <onboarding@resend.dev>',
        to: [process.env.CONTACT_TO || SITE_CONFIG.email],
        reply_to: email,
        subject: `[roden.works] ${subject}: ${name}`,
        text: `From: ${name} <${email}>\nTopic: ${subject}\n\n${message}`,
      }),
    })
    if (!res.ok) {
      console.error('Contact delivery failed:', res.status, await res.text())
      return NextResponse.json({ error: 'delivery_failed' }, { status: 502 })
    }
    return NextResponse.json({ success: true })
  } catch (err) {
    console.error('Contact delivery error:', err)
    return NextResponse.json({ error: 'delivery_failed' }, { status: 502 })
  }
}
