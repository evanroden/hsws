import { NextResponse } from 'next/server'

export async function POST(request: Request) {
  try {
    const body = await request.json()
    const { name, email, subject, message } = body

    if (!name || !email || !subject || !message) {
      return NextResponse.json({ error: 'All fields are required' }, { status: 400 })
    }

    // In production, wire this up to a service like Resend, SendGrid, or Formspree.
    // For now, log the submission and return success.
    // To integrate Resend: npm install resend, then use:
    //   const resend = new Resend(process.env.RESEND_API_KEY)
    //   await resend.emails.send({
    //     from: 'contact@roden.works',
    //     to: 'evanjroden@gmail.com',
    //     subject: `[roden.works] ${subject}: ${name}`,
    //     text: `From: ${name} (${email})\nSubject: ${subject}\n\n${message}`,
    //   })

    console.log('Contact form submission:', { name, email, subject, message })

    return NextResponse.json({ success: true })
  } catch {
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}
