import { ImageResponse } from 'next/og'

export const runtime = 'edge'

export async function GET() {
  return new ImageResponse(
    (
      <div
        style={{
          width: '100%',
          height: '100%',
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          background: 'linear-gradient(135deg, #0B1215 0%, #0F1A1E 50%, #0B1215 100%)',
          fontFamily: 'Georgia, serif',
        }}
      >
        {/* Grid pattern */}
        <div
          style={{
            position: 'absolute',
            inset: 0,
            opacity: 0.04,
            backgroundImage:
              'linear-gradient(rgba(138,155,168,0.6) 1px, transparent 1px), linear-gradient(90deg, rgba(138,155,168,0.6) 1px, transparent 1px)',
            backgroundSize: '60px 60px',
            display: 'flex',
          }}
        />
        {/* Copper accent line */}
        <div
          style={{
            width: 80,
            height: 2,
            background: 'linear-gradient(90deg, transparent, #B87333, transparent)',
            marginBottom: 32,
            display: 'flex',
          }}
        />
        <div
          style={{
            fontSize: 80,
            fontWeight: 700,
            color: '#FAFAFA',
            letterSpacing: '-0.02em',
            display: 'flex',
          }}
        >
          RODEN
        </div>
        <div
          style={{
            width: 120,
            height: 1,
            background: 'linear-gradient(90deg, transparent, rgba(184,115,51,0.5), transparent)',
            margin: '24px 0',
            display: 'flex',
          }}
        />
        <div
          style={{
            fontSize: 22,
            color: '#8A9BA8',
            letterSpacing: '0.15em',
            textTransform: 'uppercase' as const,
            display: 'flex',
          }}
        >
          Engineering &middot; Advocacy &middot; Creative
        </div>
        <div
          style={{
            fontSize: 16,
            color: 'rgba(138,155,168,0.5)',
            marginTop: 40,
            maxWidth: 600,
            textAlign: 'center',
            lineHeight: 1.6,
            display: 'flex',
          }}
        >
          Optimizing complex systems to improve human quality of life
        </div>
      </div>
    ),
    {
      width: 1200,
      height: 630,
    }
  )
}
