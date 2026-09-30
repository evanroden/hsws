import { chart } from '@/components/charts/tokens'
import type { IntakeStatus, SillState } from './wedgePhysics'

/* Status shapes carry meaning on their own (filled vs outlined triangle, ring,
 * check), so no state is ever signalled by colour alone. 14 × 14 box. */

export type Glyph = IntakeStatus | 'held' | 'none'

const TITANIUM = '#8A9BA8'

export function glyphShapes(glyph: Glyph) {
  switch (glyph) {
    case 'salty':
      return (
        <>
          <path d="M7 1.4 L13 12.3 H1 Z" fill={chart.copper} strokeLinejoin="round" />
          <rect x="6.3" y="5.1" width="1.4" height="3.9" rx="0.7" fill={chart.surface} />
          <circle cx="7" cy="10.5" r="0.85" fill={chart.surface} />
        </>
      )
    case 'below':
      return <path d="M7 2.2 L12.4 11.7 H1.6 Z" fill="none" stroke={chart.copper} strokeWidth="1.5" strokeLinejoin="round" />
    case 'watch':
      return (
        <>
          <circle cx="7" cy="7" r="5" fill="none" stroke={TITANIUM} strokeWidth="1.5" />
          <circle cx="7" cy="7" r="1.7" fill={TITANIUM} />
        </>
      )
    case 'held':
      return (
        <>
          <circle cx="7" cy="7" r="5.6" fill="none" stroke={chart.steel} strokeWidth="1.5" />
          <path d="M4.6 7.2 L6.3 8.9 L9.6 5.4" fill="none" stroke={chart.steel} strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round" />
        </>
      )
    case 'clear':
      return <path d="M3.2 7.4 L5.9 10 L10.9 4.4" fill="none" stroke={chart.text.muted} strokeWidth="1.6" strokeLinecap="round" strokeLinejoin="round" />
    default:
      return <path d="M3.5 7 H10.5" stroke={chart.text.muted} strokeWidth="1.6" strokeLinecap="round" />
  }
}

export function sillGlyph(state: SillState): Glyph {
  if (state === 'overtopped') return 'salty'
  if (state === 'holding') return 'held'
  if (state === 'approaching') return 'watch'
  return 'none'
}

export default function StatusGlyph({ glyph, className = '' }: { glyph: Glyph; className?: string }) {
  return (
    <svg width="14" height="14" viewBox="0 0 14 14" aria-hidden="true" className={`shrink-0 ${className}`}>
      {glyphShapes(glyph)}
    </svg>
  )
}
