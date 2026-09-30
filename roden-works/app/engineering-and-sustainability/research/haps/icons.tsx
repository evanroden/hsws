import type { SVGProps } from 'react'
import { POLLUTANTS, type PollutantId } from './data'

/* Instrument line icons — 24px grid, 1.5px stroke, round caps. */

type IconProps = SVGProps<SVGSVGElement> & { size?: number }

function Icon({ size = 24, children, ...rest }: IconProps) {
  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth={1.5}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      {...rest}
    >
      {children}
    </svg>
  )
}

/** pDR-1500 — real-time photometer with a size-selective inlet */
export function PhotometerIcon(props: IconProps) {
  return (
    <Icon {...props}>
      <path d="M9 3.5h6l-1.5 3h-3z" />
      <path d="M10.5 6.5v2.5M13.5 6.5v2.5" />
      <rect x="4" y="9" width="16" height="11.5" rx="2" />
      <rect x="7" y="12" width="6.5" height="4" rx="0.75" />
      <circle cx="16.5" cy="14" r="0.9" fill="currentColor" stroke="none" />
    </Icon>
  )
}

/** MicroAeth AE51 — compact filter-based black carbon monitor */
export function AethalometerIcon(props: IconProps) {
  return (
    <Icon {...props}>
      <path d="M3.5 9V6.5a2 2 0 0 1 2-2H8" />
      <rect x="3.5" y="9" width="17" height="10" rx="2" />
      <circle cx="15" cy="14" r="2.75" />
      <circle cx="15" cy="14" r="0.9" fill="currentColor" stroke="none" />
      <path d="M7 12.5h3M7 15.5h3" />
    </Icon>
  )
}

/** Ogawa passive sampler — diffusion badge hung in the room */
export function PassiveSamplerIcon(props: IconProps) {
  return (
    <Icon {...props}>
      <path d="M12 2.5v4" />
      <path d="M9.5 2.5h5" />
      <rect x="8" y="6.5" width="8" height="14" rx="4" />
      <path d="M8 10.5h8M8 16.5h8" />
      <circle cx="12" cy="13.5" r="0.9" fill="currentColor" stroke="none" />
    </Icon>
  )
}

/** Ambulatory blood-pressure monitor — arm cuff tethered to a worn recorder */
export function BloodPressureIcon(props: IconProps) {
  return (
    <Icon {...props}>
      <rect x="3" y="4" width="10" height="8" rx="2" />
      <path d="M6.5 4v8M9.5 4v8" />
      <path d="M13 8h2.5a2 2 0 0 1 2 2v3" />
      <rect x="13.5" y="13" width="7.5" height="7" rx="1.75" />
      <path d="M15.75 16.5h3" />
    </Icon>
  )
}

export const INSTRUMENT_ICONS = {
  pdr: PhotometerIcon,
  ae51: AethalometerIcon,
  ogawa: PassiveSamplerIcon,
  abpm: BloodPressureIcon,
} as const

/** Pollutant identity mark: filled dot for particles, ring for the gas. */
export function PollutantMark({
  id,
  size = 10,
  dim = false,
}: {
  id: PollutantId
  size?: number
  dim?: boolean
}) {
  const p = POLLUTANTS[id]
  const r = size / 2
  return (
    <svg
      width={size}
      height={size}
      viewBox={`0 0 ${size} ${size}`}
      aria-hidden="true"
      className="shrink-0"
      style={{ opacity: dim ? 0.35 : 1 }}
    >
      {p.kind === 'gas' ? (
        <circle cx={r} cy={r} r={r - 1.25} fill="none" stroke={p.color} strokeWidth={2} />
      ) : (
        <circle cx={r} cy={r} r={r} fill={p.color} />
      )}
    </svg>
  )
}
