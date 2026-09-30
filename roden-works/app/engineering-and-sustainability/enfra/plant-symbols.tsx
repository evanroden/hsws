'use client'

import type { ReactNode } from 'react'
import { chart } from '@/components/charts/tokens'
import type { CardId, Rect } from './plant-layout'

/* Drawing tokens. `px` everywhere = SVG user units per rendered pixel, so
 * strokes and type stay at true pixel sizes whatever the diagram's scale. */
export const SURFACE = chart.surface
export const INK = { normal: '#A9B6BF', active: '#F2F5F7' }
export const DETAIL = '#56636D'
export const BODY = '#1B2327'
export const HAIRLINE = '#2A3339'

export type Tone = 'normal' | 'active'

interface TextProps {
  x: number
  y: number
  px: number
  size?: number
  mono?: boolean
  weight?: number
  fill?: string
  anchor?: 'start' | 'middle' | 'end'
  tracking?: number
  children: ReactNode
}

/** SVG text at a true pixel size */
export function T({ x, y, px, size = 11, mono, weight, fill = chart.text.secondary, anchor, tracking, children }: TextProps) {
  return (
    <text
      x={x}
      y={y}
      fontSize={size * px}
      fontWeight={weight}
      fill={fill}
      textAnchor={anchor}
      letterSpacing={tracking ? tracking * size * px : undefined}
      className={mono ? 'font-mono' : 'font-sans'}
      style={mono ? { fontVariantNumeric: 'tabular-nums' } : undefined}
    >
      {children}
    </text>
  )
}

interface SymbolProps {
  r: Rect
  px: number
  tone: Tone
}

const ink = (tone: Tone) => INK[tone]

/* ─── Cooling tower: 2-cell induced draft ─────────────────────────────────── */
export function Tower({ r, px, tone }: SymbolProps) {
  const fanH = 14
  const basinH = 8
  const top = r.y + fanH
  const bottom = r.y + r.h - basinH
  const cellW = r.w / 2
  const fillTop = top + 26
  const clipId = `tower-clip-${Math.round(r.x)}-${Math.round(r.y)}`
  const hatch: string[] = []
  for (let k = -80; k < r.w + 80; k += 9) {
    hatch.push(`M${r.x + k},${bottom} L${r.x + k + (bottom - fillTop)},${fillTop}`)
  }
  return (
    <g>
      <defs>
        <clipPath id={clipId}>
          <rect x={r.x + 2} y={fillTop} width={r.w - 4} height={bottom - fillTop - 2} />
        </clipPath>
      </defs>
      {/* Fan stacks */}
      {[0, 1].map((i) => {
        const cx = r.x + cellW * (i + 0.5)
        const tw = cellW * 0.32
        const bw = cellW * 0.25
        return (
          <g key={i}>
            <path
              d={`M${cx - bw},${top} L${cx - tw},${r.y} L${cx + tw},${r.y} L${cx + bw},${top}`}
              fill={BODY}
              stroke={ink(tone)}
              strokeWidth={1.25 * px}
              strokeLinejoin="round"
            />
            <line x1={cx - tw + 4} x2={cx + tw - 4} y1={r.y + 4} y2={r.y + 4} stroke={DETAIL} strokeWidth={px} />
          </g>
        )
      })}
      {/* Casing */}
      <rect x={r.x} y={top} width={r.w} height={bottom - top} rx={2} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      {/* Hot-water distribution with spray nozzles */}
      <line x1={r.x + 6} x2={r.x + r.w - 6} y1={top + 9} y2={top + 9} stroke={DETAIL} strokeWidth={px} />
      {Array.from({ length: 12 }, (_, i) => r.x + 12 + i * ((r.w - 24) / 11)).map((x) => (
        <line key={x} x1={x} x2={x} y1={top + 9} y2={top + 13} stroke={DETAIL} strokeWidth={px} />
      ))}
      {/* Fill media */}
      <g clipPath={`url(#${clipId})`}>
        <path d={hatch.join(' ')} stroke={DETAIL} strokeWidth={px} fill="none" />
      </g>
      <line x1={r.x + cellW} x2={r.x + cellW} y1={top} y2={bottom} stroke={ink(tone)} strokeWidth={1.25 * px} />
      {['CT-1', 'CT-2'].map((tag, i) => (
        <T key={tag} x={r.x + cellW * (i + 0.5)} y={top + 24} px={px} mono anchor="middle" fill={chart.text.muted}>
          {tag}
        </T>
      ))}
      {/* Basin */}
      <rect x={r.x - 4} y={bottom} width={r.w + 8} height={basinH} rx={1.5} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
    </g>
  )
}

/* ─── Centrifugal chiller: condenser + evaporator barrels, compressor ────── */
export function Chiller({ r, px, tone }: SymbolProps) {
  const bh = 27
  const condY = r.y
  const evapY = r.y + r.h - bh
  const cx = r.x + 44
  const cy = r.y + r.h / 2
  const vx = r.x + r.w - 28
  const barrel = (x: number, y: number, stroke: string, fill: string) => (
    <rect x={x} y={y} width={r.w} height={bh} rx={bh / 2} fill={fill} stroke={stroke} strokeWidth={1.25 * px} />
  )
  return (
    <g>
      {/* Second unit, drawn behind */}
      <g opacity={0.55}>
        {barrel(r.x + 8, condY - 8, DETAIL, BODY)}
        {barrel(r.x + 8, evapY - 8, DETAIL, BODY)}
      </g>
      {/* Refrigerant circuit */}
      <path
        d={`M${cx},${evapY} V${cy + 10} M${cx},${cy - 10} V${condY + bh} M${vx},${condY + bh} V${evapY}`}
        stroke={DETAIL}
        strokeWidth={px}
        fill="none"
      />
      {barrel(r.x, condY, ink(tone), BODY)}
      {barrel(r.x, evapY, ink(tone), BODY)}
      {/* Compressor */}
      <circle cx={cx} cy={cy} r={10} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      <path d={`M${cx - 5},${cy - 6} L${cx + 5},${cy - 3} L${cx + 5},${cy + 3} L${cx - 5},${cy + 6} Z`} fill="none" stroke={ink(tone)} strokeWidth={px} strokeLinejoin="round" />
      {/* Expansion valve */}
      <path d={`M${vx - 5},${cy - 4} L${vx + 5},${cy + 4} L${vx + 5},${cy - 4} L${vx - 5},${cy + 4} Z`} fill={BODY} stroke={ink(tone)} strokeWidth={px} strokeLinejoin="round" />
      <T x={r.x + r.w / 2 + 10} y={condY + bh / 2 + 4 * px} px={px} mono anchor="middle" fill={chart.text.muted}>
        CONDENSER
      </T>
      <T x={r.x + r.w / 2 + 10} y={evapY + bh / 2 + 4 * px} px={px} mono anchor="middle" fill={chart.text.muted}>
        EVAPORATOR
      </T>
    </g>
  )
}

/* ─── Fire-tube steam boiler with burner and stack ──────────────────────── */
export function Boiler({ r, px, tone }: SymbolProps) {
  const shell = (x: number, y: number, stroke: string) => (
    <rect x={x} y={y} width={r.w} height={r.h} rx={r.h / 2} fill={BODY} stroke={stroke} strokeWidth={1.25 * px} />
  )
  return (
    <g>
      <g opacity={0.55}>{shell(r.x + 8, r.y - 8, DETAIL)}</g>
      {/* Stack */}
      <rect x={r.x + 16} y={r.y - 16} width={14} height={16} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      {shell(r.x, r.y, ink(tone))}
      {/* Fire tubes */}
      {[0.32, 0.5, 0.68].map((t) => (
        <line key={t} x1={r.x + 22} x2={r.x + r.w - 20} y1={r.y + r.h * t} y2={r.y + r.h * t} stroke={DETAIL} strokeWidth={px} />
      ))}
      {/* Burner */}
      <rect x={r.x - 16} y={r.y + 8} width={16} height={r.h - 16} rx={2} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      <path
        d={`M${r.x - 8},${r.y + r.h - 13} c-4,-4 -1,-8 0,-12 c1,4 4,8 0,12 Z`}
        fill="none"
        stroke={ink(tone)}
        strokeWidth={px}
        strokeLinejoin="round"
      />
    </g>
  )
}

/* ─── Diesel generator set: engine + alternator ─────────────────────────── */
export function Generator({ r, px, tone, running }: SymbolProps & { running: boolean }) {
  const ax = r.x + r.w - 24
  const ay = r.y + r.h / 2
  const box = (x: number, y: number, stroke: string) => (
    <rect x={x} y={y} width={r.w} height={r.h} rx={3} fill={BODY} stroke={stroke} strokeWidth={1.25 * px} />
  )
  return (
    <g>
      <g opacity={0.55}>{box(r.x + 8, r.y - 8, DETAIL)}</g>
      {box(r.x, r.y, ink(tone))}
      <rect x={r.x + 8} y={r.y + 8} width={r.w * 0.5} height={r.h - 16} rx={1.5} fill="none" stroke={DETAIL} strokeWidth={px} />
      {[0.2, 0.4, 0.6, 0.8].map((t) => (
        <line key={t} x1={r.x + 8 + r.w * 0.5 * t} x2={r.x + 8 + r.w * 0.5 * t} y1={r.y + 8} y2={r.y + r.h - 8} stroke={DETAIL} strokeWidth={px} />
      ))}
      <line x1={r.x + 8 + r.w * 0.5} x2={ax - 12} y1={ay} y2={ay} stroke={DETAIL} strokeWidth={1.5 * px} />
      <circle cx={ax} cy={ay} r={12} fill={BODY} stroke={running ? '#D4B43C' : ink(tone)} strokeWidth={1.25 * px} />
      <T x={ax} y={ay + 4 * px} px={px} mono anchor="middle" fill={running ? chart.text.primary : chart.text.secondary}>
        G
      </T>
    </g>
  )
}

/* ─── Automatic transfer switch (single-line symbol) ────────────────────── */
export function Ats({ r, px, tone, position }: SymbolProps & { position: 'N' | 'E' }) {
  const nY = r.y + 20
  const eY = r.y + r.h - 20
  const pY = (nY + eY) / 2
  const cX = r.x + 18
  const pX = r.x + r.w - 18
  const len = Math.hypot(pX - cX, pY - nY)
  const angle = (Math.atan2(pY - nY, pX - cX) * 180) / Math.PI
  const on = '#D4B43C'
  return (
    <g>
      <rect x={r.x} y={r.y} width={r.w} height={r.h} rx={3} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      <path d={`M${r.x},${nY} H${cX} M${r.x},${eY} H${cX} M${pX},${pY} H${r.x + r.w}`} stroke={DETAIL} strokeWidth={1.5 * px} />
      <circle cx={cX} cy={nY} r={2.5} fill={position === 'N' ? INK.active : DETAIL} />
      <circle cx={cX} cy={eY} r={2.5} fill={position === 'E' ? on : DETAIL} />
      <line
        x1={pX}
        y1={pY}
        x2={pX - len}
        y2={pY}
        stroke={position === 'E' ? on : INK.active}
        strokeWidth={2 * px}
        strokeLinecap="round"
        style={{
          transformOrigin: `${pX}px ${pY}px`,
          transform: `rotate(${position === 'N' ? angle : -angle}deg)`,
          transition: 'transform 420ms cubic-bezier(0.16, 1, 0.3, 1), stroke 300ms',
        }}
      />
      <circle cx={pX} cy={pY} r={3} fill={INK.active} />
      <T x={cX - 8} y={nY + 17 * px} px={px} mono fill={position === 'N' ? chart.text.primary : chart.text.muted}>
        N
      </T>
      <T x={cX - 8} y={eY - 8 * px} px={px} mono fill={position === 'E' ? chart.text.primary : chart.text.muted}>
        E
      </T>
      <T x={r.x + r.w - 8} y={r.y + 16 * px} px={px} mono anchor="end" fill={chart.text.secondary}>
        ATS
      </T>
    </g>
  )
}

/* ─── Pump: circle with a triangle pointing downstream ──────────────────── */
export function Pump({ x, y, dir, px, tone }: { x: number; y: number; dir: 'up' | 'down' | 'left' | 'right'; px: number; tone: Tone }) {
  const rot = { right: 0, down: 90, left: 180, up: 270 }[dir]
  return (
    <g transform={`translate(${x},${y}) rotate(${rot})`}>
      <circle r={10} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      <path d="M-4,-6 L7,0 L-4,6 Z" fill={ink(tone)} />
    </g>
  )
}

/* ─── Utility-side symbols ──────────────────────────────────────────────── */
export function Meter({ x, y, px, tone }: { x: number; y: number; px: number; tone: Tone }) {
  return (
    <g>
      <circle cx={x} cy={y} r={9} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      <path d={`M${x - 5},${y + 3} A6,6 0 0 1 ${x + 5},${y + 3}`} fill="none" stroke={DETAIL} strokeWidth={px} />
      <line x1={x} y1={y + 2} x2={x + 3.5} y2={y - 4} stroke={ink(tone)} strokeWidth={px} strokeLinecap="round" />
    </g>
  )
}

export function Transformer({ x, y, px, tone }: { x: number; y: number; px: number; tone: Tone }) {
  return (
    <g>
      <ellipse cx={x} cy={y} rx={15} ry={9} fill={BODY} />
      <circle cx={x - 5.5} cy={y} r={8} fill="none" stroke={ink(tone)} strokeWidth={1.25 * px} />
      <circle cx={x + 5.5} cy={y} r={8} fill="none" stroke={ink(tone)} strokeWidth={1.25 * px} />
    </g>
  )
}

/* ─── BAS head-end ──────────────────────────────────────────────────────── */
export function BasUnit({ r, px, tone }: SymbolProps) {
  const sx = r.x + 8
  const sy = r.y + 8
  const sw = 50
  const sh = r.h - 16
  const trend = [0.7, 0.55, 0.62, 0.4, 0.46, 0.3, 0.36]
  return (
    <g>
      <rect x={r.x} y={r.y} width={r.w} height={r.h} rx={4} fill={BODY} stroke={ink(tone)} strokeWidth={1.25 * px} />
      <rect x={sx} y={sy} width={sw} height={sh} rx={2} fill="none" stroke={DETAIL} strokeWidth={px} />
      <polyline
        points={trend.map((t, i) => `${sx + 5 + (i * (sw - 10)) / (trend.length - 1)},${sy + sh * t}`).join(' ')}
        fill="none"
        stroke={ink(tone)}
        strokeWidth={px}
        strokeLinejoin="round"
      />
      <T x={sx + sw + 10} y={r.y + r.h / 2 - 2 * px} px={px} mono fill={chart.text.secondary}>
        BAS
      </T>
      {[0, 1, 2].map((i) => (
        <circle key={i} cx={sx + sw + 12 + i * 9} cy={r.y + r.h / 2 + 10} r={2} fill={i === 2 ? DETAIL : '#3DA887'} />
      ))}
    </g>
  )
}

/* ─── Hospital end-use card ─────────────────────────────────────────────── */
function CardIcon({ id, x, y, px, color }: { id: CardId; x: number; y: number; px: number; color: string }) {
  const s = { stroke: color, strokeWidth: 1.25 * px, fill: 'none', strokeLinecap: 'round' as const, strokeLinejoin: 'round' as const }
  if (id === 'cooling')
    return (
      <g {...s}>
        {[0, 60, 120].map((a) => {
          const rad = (a * Math.PI) / 180
          return <line key={a} x1={x - Math.cos(rad) * 7} y1={y - Math.sin(rad) * 7} x2={x + Math.cos(rad) * 7} y2={y + Math.sin(rad) * 7} />
        })}
      </g>
    )
  if (id === 'heating')
    return (
      <g {...s}>
        {[-4.5, 0, 4.5].map((dx) => (
          <path key={dx} d={`M${x + dx},${y + 7} c-2.5,-2.5 2.5,-4.5 0,-7 c-2.5,-2.5 2.5,-4.5 0,-7`} />
        ))}
      </g>
    )
  if (id === 'sterilization')
    return (
      <g {...s}>
        <rect x={x - 7} y={y - 7} width={14} height={14} rx={2} />
        <circle cx={x} cy={y} r={4} />
      </g>
    )
  return <path {...s} d={`M${x + 1.5},${y - 8} L${x - 4.5},${y + 1} H${x + 0.5} L${x - 1.5},${y + 8} L${x + 4.5},${y - 1} H${x - 0.5} Z`} />
}

export function HospitalCardView({
  id,
  r,
  label,
  sub,
  px,
  tone,
  status,
}: {
  id: CardId
  r: Rect
  label: string
  sub: string
  px: number
  tone: Tone
  status?: { text: string; color: string }
}) {
  const cy = r.y + r.h / 2
  return (
    <g>
      <rect x={r.x} y={r.y} width={r.w} height={r.h} rx={6} fill={BODY} stroke={tone === 'active' ? '#4A5761' : HAIRLINE} strokeWidth={px} />
      <CardIcon id={id} x={r.x + 17} y={cy} px={px} color={ink(tone)} />
      <T x={r.x + 34} y={cy - 3 * px} px={px} size={13} weight={600} fill={chart.text.primary}>
        {label}
      </T>
      <T x={r.x + 34} y={cy + 13 * px} px={px} mono fill={status ? chart.text.primary : chart.text.muted}>
        {status ? status.text : sub}
      </T>
      {status && <circle cx={r.x + r.w - 12} cy={r.y + 12} r={3.5 * px} fill={status.color} />}
    </g>
  )
}
