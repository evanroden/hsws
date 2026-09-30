'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import Legend from '@/components/charts/Legend'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'
import { barPath, measureText } from './chartUtils'

export interface CoverageRow {
  area: string
  connected: number
  underserved: number
  unserved: number
}

type TierKey = 'connected' | 'underserved' | 'unserved'

/**
 * Ordered tiers, best → worst. Verdigris and copper are the two poles with the
 * neutral de-emphasis gray between them; the set clears CVD ΔE 12.5 and 3:1 on
 * the surface. In-segment labels use whichever ink clears 4.5:1 on the fill.
 */
const TIERS: { key: TierKey; label: string; short: string; color: string; ink: string }[] = [
  { key: 'connected', label: 'Adequately served (25+ Mbps)', short: 'adequately served', color: chart.verdigris, ink: '#0B1215' },
  { key: 'underserved', label: 'Underserved (10–25 Mbps)', short: 'underserved', color: chart.deemph, ink: chart.text.primary },
  { key: 'unserved', label: 'Unserved (<10 Mbps or none)', short: 'unserved', color: chart.copper, ink: '#0B1215' },
]

const BAR_H = 24
const GAP = 2
const AXIS_BAND = 28

export default function CoverageChart({ data, animate }: { data: CoverageRow[]; animate: boolean }) {
  return (
    <ChartFrame
      title="Household broadband coverage by area"
      subtitle="Share of households in each service tier, by download speed."
      legend={<Legend items={TIERS.map((t) => ({ label: t.label, color: t.color, shape: 'rect' as const }))} />}
      table={{
        caption: 'Share of households by broadband service tier, by area',
        columns: ['Area', 'Adequately served', 'Underserved', 'Unserved'],
        rows: data.map((d) => [d.area, `${d.connected}%`, `${d.underserved}%`, `${d.unserved}%`]),
      }}
    >
      <Bars data={data} animate={animate} />
    </ChartFrame>
  )
}

function Bars({ data, animate }: { data: CoverageRow[]; animate: boolean }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [active, setActive] = useState<{ row: number; tier: TierKey } | null>(null)
  const [focusRow, setFocusRow] = useState<number | null>(null)

  // Area names sit above each bar so the bar keeps the full width at every size
  const labelBand = 26
  const pitch = labelBand + BAR_H + 22
  const plotW = Math.max(0, width)
  const plotH = data.length * pitch
  const height = plotH + AXIS_BAND
  const drawn = animate || reduceMotion

  const segments = (d: CoverageRow) => {
    // Every row sums to 100%; gaps are carved out of the segments, not added
    const usable = plotW - GAP * (TIERS.length - 1)
    let x = 0
    return TIERS.map((t, i) => {
      const w = (d[t.key] / 100) * usable
      const seg = { ...t, x, w, value: d[t.key], first: i === 0, last: i === TIERS.length - 1 }
      x += w + GAP
      return seg
    })
  }

  const activeRow = active ? data[active.row] : null
  const activeSeg = active && activeRow ? segments(activeRow).find((s) => s.key === active.tier) : null

  return (
    <div ref={ref} className="relative w-full" style={{ height }}>
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Stacked bar chart of broadband coverage: ${data
            .map((d) => `${d.area}: ${d.connected}% adequately served, ${d.underserved}% underserved, ${d.unserved}% unserved`)
            .join('; ')}.`}
          className="block overflow-visible"
        >
          {[0, 25, 50, 75, 100].map((t) => {
            const gx = (t / 100) * plotW
            return (
              <g key={t}>
                <line x1={gx} x2={gx} y1={0} y2={plotH} stroke={chart.grid} strokeWidth={1} shapeRendering="crispEdges" />
                <text
                  x={gx}
                  y={plotH + 18}
                  textAnchor={t === 0 ? 'start' : t === 100 ? 'end' : 'middle'}
                  fontSize={11}
                  fill={chart.text.muted}
                  style={{ fontVariantNumeric: 'tabular-nums' }}
                >
                  {t}%
                </text>
              </g>
            )
          })}

          <g role="list" aria-label="Areas">
            {data.map((d, i) => {
              const top = i * pitch
              const barY = top + labelBand
              const rowOn = active?.row === i || focusRow === i
              return (
                <g
                  key={d.area}
                  role="listitem"
                  tabIndex={0}
                  aria-label={`${d.area}: ${d.connected}% adequately served, ${d.underserved}% underserved, ${d.unserved}% unserved.`}
                  onFocus={() => {
                    setFocusRow(i)
                    setActive({ row: i, tier: 'unserved' })
                  }}
                  onBlur={() => {
                    setFocusRow(null)
                    setActive(null)
                  }}
                  onKeyDown={(e) => {
                    if (e.key !== 'ArrowRight' && e.key !== 'ArrowLeft') return
                    e.preventDefault()
                    const idx = TIERS.findIndex((t) => t.key === active?.tier)
                    const next = Math.min(TIERS.length - 1, Math.max(0, idx + (e.key === 'ArrowRight' ? 1 : -1)))
                    setActive({ row: i, tier: TIERS[next].key })
                  }}
                  onPointerLeave={() => setActive(null)}
                  className="outline-none"
                >
                  {focusRow === i && (
                    <rect x={-6} y={top + 2} width={plotW + 12} height={pitch - 8} rx={8} fill="none" stroke="rgba(255,255,255,0.35)" strokeWidth={1} />
                  )}
                  <text x={0} y={top + 16} fontSize={13} fontWeight={500} fill={rowOn ? chart.text.primary : chart.text.secondary}>
                    {d.area}
                  </text>
                  <text x={plotW} y={top + 16} textAnchor="end" fontSize={12} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
                    <tspan fontWeight={600} fill={chart.text.secondary}>
                      {d.unserved}%
                    </tspan>{' '}
                    unserved
                  </text>

                  {segments(d).map((s, si) => {
                    const label = `${s.value}%`
                    const fits = s.w >= measureText(label, 11, 600) + 14
                    const isActive = active?.row === i && active.tier === s.key
                    const d0 = barPath(s.x, barY, 0, BAR_H, s.first ? 4 : 0, s.last ? 4 : 0)
                    const d1 = barPath(s.x, barY, s.w, BAR_H, s.first ? 4 : 0, s.last ? 4 : 0)
                    const delay = reduceMotion ? 0 : 0.1 + i * 0.12 + si * 0.12
                    return (
                      <g
                        key={s.key}
                        onPointerEnter={() => setActive({ row: i, tier: s.key })}
                        onPointerDown={() => setActive({ row: i, tier: s.key })}
                      >
                        <motion.path
                          d={d0}
                          initial={{ d: d0 }}
                          animate={drawn ? { d: d1 } : {}}
                          transition={{ duration: reduceMotion ? 0 : 0.7, delay, ease: [0.16, 1, 0.3, 1] }}
                          fill={s.color}
                        />
                        {isActive && <path d={d1} fill="#FFFFFF" fillOpacity={0.14} pointerEvents="none" />}
                        {/* Hit target spans the row height, not just the painted bar */}
                        <rect x={s.x} y={top + labelBand - 8} width={s.w + (s.last ? 0 : GAP)} height={BAR_H + 16} fill="transparent" />
                        {fits && (
                          <motion.text
                            x={s.x + s.w / 2}
                            y={barY + BAR_H / 2}
                            dy="0.35em"
                            textAnchor="middle"
                            fontSize={11}
                            fontWeight={600}
                            fill={s.ink}
                            pointerEvents="none"
                            style={{ fontVariantNumeric: 'tabular-nums' }}
                            initial={{ opacity: 0 }}
                            animate={drawn ? { opacity: 1 } : {}}
                            transition={{ duration: reduceMotion ? 0 : 0.3, delay: delay + (reduceMotion ? 0 : 0.5) }}
                          >
                            {label}
                          </motion.text>
                        )}
                      </g>
                    )
                  })}
                </g>
              )
            })}
          </g>
        </svg>
      )}

      {active && activeRow && activeSeg && (
        <ChartTooltip
          x={activeSeg.x + activeSeg.w / 2}
          y={active.row * pitch + labelBand + BAR_H / 2}
          containerWidth={width}
          title={activeRow.area}
        >
          <div className="space-y-1 whitespace-nowrap">
            {TIERS.map((t) => (
              <div key={t.key} className={t.key === active.tier ? '' : 'opacity-60'}>
                <TooltipRow color={t.color} value={`${activeRow[t.key]}%`} label={t.short} />
              </div>
            ))}
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}
