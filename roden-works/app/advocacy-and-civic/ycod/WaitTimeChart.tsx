'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import { chart } from '@/components/charts/tokens'
import { linearScale } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'
import { barPath, fmt, measureText } from './chartUtils'

export interface WaitRow {
  group: string
  waitDays: number
  pctWaitlist: number
  pctDonors: number
}

interface WaitTimeChartProps {
  data: WaitRow[]
  /** The group the story is about — drawn in the accent, the rest in gray */
  highlight: string
  /** Group the tooltip multiplier is measured against */
  reference: string
  animate: boolean
}

const DOMAIN_MAX = 1500
const BAR_H = 20
const TOP = 4
const AXIS_BAND = 30

export default function WaitTimeChart({ data, highlight, reference, animate }: WaitTimeChartProps) {
  const rows = [...data].sort((a, b) => b.waitDays - a.waitDays)
  const hi = rows.find((r) => r.group === highlight)
  const ref = rows.find((r) => r.group === reference)

  return (
    <ChartFrame
      className="h-full"
      title="Racial disparities in transplant waiting times"
      subtitle="60% of all waitlisted patients are people of color. Black Americans make up 27% of the waiting list but only 13% of donors."
      note="Average wait for a kidney transplant, by race and ethnicity."
      table={{
        caption: 'Average kidney transplant wait, and share of the waitlist and of donors, by group',
        columns: ['Group', 'Avg. kidney wait', 'Share of waitlist', 'Share of donors'],
        rows: rows.map((r) => [r.group, `${fmt(r.waitDays)} days`, `${r.pctWaitlist}%`, `${r.pctDonors}%`]),
      }}
    >
      <Bars rows={rows} highlight={highlight} reference={ref} animate={animate} />
      {hi && ref && (
        <p className="sr-only">
          {`${hi.group} patients wait an average of ${fmt(hi.waitDays)} days, ${(hi.waitDays / ref.waitDays).toFixed(1)} times the ${fmt(ref.waitDays)}-day wait for ${ref.group} patients.`}
        </p>
      )}
    </ChartFrame>
  )
}

function Bars({
  rows,
  highlight,
  reference,
  animate,
}: {
  rows: WaitRow[]
  highlight: string
  reference?: WaitRow
  animate: boolean
}) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [active, setActive] = useState<string | null>(null)
  const [focused, setFocused] = useState<string | null>(null)

  const compact = width > 0 && width < 440
  const labelW = compact ? 70 : 84
  const right = 10
  const pitch = compact ? 46 : 52
  const plotW = Math.max(0, width - labelW - right)
  const plotH = rows.length * pitch
  const height = TOP + plotH + AXIS_BAND

  const x = linearScale([0, DOMAIN_MAX], [0, plotW])
  // Hairline every 250 days on wide layouts; labels every 500 so they never crowd
  const gridStep = compact ? 500 : 250
  const ticks = Array.from({ length: DOMAIN_MAX / gridStep + 1 }, (_, i) => i * gridStep)
  const drawn = animate || reduceMotion

  const activeRow = rows.find((r) => r.group === active) ?? null
  const activeIndex = activeRow ? rows.indexOf(activeRow) : -1

  return (
    <div ref={ref} className="relative w-full" style={{ height }}>
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Bar chart of average kidney transplant wait by group: ${rows
            .map((r) => `${r.group} ${fmt(r.waitDays)} days`)
            .join(', ')}.`}
          className="block overflow-visible"
        >
          <g transform={`translate(${labelW},${TOP})`}>
            {/* Day axis: hairline grid + ticks */}
            {ticks.map((t) => (
              <g key={t} transform={`translate(${x(t)},0)`}>
                <line
                  y1={0}
                  y2={plotH}
                  stroke={t === 0 ? chart.axis : chart.grid}
                  strokeWidth={1}
                  shapeRendering="crispEdges"
                />
                {t % 500 === 0 && (
                  <text
                    y={plotH + 19}
                    textAnchor={t === DOMAIN_MAX ? 'end' : t === 0 ? 'start' : 'middle'}
                    fontSize={11}
                    fill={chart.text.muted}
                    style={{ fontVariantNumeric: 'tabular-nums' }}
                  >
                    {fmt(t)}
                  </text>
                )}
              </g>
            ))}

            <g role="list" aria-label="Groups">
              {rows.map((r, i) => {
                const isHi = r.group === highlight
                const isActive = r.group === active
                const fill = isHi ? chart.copper : chart.deemph
                const rowTop = i * pitch
                const barY = rowTop + (pitch - BAR_H) / 2
                const barW = x(r.waitDays)
                const cy = rowTop + pitch / 2

                // Value label at the bar tip; tucked inside the bar end only when it
                // can't fit outside, and only if the bar is wide enough to hold it.
                // The first (longest) bar carries the unit; the rest are plain numbers
                const label = i === 0 ? `${fmt(r.waitDays)} days` : fmt(r.waitDays)
                const tw = measureText(label, 12, 600)
                const outside = barW + 8 + tw <= plotW + right
                const inside = !outside && barW >= tw + 16
                const insideInk = isHi ? '#0B1215' : chart.text.primary

                return (
                  <g
                    key={r.group}
                    role="listitem"
                    tabIndex={0}
                    aria-label={`${r.group}: average kidney wait ${fmt(r.waitDays)} days; ${r.pctWaitlist}% of the waitlist, ${r.pctDonors}% of donors.`}
                    onPointerEnter={() => setActive(r.group)}
                    onPointerLeave={() => setActive(null)}
                    onFocus={() => {
                      setActive(r.group)
                      setFocused(r.group)
                    }}
                    onBlur={() => {
                      setActive(null)
                      setFocused(null)
                    }}
                    className="outline-none"
                  >
                    {/* Hit area: the whole row, label included */}
                    <rect x={-labelW} y={rowTop} width={labelW + plotW + right} height={pitch} fill="transparent" />
                    {focused === r.group && (
                      <rect
                        x={-labelW + 2}
                        y={rowTop + 3}
                        width={labelW + plotW + right - 4}
                        height={pitch - 6}
                        rx={8}
                        fill="none"
                        stroke="rgba(255,255,255,0.35)"
                        strokeWidth={1}
                      />
                    )}

                    <text
                      x={-12}
                      y={cy}
                      dy="0.35em"
                      textAnchor="end"
                      fontSize={13}
                      fontWeight={isHi ? 600 : 400}
                      fill={isHi || isActive ? chart.text.primary : chart.text.secondary}
                    >
                      {r.group}
                    </text>

                    <motion.path
                      initial={{ d: barPath(0, barY, 0, BAR_H, 0, 4) }}
                      animate={drawn ? { d: barPath(0, barY, barW, BAR_H, 0, 4) } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.9, delay: reduceMotion ? 0 : 0.1 + i * 0.08, ease: [0.16, 1, 0.3, 1] }}
                      fill={fill}
                    />
                    {isActive && <path d={barPath(0, barY, barW, BAR_H, 0, 4)} fill="#FFFFFF" fillOpacity={0.12} />}

                    {(outside || inside) && (
                      <motion.text
                        x={outside ? barW + 8 : barW - 8}
                        y={cy}
                        dy="0.35em"
                        textAnchor={outside ? 'start' : 'end'}
                        fontSize={12}
                        fontWeight={600}
                        fill={outside ? (isHi ? chart.text.primary : chart.text.secondary) : insideInk}
                        initial={{ opacity: reduceMotion ? 1 : 0 }}
                        animate={drawn ? { opacity: 1 } : {}}
                        transition={{ duration: reduceMotion ? 0 : 0.4, delay: reduceMotion ? 0 : 0.7 + i * 0.08 }}
                      >
                        {label}
                      </motion.text>
                    )}
                  </g>
                )
              })}
            </g>
          </g>
        </svg>
      )}

      {activeRow && (
        <ChartTooltip
          x={labelW + x(activeRow.waitDays)}
          y={TOP + activeIndex * pitch + pitch / 2}
          containerWidth={width}
          title={activeRow.group}
        >
          <div className="whitespace-nowrap space-y-1">
          <TooltipRow
            color={activeRow.group === highlight ? chart.copper : chart.deemph}
            value={`${fmt(activeRow.waitDays)} days`}
            label="avg. kidney wait"
          />
          {reference && activeRow.group !== reference.group && (
            <TooltipRow
              value={`${(activeRow.waitDays / reference.waitDays).toFixed(1)}×`}
              label={`the ${reference.group} wait`}
            />
          )}
          <div className="pt-1 mt-1 border-t border-white/10 space-y-1">
            <TooltipRow value={`${activeRow.pctWaitlist}%`} label="of the waitlist" />
            <TooltipRow value={`${activeRow.pctDonors}%`} label="of donors" />
          </div>
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}
