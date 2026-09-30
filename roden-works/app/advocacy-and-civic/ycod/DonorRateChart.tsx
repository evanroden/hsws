'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import { chart } from '@/components/charts/tokens'
import { linearScale } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'
import { measureText } from './chartUtils'

export interface StateRate {
  state: string
  rate: number
  label: string
}

const TOP = 4
const AXIS_BAND = 30
const DOT_R = 5.5
const TICKS = [0, 25, 50, 75, 100]

export default function DonorRateChart({
  data,
  highlight,
  animate,
}: {
  data: StateRate[]
  highlight: string
  animate: boolean
}) {
  // Ranked high → low, so the highlighted state's position is its rank
  const rows = [...data].sort((a, b) => b.rate - a.rate)
  const n = rows.length

  return (
    <ChartFrame
      className="h-full"
      title="Donor registration rates by state"
      subtitle="New York consistently ranks last in organ donor designation rate."
      note={`Organ donor designation rate for the ${n} states shown, ranked high to low.`}
      table={{
        caption: 'Organ donor designation rate by state, ranked',
        columns: ['State', 'Designation rate', 'Rank'],
        rows: rows.map((r, i) => [r.label, `${r.rate}%`, `${i + 1} of ${n}`]),
      }}
    >
      <DotPlot rows={rows} highlight={highlight} animate={animate} />
    </ChartFrame>
  )
}

function DotPlot({ rows, highlight, animate }: { rows: StateRate[]; highlight: string; animate: boolean }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [active, setActive] = useState<string | null>(null)
  const [focused, setFocused] = useState<string | null>(null)

  const n = rows.length
  const compact = width > 0 && width < 440
  const labelW = compact ? 90 : 112
  const labelSize = compact ? 12 : 13
  const right = 40 // room for the value label beside a dot near 100%
  const pitch = compact ? 30 : 32
  const plotW = Math.max(0, width - labelW - right)
  const plotH = n * pitch
  const height = TOP + plotH + AXIS_BAND
  const x = linearScale([0, 100], [0, plotW])
  const drawn = animate || reduceMotion
  const top = rows[0]

  const activeRow = rows.find((r) => r.state === active) ?? null
  const activeIndex = activeRow ? rows.indexOf(activeRow) : -1

  return (
    <div ref={ref} className="relative w-full" style={{ height }}>
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Dot plot of organ donor designation rates, ranked: ${rows
            .map((r) => `${r.label} ${r.rate}%`)
            .join(', ')}. New York ranks last of these ${n} states.`}
          className="block overflow-visible"
        >
          <g transform={`translate(${labelW},${TOP})`}>
            {TICKS.map((t) => (
              <g key={t} transform={`translate(${x(t)},0)`}>
                <line y1={0} y2={plotH} stroke={t === 0 ? chart.axis : chart.grid} strokeWidth={1} shapeRendering="crispEdges" />
                {(!compact || t % 50 === 0) && (
                  <text
                    y={plotH + 19}
                    textAnchor={t === 0 ? 'start' : t === 100 ? 'end' : 'middle'}
                    fontSize={11}
                    fill={chart.text.muted}
                    style={{ fontVariantNumeric: 'tabular-nums' }}
                  >
                    {t}%
                  </text>
                )}
              </g>
            ))}

            <g role="list" aria-label="States">
              {rows.map((r, i) => {
                const isHi = r.state === highlight
                const isActive = r.state === active
                const color = isHi ? chart.copper : chart.deemph
                const cy = i * pitch + pitch / 2
                const cx = x(r.rate)
                const delay = reduceMotion ? 0 : 0.15 + i * 0.06

                // Direct labels only where the story is: the leader and the last-place state.
                let note: { value: string; text: string } | null = null
                if (isHi) {
                  const room = plotW + right - (cx + DOT_R + 10) - measureText(`${r.rate}%`, 13, 600)
                  const variants = [` · ranks last of these ${n} states`, ' · ranks last', ' · last']
                  const text = variants.find((v) => measureText(v, 12) <= room) ?? ''
                  note = { value: `${r.rate}%`, text }
                } else if (r === top) {
                  note = { value: `${r.rate}%`, text: '' }
                }

                return (
                  <g
                    key={r.state}
                    role="listitem"
                    tabIndex={0}
                    aria-label={`${r.label}: ${r.rate}% donor designation rate, rank ${i + 1} of ${n}.`}
                    onPointerEnter={() => setActive(r.state)}
                    onPointerLeave={() => setActive(null)}
                    onFocus={() => {
                      setActive(r.state)
                      setFocused(r.state)
                    }}
                    onBlur={() => {
                      setActive(null)
                      setFocused(null)
                    }}
                    className="outline-none"
                  >
                    <rect x={-labelW} y={i * pitch} width={labelW + plotW + right} height={pitch} fill="transparent" />
                    {(isActive || focused === r.state) && (
                      <rect
                        x={-labelW + 2}
                        y={i * pitch + 1}
                        width={labelW + plotW + right - 4}
                        height={pitch - 2}
                        rx={6}
                        fill={isActive ? 'rgba(255,255,255,0.035)' : 'none'}
                        stroke={focused === r.state ? 'rgba(255,255,255,0.35)' : 'none'}
                        strokeWidth={1}
                      />
                    )}

                    <text
                      x={-12}
                      y={cy}
                      dy="0.35em"
                      textAnchor="end"
                      fontSize={labelSize}
                      fontWeight={isHi ? 600 : 400}
                      fill={isHi || isActive ? chart.text.primary : chart.text.secondary}
                    >
                      {r.label}
                    </text>

                    {/* Stem from 0 to the dot, then the dot with a surface ring */}
                    <motion.line
                      x1={0}
                      y1={cy}
                      y2={cy}
                      stroke={color}
                      strokeOpacity={0.45}
                      strokeWidth={2}
                      strokeLinecap="round"
                      initial={{ x2: 0 }}
                      animate={drawn ? { x2: Math.max(0, cx - DOT_R) } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.8, delay, ease: [0.16, 1, 0.3, 1] }}
                    />
                    <motion.circle
                      cy={cy}
                      r={isActive ? DOT_R + 1 : DOT_R}
                      fill={color}
                      stroke={chart.surface}
                      strokeWidth={2}
                      initial={{ cx: 0, opacity: reduceMotion ? 1 : 0 }}
                      animate={drawn ? { cx, opacity: 1 } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.8, delay, ease: [0.16, 1, 0.3, 1] }}
                    />

                    {note && (
                      <motion.text
                        x={cx + DOT_R + 10}
                        y={cy}
                        dy="0.35em"
                        fontSize={13}
                        initial={{ opacity: reduceMotion ? 1 : 0 }}
                        animate={drawn ? { opacity: 1 } : {}}
                        transition={{ duration: reduceMotion ? 0 : 0.4, delay: reduceMotion ? 0 : 0.9 + i * 0.03 }}
                      >
                        <tspan fontWeight={600} fill={isHi ? chart.text.primary : chart.text.secondary}>
                          {note.value}
                        </tspan>
                        {note.text && (
                          <tspan fontSize={12} fill={chart.text.muted}>
                            {note.text}
                          </tspan>
                        )}
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
          x={labelW + x(activeRow.rate)}
          y={TOP + activeIndex * pitch + pitch / 2}
          containerWidth={width}
          title={activeRow.label}
        >
          <div className="whitespace-nowrap space-y-1">
          <TooltipRow
            color={activeRow.state === highlight ? chart.copper : chart.deemph}
            value={`${activeRow.rate}%`}
            label="donor designation rate"
          />
          <TooltipRow value={`${activeIndex + 1} of ${n}`} label="rank among these states" />
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}
