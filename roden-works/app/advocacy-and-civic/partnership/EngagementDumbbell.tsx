'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import Legend from '@/components/charts/Legend'
import { chart } from '@/components/charts/tokens'
import { linearScale } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'

export interface EngagementRow {
  category: string
  before: number
  after: number
}

const TICKS = [0, 25, 50, 75, 100]
const DOT_R = 6
const AXIS_BAND = 30

export default function EngagementDumbbell({ data, animate }: { data: EngagementRow[]; animate: boolean }) {
  return (
    <ChartFrame
      title="SAMHSA engagement scores, before and after the Partnership collaboration"
      subtitle="Score out of 100 for each measured category. Every category improved."
      legend={
        <Legend
          items={[
            { label: 'Before Partnership engagement', color: chart.deemph, shape: 'dot' },
            { label: 'After Partnership engagement', color: chart.verdigris, shape: 'dot' },
          ]}
        />
      }
      note="Source: Best Places to Work in the Federal Government"
      table={{
        caption: 'SAMHSA engagement scores before and after, by category',
        columns: ['Category', 'Before', 'After', 'Change'],
        rows: data.map((d) => [d.category, d.before, d.after, `+${d.after - d.before}`]),
      }}
    >
      <Plot data={data} animate={animate} />
    </ChartFrame>
  )
}

function Plot({ data, animate }: { data: EngagementRow[]; animate: boolean }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [active, setActive] = useState<number | null>(null)
  const [focused, setFocused] = useState<number | null>(null)

  // On narrow screens the category label sits above its row instead of beside it
  const compact = width > 0 && width < 640
  const labelW = compact ? 0 : 196
  const right = compact ? 44 : 64 // room for the "+N" delta beyond the after dot
  const labelBand = compact ? 22 : 0
  const pitch = compact ? 64 : 58
  const plotW = Math.max(0, width - labelW - right)
  const plotH = data.length * pitch
  const height = plotH + AXIS_BAND
  const x = linearScale([0, 100], [0, plotW])
  const drawn = animate || reduceMotion
  const ease = [0.16, 1, 0.3, 1] as const

  const activeRow = active !== null ? data[active] : null

  return (
    <div ref={ref} className="relative w-full" style={{ height }}>
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Dumbbell chart of engagement scores: ${data
            .map((d) => `${d.category} from ${d.before} to ${d.after}`)
            .join('; ')}.`}
          className="block overflow-visible"
        >
          <g transform={`translate(${labelW},0)`}>
            {TICKS.map((t) => (
              <g key={t} transform={`translate(${x(t)},0)`}>
                <line y1={0} y2={plotH} stroke={t === 0 ? chart.axis : chart.grid} strokeWidth={1} shapeRendering="crispEdges" />
                <text
                  y={plotH + 19}
                  textAnchor={t === 0 ? 'start' : t === 100 ? 'end' : 'middle'}
                  fontSize={11}
                  fill={chart.text.muted}
                  style={{ fontVariantNumeric: 'tabular-nums' }}
                >
                  {t}
                </text>
              </g>
            ))}

            <g role="list" aria-label="Categories">
              {data.map((d, i) => {
                const top = i * pitch
                const cy = top + labelBand + (pitch - labelBand) / 2
                const xb = x(d.before)
                const xa = x(d.after)
                const isActive = active === i
                const delay = reduceMotion ? 0 : 0.15 + i * 0.1
                const delta = d.after - d.before
                return (
                  <g
                    key={d.category}
                    role="listitem"
                    tabIndex={0}
                    aria-label={`${d.category}: ${d.before} before, ${d.after} after, up ${delta} points.`}
                    onPointerEnter={() => setActive(i)}
                    onPointerLeave={() => setActive(null)}
                    onFocus={() => {
                      setActive(i)
                      setFocused(i)
                    }}
                    onBlur={() => {
                      setActive(null)
                      setFocused(null)
                    }}
                    className="outline-none"
                  >
                    <rect x={-labelW} y={top} width={labelW + plotW + right} height={pitch} fill="transparent" />
                    {(isActive || focused === i) && (
                      <rect
                        x={-labelW + 2}
                        y={top + 2}
                        width={labelW + plotW + right - 4}
                        height={pitch - 4}
                        rx={8}
                        fill={isActive ? 'rgba(255,255,255,0.03)' : 'none'}
                        stroke={focused === i ? 'rgba(255,255,255,0.35)' : 'none'}
                        strokeWidth={1}
                      />
                    )}

                    <text
                      x={compact ? 0 : -16}
                      y={compact ? top + 14 : cy}
                      dy={compact ? 0 : '0.35em'}
                      textAnchor={compact ? 'start' : 'end'}
                      fontSize={13}
                      fontWeight={i === 0 ? 600 : 400}
                      fill={isActive || i === 0 ? chart.text.primary : chart.text.secondary}
                    >
                      {d.category}
                    </text>

                    {/* Connector grows from the before dot to the after dot */}
                    <motion.line
                      y1={cy}
                      y2={cy}
                      x1={xb}
                      stroke={chart.verdigris}
                      strokeOpacity={0.45}
                      strokeWidth={2}
                      initial={{ x2: xb }}
                      animate={drawn ? { x2: xa } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.9, delay, ease }}
                    />
                    <circle cx={xb} cy={cy} r={DOT_R} fill={chart.deemph} stroke={chart.surface} strokeWidth={2} />
                    <motion.circle
                      cy={cy}
                      r={isActive ? DOT_R + 1 : DOT_R}
                      fill={chart.verdigris}
                      stroke={chart.surface}
                      strokeWidth={2}
                      initial={{ cx: xb }}
                      animate={drawn ? { cx: xa } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.9, delay, ease }}
                    />

                    {/* Direct labels: before value left of its dot, after value above its dot, delta at the end */}
                    <text x={xb - DOT_R - 7} y={cy} dy="0.35em" textAnchor="end" fontSize={12} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
                      {d.before}
                    </text>
                    <motion.g
                      initial={{ opacity: 0 }}
                      animate={drawn ? { opacity: 1 } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.4, delay: reduceMotion ? 0 : delay + 0.7 }}
                    >
                      <text x={xa + DOT_R + 7} y={cy} dy="0.35em" fontSize={12} fontWeight={600} fill={chart.text.primary} style={{ fontVariantNumeric: 'tabular-nums' }}>
                        {d.after}
                      </text>
                      <text
                        x={plotW + right}
                        y={cy}
                        dy="0.35em"
                        textAnchor="end"
                        fontSize={12}
                        fontWeight={600}
                        fill={chart.text.secondary}
                        style={{ fontVariantNumeric: 'tabular-nums' }}
                      >
                        +{delta}
                      </text>
                    </motion.g>
                  </g>
                )
              })}
            </g>
          </g>
        </svg>
      )}

      {activeRow && active !== null && (
        <ChartTooltip
          x={labelW + x(activeRow.after)}
          y={active * pitch + labelBand + (pitch - labelBand) / 2}
          containerWidth={width}
          title={activeRow.category}
        >
          <div className="space-y-1 whitespace-nowrap">
            <TooltipRow color={chart.verdigris} value={activeRow.after} label="after" />
            <TooltipRow color={chart.deemph} value={activeRow.before} label="before" />
            <div className="mt-1 border-t border-white/10 pt-1">
              <TooltipRow value={`+${activeRow.after - activeRow.before} pts`} label={`(${(activeRow.after / activeRow.before).toFixed(1)}× the starting score)`} />
            </div>
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}
