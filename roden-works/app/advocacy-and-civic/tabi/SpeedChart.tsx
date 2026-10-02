'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import SegmentedControl from '@/components/ui/SegmentedControl'
import { chart } from '@/components/charts/tokens'
import { linearScale, niceMax, niceTicks } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'
import { barPath, measureText } from './chartUtils'

export interface SpeedTier {
  label: string
  down: number
  up: number
  adequate: boolean
}

type Metric = 'down' | 'up'

const BAR_H = 20
const AXIS_BAND = 30

export default function SpeedChart({ data, highlight, animate }: { data: SpeedTier[]; highlight: string; animate: boolean }) {
  const [metric, setMetric] = useState<Metric>('down')
  const hi = data.find((d) => d.label === highlight)
  const fcc = data.find((d) => d.label.startsWith('FCC'))

  return (
    <ChartFrame
      title={metric === 'down' ? 'Download speed, Mbps' : 'Upload speed, Mbps'}
      subtitle={
        hi && fcc
          ? `Rural Aurora averages ${hi.down}/${hi.up} Mbps, below the FCC's ${fcc.down}/${fcc.up} Mbps broadband minimum on both download and upload.`
          : undefined
      }
      actions={
        <SegmentedControl<Metric>
          label="Speed direction"
          value={metric}
          onChange={setMetric}
          options={[
            { value: 'down', label: 'Download' },
            { value: 'up', label: 'Upload' },
          ]}
        />
      }
      note="Linear scale, megabits per second. Adequacy assessments are in the tooltip and table view."
      table={{
        caption: 'Download and upload speeds by tier, Mbps',
        columns: ['Tier', 'Download', 'Upload', 'Assessment'],
        rows: data.map((d) => [d.label, `${d.down} Mbps`, `${d.up} Mbps`, d.adequate ? 'Adequate' : 'Inadequate']),
      }}
    >
      <Bars data={data} metric={metric} highlight={highlight} animate={animate} />
    </ChartFrame>
  )
}

function Bars({ data, metric, highlight, animate }: { data: SpeedTier[]; metric: Metric; highlight: string; animate: boolean }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [active, setActive] = useState<number | null>(null)
  const [focused, setFocused] = useState<number | null>(null)

  const compact = width > 0 && width < 560
  const labelW = compact ? 0 : 200
  const labelBand = compact ? 22 : 0
  const right = 16
  const pitch = compact ? 60 : 52
  const plotW = Math.max(0, width - labelW - right)
  const plotH = data.length * pitch
  const height = plotH + AXIS_BAND
  const max = niceMax(Math.max(...data.map((d) => d[metric])), 4)
  const x = linearScale([0, max], [0, plotW])
  const ticks = niceTicks(0, max, compact ? 2 : 8)
  const drawn = animate || reduceMotion
  const activeRow = active !== null ? data[active] : null

  return (
    <div ref={ref} className="relative w-full" style={{ height }}>
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Bar chart of ${metric === 'down' ? 'download' : 'upload'} speed: ${data
            .map((d) => `${d.label} ${d[metric]} Mbps`)
            .join(', ')}.`}
          className="block overflow-visible"
        >
          <g transform={`translate(${labelW},0)`}>
            {ticks.map((t) => (
              <g key={t} transform={`translate(${x(t)},0)`}>
                <line y1={0} y2={plotH} stroke={t === 0 ? chart.axis : chart.grid} strokeWidth={1} shapeRendering="crispEdges" />
                <text
                  y={plotH + 19}
                  textAnchor={t === 0 ? 'start' : t === max ? 'end' : 'middle'}
                  fontSize={11}
                  fill={chart.text.muted}
                  style={{ fontVariantNumeric: 'tabular-nums' }}
                >
                  {t === max ? `${t} Mbps` : t}
                </text>
              </g>
            ))}

            <g role="list" aria-label="Speed tiers">
              {data.map((d, i) => {
                const isHi = d.label === highlight
                const isActive = active === i
                const top = i * pitch
                const barY = top + labelBand + (pitch - labelBand - BAR_H) / 2
                const cy = barY + BAR_H / 2
                const w = x(d[metric])
                const value = `${d[metric]} Mbps`
                const outside = w + 8 + measureText(value, 12, 600) <= plotW + right
                const path = barPath(0, barY, w, BAR_H, 0, 4)
                return (
                  <g
                    key={d.label}
                    role="listitem"
                    tabIndex={0}
                    aria-label={`${d.label}: ${d.down} Mbps down, ${d.up} Mbps up, ${d.adequate ? 'adequate' : 'inadequate'}.`}
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
                    {focused === i && (
                      <rect x={-labelW + 2} y={top + 2} width={labelW + plotW + right - 4} height={pitch - 4} rx={8} fill="none" stroke="rgba(255,255,255,0.35)" strokeWidth={1} />
                    )}
                    <text
                      x={compact ? 0 : -14}
                      y={compact ? top + 15 : cy}
                      dy={compact ? 0 : '0.35em'}
                      textAnchor={compact ? 'start' : 'end'}
                      fontSize={13}
                      fontWeight={isHi ? 600 : 400}
                      fill={isHi || isActive ? chart.text.primary : chart.text.secondary}
                    >
                      {d.label}
                    </text>
                    <motion.path
                      initial={{ d: barPath(0, barY, 0, BAR_H, 0, 4) }}
                      animate={drawn ? { d: path } : {}}
                      transition={{ duration: reduceMotion ? 0 : 0.8, delay: reduceMotion ? 0 : 0.1 + i * 0.08, ease: [0.16, 1, 0.3, 1] }}
                      fill={isHi ? chart.copper : chart.deemph}
                    />
                    {isActive && <path d={path} fill="#FFFFFF" fillOpacity={0.12} />}
                    <text
                      x={outside ? w + 8 : w - 8}
                      y={cy}
                      dy="0.35em"
                      textAnchor={outside ? 'start' : 'end'}
                      fontSize={12}
                      fontWeight={600}
                      fill={outside ? (isHi ? chart.text.primary : chart.text.secondary) : isHi ? '#0B1215' : chart.text.primary}
                      style={{ fontVariantNumeric: 'tabular-nums' }}
                    >
                      {value}
                    </text>
                  </g>
                )
              })}
            </g>
          </g>
        </svg>
      )}

      {activeRow && active !== null && (
        <ChartTooltip
          x={labelW + x(activeRow[metric])}
          y={active * pitch + labelBand + (pitch - labelBand) / 2}
          containerWidth={width}
          title={activeRow.label}
        >
          <div className="space-y-1 whitespace-nowrap">
            <TooltipRow color={activeRow.label === highlight ? chart.copper : chart.deemph} value={`${activeRow.down} Mbps`} label="download" />
            <TooltipRow value={`${activeRow.up} Mbps`} label="upload" />
            <div className="mt-1 border-t border-white/10 pt-1 text-xs text-muted">
              {activeRow.adequate ? 'Adequate' : 'Inadequate'}
            </div>
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}
