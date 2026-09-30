'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useMemo, useState, type KeyboardEvent, type PointerEvent } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import { chart } from '@/components/charts/tokens'
import { linearScale } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'

export interface FiringStage {
  stage: string
  temperature: string
  duration: string
  description: string
}

/*
 * Representative full-fuse curve built from the stage table on this page, using
 * the midpoint of each stated duration: ramp 70→1000°F (~3 h at ~300°F/h),
 * rapid heat to 1480°F (1.5 h), 20-min soak to 1500°F, crash to the 960°F
 * anneal point and hold (1.5 h total), then a 10-hour controlled cool-down
 * at ≤50°F/h through the strain range before cooling naturally.
 */
const SEGMENTS: { stage: number; points: [number, number][] }[] = [
  { stage: 1, points: [[0, 70], [3, 1000]] },
  { stage: 2, points: [[3, 1000], [4.5, 1480]] },
  { stage: 3, points: [[4.5, 1480], [4.83, 1500]] },
  { stage: 4, points: [[4.83, 1500], [5.33, 960], [6.33, 960]] },
  {
    stage: 5,
    points: (() => {
      const pts: [number, number][] = [[6.33, 960], [11.53, 700]] // 50°F/h through the strain range
      // then natural (exponential) cooling toward room temperature by hour ~16.3
      for (let i = 1; i <= 8; i++) {
        const t = 11.53 + (i / 8) * 4.8
        pts.push([t, 70 + 630 * Math.exp(-3.2 * (i / 8))])
      }
      return pts
    })(),
  },
]

const HOURS = 16.5
const REFERENCES = [
  { temp: 1480, label: 'Full fuse 1480–1500°F' },
  { temp: 1380, label: 'Tack fuse 1380°F' },
  { temp: 960, label: 'Anneal 960°F' },
]

const HEIGHT = 340

function tempAt(hour: number) {
  for (const seg of SEGMENTS) {
    for (let i = 0; i < seg.points.length - 1; i++) {
      const [t0, v0] = seg.points[i]
      const [t1, v1] = seg.points[i + 1]
      if (hour >= t0 && hour <= t1) return { temp: v0 + ((hour - t0) / (t1 - t0 || 1)) * (v1 - v0), stage: seg.stage }
    }
  }
  return { temp: 70, stage: 5 }
}

export default function FiringScheduleChart({ stages }: { stages: FiringStage[] }) {
  // stages[0] is design & layout (out of the kiln); the curve covers stages[1..5]
  const [active, setActive] = useState<number>(3)
  const kilnStages = stages.slice(1)

  return (
    <ChartFrame
      title="A representative full-fuse firing curve"
      subtitle="Kiln temperature over one cycle, °F. Select a stage to see what happens inside the kiln."
      note="Built from the stage table on this page using the midpoint of each duration. Real schedules vary with glass thickness, layup, and kiln."
      table={{
        caption: 'Firing schedule stages',
        columns: ['Stage', 'Temperature', 'Duration'],
        rows: stages.map((s) => [s.stage, s.temperature, s.duration]),
      }}
    >
      <Curve active={active} onSelect={setActive} labels={kilnStages.map((s) => s.stage)} />

      {/* Stage selector + detail */}
      <div className="mt-6 grid grid-cols-1 lg:grid-cols-5 gap-6">
        <div className="lg:col-span-2 flex flex-wrap gap-2 content-start" role="group" aria-label="Firing stages">
          {stages.map((s, i) => (
            <button
              key={s.stage}
              type="button"
              onClick={() => setActive(i)}
              aria-pressed={active === i}
              className={`rounded-lg border px-3 py-1.5 text-xs font-medium transition-colors ${
                active === i
                  ? 'border-copper/40 bg-copper/15 text-copper-light'
                  : 'border-white/10 text-titanium hover:border-white/25 hover:text-white'
              }`}
            >
              <span className="mr-1.5 font-mono text-faint">{String(i + 1).padStart(2, '0')}</span>
              {s.stage}
            </button>
          ))}
        </div>
        <motion.div
          key={active}
          initial={{ opacity: 0, y: 6 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.25 }}
          className="lg:col-span-3 rounded-xl border border-white/[0.06] bg-white/[0.02] p-5"
          aria-live="polite"
        >
          <div className="flex flex-wrap items-baseline gap-x-5 gap-y-1">
            <h4 className="font-serif text-xl text-white">{stages[active].stage}</h4>
            <span className="font-mono text-xs text-copper-light">{stages[active].temperature}</span>
            <span className="font-mono text-xs text-muted">{stages[active].duration}</span>
          </div>
          <p className="mt-3 text-sm text-titanium leading-relaxed">{stages[active].description}</p>
          {active === 0 && <p className="mt-3 text-xs text-muted">Happens at the bench, before the kiln is switched on.</p>}
        </motion.div>
      </div>
    </ChartFrame>
  )
}

function Curve({ active, onSelect, labels }: { active: number; onSelect: (i: number) => void; labels: string[] }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [hour, setHour] = useState<number | null>(null)

  const compact = width > 0 && width < 640
  const margin = { top: 16, right: compact ? 12 : 150, bottom: 36, left: 52 }
  const innerW = Math.max(0, width - margin.left - margin.right)
  const innerH = HEIGHT - margin.top - margin.bottom
  const x = useMemo(() => linearScale([0, HOURS], [0, innerW]), [innerW])
  const y = useMemo(() => linearScale([0, 1650], [innerH, 0]), [innerH])

  const path = (pts: [number, number][]) => pts.map(([t, v], i) => `${i === 0 ? 'M' : 'L'}${x(t).toFixed(1)},${y(v).toFixed(1)}`).join('')
  const allPoints = SEGMENTS.flatMap((s, i) => (i === 0 ? s.points : s.points.slice(1)))
  const area = `${path(allPoints)}L${x(allPoints[allPoints.length - 1][0])},${y(0)}L${x(0)},${y(0)}Z`

  const onMove = (e: PointerEvent<SVGRectElement>) => {
    const rect = e.currentTarget.getBoundingClientRect()
    setHour(Math.min(allPoints[allPoints.length - 1][0], Math.max(0, x.invert(e.clientX - rect.left))))
  }
  const onKey = (e: KeyboardEvent<SVGSVGElement>) => {
    if (e.key === 'ArrowRight' || e.key === 'ArrowLeft') {
      e.preventDefault()
      onSelect(Math.min(5, Math.max(1, (active || 1) + (e.key === 'ArrowRight' ? 1 : -1))))
    }
  }
  const readout = hour === null ? null : tempAt(hour)

  return (
    <div ref={ref} className="relative w-full" style={{ height: HEIGHT }}>
      {width > 0 && (
        <svg
          width={width}
          height={HEIGHT}
          role="img"
          tabIndex={0}
          onKeyDown={onKey}
          aria-label="Line chart: kiln temperature rises from 70°F to a 1500°F fuse peak around hour 5, drops to a 960°F anneal hold, then cools to room temperature by about hour 16."
          className="block overflow-visible outline-none"
        >
          <g transform={`translate(${margin.left},${margin.top})`}>
            {[0, 400, 800, 1200, 1600].map((t) => (
              <g key={t} transform={`translate(0,${y(t)})`}>
                <line x2={innerW} stroke={t === 0 ? chart.axis : chart.grid} shapeRendering="crispEdges" />
                <text x={-12} dy="0.32em" textAnchor="end" fontSize={11} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
                  {t === 0 ? '0' : `${t.toLocaleString()}°`}
                </text>
              </g>
            ))}
            {[0, 2, 4, 6, 8, 10, 12, 14, 16].filter((h) => !compact || h % 4 === 0).map((h) => (
              <text key={h} x={x(h)} y={innerH + 22} textAnchor="middle" fontSize={11} fill={chart.text.muted}>
                {h === 0 ? '0 h' : `${h}`}
              </text>
            ))}

            {/* Reference temperatures */}
            {REFERENCES.map((r) => (
              <g key={r.label} transform={`translate(0,${y(r.temp)})`}>
                <line x2={innerW} stroke="rgba(208,140,79,0.28)" shapeRendering="crispEdges" />
                {compact ? (
                  // On narrow screens the label rides just above its line, inside the plot
                  <text x={innerW} dy="-0.45em" textAnchor="end" fontSize={10} fill={chart.text.secondary}>
                    {r.label}
                  </text>
                ) : (
                  <text x={innerW + 10} dy="0.32em" fontSize={11} fill={chart.text.secondary}>
                    {r.label}
                  </text>
                )}
              </g>
            ))}

            <motion.path
              d={area}
              fill={chart.copper}
              initial={{ opacity: 0 }}
              whileInView={{ opacity: 0.12 }}
              viewport={{ once: true }}
              transition={{ duration: reduceMotion ? 0 : 1, delay: reduceMotion ? 0 : 0.6 }}
            />
            {SEGMENTS.map((seg) => {
              const on = seg.stage === active
              return (
                <motion.path
                  key={seg.stage}
                  d={path(seg.points)}
                  fill="none"
                  stroke={chart.copper}
                  strokeWidth={on ? 3 : 2}
                  strokeOpacity={active === 0 || on ? 1 : 0.72}
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  initial={{ pathLength: reduceMotion ? 1 : 0 }}
                  whileInView={{ pathLength: 1 }}
                  viewport={{ once: true }}
                  transition={{ duration: reduceMotion ? 0 : 0.5, delay: reduceMotion ? 0 : 0.2 + seg.stage * 0.18, ease: 'easeOut' }}
                  style={{ transition: 'stroke-opacity 0.3s, stroke-width 0.3s' }}
                />
              )
            })}

            {/* Clickable stage bands */}
            {SEGMENTS.map((seg) => {
              const t0 = seg.points[0][0]
              const t1 = seg.points[seg.points.length - 1][0]
              const on = seg.stage === active
              return (
                <g key={`band-${seg.stage}`}>
                  <rect
                    x={x(t0)}
                    y={0}
                    width={Math.max(2, x(t1) - x(t0))}
                    height={innerH}
                    fill={on ? 'rgba(184,115,51,0.06)' : 'transparent'}
                  />
                  <line x1={x(t1)} x2={x(t1)} y1={0} y2={innerH} stroke={chart.grid} shapeRendering="crispEdges" />
                </g>
              )
            })}

            {readout && hour !== null && (
              <g pointerEvents="none">
                <line x1={x(hour)} x2={x(hour)} y1={0} y2={innerH} stroke="rgba(255,255,255,0.22)" shapeRendering="crispEdges" />
                <circle cx={x(hour)} cy={y(readout.temp)} r={4.5} fill={chart.copper} stroke={chart.surface} strokeWidth={2} />
              </g>
            )}

            <rect
              width={innerW}
              height={innerH}
              fill="transparent"
              onPointerMove={onMove}
              onPointerLeave={() => setHour(null)}
              onClick={() => readout && onSelect(readout.stage)}
              style={{ cursor: 'pointer' }}
            />
          </g>
        </svg>
      )}
      {readout && hour !== null && (
        <ChartTooltip x={margin.left + x(hour)} y={margin.top + y(readout.temp)} containerWidth={width} title={`Hour ${hour.toFixed(1)}`}>
          <TooltipRow color={chart.copper} value={`${Math.round(readout.temp).toLocaleString()}°F`} label="kiln temperature" />
          <TooltipRow value={labels[readout.stage - 1]} label="" />
        </ChartTooltip>
      )}
    </div>
  )
}
