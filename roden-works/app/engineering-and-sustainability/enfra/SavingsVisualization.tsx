'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useMemo, useState, type KeyboardEvent, type PointerEvent } from 'react'
import { useInView } from '@/lib/hooks'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import Legend from '@/components/charts/Legend'
import SegmentedControl from '@/components/ui/SegmentedControl'
import { chart } from '@/components/charts/tokens'
import { formatMillions, linearScale, niceTicks } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'

/* ─── Modeled series ────────────────────────────────────────────────────────
 * Calibrated to the announced partnership figures: $6.9M first-year savings
 * (a $14.2M baseline vs $7.3M optimized) and $354.6M over the 30-year term.
 * The optimized plant escalates at 1.6%/yr; the baseline escalation rate is
 * solved so the 30-year savings total lands exactly on $354.6M.
 */
const TERM = 30
const BASELINE_Y1 = 14.2
const OPTIMIZED_Y1 = 7.3
const OPTIMIZED_ESCALATION = 0.016
const TERM_SAVINGS = 354.6

function seriesSum(first: number, rate: number) {
  let sum = 0
  for (let t = 0; t < TERM; t++) sum += first * Math.pow(1 + rate, t)
  return sum
}

function solveBaselineEscalation() {
  const optimizedTotal = seriesSum(OPTIMIZED_Y1, OPTIMIZED_ESCALATION)
  let lo = 0
  let hi = 0.1
  for (let i = 0; i < 60; i++) {
    const mid = (lo + hi) / 2
    if (seriesSum(BASELINE_Y1, mid) - optimizedTotal > TERM_SAVINGS) hi = mid
    else lo = mid
  }
  return (lo + hi) / 2
}

const BASELINE_ESCALATION = solveBaselineEscalation()

interface YearRow {
  year: number
  baseline: number
  optimized: number
  savings: number
  cumulative: number
}

const DATA: YearRow[] = (() => {
  let cumulative = 0
  return Array.from({ length: TERM }, (_, i) => {
    const baseline = BASELINE_Y1 * Math.pow(1 + BASELINE_ESCALATION, i)
    const optimized = OPTIMIZED_Y1 * Math.pow(1 + OPTIMIZED_ESCALATION, i)
    const savings = baseline - optimized
    cumulative += savings
    return { year: i + 1, baseline, optimized, savings, cumulative }
  })
})()

type View = 'annual' | 'cumulative'

const HEIGHT = 360
const X_BAND = 36 // x-axis labels live inside the fixed height

export default function SavingsVisualization() {
  const { ref: sectionRef, isInView } = useInView(0.15)
  const [view, setView] = useState<View>('annual')

  return (
    <section className="section-padding bg-slate-950" ref={sectionRef}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 md:mb-12 max-w-3xl"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Energy Savings</span>
          <h2 className="font-serif text-heading text-white mt-3">Savings over the 30-year term.</h2>
          <p className="mt-4 text-titanium leading-relaxed">
            Without the project, the hospitals&apos; energy costs keep rising from today&apos;s baseline. The gap
            between the two curves is the guaranteed savings: $6.9 million in the first year and $354.6 million
            over the full term.
          </p>
        </motion.div>

        <motion.div
          initial={{ opacity: 0, y: 24 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.7, delay: 0.15 }}
        >
          <ChartFrame
            title={
              view === 'annual'
                ? 'Annual energy cost, with and without optimization'
                : 'Cumulative savings over the 30-year term'
            }
            subtitle={
              view === 'annual'
                ? 'Millions of dollars per year, both hospitals combined'
                : 'Running total of annual savings, millions of dollars'
            }
            actions={
              <SegmentedControl<View>
                label="Chart view"
                value={view}
                onChange={setView}
                options={[
                  { value: 'annual', label: 'Annual cost' },
                  { value: 'cumulative', label: 'Cumulative savings' },
                ]}
              />
            }
            legend={
              view === 'annual' ? (
                <Legend
                  items={[
                    { label: 'Without optimization', color: chart.deemph, shape: 'line' },
                    { label: 'With ENFRA', color: chart.verdigris, shape: 'line' },
                    { label: 'Annual savings', color: 'rgba(61,168,135,0.28)', shape: 'rect' },
                  ]}
                />
              ) : null
            }
            note={
              <>
                Modeled illustration calibrated to announced figures ($6.9M first-year and $354.6M term savings),
                assuming {(BASELINE_ESCALATION * 100).toFixed(1)}%/yr baseline and{' '}
                {(OPTIMIZED_ESCALATION * 100).toFixed(1)}%/yr optimized cost escalation. Not billing data.
              </>
            }
            table={{
              caption: 'Modeled annual energy cost and savings by contract year',
              columns: ['Year', 'Without optimization', 'With ENFRA', 'Annual savings', 'Cumulative savings'],
              rows: DATA.map((d) => [
                `Year ${d.year}`,
                formatMillions(d.baseline),
                formatMillions(d.optimized),
                formatMillions(d.savings),
                formatMillions(d.cumulative),
              ]),
            }}
          >
            <SavingsChart view={view} animate={isInView} />
          </ChartFrame>
        </motion.div>
      </div>
    </section>
  )
}

function SavingsChart({ view, animate }: { view: View; animate: boolean }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [hover, setHover] = useState<number | null>(null)

  const compact = width > 0 && width < 640
  const margin = { top: 16, right: compact ? 12 : 168, bottom: X_BAND, left: 52 }
  const innerW = Math.max(0, width - margin.left - margin.right)
  const innerH = HEIGHT - margin.top - margin.bottom

  // Modest headroom above the peak; ticks stay on clean values below it
  const yMax = view === 'annual' ? Math.max(...DATA.map((d) => d.baseline)) * 1.1 : TERM_SAVINGS * 1.08
  const x = useMemo(() => linearScale([1, TERM], [0, innerW]), [innerW])
  const y = useMemo(() => linearScale([0, yMax], [innerH, 0]), [yMax, innerH])
  const yTicks = niceTicks(0, yMax, 4).filter((t) => t <= yMax)
  const xTicks = compact ? [1, 10, 20, 30] : [1, 5, 10, 15, 20, 25, 30]

  const line = (key: keyof YearRow) =>
    DATA.map((d, i) => `${i === 0 ? 'M' : 'L'}${x(d.year).toFixed(1)},${y(d[key]).toFixed(1)}`).join('')

  const gapArea =
    DATA.map((d, i) => `${i === 0 ? 'M' : 'L'}${x(d.year).toFixed(1)},${y(d.baseline).toFixed(1)}`).join('') +
    [...DATA].reverse().map((d) => `L${x(d.year).toFixed(1)},${y(d.optimized).toFixed(1)}`).join('') +
    'Z'

  const cumulativeArea = `${line('cumulative')}L${x(TERM).toFixed(1)},${y(0)}L${x(1).toFixed(1)},${y(0)}Z`

  const drawn = animate || reduceMotion
  const draw = (delay = 0) => ({
    initial: { pathLength: reduceMotion ? 1 : 0 },
    animate: drawn ? { pathLength: 1 } : {},
    transition: { duration: reduceMotion ? 0 : 1.4, delay, ease: [0.16, 1, 0.3, 1] as const },
  })

  const setYearFromPointer = (e: PointerEvent<SVGRectElement>) => {
    const rect = e.currentTarget.getBoundingClientRect()
    const yr = Math.round(x.invert(e.clientX - rect.left))
    setHover(Math.min(TERM, Math.max(1, yr)))
  }

  const onKey = (e: KeyboardEvent<SVGSVGElement>) => {
    if (e.key === 'ArrowRight' || e.key === 'ArrowLeft') {
      e.preventDefault()
      setHover((h) => {
        const cur = h ?? (e.key === 'ArrowRight' ? 0 : TERM + 1)
        return Math.min(TERM, Math.max(1, cur + (e.key === 'ArrowRight' ? 1 : -1)))
      })
    } else if (e.key === 'Escape') setHover(null)
  }

  const last = DATA[TERM - 1]
  const hovered = hover ? DATA[hover - 1] : null

  return (
    <div ref={ref} className="relative w-full" style={{ height: HEIGHT }}>
      {width > 0 && (
        <svg
          width={width}
          height={HEIGHT}
          role="img"
          aria-label={
            view === 'annual'
              ? `Line chart: annual energy cost falls from ${formatMillions(DATA[0].baseline)} to ${formatMillions(DATA[0].optimized)} in year one; by year 30 the baseline reaches ${formatMillions(last.baseline)} versus ${formatMillions(last.optimized)} optimized.`
              : `Area chart: cumulative savings grow to ${formatMillions(TERM_SAVINGS)} over 30 years.`
          }
          tabIndex={0}
          onKeyDown={onKey}
          onBlur={() => setHover(null)}
          className="block overflow-visible outline-none focus-visible:outline-none"
        >
          <g transform={`translate(${margin.left},${margin.top})`}>
            {/* Gridlines + y ticks */}
            {yTicks.map((t) => (
              <g key={t} transform={`translate(0,${y(t)})`}>
                <line x1={0} x2={innerW} stroke={t === 0 ? chart.axis : chart.grid} strokeWidth={1} shapeRendering="crispEdges" />
                <text x={-12} dy="0.32em" textAnchor="end" fontSize={11} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
                  {t === 0 ? '$0' : `$${t}M`}
                </text>
              </g>
            ))}

            {/* X ticks */}
            {xTicks.map((t) => (
              <text key={t} x={x(t)} y={innerH + 22} textAnchor="middle" fontSize={11} fill={chart.text.muted}>
                {t === 1 ? 'Year 1' : t}
              </text>
            ))}

            {view === 'annual' ? (
              <g key="annual">
                <motion.path
                  d={gapArea}
                  fill={chart.verdigris}
                  initial={{ opacity: 0 }}
                  animate={drawn ? { opacity: chart.areaOpacity + 0.04 } : {}}
                  transition={{ duration: reduceMotion ? 0 : 0.9, delay: reduceMotion ? 0 : 0.7 }}
                />
                <motion.path d={line('baseline')} fill="none" stroke={chart.deemph} strokeWidth={2} strokeLinecap="round" strokeLinejoin="round" {...draw(0)} />
                <motion.path d={line('optimized')} fill="none" stroke={chart.verdigris} strokeWidth={2} strokeLinecap="round" strokeLinejoin="round" {...draw(0.15)} />

                {/* Savings annotation inside the gap */}
                {!compact && (
                  <text
                    x={x(19)}
                    y={(y(DATA[18].baseline) + y(DATA[18].optimized)) / 2}
                    textAnchor="middle"
                    dy="0.32em"
                    fontSize={12}
                    fill={chart.text.secondary}
                  >
                    Savings gap
                  </text>
                )}

                {/* End markers + direct labels */}
                {[
                  { v: last.baseline, c: chart.deemph, label: 'Without optimization' },
                  { v: last.optimized, c: chart.verdigris, label: 'With ENFRA' },
                ].map((e) => (
                  <g key={e.label} transform={`translate(${x(TERM)},${y(e.v)})`}>
                    <circle r={4} fill={e.c} stroke={chart.surface} strokeWidth={2} />
                    {!compact && (
                      <>
                        <text x={12} dy="-0.2em" fontSize={13} fontWeight={600} fill={chart.text.primary}>
                          {formatMillions(e.v)}
                        </text>
                        <text x={12} dy="1.1em" fontSize={11} fill={chart.text.muted}>
                          {e.label}
                        </text>
                      </>
                    )}
                  </g>
                ))}
              </g>
            ) : (
              <g key="cumulative">
                <motion.path
                  d={cumulativeArea}
                  fill={chart.verdigris}
                  initial={{ opacity: 0 }}
                  animate={drawn ? { opacity: chart.areaOpacity } : {}}
                  transition={{ duration: reduceMotion ? 0 : 0.9, delay: reduceMotion ? 0 : 0.5 }}
                />
                <motion.path d={line('cumulative')} fill="none" stroke={chart.verdigris} strokeWidth={2} strokeLinecap="round" strokeLinejoin="round" {...draw(0)} />
                {[DATA[0], last].map((d) => (
                  <g key={d.year} transform={`translate(${x(d.year)},${y(d.cumulative)})`}>
                    <circle r={4} fill={chart.verdigris} stroke={chart.surface} strokeWidth={2} />
                    {(d.year === TERM ? !compact : true) && (
                      <>
                        {/* Year-one label sits well above the baseline so it never meets the axis */}
                        <text
                          x={d.year === TERM ? 12 : 6}
                          y={d.year === TERM ? 0 : -34}
                          dy={d.year === TERM ? '-0.2em' : 0}
                          fontSize={13}
                          fontWeight={600}
                          fill={chart.text.primary}
                        >
                          {formatMillions(d.cumulative)}
                        </text>
                        <text
                          x={d.year === TERM ? 12 : 6}
                          y={d.year === TERM ? 0 : -19}
                          dy={d.year === TERM ? '1.1em' : 0}
                          fontSize={11}
                          fill={chart.text.muted}
                        >
                          {d.year === TERM ? 'Guaranteed over the term' : 'Year one'}
                        </text>
                      </>
                    )}
                  </g>
                ))}
              </g>
            )}

            {/* Crosshair */}
            {hovered && (
              <g pointerEvents="none">
                <line x1={x(hovered.year)} x2={x(hovered.year)} y1={0} y2={innerH} stroke="rgba(255,255,255,0.22)" strokeWidth={1} shapeRendering="crispEdges" />
                {(view === 'annual'
                  ? [
                      { v: hovered.baseline, c: chart.deemph },
                      { v: hovered.optimized, c: chart.verdigris },
                    ]
                  : [{ v: hovered.cumulative, c: chart.verdigris }]
                ).map((p, i) => (
                  <circle key={i} cx={x(hovered.year)} cy={y(p.v)} r={4.5} fill={p.c} stroke={chart.surface} strokeWidth={2} />
                ))}
              </g>
            )}

            {/* Hit layer — the whole plot, snapping to the nearest year */}
            <rect
              width={innerW}
              height={innerH}
              fill="transparent"
              onPointerMove={setYearFromPointer}
              onPointerDown={setYearFromPointer}
              onPointerLeave={() => setHover(null)}
            />
          </g>
        </svg>
      )}

      {hovered && (
        <ChartTooltip
          x={margin.left + x(hovered.year)}
          y={margin.top + (view === 'annual' ? y((hovered.baseline + hovered.optimized) / 2) : y(hovered.cumulative))}
          containerWidth={width}
          title={`Contract year ${hovered.year}`}
        >
          {view === 'annual' ? (
            <>
              <TooltipRow color={chart.deemph} value={formatMillions(hovered.baseline)} label="without optimization" />
              <TooltipRow color={chart.verdigris} value={formatMillions(hovered.optimized)} label="with ENFRA" />
              <div className="pt-1 mt-1 border-t border-white/10">
                <TooltipRow value={formatMillions(hovered.savings)} label="saved this year" />
              </div>
            </>
          ) : (
            <>
              <TooltipRow color={chart.verdigris} value={formatMillions(hovered.cumulative)} label="saved to date" />
              <TooltipRow value={formatMillions(hovered.savings)} label="saved this year" />
            </>
          )}
        </ChartTooltip>
      )}
    </div>
  )
}
