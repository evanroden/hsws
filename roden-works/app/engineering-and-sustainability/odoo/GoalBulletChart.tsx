'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip, { TooltipRow } from '@/components/charts/ChartTooltip'
import Legend from '@/components/charts/Legend'
import { chart } from '@/components/charts/tokens'
import { linearScale } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'

/* The only two facts on the page: the monthly goal (100%) and the best month
 * (160% of it). No other months are reported, so none are drawn. */
const GOAL = 100
const BEST = 160
const DOMAIN_MAX = 175

const HEIGHT = 112
const ROW_ACTUAL = 18 // baseline of the "160%, best month" label
const ROW_GOAL = 42 // baseline of the "Monthly goal" label
const TRACK_Y = 52
const TRACK_H = 24
const BAR_H = 12
const TICK_LABEL_Y = 104

type Mark = 'best' | 'goal'

export default function GoalBulletChart({ animate }: { animate: boolean }) {
  return (
    <ChartFrame
      title="Non-recurring revenue against the monthly goal"
      subtitle="Best month at Odoo, as a percent of the monthly target"
      legend={
        <Legend
          items={[
            { label: 'Best month', color: chart.copper, shape: 'rect' },
            { label: 'Monthly goal', color: chart.text.primary, shape: 'line' },
          ]}
        />
      }
      note="Non-recurring revenue vs monthly goal, best month. Only the goal and best-month values are shown; other months aren't reported."
      table={{
        caption: 'Best month versus monthly non-recurring revenue goal',
        columns: ['Measure', 'Percent of monthly goal'],
        rows: [
          ['Monthly goal', '100%'],
          ['Best month', '160%'],
          ['Above goal', '+60 points'],
        ],
      }}
    >
      <Bullet animate={animate} />
    </ChartFrame>
  )
}

function Bullet({ animate }: { animate: boolean }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const [active, setActive] = useState<Mark | null>(null)

  const margin = { left: 2, right: 2 }
  const innerW = Math.max(0, width - margin.left - margin.right)
  const x = linearScale([0, DOMAIN_MAX], [0, innerW])
  const ticks = Array.from({ length: DOMAIN_MAX / 25 + 1 }, (_, i) => i * 25)
  const labelEvery = innerW > 560 ? 25 : 50

  const barY = TRACK_Y + (TRACK_H - BAR_H) / 2
  const barEnd = x(BEST)
  const r = 4
  // Square at the baseline, 4px rounded at the data end
  const barPath = `M0,${barY}H${barEnd - r}a${r},${r} 0 0 1 ${r},${r}V${barY + BAR_H - r}a${r},${r} 0 0 1 -${r},${r}H0Z`
  const drawn = animate || reduceMotion

  const bind = (m: Mark) => ({
    tabIndex: 0,
    onPointerEnter: () => setActive(m),
    onPointerLeave: () => setActive(null),
    onFocus: () => setActive(m),
    onBlur: () => setActive(null),
    style: { outline: 'none', cursor: 'default' },
  })

  return (
    <div ref={ref} className="relative w-full" style={{ height: HEIGHT }}>
      {width > 0 && (
        <svg width={width} height={HEIGHT} className="block overflow-visible">
          <g transform={`translate(${margin.left},0)`}>
            {/* Track: the full 0–175% scale */}
            <rect x={0} y={TRACK_Y} width={innerW} height={TRACK_H} rx={4} fill={chart.grid} />

            {/* Hairline ticks + labels */}
            {ticks.map((t) => (
              <g key={t} transform={`translate(${x(t)},0)`}>
                <line
                  y1={TRACK_Y + TRACK_H + 4}
                  y2={TRACK_Y + TRACK_H + (t % 50 === 0 ? 10 : 7)}
                  stroke={chart.axis}
                  strokeWidth={1}
                  shapeRendering="crispEdges"
                />
                {t % labelEvery === 0 && (
                  <text
                    y={TICK_LABEL_Y}
                    textAnchor={t === 0 ? 'start' : t === DOMAIN_MAX ? 'end' : 'middle'}
                    fontSize={11}
                    fill={chart.text.muted}
                    style={{ fontVariantNumeric: 'tabular-nums' }}
                  >
                    {t}%
                  </text>
                )}
              </g>
            ))}

            {/* Actual: best month */}
            <motion.path
              d={barPath}
              fill={active === 'best' ? '#D08C4F' : chart.copper}
              style={{ transformBox: 'fill-box', transformOrigin: 'left center' }}
              initial={{ scaleX: reduceMotion ? 1 : 0 }}
              animate={drawn ? { scaleX: 1 } : {}}
              transition={{ duration: reduceMotion ? 0 : 1.1, delay: reduceMotion ? 0 : 0.2, ease: [0.16, 1, 0.3, 1] }}
            />

            {/* Direct label for the actual, with a leader to the bar end */}
            <motion.g
              initial={{ opacity: reduceMotion ? 1 : 0 }}
              animate={drawn ? { opacity: 1 } : {}}
              transition={{ duration: reduceMotion ? 0 : 0.4, delay: reduceMotion ? 0 : 1.1 }}
            >
              <line x1={barEnd - 0.5} x2={barEnd - 0.5} y1={ROW_ACTUAL + 6} y2={barY - 3} stroke={chart.axis} strokeWidth={1} />
              <text x={barEnd} y={ROW_ACTUAL} textAnchor="end" fill={chart.text.primary}>
                <tspan fontSize={15} fontWeight={600}>
                  160%
                </tspan>
                <tspan fontSize={12} fill={chart.text.secondary}>
                  , best month
                </tspan>
              </text>
            </motion.g>

            {/* Target marker: monthly goal */}
            <line
              x1={x(GOAL)}
              x2={x(GOAL)}
              y1={ROW_GOAL - 12}
              y2={TRACK_Y + TRACK_H + 4}
              stroke={chart.text.primary}
              strokeWidth={active === 'goal' ? 3 : 2}
              strokeLinecap="round"
            />
            <text x={x(GOAL) - 8} y={ROW_GOAL} textAnchor="end" fontSize={12} fill={chart.text.secondary}>
              Monthly goal
            </text>

            {/* Hit targets (≥ 24px), keyboard focusable */}
            <g {...bind('best')} role="img" aria-label="Best month: 160% of the monthly goal, 60 points above it">
              <rect x={0} y={TRACK_Y} width={barEnd} height={TRACK_H} fill="transparent" />
            </g>
            <g {...bind('goal')} role="img" aria-label="Monthly goal: 100%">
              <rect x={x(GOAL) - 12} y={ROW_GOAL - 14} width={24} height={TRACK_Y + TRACK_H + 6 - (ROW_GOAL - 14)} fill="transparent" />
            </g>

            {/* Focus ring for keyboard users */}
            {active === 'best' && (
              <rect x={-3} y={TRACK_Y - 3} width={barEnd + 6} height={TRACK_H + 6} rx={6} fill="none" stroke="rgba(255,255,255,0.18)" strokeWidth={1} pointerEvents="none" />
            )}
          </g>
        </svg>
      )}

      {active && width > 0 && (
        <ChartTooltip
          x={margin.left + (active === 'best' ? barEnd : x(GOAL))}
          y={TRACK_Y + TRACK_H / 2}
          containerWidth={width}
          title={active === 'best' ? 'Best month' : 'Monthly goal'}
        >
          {active === 'best' ? (
            <>
              <TooltipRow color={chart.copper} value="160%" label="of monthly goal" />
              <TooltipRow value="+60 pts" label="above goal" />
            </>
          ) : (
            <TooltipRow color={chart.text.primary} value="100%" label="non-recurring revenue target" />
          )}
        </ChartTooltip>
      )}
    </div>
  )
}
