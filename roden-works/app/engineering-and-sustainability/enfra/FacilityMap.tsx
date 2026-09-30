'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useState, type KeyboardEvent } from 'react'
import { useInView } from '@/lib/hooks'
import ChartFrame from '@/components/charts/ChartFrame'
import ChartTooltip from '@/components/charts/ChartTooltip'
import Legend from '@/components/charts/Legend'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'
import { FACILITY_DISTANCE_MI, MAP_FRAME, MAP_PATHS, MAP_PLACES, MAP_UNITS_PER_MILE } from './map-data'

/* Geography is real, generated into ./map-data.ts from U.S. Census Bureau cartographic
 * boundary + TIGER/Line files, Statistics Canada boundaries and OpenStreetMap (sources
 * listed in that file). Geography lives in map units; labels are drawn in screen pixels
 * so text stays a true 11–14px at every width. */

type FacilityId = 'ummc' | 'stmarys'

interface Facility {
  id: FacilityId
  name: string
  label: string
  city: string
  county: string
  description: string
}

const facilities: Facility[] = [
  {
    id: 'ummc',
    name: 'UMMC — Batavia',
    label: 'UMMC',
    city: 'Batavia',
    county: 'Genesee County',
    description:
      '131 beds, 785+ employees. Largest private employer in Genesee County. Sole maternity provider for two counties.',
  },
  {
    id: 'stmarys',
    name: "St. Mary's — Rochester",
    label: "St. Mary's",
    city: 'Rochester',
    county: 'Monroe County',
    description:
      'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
  },
]

const DISTANCE_LABEL = `≈ ${Math.round(FACILITY_DISTANCE_MI)} mi`

/* Map palette — one step either side of the chart surface */
const WATER = '#0F171A'
const LAND = '#1C252A'
const LAND_CA = '#182024'
const COAST = chart.axis
const COUNTY = '#2B363C'
const ROAD = '#56646D'

/* Visible window in map units: full frame on wide screens, a Niagara → Finger Lakes crop on phones */
const WIDE = { x: 0, y: 0, w: MAP_FRAME.width, h: MAP_FRAME.height }
const COMPACT = { x: 100, y: 60, w: 520, h: 340 }

interface CityLabel {
  id: keyof typeof MAP_PLACES
  name: string
  dx: number
  dy: number
  anchor: 'start' | 'end' | 'middle'
  wideOnly?: boolean
}

const cities: CityLabel[] = [
  { id: 'buffalo', name: 'Buffalo', dx: 9, dy: 4, anchor: 'start' },
  { id: 'niagaraFalls', name: 'Niagara Falls', dx: 9, dy: -6, anchor: 'start', wideOnly: true },
  { id: 'syracuse', name: 'Syracuse', dx: 0, dy: 20, anchor: 'middle', wideOnly: true },
  { id: 'oswego', name: 'Oswego', dx: 9, dy: 4, anchor: 'start', wideOnly: true },
]

export default function FacilityMap() {
  const { ref, isInView } = useInView(0.1)
  const [active, setActive] = useState<FacilityId | null>(null)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 md:mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Facilities</span>
          <h2 className="font-serif text-heading text-white mt-3">Western New York footprint.</h2>
        </motion.div>

        <motion.div
          initial={{ opacity: 0, y: 24 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.7, delay: 0.15 }}
        >
          <ChartFrame
            title="Two central energy plants on the Thruway corridor"
            subtitle={`United Memorial Medical Center in Batavia and St. Mary's in Rochester sit ${DISTANCE_LABEL} apart in a straight line.`}
            legend={
              <Legend
                items={[
                  { label: 'Managed facility', color: chart.verdigris, shape: 'dot' },
                  { label: 'City', color: chart.text.muted, shape: 'dot' },
                  { label: 'I-90 (NYS Thruway)', color: ROAD, shape: 'line' },
                  { label: 'County boundary', color: '#3F4C54', shape: 'line' },
                ]}
              />
            }
            note={
              <>
                Straight-line (geodesic) distance between the two campuses. Geography: U.S. Census Bureau
                TIGER/Line &amp; cartographic boundaries, Statistics Canada, © OpenStreetMap contributors.
              </>
            }
            table={{
              caption: 'Facilities managed under the ENFRA partnership',
              columns: ['Facility', 'City', 'County', 'Coordinates', 'Distance'],
              rows: facilities.map((f) => {
                const p = MAP_PLACES[f.id]
                return [
                  f.name,
                  `${f.city}, NY`,
                  f.county,
                  `${p.lat.toFixed(3)}° N, ${Math.abs(p.lon).toFixed(3)}° W`,
                  `${DISTANCE_LABEL} to ${f.id === 'ummc' ? "St. Mary's" : 'UMMC'}`,
                ]
              }),
            }}
          >
            <MapCanvas active={active} setActive={setActive} animate={isInView} />
          </ChartFrame>
        </motion.div>

        {/* Facility details — hover or focus a card to find it on the map */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 md:gap-6 mt-6">
          {facilities.map((f, i) => {
            const on = active === f.id
            return (
              <motion.div
                key={f.id}
                tabIndex={0}
                aria-label={`${f.name}: ${f.description}`}
                onMouseEnter={() => setActive(f.id)}
                onMouseLeave={() => setActive(null)}
                onFocus={() => setActive(f.id)}
                onBlur={() => setActive(null)}
                initial={{ opacity: 0, y: 20 }}
                animate={isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.3 + i * 0.1 }}
                className={`rounded-xl border p-5 md:p-6 outline-none transition-colors duration-200 focus-visible:ring-2 focus-visible:ring-verdigris/60 ${
                  on ? 'border-verdigris/50 bg-verdigris/[0.06]' : 'border-white/[0.08] bg-surface'
                }`}
              >
                <div className="flex items-center gap-2.5 mb-2">
                  <span
                    aria-hidden="true"
                    className={`inline-block w-2.5 h-2.5 rounded-full bg-verdigris transition-shadow ${
                      on ? 'shadow-[0_0_0_4px_rgba(61,168,135,0.25)]' : ''
                    }`}
                  />
                  <h3 className="font-serif text-lg text-white">{f.name}</h3>
                </div>
                <p className="text-titanium text-sm leading-relaxed">{f.description}</p>
              </motion.div>
            )
          })}
        </div>
      </div>
    </section>
  )
}

function MapCanvas({
  active,
  setActive,
  animate,
}: {
  active: FacilityId | null
  setActive: (id: FacilityId | null) => void
  animate: boolean
}) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = useReducedMotion()
  const compact = width > 0 && width < 640
  const view = compact ? COMPACT : WIDE
  const k = width > 0 ? width / view.w : 0
  const height = Math.round(view.h * k)
  const px = (x: number, y: number) => [(x - view.x) * k, (y - view.y) * k] as const
  const shown = animate || !!reduceMotion

  const [ux, uy] = px(MAP_PLACES.ummc.x, MAP_PLACES.ummc.y)
  const [sx, sy] = px(MAP_PLACES.stmarys.x, MAP_PLACES.stmarys.y)
  // Distance label sits just above-left of the connector's midpoint (clear of I-90 below)
  const len = Math.hypot(sx - ux, sy - uy) || 1
  const nx = (sy - uy) / len
  const ny = -(sx - ux) / len
  const mid = { x: (ux + sx) / 2 + nx * 17, y: (uy + sy) / 2 + ny * 17 }

  const scaleMiles = compact ? 20 : 25
  const scaleLen = scaleMiles * MAP_UNITS_PER_MILE * k
  const fs = compact ? 11 : 12

  const onKey = (id: FacilityId) => (e: KeyboardEvent<SVGGElement>) => {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault()
      setActive(active === id ? null : id)
    } else if (e.key === 'Escape') setActive(null)
  }

  const activeFacility = facilities.find((f) => f.id === active)
  const tip = activeFacility ? (activeFacility.id === 'ummc' ? { x: ux, y: uy } : { x: sx, y: sy }) : null

  const fade = (delay: number) => ({
    initial: { opacity: reduceMotion ? 1 : 0 },
    animate: shown ? { opacity: 1 } : {},
    transition: { duration: reduceMotion ? 0 : 0.6, delay: reduceMotion ? 0 : delay },
  })

  return (
    <div
      ref={ref}
      className="relative w-full overflow-hidden rounded-xl border border-white/[0.06]"
      style={{ height: height || undefined, aspectRatio: height ? undefined : `${WIDE.w} / ${WIDE.h}`, background: WATER }}
    >
      {width > 0 && (
        <svg
          width={width}
          height={height}
          role="img"
          aria-label={`Map of Western New York from Lake Erie to Syracuse. United Memorial Medical Center in Batavia and St. Mary's in Rochester are marked, ${DISTANCE_LABEL} apart; the I-90 Thruway runs between them.`}
          className="block"
          style={{ fontFamily: 'var(--font-geist-sans), system-ui, sans-serif' }}
        >
          {/* ── Geography (map units) ── */}
          <motion.g transform={`scale(${k}) translate(${-view.x} ${-view.y})`} {...fade(0)}>
            <path d={MAP_PATHS.canada} fill={LAND_CA} stroke={COAST} strokeWidth={1} vectorEffect="non-scaling-stroke" strokeLinejoin="round" />
            <path d={MAP_PATHS.ny} fill={LAND} fillRule="evenodd" stroke={COAST} strokeWidth={1} vectorEffect="non-scaling-stroke" strokeLinejoin="round" />
            <path d={MAP_PATHS.countyLines} fill="none" stroke={COUNTY} strokeWidth={1} vectorEffect="non-scaling-stroke" strokeLinejoin="round" />
            <path d={MAP_PATHS.i90} fill="none" stroke={ROAD} strokeWidth={1.5} vectorEffect="non-scaling-stroke" strokeLinejoin="round" strokeLinecap="round" />
          </motion.g>

          {/* ── Region labels ── */}
          <motion.g {...fade(0.2)} fontStyle="italic" fill={chart.text.muted} style={{ fontFamily: 'var(--font-newsreader), Georgia, serif' }}>
            <RegionLabel at={px(compact ? 290 : 590, compact ? 90 : 88)} size={compact ? 14 : 17} spacing={0.04}>
              Lake Ontario
            </RegionLabel>
            {!compact && (
              <>
                <RegionLabel at={px(14, 404)} size={14} anchor="start">
                  Lake Erie
                </RegionLabel>
                <RegionLabel at={px(40, 250)} size={13}>
                  Ontario
                </RegionLabel>
                <RegionLabel at={px(40, 250)} size={13} dy={15}>
                  Canada
                </RegionLabel>
              </>
            )}
            <RegionLabel at={px(compact ? 505 : 720, compact ? 368 : 478)} size={compact ? 13 : 14}>
              Finger Lakes
            </RegionLabel>
          </motion.g>

          {/* I-90 route tag (the legend covers it on phones) */}
          <motion.g {...fade(0.3)}>
            {!compact && (() => {
              const [x, y] = px(700, 262)
              return (
                <text x={x} y={y + 17} textAnchor="middle" fontSize={11} fontWeight={500} fill={chart.text.muted} letterSpacing="0.04em">
                  I-90
                </text>
              )
            })()}
          </motion.g>

          {/* ── Cities ── */}
          <motion.g {...fade(0.35)}>
            {cities
              .filter((c) => !(compact && c.wideOnly))
              .map((c) => {
                const [x, y] = px(MAP_PLACES[c.id].x, MAP_PLACES[c.id].y)
                return (
                  <g key={c.id}>
                    <circle cx={x} cy={y} r={3} fill={chart.text.muted} stroke={LAND} strokeWidth={1.5} />
                    <text x={x + c.dx} y={y + c.dy} textAnchor={c.anchor} fontSize={fs} fill={chart.text.secondary}>
                      {c.name}
                    </text>
                  </g>
                )
              })}
          </motion.g>

          {/* ── Connector + distance ── */}
          <motion.line
            x1={ux}
            y1={uy}
            x2={sx}
            y2={sy}
            stroke={chart.verdigris}
            strokeWidth={2}
            strokeLinecap="round"
            initial={{ pathLength: reduceMotion ? 1 : 0, opacity: 0.85 }}
            animate={shown ? { pathLength: 1 } : {}}
            transition={{ duration: reduceMotion ? 0 : 1.1, delay: reduceMotion ? 0 : 0.5, ease: [0.16, 1, 0.3, 1] }}
          />
          <motion.g {...fade(1.2)} transform={`translate(${mid.x} ${mid.y})`}>
            <rect x={-29} y={-11} width={58} height={22} rx={11} fill={chart.surface} stroke="rgba(61,168,135,0.55)" strokeWidth={1} />
            <text textAnchor="middle" dy="0.35em" fontSize={12} fontWeight={600} fill={chart.text.primary} style={{ fontVariantNumeric: 'tabular-nums' }}>
              {DISTANCE_LABEL}
            </text>
          </motion.g>

          {/* ── Facilities ── */}
          {facilities.map((f, i) => {
            const [x, y] = f.id === 'ummc' ? [ux, uy] : [sx, sy]
            const on = active === f.id
            const dim = active !== null && !on
            // UMMC labels above-left (I-90 runs just below Batavia); St. Mary's to the right
            // (on phones St. Mary's sits above its marker so it never clips the right edge)
            const above = compact && f.id === 'stmarys'
            const lx = f.id === 'ummc' ? x - 12 : above ? x : x + 14
            const ly = f.id === 'ummc' ? y - 24 : above ? y - 31 : y - 3
            const anchor = f.id === 'ummc' ? 'end' : above ? 'middle' : 'start'
            return (
              <motion.g
                key={f.id}
                {...fade(0.8 + i * 0.15)}
                role="button"
                tabIndex={0}
                aria-label={`${f.name}. ${f.description}`}
                aria-pressed={on}
                onMouseEnter={() => setActive(f.id)}
                onMouseLeave={() => setActive(null)}
                onFocus={() => setActive(f.id)}
                onBlur={() => setActive(null)}
                onKeyDown={onKey(f.id)}
                onClick={() => setActive(on ? null : f.id)}
                className="cursor-pointer outline-none"
                style={{ opacity: dim ? 0.55 : 1, transition: 'opacity 200ms' }}
              >
                <circle cx={x} cy={y} r={on ? 16 : 11} fill={chart.verdigris} opacity={on ? 0.22 : 0.14} style={{ transition: 'r 200ms, opacity 200ms' }} />
                <circle cx={x} cy={y} r={on ? 7.5 : 6} fill={chart.verdigris} stroke={chart.surface} strokeWidth={2} style={{ transition: 'r 200ms' }} />
                {on && <circle cx={x} cy={y} r={20} fill="none" stroke={chart.verdigris} strokeWidth={1} opacity={0.6} />}
                <text x={lx} y={ly} textAnchor={anchor} fontSize={compact ? 13 : 14} fontWeight={600} fill={chart.text.primary}>
                  {f.label}
                </text>
                <text x={lx} y={ly + 15} textAnchor={anchor} fontSize={compact ? 11 : 12} fill={chart.text.secondary}>
                  {f.city}
                </text>
                {/* Generous invisible hit target */}
                <circle cx={x} cy={y} r={18} fill="transparent" />
              </motion.g>
            )
          })}

          {/* ── North + scale ── */}
          {/* bottom-right on wide screens; bottom-left (open land south of Buffalo) on phones */}
          <motion.g {...fade(0.4)} transform={`translate(${compact ? 12 : width - 24 - scaleLen} ${height - (compact ? 14 : 24)})`}>
            <g transform={`translate(${scaleLen} ${compact ? -36 : -42})`}>
              <path d="M0,-9 L4,3 L0,0.5 L-4,3 Z" fill={chart.text.muted} />
              <text y={16} textAnchor="middle" fontSize={11} fontWeight={600} fill={chart.text.muted}>
                N
              </text>
            </g>
            <line x1={0} x2={scaleLen} y1={0} y2={0} stroke={chart.text.muted} strokeWidth={1} shapeRendering="crispEdges" />
            {[0, scaleLen / 2, scaleLen].map((t, i) => (
              <line key={i} x1={t} x2={t} y1={i === 1 ? -3 : -5} y2={0} stroke={chart.text.muted} strokeWidth={1} shapeRendering="crispEdges" />
            ))}
            <text x={0} y={-9} textAnchor="start" fontSize={11} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
              0
            </text>
            <text x={scaleLen} y={-9} textAnchor="end" fontSize={11} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
              {scaleMiles} mi
            </text>
          </motion.g>
        </svg>
      )}

      {activeFacility && tip && width > 0 && (
        <ChartTooltip x={tip.x} y={tip.y} containerWidth={width} title={activeFacility.county}>
          <div className="max-w-[230px] whitespace-normal">
            <div className="text-sm font-semibold text-white">{activeFacility.name}</div>
            <p className="mt-1 text-xs leading-relaxed text-titanium">{activeFacility.description}</p>
            <div className="mt-2 pt-2 border-t border-white/10 flex items-center gap-2 text-xs">
              <span aria-hidden="true" className="inline-block w-3 h-0.5 rounded-full bg-verdigris" />
              <span className="font-semibold text-white tabular-nums">{DISTANCE_LABEL}</span>
              <span className="text-muted">to {activeFacility.id === 'ummc' ? "St. Mary's" : 'UMMC'}</span>
            </div>
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}

function RegionLabel({
  at,
  size,
  anchor = 'middle',
  dy = 0,
  spacing = 0.02,
  children,
}: {
  at: readonly [number, number]
  size: number
  anchor?: 'start' | 'middle' | 'end'
  dy?: number
  spacing?: number
  children: string
}) {
  return (
    <text x={at[0]} y={at[1] + dy} textAnchor={anchor} fontSize={size} letterSpacing={`${spacing}em`}>
      {children}
    </text>
  )
}
