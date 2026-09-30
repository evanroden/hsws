'use client'

import { AnimatePresence, motion } from 'framer-motion'
import { useEffect, useState, type KeyboardEvent, type ReactNode } from 'react'
import { useInView, useReducedMotion } from '@/lib/hooks'
import SegmentedControl from '@/components/ui/SegmentedControl'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'
import { GROUPS, GROUP_BY_ID, LOOPS, LOOP_ORDER, OUTAGE_STEPS, type GroupId, type LoopId, type Mode } from './plant-model'
import { WIDE, type PipeDef, type Pt, type Rect } from './plant-layout'
import {
  Ats,
  BasUnit,
  Boiler,
  Chiller,
  Generator,
  HAIRLINE,
  HospitalCardView,
  Meter,
  Pump,
  SURFACE,
  T,
  Tower,
  Transformer,
  type Tone,
} from './plant-symbols'

const L = WIDE
const DEAD = chart.deemph
const ALERT = '#E5605A' // 5.0:1 on surface; used only for "utility feed lost"

export default function EnergyPlantDiagram() {
  const { ref, isInView } = useInView(0.1)
  // Starts false on server and client, then syncs — avoids a hydration mismatch
  const reduce = useReducedMotion()
  const [selected, setSelected] = useState<GroupId | null>(null)
  const [hovered, setHovered] = useState<GroupId | null>(null)
  const [touched, setTouched] = useState(false)
  const [mode, setMode] = useState<Mode>('normal')
  const [stage, setStage] = useState(0)

  // Outage sequence: 1 feed lost → 2 generators start → 3 ATS transfers → 4 loads carried
  useEffect(() => {
    if (mode === 'normal') {
      setStage(0)
      return
    }
    if (reduce) {
      setStage(4)
      return
    }
    setStage(1)
    const timers = [700, 1500, 2300].map((ms, i) => setTimeout(() => setStage(i + 2), ms))
    return () => timers.forEach(clearTimeout)
  }, [mode, reduce])

  const select = (id: GroupId | null) => {
    setTouched(true)
    setSelected(id)
  }
  const changeMode = (m: Mode) => {
    setTouched(true)
    setMode(m)
    setSelected(m === 'outage' ? 'generators' : null)
  }

  const active = hovered ?? selected

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 md:mb-12 max-w-3xl"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Interactive Diagram</span>
          <h2 className="font-serif text-heading text-white mt-3">Anatomy of a Central Energy Plant.</h2>
          <p className="mt-4 text-titanium leading-relaxed">
            The &ldquo;heart and lungs&rdquo; of a hospital campus — producing the steam, chilled water, and emergency
            power a hospital needs for heating, cooling, sterilization, and critical care. Select a system to see what it
            does, or switch to a utility outage to watch the plant keep critical loads powered.
          </p>
        </motion.div>

        <motion.figure
          initial={{ opacity: 0, y: 24 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.7, delay: 0.15 }}
          aria-labelledby="plant-title"
          className="rounded-2xl border border-white/[0.08] bg-surface p-5 md:p-8"
        >
          <div className="flex flex-col gap-4 md:flex-row md:items-start md:justify-between mb-6">
            <div className="min-w-0">
              <h3 id="plant-title" className="font-sans text-base md:text-lg font-medium text-white">
                Hospital central energy plant, simplified schematic
              </h3>
              <p className="mt-1 text-sm text-muted max-w-2xl">
                Six systems, five utility loops, one campus. Pipes animate in the direction of flow.
              </p>
            </div>
            <SegmentedControl<Mode>
              label="Plant operating mode"
              value={mode}
              onChange={changeMode}
              options={[
                { value: 'normal', label: 'Normal operation' },
                { value: 'outage', label: 'Utility outage' },
              ]}
            />
          </div>

          <div className="grid grid-cols-1 xl:grid-cols-3 gap-6 xl:gap-8">
            <div className="xl:col-span-2 min-w-0">
              <Schematic
                active={active}
                selected={selected}
                mode={mode}
                stage={stage}
                reduce={reduce}
                showMarkers={!touched}
                onSelect={(id) => select(selected === id ? null : id)}
                onHover={setHovered}
              />
            </div>
            <DetailPanel
              selected={selected}
              mode={mode}
              stage={stage}
              touched={touched}
              onSelect={select}
            />
          </div>

          <figcaption className="mt-6 pt-5 border-t border-white/[0.06] flex flex-col gap-4 lg:flex-row lg:items-start lg:justify-between">
            <LoopLegend />
            <p className="text-xs text-muted lg:text-right lg:max-w-sm shrink-0">
              Representative schematic — not an as-built drawing. Equipment ratings are illustrative; the outage sequence
              is simulated and not to scale.
            </p>
          </figcaption>
        </motion.figure>
      </div>
    </section>
  )
}

/* ═════════════════════════════════════════════════════════════════════════
   Schematic
   ═════════════════════════════════════════════════════════════════════════ */

interface SchematicProps {
  active: GroupId | null
  selected: GroupId | null
  mode: Mode
  stage: number
  reduce: boolean
  showMarkers: boolean
  onSelect: (id: GroupId) => void
  onHover: (id: GroupId | null) => void
}

type PipeState = 'live' | 'idle' | 'lost'

function Schematic({ active, selected, mode, stage, reduce, showMarkers, onSelect, onHover }: SchematicProps) {
  const { ref: scrollRef, width: viewport } = useElementSize<HTMLDivElement>()
  const { ref, width } = useElementSize<HTMLDivElement>()
  const [focused, setFocused] = useState<GroupId | null>(null)
  const px = width > 0 ? L.W / width : 1.1
  const overflowing = viewport > 0 && viewport < L.minWidth

  const outage = mode === 'outage'
  const utilityLost = outage && stage >= 1
  const genRunning = outage && stage >= 2
  const onEmergency = outage && stage >= 3

  const pipeState = (p: PipeDef): PipeState => {
    if (p.power === 'utility') return utilityLost ? 'lost' : 'live'
    if (p.power === 'emergency') return genRunning ? 'live' : 'idle'
    if (p.power === 'critical') return utilityLost && !onEmergency ? 'lost' : 'live'
    return 'live'
  }

  const lit = (groups: GroupId[]) => !active || groups.includes(active)
  // In an outage the electrical path is the story: other loops recede
  const recede = (p: PipeDef) => outage && !active && p.loop !== 'power'
  const eqOpacity = (g: GroupId) => (active && active !== g ? 0.35 : 1)
  const tone = (g: GroupId): Tone => (active === g ? 'active' : 'normal')

  const flowDash = `${3 * px} ${13 * px}`

  return (
    <div>
      <div ref={scrollRef} className="overflow-x-auto -mx-5 px-5 md:mx-0 md:px-0" data-lenis-prevent>
        <div ref={ref} className="relative mx-auto" style={{ minWidth: L.minWidth, maxWidth: L.maxWidth }}>
          <svg
            viewBox={`0 0 ${L.W} ${L.H}`}
            className="block w-full h-auto select-none"
            role="group"
            aria-label="Schematic of a hospital central energy plant. Use Tab to move between systems and Enter to select one."
          >
            <defs>
              <pattern id="plant-grid" width={16} height={16} patternUnits="userSpaceOnUse">
                <path d="M16,0 H0 V16" fill="none" stroke="#1A2226" strokeWidth={px} />
              </pattern>
            </defs>

            {/* Boundaries */}
            <rect x={L.plant.x} y={L.plant.y} width={L.plant.w} height={L.plant.h} rx={10} fill="url(#plant-grid)" stroke={HAIRLINE} strokeWidth={px} />
            <T x={L.plantLabel[0]} y={L.plantLabel[1]} px={px} mono fill={chart.text.muted} tracking={0.08}>
              CENTRAL ENERGY PLANT
            </T>
            <rect x={L.hospital.x} y={L.hospital.y} width={L.hospital.w} height={L.hospital.h} rx={10} fill="none" stroke={active === 'hospital' ? '#4A5761' : HAIRLINE} strokeWidth={px} />
            <T x={L.hospitalLabel[0]} y={L.hospitalLabel[1]} px={px} mono fill={chart.text.muted} tracking={0.08}>
              HOSPITAL CAMPUS
            </T>

            {/* Utility side */}
            <T x={L.utilityLabels.gas[0]} y={L.utilityLabels.gas[1]} px={px} fill={chart.text.secondary}>
              Natural gas
            </T>
            <T x={L.utilityLabels.power[0]} y={L.utilityLabels.power[1]} px={px} fill={chart.text.secondary}>
              Utility power
            </T>

            {/* Lane labels */}
            {L.lanes.map((lane) => (
              <g key={lane.group} opacity={eqOpacity(lane.group)} style={{ transition: 'opacity 250ms' }}>
                <T x={lane.x} y={lane.y} px={px} size={13} weight={600} fill={chart.text.primary}>
                  {lane.name}
                </T>
                <T x={lane.x} y={lane.y + 17 * px} px={px} mono fill={chart.text.muted}>
                  {lane.spec}
                </T>
              </g>
            ))}
            <g opacity={eqOpacity('generators')} style={{ transition: 'opacity 250ms' }}>
              <circle cx={L.status[0] + 4 * px} cy={L.status[1] - 4 * px} r={3.5 * px} fill={genRunning ? LOOPS.power.color : DEAD} />
              <T x={L.status[0] + 14 * px} y={L.status[1]} px={px} mono fill={genRunning ? chart.text.primary : chart.text.muted}>
                {genRunning ? (onEmergency ? 'Running · on load' : 'Starting') : 'Standby'}
              </T>
            </g>

            {/* Control signals */}
            <g opacity={lit(['bas']) && !(outage && !active) ? 1 : 0.2} style={{ transition: 'opacity 250ms' }}>
              {L.signals.map((s, i) => (
                <polyline
                  key={i}
                  points={s.map((p) => p.join(',')).join(' ')}
                  fill="none"
                  stroke={LOOPS.controls.color}
                  strokeWidth={px}
                  strokeDasharray={`${1.5 * px} ${3 * px}`}
                  opacity={active === 'bas' ? 1 : 0.55}
                />
              ))}
              {active === 'bas' &&
                !reduce &&
                L.signals.map((s, i) => (
                  <polyline
                    key={`p${i}`}
                    points={s.map((p) => p.join(',')).join(' ')}
                    fill="none"
                    stroke={LOOPS.controls.tint}
                    strokeWidth={2 * px}
                    strokeLinecap="round"
                    strokeDasharray={`${2 * px} ${22 * px}`}
                  >
                    <animate attributeName="stroke-dashoffset" from="0" to={-24 * px} dur="0.9s" repeatCount="indefinite" />
                  </polyline>
                ))}
            </g>

            {/* Pipes */}
            {L.pipes.map((p) => {
              const state = pipeState(p)
              const loop = LOOPS[p.loop]
              const on = lit(p.groups)
              const color = state === 'live' ? loop.color : DEAD
              const pts = p.pts.map((q) => q.join(',')).join(' ')
              const opacity = !on ? 0.14 : recede(p) ? 0.3 : 1
              return (
                <g key={p.id} opacity={opacity} style={{ transition: 'opacity 250ms' }}>
                  <polyline
                    points={pts}
                    fill="none"
                    stroke={color}
                    strokeWidth={(state === 'idle' ? 1.25 : 2) * px}
                    strokeLinejoin="round"
                    strokeDasharray={p.dashed || state !== 'live' ? `${6 * px} ${4 * px}` : undefined}
                    style={{ transition: 'stroke 300ms' }}
                  />
                  {state === 'live' && !reduce && (
                    <polyline points={pts} fill="none" stroke={loop.tint} strokeWidth={2 * px} strokeLinecap="round" strokeDasharray={flowDash}>
                      <animate attributeName="stroke-dashoffset" from="0" to={-16 * px} dur={p.loop === 'power' ? '0.6s' : '1.1s'} repeatCount="indefinite" />
                    </polyline>
                  )}
                  {state === 'live' && p.arrows?.map((a, i) => <Chevron key={i} at={a} pts={p.pts} color={color} px={px} />)}
                </g>
              )
            })}

            {/* Pipe labels */}
            {L.pipeLabels.map((lab) => {
              const pipe = L.pipes.find((p) => p.id === lab.pipe)!
              const opacity = !lit(pipe.groups) ? 0.2 : recede(pipe) ? 0.45 : 1
              return (
                <g key={lab.pipe} opacity={opacity} style={{ transition: 'opacity 250ms' }}>
                  <T x={lab.x} y={lab.y} px={px} mono anchor={lab.anchor} fill={chart.text.secondary}>
                    {lab.text}
                  </T>
                </g>
              )
            })}
            <g opacity={lit(['generators']) ? 1 : 0.2} style={{ transition: 'opacity 250ms' }}>
              <T x={470} y={526} px={px} mono fill={utilityLost ? chart.text.muted : chart.text.secondary}>
                Normal
              </T>
              <T x={470} y={578} px={px} mono fill={genRunning ? chart.text.secondary : chart.text.muted}>
                Emergency
              </T>
            </g>

            {/* Utility feed lost */}
            {utilityLost && (
              <g>
                <circle cx={L.outageMark[0]} cy={L.outageMark[1]} r={9} fill={SURFACE} stroke={ALERT} strokeWidth={1.5 * px} />
                <path
                  d={`M${L.outageMark[0] - 4},${L.outageMark[1] - 4} l8,8 m0,-8 l-8,8`}
                  stroke={ALERT}
                  strokeWidth={1.75 * px}
                  strokeLinecap="round"
                />
                <T x={L.utilityLabels.lost[0]} y={L.utilityLabels.lost[1]} px={px} weight={600} fill={chart.text.primary}>
                  Feed lost
                </T>
              </g>
            )}

            {/* Equipment */}
            <Equip opacity={eqOpacity('bas')}>
              <BasUnit r={L.bas} px={px} tone={tone('bas')} />
            </Equip>
            <Equip opacity={eqOpacity('towers')}>
              <Tower r={L.tower} px={px} tone={tone('towers')} />
            </Equip>
            <Equip opacity={eqOpacity('chillers')}>
              <Chiller r={L.chiller} px={px} tone={tone('chillers')} />
            </Equip>
            <Equip opacity={eqOpacity('boilers')}>
              <Boiler r={L.boiler} px={px} tone={tone('boilers')} />
              <Meter x={L.meter[0]} y={L.meter[1]} px={px} tone={tone('boilers')} />
            </Equip>
            <Equip opacity={eqOpacity('generators')}>
              <Generator r={L.generator} px={px} tone={tone('generators')} running={genRunning} />
              <Ats r={L.ats} px={px} tone={tone('generators')} position={onEmergency ? 'E' : 'N'} />
              <Transformer x={L.transformer[0]} y={L.transformer[1]} px={px} tone={tone('generators')} />
            </Equip>
            <Equip opacity={eqOpacity('pumps')}>
              {L.pumps.map((p) => (
                <g key={p.id}>
                  <Pump x={p.x} y={p.y} dir={p.dir} px={px} tone={tone('pumps')} />
                  <T x={p.tagX} y={p.tagY} px={px} mono anchor={p.tagAnchor} fill={active === 'pumps' ? chart.text.primary : chart.text.muted}>
                    {p.tag}
                  </T>
                </g>
              ))}
            </Equip>
            <Equip opacity={active && active !== 'hospital' && !GROUP_BY_ID[active].loops.some((l) => l !== 'gas' && l !== 'controls' && l !== 'cw') ? 0.35 : 1}>
              {L.cards.map((c) => {
                let status: { text: string; color: string } | undefined
                if (c.id === 'power' && utilityLost)
                  status = onEmergency ? { text: 'On generator', color: LOOPS.power.color } : { text: 'Transferring…', color: ALERT }
                return <HospitalCardView key={c.id} id={c.id} r={c.rect} label={c.label} sub={c.sub} px={px} tone={tone('hospital')} status={status} />
              })}
            </Equip>

            {/* Selection brackets */}
            {GROUPS.map((g) => {
              const show = g.id === active || g.id === focused
              if (!show) return null
              const strong = g.id === selected || g.id === focused
              return L.brackets[g.id].map((r, i) => (
                <Bracket key={`${g.id}${i}`} r={r} px={px} color={strong ? chart.text.primary : chart.text.muted} />
              ))
            })}

            {/* First-visit markers */}
            {showMarkers &&
              GROUPS.map((g) => {
                const [x, y] = L.markers[g.id]
                return (
                  <g key={g.id} pointerEvents="none">
                    {!reduce && (
                      <circle cx={x} cy={y} r={5 * px} fill="none" stroke={chart.copper} strokeWidth={1.5 * px}>
                        <animate attributeName="r" values={`${5 * px};${13 * px}`} dur="1.8s" repeatCount="indefinite" />
                        <animate attributeName="opacity" values="0.9;0" dur="1.8s" repeatCount="indefinite" />
                      </circle>
                    )}
                    <circle cx={x} cy={y} r={4.5 * px} fill="#D08C4F" stroke={SURFACE} strokeWidth={2 * px} />
                  </g>
                )
              })}

            {/* Hit targets — one keyboard stop per system */}
            {GROUPS.map((g) => (
              <g
                key={g.id}
                role="button"
                tabIndex={0}
                aria-pressed={selected === g.id}
                aria-label={`${g.name}: ${g.short}`}
                data-cursor=""
                className="outline-none"
                onClick={() => onSelect(g.id)}
                onKeyDown={(e: KeyboardEvent<SVGGElement>) => {
                  if (e.key === 'Enter' || e.key === ' ') {
                    e.preventDefault()
                    onSelect(g.id)
                  }
                }}
                onPointerEnter={(e) => e.pointerType === 'mouse' && onHover(g.id)}
                onPointerLeave={() => onHover(null)}
                onFocus={() => setFocused(g.id)}
                onBlur={() => setFocused(null)}
              >
                {L.hits[g.id].map((r, i) => (
                  <rect key={i} x={r.x} y={r.y} width={r.w} height={r.h} rx={6} fill="transparent" />
                ))}
              </g>
            ))}
          </svg>
        </div>
      </div>
      {overflowing && (
        <p className="mt-3 text-xs text-muted flex items-center gap-2">
          <svg aria-hidden="true" className="w-4 h-4" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5">
            <path d="M3 8h10M10 5l3 3-3 3" strokeLinecap="round" strokeLinejoin="round" />
          </svg>
          Scroll sideways to see the whole plant
        </p>
      )}
    </div>
  )
}

function Equip({ opacity, children }: { opacity: number; children: ReactNode }) {
  return (
    <g opacity={opacity} style={{ transition: 'opacity 250ms' }}>
      {children}
    </g>
  )
}

/** Flow chevron at a point on a polyline, oriented along its segment */
function Chevron({ at, pts, color, px }: { at: Pt; pts: Pt[]; color: string; px: number }) {
  let angle = 0
  for (let i = 0; i < pts.length - 1; i++) {
    const [ax, ay] = pts[i]
    const [bx, by] = pts[i + 1]
    const onX = ay === by && at[1] === ay && at[0] >= Math.min(ax, bx) && at[0] <= Math.max(ax, bx)
    const onY = ax === bx && at[0] === ax && at[1] >= Math.min(ay, by) && at[1] <= Math.max(ay, by)
    if (onX || onY) {
      angle = (Math.atan2(by - ay, bx - ax) * 180) / Math.PI
      break
    }
  }
  return (
    <path
      d={`M${-4 * px},${-5 * px} L${3 * px},0 L${-4 * px},${5 * px}`}
      transform={`translate(${at[0]},${at[1]}) rotate(${angle})`}
      fill="none"
      stroke={color}
      strokeWidth={2 * px}
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  )
}

/** Corner brackets — the "selected" state, like a crop mark on a drawing */
function Bracket({ r, px, color }: { r: Rect; px: number; color: string }) {
  const g = 6 * px
  const c = Math.min(12 * px, r.w / 3, r.h / 3)
  const x0 = r.x - g
  const y0 = r.y - g
  const x1 = r.x + r.w + g
  const y1 = r.y + r.h + g
  const d = [
    `M${x0},${y0 + c} V${y0} H${x0 + c}`,
    `M${x1 - c},${y0} H${x1} V${y0 + c}`,
    `M${x1},${y1 - c} V${y1} H${x1 - c}`,
    `M${x0 + c},${y1} H${x0} V${y1 - c}`,
  ].join(' ')
  return <path d={d} fill="none" stroke={color} strokeWidth={1.5 * px} strokeLinecap="round" pointerEvents="none" />
}

/* ═════════════════════════════════════════════════════════════════════════
   Detail panel
   ═════════════════════════════════════════════════════════════════════════ */

function LoopChip({ id }: { id: LoopId }) {
  const loop = LOOPS[id]
  return (
    <span className="inline-flex items-center gap-1.5 rounded-md border border-white/[0.08] px-2 py-1 text-xs text-titanium">
      <LoopSwatch id={id} />
      {loop.label}
    </span>
  )
}

function LoopSwatch({ id, dashed }: { id: LoopId; dashed?: boolean }) {
  const loop = LOOPS[id]
  const dotted = id === 'controls'
  return (
    <svg aria-hidden="true" width="18" height="6" viewBox="0 0 18 6" className="shrink-0">
      <line
        x1="1"
        x2="17"
        y1="3"
        y2="3"
        stroke={loop.color}
        strokeWidth={dotted ? 1.5 : 2}
        strokeLinecap={dotted ? 'round' : 'butt'}
        strokeDasharray={dotted ? '0.5 3' : dashed ? '4 2.5' : undefined}
      />
    </svg>
  )
}

interface PanelProps {
  selected: GroupId | null
  mode: Mode
  stage: number
  touched: boolean
  onSelect: (id: GroupId | null) => void
}

function DetailPanel({ selected, mode, stage, touched, onSelect }: PanelProps) {
  const group = selected ? GROUP_BY_ID[selected] : null
  const idx = selected ? GROUPS.findIndex((g) => g.id === selected) : -1

  return (
    <aside aria-live="polite" className="rounded-xl border border-white/[0.08] bg-surface-raised p-5 md:p-6 xl:min-h-full">
      <AnimatePresence mode="wait" initial={false}>
        {group ? (
          <motion.div key={group.id} initial={{ opacity: 0, y: 6 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="flex items-center justify-between gap-3 mb-4">
              <button
                type="button"
                onClick={() => onSelect(null)}
                className="text-xs font-medium text-muted hover:text-white transition-colors inline-flex items-center gap-1"
              >
                <span aria-hidden="true">←</span> All systems
              </button>
              <span className="font-mono text-[11px] text-muted tabular-nums">
                {idx + 1} / {GROUPS.length}
              </span>
            </div>

            {mode === 'outage' && selected === 'generators' && <OutageSequence stage={stage} />}

            <h4 className="font-sans text-lg font-semibold tracking-tight text-white">{group.name}</h4>
            <div className="mt-3 flex flex-wrap gap-1.5">
              {group.loops.map((l) => (
                <LoopChip key={l} id={l} />
              ))}
            </div>
            <p className="mt-4 text-sm text-titanium leading-relaxed">{group.description}</p>

            <dl className="mt-5 divide-y divide-white/[0.06] border-y border-white/[0.06]">
              {group.params.map((p) => (
                <div key={p.label} className="flex items-baseline justify-between gap-4 py-2">
                  <dt className="text-xs text-muted">{p.label}</dt>
                  <dd className="text-sm text-white text-right">{p.value}</dd>
                </div>
              ))}
            </dl>
            {group.note && <p className="mt-2 font-mono text-[11px] text-muted leading-relaxed">{group.note}</p>}

            <div className="mt-5 border-l-2 border-copper pl-3">
              <div className="font-mono text-[11px] uppercase tracking-widest text-copper-light">Evan&rsquo;s role</div>
              <p className="mt-1 text-sm text-white/90 leading-relaxed">{group.role}</p>
            </div>

            <div className="mt-5 flex justify-between gap-3">
              <button
                type="button"
                onClick={() => onSelect(GROUPS[(idx - 1 + GROUPS.length) % GROUPS.length].id)}
                className="rounded-lg border border-white/[0.08] px-3 py-1.5 text-xs font-medium text-muted hover:text-white hover:border-white/20 transition-colors"
              >
                ← Previous
              </button>
              <button
                type="button"
                onClick={() => onSelect(GROUPS[(idx + 1) % GROUPS.length].id)}
                className="rounded-lg border border-white/[0.08] px-3 py-1.5 text-xs font-medium text-muted hover:text-white hover:border-white/20 transition-colors"
              >
                Next →
              </button>
            </div>
          </motion.div>
        ) : (
          <motion.div key="list" initial={{ opacity: 0, y: 6 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            {mode === 'outage' && <OutageSequence stage={stage} />}
            <div className="flex items-center gap-2">
              {!touched && <span aria-hidden="true" className="inline-block w-2 h-2 rounded-full bg-copper-light animate-heartbeat" />}
              <span className="font-mono text-[11px] uppercase tracking-widest text-copper-light">Select a system</span>
            </div>
            <p className="mt-2 text-sm text-titanium">Tap equipment in the drawing, or choose from the list.</p>
            <ul className="mt-4 divide-y divide-white/[0.06]">
              {GROUPS.map((g) => (
                <li key={g.id}>
                  <button
                    type="button"
                    onClick={() => onSelect(g.id)}
                    className="group w-full flex items-center justify-between gap-3 py-2.5 text-left"
                  >
                    <span className="min-w-0">
                      <span className="block text-sm font-medium text-white group-hover:text-copper-light transition-colors">{g.name}</span>
                      <span className="block text-xs text-muted">{g.short}</span>
                    </span>
                    <span className="flex items-center gap-1 shrink-0">
                      {g.loops.map((l) => (
                        <LoopSwatch key={l} id={l} />
                      ))}
                    </span>
                  </button>
                </li>
              ))}
            </ul>
          </motion.div>
        )}
      </AnimatePresence>
    </aside>
  )
}

function OutageSequence({ stage }: { stage: number }) {
  return (
    <div className="mb-5 rounded-lg border border-white/[0.08] bg-surface p-4">
      <div className="font-mono text-[11px] uppercase tracking-widest text-copper-light">Utility outage · simulated</div>
      <ol className="mt-3 space-y-2.5">
        {OUTAGE_STEPS.map((s, i) => {
          const done = stage >= i + 1
          return (
            <li key={s.title} className={`flex gap-3 transition-opacity duration-300 ${done ? 'opacity-100' : 'opacity-40'}`}>
              <span
                className="mt-0.5 flex h-5 w-5 shrink-0 items-center justify-center rounded-full border text-[11px] font-semibold tabular-nums"
                style={{
                  borderColor: done ? (i === 0 ? ALERT : LOOPS.power.color) : 'rgba(255,255,255,0.15)',
                  color: done ? chart.text.primary : chart.text.muted,
                }}
              >
                {i + 1}
              </span>
              <span>
                <span className="block text-sm font-medium text-white">{s.title}</span>
                <span className="block text-xs text-muted leading-relaxed">{s.detail}</span>
              </span>
            </li>
          )
        })}
      </ol>
    </div>
  )
}

function LoopLegend() {
  const items: { id: LoopId; text: string; ret?: string }[] = LOOP_ORDER.map((id) => {
    const l = LOOPS[id]
    return { id, text: l.supply, ret: l.return }
  })
  return (
    <ul className="flex flex-wrap gap-x-6 gap-y-2.5" aria-label="Loop legend">
      {items.map((it) => (
        <li key={it.id} className="flex items-center gap-2 text-xs text-titanium">
          <LoopSwatch id={it.id} />
          {it.ret && <LoopSwatch id={it.id} dashed />}
          <span>
            {it.text}
            {it.ret && <span className="text-muted"> / {it.ret}</span>}
          </span>
        </li>
      ))}
      <li className="flex items-center gap-2 text-xs text-titanium">
        <svg aria-hidden="true" width="18" height="6" viewBox="0 0 18 6">
          <line x1="1" x2="17" y1="3" y2="3" stroke={DEAD} strokeWidth="1.5" strokeDasharray="4 2.5" />
        </svg>
        <span className="text-muted">De-energized / standby</span>
      </li>
    </ul>
  )
}
