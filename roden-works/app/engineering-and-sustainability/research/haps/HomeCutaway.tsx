'use client'

import { motion, useReducedMotion } from 'framer-motion'
import {
  useCallback,
  useEffect,
  useMemo,
  useState,
  type FocusEvent,
  type KeyboardEvent,
  type ReactNode,
} from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import SegmentedControl from '@/components/ui/SegmentedControl'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'
import {
  MONITORS,
  POLLUTANTS,
  POLLUTANT_ORDER,
  SOURCES,
  monitorMatches,
  sourceMatches,
  type Filter,
  type Monitor,
  type PollutantId,
  type Pt,
  type Source,
} from './data'
import { PollutantMark } from './icons'
import { Cite } from '@/components/ui/Sources'
import { HAPS_SOURCES } from './sources'

export const FILTER_OPTIONS: { value: Filter; label: string }[] = [
  { value: 'all', label: 'All' },
  { value: 'pm25', label: 'PM2.5' },
  { value: 'bc', label: 'Black carbon' },
  { value: 'no2', label: 'NO₂' },
]

/* ─── Layout constants ──────────────────────────────────────────────────── */

const W = 1200 // base drawing width
const CROP = { y0: 100, y1: 472 } // vertical window onto the drawing (full view)
const COMPACT_Y0 = 104
const FULL_MIN = 860 // narrower than this → numbered markers + key list
const SPLIT_MIN = 600 // narrower than this → section broken into two rows
const ROWS_BROKEN = [
  { x0: 0, x1: 574 }, // street, porch, front room
  { x0: 552, x1: W }, // middle room, kitchen, yard
]
const TOP = 86 // px above the drawing for source callouts
const BOTTOM = 66 // px below the drawing for monitor callouts
const RING_SOURCE = 17
const RING_MONITOR = 12
const DIM = 0.28

/* Neutral illustration inks — identity colour lives only in pollutant marks. */
const INK = {
  grid: '#1A2226',
  ground: '#10161A',
  cut: '#65737D', // section-cut outline
  poche: '#262F35', // cut fill: walls, floor, piers
  line: '#56636D', // object outlines
  soft: '#39444C', // beyond-cut detail
  fill: '#1D252A', // furniture
  interior: '#182125',
  attic: '#151C20',
  glass: '#1B252A',
  person: '#3B4852',
  warm: '#C9D2D8',
  flame: '#9AAAB5',
}

type ItemKind = { kind: 'source'; src: Source } | { kind: 'monitor'; mon: Monitor }
const ITEMS: Record<string, ItemKind> = {}
SOURCES.forEach((src) => (ITEMS[src.id] = { kind: 'source', src }))
MONITORS.forEach((mon) => (ITEMS[mon.id] = { kind: 'monitor', mon }))

const isMatch = (id: string, f: Filter) => {
  const it = ITEMS[id]
  return it.kind === 'source' ? sourceMatches(it.src, f) : monitorMatches(it.mon, f)
}

/* ─── Text measurement (label layout in real pixels) ────────────────────── */

let measureCtx: CanvasRenderingContext2D | null = null
function useTextWidth() {
  const [fontsReady, setFontsReady] = useState(0)
  useEffect(() => {
    let live = true
    document.fonts?.ready.then(() => live && setFontsReady((n) => n + 1))
    return () => {
      live = false
    }
  }, [])
  return useCallback(
    (text: string, size: number, weight = 400, family: 'sans' | 'mono' = 'sans') => {
      if (typeof document === 'undefined') return text.length * size * 0.56
      if (!measureCtx) measureCtx = document.createElement('canvas').getContext('2d')
      if (!measureCtx) return text.length * size * 0.56
      const root = getComputedStyle(document.documentElement)
      const face =
        root.getPropertyValue(family === 'mono' ? '--font-geist-mono' : '--font-geist-sans').trim() ||
        (family === 'mono' ? 'monospace' : 'sans-serif')
      measureCtx.font = `${weight} ${size}px ${face}`
      return measureCtx.measureText(text).width
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [fontsReady],
  )
}

/** Packs labels along a rail: each wants to sit over its anchor, overlapping
 *  neighbours merge into a cluster centred on their anchors, clamped to bounds. */
function layoutRail(
  items: { id: string; want: number; w: number }[],
  minX: number,
  maxX: number,
  gap: number,
) {
  type Cluster = { items: typeof items; start: number; width: number }
  const sorted = [...items].sort((a, b) => a.want - b.want)
  const place = (c: Cluster) => {
    let offset = 0
    let sum = 0
    c.items.forEach((it) => {
      sum += it.want - (offset + it.w / 2)
      offset += it.w + gap
    })
    c.width = offset - gap
    c.start = Math.min(Math.max(sum / c.items.length, minX), maxX - c.width)
  }
  let clusters: Cluster[] = sorted.map((it) => {
    const c = { items: [it], start: 0, width: 0 }
    place(c)
    return c
  })
  let merged = true
  while (merged) {
    merged = false
    for (let i = 0; i < clusters.length - 1; i++) {
      const a = clusters[i]
      const b = clusters[i + 1]
      if (a.start + a.width + gap > b.start) {
        const c = { items: [...a.items, ...b.items], start: 0, width: 0 }
        place(c)
        clusters = [...clusters.slice(0, i), c, ...clusters.slice(i + 2)]
        merged = true
        break
      }
    }
  }
  const out: Record<string, number> = {}
  clusters.forEach((c) => {
    let x = c.start
    c.items.forEach((it) => {
      out[it.id] = x + it.w / 2
      x += it.w + gap
    })
  })
  return out
}

/* ─── Public component ──────────────────────────────────────────────────── */

export default function HomeCutaway({
  filter,
  onFilterChange,
}: {
  filter: Filter
  onFilterChange: (f: Filter) => void
}) {
  return (
    <ChartFrame
      title="Where indoor pollution comes from, and how the study measured it"
      subtitle="Schematic section of a New Orleans shotgun house. Select a source or monitor for details."
      actions={
        <SegmentedControl<Filter>
          label="Filter by pollutant"
          value={filter}
          onChange={onFilterChange}
          options={FILTER_OPTIONS}
        />
      }
      legend={<DiagramLegend />}
      note="Illustrative schematic: source and monitor positions are representative, not a specific study home."
      table={{
        caption: 'Pollution sources and study monitors shown in the house section',
        columns: ['Element', 'Type', 'Location', 'Pollutant or measure'],
        rows: [
          ...SOURCES.map((s) => [
            s.name,
            'Source',
            s.location,
            s.emits.map((p) => POLLUTANTS[p].label).join(', '),
          ]),
          ...MONITORS.map((m) => [m.name, 'Monitor', m.placement, m.measures]),
        ],
      }}
    >
      <CutawayDiagram filter={filter} />
    </ChartFrame>
  )
}

function DiagramLegend() {
  return (
    <ul className="flex flex-wrap items-center gap-x-5 gap-y-2 text-xs text-titanium">
      {POLLUTANT_ORDER.map((id) => (
        <li key={id} className="flex items-center gap-2">
          <PollutantMark id={id} size={10} />
          {POLLUTANTS[id].label}
          {POLLUTANTS[id].kind === 'gas' ? ' (gas)' : ''}
        </li>
      ))}
      <li className="flex items-center gap-2">
        <svg width="20" height="6" aria-hidden="true">
          <line
            x1="2"
            y1="3"
            x2="18"
            y2="3"
            stroke={chart.text.muted}
            strokeWidth="2"
            strokeDasharray="0.01 5"
            strokeLinecap="round"
          />
        </svg>
        Outdoor air seeping in
      </li>
    </ul>
  )
}

/* ─── Interaction state ─────────────────────────────────────────────────── */

interface UI {
  activeId: string | null
  pinnedId: string | null
  focusId: string | null
  hover: (id: string | null) => void
  togglePin: (id: string) => void
  clear: () => void
  focus: (id: string | null) => void
}

function CutawayDiagram({ filter }: { filter: Filter }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduce = useReducedMotion() ?? false
  const [hoverId, setHoverId] = useState<string | null>(null)
  const [pinnedId, setPinnedId] = useState<string | null>(null)
  const [focusId, setFocusId] = useState<string | null>(null)

  const ui: UI = {
    activeId: hoverId ?? pinnedId,
    pinnedId,
    focusId,
    hover: setHoverId,
    togglePin: (id) => setPinnedId((p) => (p === id ? null : id)),
    clear: () => {
      setHoverId(null)
      setPinnedId(null)
    },
    focus: setFocusId,
  }

  const mode = width >= FULL_MIN ? 'full' : width >= SPLIT_MIN ? 'compact' : 'broken'

  return (
    <div
      ref={ref}
      className="relative w-full"
      style={{ minHeight: width ? undefined : 340 }}
      onKeyDown={(e) => {
        if (e.key === 'Escape') ui.clear()
      }}
    >
      {width > 0 &&
        (mode === 'full' ? (
          <FullView width={width} filter={filter} reduce={reduce} ui={ui} />
        ) : (
          <CompactView width={width} broken={mode === 'broken'} filter={filter} reduce={reduce} ui={ui} />
        ))}
    </div>
  )
}

/** Keyboard/pointer wiring shared by every selectable element. */
function selectable(id: string, label: string, ui: UI, focusable = true) {
  return {
    role: 'button' as const,
    tabIndex: focusable ? 0 : -1,
    'aria-label': label,
    'aria-pressed': ui.pinnedId === id,
    style: { outline: 'none', cursor: 'pointer' },
    onPointerEnter: (e: { pointerType: string }) => {
      if (e.pointerType !== 'touch') ui.hover(id)
    },
    onPointerLeave: () => ui.hover(null),
    onClick: (e: { stopPropagation: () => void }) => {
      e.stopPropagation()
      ui.togglePin(id)
    },
    onKeyDown: (e: KeyboardEvent) => {
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault()
        ui.togglePin(id)
      }
    },
    onFocus: (e: FocusEvent<Element>) => {
      ui.hover(id)
      if (e.currentTarget.matches(':focus-visible')) ui.focus(id)
    },
    onBlur: () => {
      ui.hover(null)
      ui.focus(null)
    },
  }
}

function ariaFor(id: string) {
  const it = ITEMS[id]
  if (it.kind === 'source') {
    const s = it.src
    return `${s.name}, ${s.location}. Releases ${s.emits.map((p) => POLLUTANTS[p].label).join(' and ')}. ${s.detail}`
  }
  const m = it.mon
  return `${m.name}, ${m.placement.toLowerCase()}. Measures ${m.measures}. ${m.detail}`
}

/* ─── Full view: rails of callouts above and below the section ─────────── */

function FullView({ width, filter, reduce, ui }: { width: number; filter: Filter; reduce: boolean; ui: UI }) {
  const tw = useTextWidth()
  const s = width / W
  const houseH = (CROP.y1 - CROP.y0) * s
  const hb = TOP + houseH // bottom edge of the drawing
  const height = hb + BOTTOM
  const toPx = (p: Pt) => ({ x: p.x * s, y: TOP + (p.y - CROP.y0) * s })

  const railTitle = 'STUDY MONITORS'
  const railTitleW = tw(railTitle, 11, 400, 'mono') + railTitle.length * 11 * 0.12

  const sourceW = (src: Source) => {
    const head = tw(`0${src.n}`, 11, 400, 'mono') + 6 + tw(src.name, 12.5, 500)
    return Math.max(head, chipsWidth(src.emits, tw))
  }
  const monitorW = (m: Monitor) => {
    const head = tw(m.key, 11, 400, 'mono') + 6 + tw(m.short, 12.5, 500)
    const sub = (m.pollutant ? 15 : 0) + tw(m.measures, 11)
    return Math.max(head, sub)
  }

  const topX = layoutRail(
    SOURCES.map((src) => ({ id: src.id, want: src.anchor.x * s, w: sourceW(src) })),
    0,
    width,
    20,
  )
  const bottomX = layoutRail(
    MONITORS.map((m) => ({ id: m.id, want: m.anchor.x * s, w: monitorW(m) })),
    railTitleW + 28,
    width,
    20,
  )

  const active = ui.activeId ? ITEMS[ui.activeId] : null

  return (
    <div className="relative" style={{ height }}>
      <svg
        width={width}
        height={height}
        className="block overflow-visible select-none"
        role="group"
        aria-label="Section drawing of a shotgun house showing five indoor pollution sources and the study's four monitors"
        onClick={() => ui.clear()}
      >
        <g transform={`translate(0 ${TOP - CROP.y0 * s}) scale(${s})`}>
          <HouseScene u={1 / s} filter={filter} reduce={reduce} activeId={ui.activeId} />
        </g>

        <text x={0} y={14} className="font-mono" fontSize={11} letterSpacing="0.12em" fill={chart.text.muted}>
          SOURCES
        </text>
        <text x={0} y={hb + 28} className="font-mono" fontSize={11} letterSpacing="0.12em" fill={chart.text.muted}>
          {railTitle}
        </text>

        {SOURCES.map((src) => {
          const a = toPx(src.anchor)
          const cx = topX[src.id]
          const on = ui.activeId === src.id
          const match = sourceMatches(src, filter)
          const lead = leaderEnd(a, { x: cx, y: 66 }, RING_SOURCE)
          return (
            <g key={src.id} {...selectable(src.id, ariaFor(src.id), ui)} opacity={match ? 1 : DIM}>
              <path
                d={`M${cx} 60V66L${lead.x} ${lead.y}`}
                fill="none"
                stroke={on ? chart.text.secondary : chart.axis}
                strokeWidth={1}
              />
              <SourceLabel src={src} cx={cx} filter={filter} tw={tw} />
              <Ring
                x={a.x}
                y={a.y}
                r={RING_SOURCE}
                on={on}
                focused={ui.focusId === src.id}
                tint={filter !== 'all' && match ? POLLUTANTS[filter as PollutantId].color : undefined}
                reduce={reduce}
              />
              {/* generous hit area: the ring plus the label block */}
              <circle cx={a.x} cy={a.y} r={RING_SOURCE + 6} fill="transparent" />
              <rect x={cx - sourceW(src) / 2 - 6} y={20} width={sourceW(src) + 12} height={40} fill="transparent" />
            </g>
          )
        })}

        {MONITORS.map((m) => {
          const a = toPx(m.anchor)
          const cx = bottomX[m.id]
          const on = ui.activeId === m.id
          const match = monitorMatches(m, filter)
          const lead = leaderEnd(a, { x: cx, y: hb + 6 }, RING_MONITOR)
          return (
            <g key={m.id} {...selectable(m.id, ariaFor(m.id), ui)} opacity={match ? 1 : DIM}>
              <path
                d={`M${lead.x} ${lead.y}L${cx} ${hb + 6}V${hb + 13}`}
                fill="none"
                stroke={on ? chart.text.secondary : chart.axis}
                strokeWidth={1}
              />
              <MonitorLabel mon={m} cx={cx} y={hb + 28} tw={tw} />
              <Ring
                x={a.x}
                y={a.y}
                r={RING_MONITOR}
                on={on}
                focused={ui.focusId === m.id}
                tint={filter !== 'all' && match && m.pollutant ? POLLUTANTS[m.pollutant].color : undefined}
                reduce={reduce}
                square
              />
              <circle cx={a.x} cy={a.y} r={RING_MONITOR + 5} fill="transparent" />
              <rect x={cx - monitorW(m) / 2 - 6} y={hb + 14} width={monitorW(m) + 12} height={38} fill="transparent" />
            </g>
          )
        })}
      </svg>

      {active && <FloatingDetail item={active} filter={filter} toPx={toPx} width={width} height={height} />}
    </div>
  )
}

function leaderEnd(anchor: Pt, from: Pt, r: number): Pt {
  const dx = from.x - anchor.x
  const dy = from.y - anchor.y
  const len = Math.hypot(dx, dy) || 1
  return { x: anchor.x + (dx / len) * (r + 1), y: anchor.y + (dy / len) * (r + 1) }
}

type Measure = ReturnType<typeof useTextWidth>

function chipsWidth(ids: PollutantId[], tw: Measure) {
  return ids.reduce((sum, p, i) => sum + 15 + tw(POLLUTANTS[p].label, 11) + (i ? 12 : 0), 0)
}

function SourceLabel({ src, cx, filter, tw }: { src: Source; cx: number; filter: Filter; tw: Measure }) {
  const numW = tw(`0${src.n}`, 11, 400, 'mono')
  const nameW = tw(src.name, 12.5, 500)
  const headStart = cx - (numW + 6 + nameW) / 2
  const chipsW = chipsWidth(src.emits, tw)
  let x = cx - chipsW / 2
  return (
    <g>
      <text x={headStart} y={36} fontSize={11} className="font-mono" fill={chart.text.muted}>
        {`0${src.n}`}
      </text>
      <text x={headStart + numW + 6} y={36} fontSize={12.5} fontWeight={500} fill={chart.text.primary}>
        {src.name}
      </text>
      {src.emits.map((p, i) => {
        const x0 = x + (i ? 12 : 0)
        const labelW = tw(POLLUTANTS[p].label, 11)
        x = x0 + 15 + labelW
        const faded = filter !== 'all' && filter !== p
        return (
          <g key={p} opacity={faded ? 0.4 : 1}>
            <MarkSvg id={p} x={x0 + 5} y={49} />
            <text x={x0 + 15} y={53} fontSize={11} fill={chart.text.secondary}>
              {POLLUTANTS[p].label}
            </text>
          </g>
        )
      })}
    </g>
  )
}

function MonitorLabel({ mon, cx, y, tw }: { mon: Monitor; cx: number; y: number; tw: Measure }) {
  const keyW = tw(mon.key, 11, 400, 'mono')
  const nameW = tw(mon.short, 12.5, 500)
  const headStart = cx - (keyW + 6 + nameW) / 2
  const subW = (mon.pollutant ? 15 : 0) + tw(mon.measures, 11)
  const subStart = cx - subW / 2
  return (
    <g>
      <text x={headStart} y={y} fontSize={11} className="font-mono" fill={chart.text.muted}>
        {mon.key}
      </text>
      <text x={headStart + keyW + 6} y={y} fontSize={12.5} fontWeight={500} fill={chart.text.primary}>
        {mon.short}
      </text>
      {mon.pollutant && <MarkSvg id={mon.pollutant} x={subStart + 5} y={y + 13} />}
      <text x={subStart + (mon.pollutant ? 15 : 0)} y={y + 17} fontSize={11} fill={chart.text.secondary}>
        {mon.measures}
      </text>
    </g>
  )
}

/** Pollutant mark drawn inside an SVG (dot = particles, ring = gas). */
function MarkSvg({ id, x, y, r = 4.5 }: { id: PollutantId; x: number; y: number; r?: number }) {
  const p = POLLUTANTS[id]
  return p.kind === 'gas' ? (
    <circle cx={x} cy={y} r={r - 1} fill="none" stroke={p.color} strokeWidth={2} />
  ) : (
    <circle cx={x} cy={y} r={r} fill={p.color} />
  )
}

function Ring({
  x,
  y,
  r,
  on,
  focused,
  tint,
  reduce,
  square = false,
}: {
  x: number
  y: number
  r: number
  on: boolean
  focused: boolean
  tint?: string
  reduce: boolean
  square?: boolean
}) {
  const stroke = focused ? '#D08C4F' : on ? chart.text.primary : tint ?? 'rgba(242,245,247,0.42)'
  const sw = focused || on || tint ? 1.5 : 1
  const shape = (rr: number, props: Record<string, unknown>) =>
    square ? (
      <rect x={x - rr} y={y - rr} width={rr * 2} height={rr * 2} rx={4} {...props} />
    ) : (
      <circle cx={x} cy={y} r={rr} {...props} />
    )
  return (
    <g pointerEvents="none">
      {shape(r, { fill: on ? 'rgba(242,245,247,0.07)' : 'rgba(242,245,247,0.02)', stroke, strokeWidth: sw })}
      {on && !reduce && (
        <motion.circle
          cx={x}
          cy={y}
          fill="none"
          stroke={chart.text.primary}
          strokeWidth={1}
          initial={{ r, opacity: 0.5 }}
          animate={{ r: r + 10, opacity: 0 }}
          transition={{ duration: 1.4, repeat: Infinity, ease: 'easeOut' }}
        />
      )}
    </g>
  )
}

function FloatingDetail({
  item,
  filter,
  toPx,
  width,
  height,
}: {
  item: ItemKind
  filter: Filter
  toPx: (p: Pt) => Pt
  width: number
  height: number
}) {
  const CARD_W = 272
  const anchor = toPx(item.kind === 'source' ? item.src.anchor : item.mon.anchor)
  const gapX = item.kind === 'source' ? RING_SOURCE + 14 : RING_MONITOR + 14
  const right = anchor.x + gapX + CARD_W < width - 4
  const left = right ? anchor.x + gapX : anchor.x - gapX - CARD_W
  const top = Math.min(Math.max(anchor.y, TOP + 64), height - 70)
  return (
    <div
      aria-hidden="true"
      className="pointer-events-none absolute z-20 rounded-xl border border-white/10 bg-[#0D1417]/95 p-4 shadow-2xl shadow-black/40 backdrop-blur-sm"
      style={{ left, top, width: CARD_W, transform: 'translateY(-50%)' }}
    >
      <DetailBody item={item} filter={filter} />
    </div>
  )
}

function DetailBody({ item, filter }: { item: ItemKind; filter: Filter }) {
  if (item.kind === 'source') {
    const s = item.src
    return (
      <>
        <p className="font-mono text-[11px] uppercase tracking-wider text-muted">
          Source 0{s.n} · {s.location}
        </p>
        <p className="mt-1 text-[15px] font-medium text-white">{s.name}</p>
        <Chips ids={s.emits} filter={filter} />
        {/* No <Cite> here: this hover card is aria-hidden and pointer-events-none, so its
            links could not be clicked and would be focusable inside hidden content. The
            same detail is cited in the key list below the drawing (KeyRow). */}
        <p className="mt-2 text-[13px] leading-relaxed text-titanium">{s.detail}</p>
      </>
    )
  }
  const m = item.mon
  return (
    <>
      <p className="font-mono text-[11px] uppercase tracking-wider text-muted">
        Monitor {m.key} · {m.placement}
      </p>
      <p className="mt-1 text-[15px] font-medium text-white">{m.name}</p>
      <div className="mt-2 flex items-center gap-1.5 text-xs text-white/90">
        {m.pollutant && <PollutantMark id={m.pollutant} size={9} />}
        {m.measures}
      </div>
      <p className="mt-2 text-[13px] leading-relaxed text-titanium">{m.detail}</p>
    </>
  )
}

function Chips({ ids, filter }: { ids: PollutantId[]; filter: Filter }) {
  return (
    <ul className="mt-2 flex flex-wrap gap-1.5">
      {ids.map((p) => (
        <li
          key={p}
          className="inline-flex items-center gap-1.5 rounded-full border border-white/10 bg-white/[0.03] px-2 py-0.5 text-xs text-white/90"
          style={{ opacity: filter !== 'all' && filter !== p ? 0.45 : 1 }}
        >
          <PollutantMark id={p} size={8} />
          {POLLUTANTS[p].label}
        </li>
      ))}
    </ul>
  )
}

/* ─── Compact views: numbered markers + key list ────────────────────────── */

function CompactView({
  width,
  broken,
  filter,
  reduce,
  ui,
}: {
  width: number
  broken: boolean
  filter: Filter
  reduce: boolean
  ui: UI
}) {
  const rows = broken ? ROWS_BROKEN : [{ x0: 0, x1: W }]
  const span = broken ? ROWS_BROKEN[1].x1 - ROWS_BROKEN[1].x0 : W
  const s = Math.min(width / span, 1)
  return (
    <div>
      <div className={broken ? 'space-y-2' : ''}>
        {rows.map((row, i) => (
          <CompactRow
            key={row.x0}
            row={row}
            s={s}
            filter={filter}
            reduce={reduce}
            ui={ui}
            breakRight={broken && i === 0}
            breakLeft={broken && i === 1}
          />
        ))}
      </div>
      <KeyList filter={filter} ui={ui} />
    </div>
  )
}

function CompactRow({
  row,
  s,
  filter,
  reduce,
  ui,
  breakLeft,
  breakRight,
}: {
  row: { x0: number; x1: number }
  s: number
  filter: Filter
  reduce: boolean
  ui: UI
  breakLeft: boolean
  breakRight: boolean
}) {
  const w = (row.x1 - row.x0) * s
  const h = (CROP.y1 - COMPACT_Y0) * s
  const toPx = (p: Pt) => ({ x: (p.x - row.x0) * s, y: (p.y - COMPACT_Y0) * s })
  const inRow = (p: Pt) => p.x >= row.x0 && p.x < row.x1 && !(breakLeft && p.x < ROWS_BROKEN[0].x1)
  const R = 9.5

  const markers = [
    ...SOURCES.filter((src) => inRow(src.anchor)).map((src) => ({
      id: src.id,
      label: String(src.n),
      anchor: src.anchor,
      badge: src.badge,
      square: false,
      tint: filter !== 'all' && sourceMatches(src, filter) ? POLLUTANTS[filter as PollutantId].color : undefined,
    })),
    ...MONITORS.filter((m) => inRow(m.anchor)).map((m) => ({
      id: m.id,
      label: m.key,
      anchor: m.anchor,
      badge: m.badge,
      square: true,
      tint: filter !== 'all' && monitorMatches(m, filter) && m.pollutant ? POLLUTANTS[m.pollutant].color : undefined,
    })),
  ]

  return (
    <svg
      width={w}
      height={h}
      className="block max-w-full select-none"
      aria-hidden="true"
      onClick={() => ui.clear()}
    >
      <g transform={`translate(${-row.x0 * s} ${-COMPACT_Y0 * s}) scale(${s})`}>
        <HouseScene u={1 / s} filter={filter} reduce={reduce} activeId={ui.activeId} />
      </g>
      {breakRight && <BreakLine x={w - 1} h={h} />}
      {breakLeft && <BreakLine x={1} h={h} />}
      {markers.map((mk) => {
        const a = toPx(mk.anchor)
        const b = toPx(mk.badge)
        const on = ui.activeId === mk.id
        const match = isMatch(mk.id, filter)
        const end = leaderEnd(a, b, 2.5)
        const start = leaderEnd(b, a, R)
        return (
          <g key={mk.id} {...selectable(mk.id, ariaFor(mk.id), ui, false)} opacity={match ? 1 : DIM}>
            <line
              x1={start.x}
              y1={start.y}
              x2={end.x}
              y2={end.y}
              stroke={on ? chart.text.primary : 'rgba(242,245,247,0.45)'}
              strokeWidth={1}
            />
            <circle cx={a.x} cy={a.y} r={2.5} fill={on ? chart.text.primary : 'rgba(242,245,247,0.7)'} />
            {mk.square ? (
              <rect
                x={b.x - R}
                y={b.y - R}
                width={R * 2}
                height={R * 2}
                rx={4}
                fill="#0D1417"
                stroke={on ? chart.text.primary : mk.tint ?? 'rgba(242,245,247,0.45)'}
                strokeWidth={on || mk.tint ? 1.5 : 1}
              />
            ) : (
              <circle
                cx={b.x}
                cy={b.y}
                r={R}
                fill="#0D1417"
                stroke={on ? chart.text.primary : mk.tint ?? 'rgba(242,245,247,0.45)'}
                strokeWidth={on || mk.tint ? 1.5 : 1}
              />
            )}
            <text
              x={b.x}
              y={b.y}
              dy="0.35em"
              textAnchor="middle"
              fontSize={11}
              fontWeight={600}
              fill={chart.text.primary}
            >
              {mk.label}
            </text>
            <circle cx={b.x} cy={b.y} r={16} fill="transparent" />
          </g>
        )
      })}
    </svg>
  )
}

function BreakLine({ x, h }: { x: number; h: number }) {
  const m = h / 2
  return (
    <path
      d={`M${x} 0V${m - 9}l-4 3 8 6-4 3V${h}`}
      fill="none"
      stroke={chart.axis}
      strokeWidth={1}
      strokeLinejoin="round"
    />
  )
}

function KeyList({ filter, ui }: { filter: Filter; ui: UI }) {
  return (
    <div className="mt-5 grid gap-6 sm:grid-cols-2">
      <KeyGroup title="Sources">
        {SOURCES.map((src) => (
          <KeyRow
            key={src.id}
            id={src.id}
            badge={String(src.n)}
            name={src.name}
            meta={src.location}
            filter={filter}
            ui={ui}
            marks={src.emits}
            detail={src.detail}
            cites={src.cites}
          />
        ))}
      </KeyGroup>
      <KeyGroup title="Study monitors">
        {MONITORS.map((m) => (
          <KeyRow
            key={m.id}
            id={m.id}
            badge={m.key}
            square
            name={m.name}
            meta={m.measures}
            filter={filter}
            ui={ui}
            marks={m.pollutant ? [m.pollutant] : []}
            detail={m.detail}
          />
        ))}
      </KeyGroup>
    </div>
  )
}

function KeyGroup({ title, children }: { title: string; children: ReactNode }) {
  return (
    <div>
      <h4 className="mb-1 font-mono text-[11px] uppercase tracking-widest text-muted">{title}</h4>
      <ul className="divide-y divide-white/[0.06]">{children}</ul>
    </div>
  )
}

function KeyRow({
  id,
  badge,
  square = false,
  name,
  meta,
  marks,
  detail,
  cites,
  filter,
  ui,
}: {
  id: string
  badge: string
  square?: boolean
  name: string
  meta: string
  marks: PollutantId[]
  detail: string
  cites?: string[]
  filter: Filter
  ui: UI
}) {
  const open = ui.pinnedId === id
  const on = ui.activeId === id
  const match = isMatch(id, filter)
  return (
    <li style={{ opacity: match ? 1 : 0.4 }} className="transition-opacity duration-300">
      <button
        type="button"
        aria-expanded={open}
        onClick={() => ui.togglePin(id)}
        onPointerEnter={(e) => e.pointerType !== 'touch' && ui.hover(id)}
        onPointerLeave={() => ui.hover(null)}
        className="flex w-full items-center gap-3 py-2.5 text-left"
      >
        <span
          aria-hidden="true"
          className={`flex h-[22px] w-[22px] shrink-0 items-center justify-center border text-[11px] font-semibold text-white ${
            square ? 'rounded-md' : 'rounded-full'
          } ${on || open ? 'border-white bg-white/10' : 'border-white/30'}`}
        >
          {badge}
        </span>
        <span className="min-w-0 flex-1">
          <span className="block text-sm text-white">{name}</span>
          <span className="block text-xs text-muted">{meta}</span>
        </span>
        <span className="flex items-center gap-1.5">
          {marks.map((p) => (
            <PollutantMark key={p} id={p} size={9} dim={filter !== 'all' && filter !== p} />
          ))}
          <span className="sr-only">
            {marks.length ? `${marks.map((p) => POLLUTANTS[p].label).join(', ')}` : ''}
          </span>
        </span>
        <svg
          aria-hidden="true"
          viewBox="0 0 16 16"
          className={`h-3.5 w-3.5 shrink-0 text-muted transition-transform duration-200 ${open ? 'rotate-180' : ''}`}
          fill="none"
          stroke="currentColor"
          strokeWidth="1.5"
        >
          <path d="M4 6l4 4 4-4" strokeLinecap="round" strokeLinejoin="round" />
        </svg>
      </button>
      {open && (
        <p className="pb-3 pl-[34px] pr-6 text-[13px] leading-relaxed text-titanium">
          {detail}
          {cites && cites.length > 0 && <Cite sources={HAPS_SOURCES} id={cites} />}
        </p>
      )}
    </li>
  )
}

/* ─── The drawing (base units; `u` = drawing units per screen pixel) ───── */

function flame(cx: number, by: number, h: number) {
  const w = h * 0.42
  return `M${cx} ${by}C${cx - w} ${by - h * 0.25} ${cx - w * 0.6} ${by - h * 0.7} ${cx} ${by - h}C${cx + w * 0.6} ${
    by - h * 0.7
  } ${cx + w} ${by - h * 0.25} ${cx} ${by}Z`
}

const GRID_X = Array.from({ length: 31 }, (_, i) => i * 40)
const GRID_Y = Array.from({ length: 9 }, (_, i) => 120 + i * 40)
const PIERS = [240, 330, 440, 550, 660, 770, 880, 990, 1072]
const WALLS = [
  { x: 300, w: 8 },
  { x: 560, w: 6 },
  { x: 800, w: 6 },
  { x: 1082, w: 8 },
]
const GROUND = 452

function HouseScene({
  u,
  filter,
  reduce,
  activeId,
}: {
  u: number
  filter: Filter
  reduce: boolean
  activeId: string | null
}) {
  const sw = (px: number) => px * u
  const dim = (id: string) => (isMatch(id, filter) ? 1 : DIM + 0.12)
  const cuffOn = activeId === 'abpm'

  return (
    <g aria-hidden="true">
      {/* Blueprint grid + ground */}
      <g stroke={INK.grid} strokeWidth={sw(1)}>
        {GRID_X.map((x) => (
          <line key={`gx${x}`} x1={x} x2={x} y1={80} y2={GROUND} />
        ))}
        {GRID_Y.map((y) => (
          <line key={`gy${y}`} x1={-20} x2={W + 20} y1={y} y2={y} />
        ))}
      </g>
      <rect x={-20} y={GROUND} width={W + 40} height={40} fill={INK.ground} />
      <line x1={-20} x2={W + 20} y1={GROUND} y2={GROUND} stroke={chart.axis} strokeWidth={sw(1)} />

      {/* Street: sidewalk + car */}
      <rect x={196} y={447} width={44} height={5} fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1)} />
      <g opacity={dim('traffic')}>
        <path
          d="M22 437L22 423Q22 415 31 413L62 410L81 392Q85 388 93 388L137 388Q145 388 149 393L163 410L180 412Q188 414 188 422L188 437Z"
          fill={INK.fill}
          stroke={INK.line}
          strokeWidth={sw(1.25)}
          strokeLinejoin="round"
        />
        <path d="M86 395Q88 393 92 393L115 393L115 408L72 408Z" fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
        <path d="M121 393L136 393Q141 393 144 397L154 408L121 408Z" fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
        <line x1={118} x2={118} y1={393} y2={434} stroke={INK.soft} strokeWidth={sw(1)} />
        {[54, 156].map((cx) => (
          <g key={cx}>
            <circle cx={cx} cy={440} r={11.5} fill={INK.ground} stroke={INK.line} strokeWidth={sw(1.25)} />
            <circle cx={cx} cy={440} r={4} fill="none" stroke={INK.soft} strokeWidth={sw(1)} />
          </g>
        ))}
        <rect x={188} y={431.5} width={7} height={3} rx={1} fill={INK.line} />
      </g>

      {/* Roof, attic, ceiling */}
      <path
        d="M216 190L216 184L380 112L944 112L1108 184L1108 190Z"
        fill={INK.attic}
        stroke={INK.cut}
        strokeWidth={sw(1.5)}
        strokeLinejoin="round"
      />
      <g stroke={INK.soft} strokeWidth={sw(1)}>
        {[460, 560, 660, 760, 860].map((x) => (
          <line key={x} x1={x} x2={x} y1={113} y2={190} />
        ))}
      </g>
      <rect x={300} y={190} width={790} height={6} fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1)} />

      {/* Room air */}
      <rect x={308} y={196} width={774} height={216} fill={INK.interior} />

      {/* Far-wall details (beyond the cut) */}
      {/* front-room window */}
      <rect x={438} y={224} width={40} height={128} rx={1} fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={458} x2={458} y1={224} y2={352} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={438} x2={478} y1={288} y2={288} stroke={INK.soft} strokeWidth={sw(1)} />
      <rect x={434} y={352} width={48} height={3} fill={INK.fill} stroke={INK.soft} strokeWidth={sw(1)} />
      {/* fireplace */}
      <rect x={318} y={196} width={100} height={216} fill="#1B2428" stroke={INK.soft} strokeWidth={sw(1)} />
      <path
        d="M340 412V360Q340 348 352 348H384Q396 348 396 360V412Z"
        fill="#0F1518"
        stroke={INK.line}
        strokeWidth={sw(1)}
      />
      <rect x={312} y={316} width={112} height={6} rx={1} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      {/* mirror over the dresser */}
      <rect x={594} y={262} width={48} height={62} rx={3} fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
      {/* kitchen window */}
      <rect x={1000} y={262} width={52} height={62} rx={1} fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={1026} x2={1026} y1={262} y2={324} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={1000} x2={1052} y1={293} y2={293} stroke={INK.soft} strokeWidth={sw(1)} />

      {/* Floor slab (porch → back wall) and piers */}
      <rect x={236} y={412} width={854} height={7} fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1.25)} />
      {PIERS.map((x) => (
        <rect key={x} x={x} y={419} width={12} height={33} fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1)} />
      ))}

      {/* Walls cut above the aligned doorways, with transoms */}
      {WALLS.map((wl) => (
        <g key={wl.x}>
          <rect x={wl.x} y={190} width={wl.w} height={64} fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1.25)} />
          <rect x={wl.x} y={254} width={wl.w} height={26} fill={INK.glass} stroke={INK.cut} strokeWidth={sw(1)} />
          <rect x={wl.x} y={280} width={wl.w} height={6} fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1.25)} />
        </g>
      ))}

      {/* Porch: steps, column, brackets */}
      <path d="M204 452V439H214V426H224V412H236V452Z" fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1)} />
      <rect x={245} y={193} width={7} height={219} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <rect x={242} y={189} width={13} height={5} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <rect x={242} y={406} width={13} height={6} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <path d="M252 206Q268 206 272 191M245 206Q229 206 225 191" fill="none" stroke={INK.line} strokeWidth={sw(1)} />

      {/* Back steps, yard */}
      <path d="M1090 412H1100V426H1110V439H1120V452H1090Z" fill={INK.poche} stroke={INK.cut} strokeWidth={sw(1)} />
      <g fill="none" stroke={INK.soft} strokeWidth={sw(1)}>
        <line x1={1166} x2={1166} y1={430} y2={372} />
        <circle cx={1166} cy={346} r={28} />
        {[1136, 1145, 1154, 1163, 1172, 1181, 1190].map((x) => (
          <path key={x} d={`M${x} 452V425L${x + 2.5} 421L${x + 5} 425V452`} fill={INK.ground} />
        ))}
        <line x1={1134} x2={1197} y1={431} y2={431} />
        <line x1={1134} x2={1197} y1={445} y2={445} />
      </g>

      {/* ── Front room ── */}
      {/* gas space heater in the hearth */}
      <g opacity={dim('heater')}>
        <rect x={348} y={368} width={40} height={44} rx={3} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1.25)} />
        {[354, 363, 372, 381].map((x) => (
          <rect key={x} x={x} y={374} width={4} height={21} rx={2} fill="none" stroke={INK.soft} strokeWidth={sw(1)} />
        ))}
        {[356, 364, 372, 380].map((x) => (
          <path key={x} d={flame(x, 407, 5)} fill={INK.flame} />
        ))}
      </g>
      {/* side table + cigarette in an ashtray */}
      <rect x={426} y={370} width={34} height={4} rx={1} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={431} x2={431} y1={374} y2={412} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={455} x2={455} y1={374} y2={412} stroke={INK.line} strokeWidth={sw(1)} />
      <g opacity={dim('tobacco')}>
        <rect x={434} y={365} width={18} height={5} rx={2.5} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
        <line x1={437} y1={365} x2={452} y2={360.5} stroke={INK.warm} strokeWidth={2.2} strokeLinecap="round" />
        <circle cx={452.8} cy={360.2} r={1.5} fill="#F2F5F7" />
      </g>
      {/* armchair + participant wearing the ambulatory BP monitor */}
      <rect x={536} y={314} width={14} height={74} rx={5} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <rect x={466} y={372} width={80} height={14} rx={4} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={472} x2={472} y1={386} y2={412} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={540} x2={540} y1={386} y2={412} stroke={INK.line} strokeWidth={sw(1)} />
      <g>
        <path d="M467 370L460 408H470L478 372Z" fill={INK.person} />
        <rect x={450} y={406} width={22} height={6} rx={3} fill={INK.person} />
        <path d="M471 359H512Q521 359 521 368V376H471Q463 376 463 367.5Q463 359 471 359Z" fill={INK.person} />
        <path d="M500 372L498 331Q498 314 514 314Q530 314 530 331L532 372Z" fill={INK.person} />
        <circle cx={512} cy={299} r={11} fill={INK.person} />
        {/* arm with a surface-coloured outline so it reads over the torso */}
        <path
          d="M513 322L507 350L487 356"
          fill="none"
          stroke={INK.interior}
          strokeWidth={12}
          strokeLinecap="round"
          strokeLinejoin="round"
        />
        <path
          d="M513 322L507 350L487 356"
          fill="none"
          stroke={INK.person}
          strokeWidth={8.5}
          strokeLinecap="round"
          strokeLinejoin="round"
        />
      </g>
      <g opacity={dim('abpm')}>
        <line
          x1={512.4}
          y1={327}
          x2={509.6}
          y2={341}
          stroke={cuffOn ? chart.text.primary : '#8A9BA8'}
          strokeWidth={11}
        />
        <path d="M515 338Q527 339 528 348" fill="none" stroke={INK.line} strokeWidth={sw(1)} />
        <rect x={523} y={348} width={11} height={13} rx={2} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      </g>

      {/* ── Middle room ── */}
      <rect x={584} y={346} width={68} height={66} rx={2} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={584} x2={652} y1={367} y2={367} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={584} x2={652} y1={389} y2={389} stroke={INK.soft} strokeWidth={sw(1)} />
      {[356.5, 378, 400.5].map((y) => (
        <circle key={y} cx={618} cy={y} r={1.6} fill={INK.line} />
      ))}
      <g opacity={dim('candles')}>
        <rect x={595} y={331} width={6} height={15} rx={1} fill={INK.warm} />
        <rect x={605} y={337} width={6} height={9} rx={1} fill={INK.warm} />
        <path d={flame(598, 330, 7)} fill={INK.flame} />
        <path d={flame(608, 336, 6)} fill={INK.flame} />
        <rect x={628} y={343} width={14} height={3} rx={1.5} fill={INK.line} />
        <line x1={632} y1={343} x2={642} y2={318} stroke={INK.flame} strokeWidth={1.2} strokeLinecap="round" />
        <circle cx={642.3} cy={317.6} r={1.2} fill="#F2F5F7" />
      </g>
      {/* monitoring station */}
      <rect x={672} y={348} width={116} height={5} rx={1} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={679} x2={679} y1={353} y2={412} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={781} x2={781} y1={353} y2={412} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={679} x2={781} y1={392} y2={392} stroke={INK.soft} strokeWidth={sw(1)} />
      <g opacity={dim('pdr')}>
        <path d="M694 318H706L703 324H697Z" fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} strokeLinejoin="round" />
        <rect x={697.5} y={324} width={5} height={6} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
        <rect x={686} y={330} width={28} height={18} rx={3} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1.25)} />
        <rect x={690} y={334} width={12} height={7} rx={1} fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
        <circle cx={708.5} cy={337.5} r={2} fill={chart.copper} />
      </g>
      <g opacity={dim('ae51')}>
        <path
          d="M724 340H719Q716 340 716 337V330"
          fill="none"
          stroke={INK.line}
          strokeWidth={sw(1.25)}
          strokeLinecap="round"
        />
        <rect x={724} y={336} width={26} height={12} rx={3} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1.25)} />
        <circle cx={742} cy={342} r={3.2} fill="none" stroke={INK.soft} strokeWidth={sw(1)} />
        <circle cx={742} cy={342} r={1.3} fill={INK.flame} />
        <circle cx={730} cy={342} r={2} fill={chart.verdigris} />
      </g>
      <g opacity={dim('ogawa')}>
        <path d="M772 348V291H760V296" fill="none" stroke={INK.line} strokeWidth={sw(1.25)} strokeLinejoin="round" />
        <rect x={756} y={296} width={8} height={18} rx={4} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1.25)} />
        <line x1={756} x2={764} y1={300.5} y2={300.5} stroke={INK.soft} strokeWidth={sw(1)} />
        <line x1={756} x2={764} y1={309.5} y2={309.5} stroke={INK.soft} strokeWidth={sw(1)} />
        <circle cx={760} cy={305} r={1.7} fill={chart.steel} />
      </g>

      {/* ── Kitchen ── */}
      <rect x={818} y={286} width={46} height={126} rx={4} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={818} x2={864} y1={326} y2={326} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={857} x2={857} y1={296} y2={318} stroke={INK.line} strokeWidth={sw(1.5)} strokeLinecap="round" />
      <line x1={857} x2={857} y1={334} y2={364} stroke={INK.line} strokeWidth={sw(1.5)} strokeLinecap="round" />
      <g opacity={dim('stove')}>
        <rect x={950} y={330} width={10} height={16} rx={1} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
        <rect x={884} y={346} width={78} height={4} rx={1} fill={INK.poche} stroke={INK.line} strokeWidth={sw(1)} />
        <rect x={888} y={350} width={70} height={62} rx={2} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1.25)} />
        {[900, 912, 924, 936].map((x) => (
          <circle key={x} cx={x} cy={356} r={2.2} fill="none" stroke={INK.soft} strokeWidth={sw(1)} />
        ))}
        <rect x={896} y={363} width={54} height={40} rx={2} fill="none" stroke={INK.line} strokeWidth={sw(1)} />
        <rect x={904} y={371} width={38} height={14} rx={1.5} fill={INK.glass} stroke={INK.soft} strokeWidth={sw(1)} />
        {[909, 916, 923].map((x, i) => (
          <path key={x} d={flame(x, 345.5, i === 1 ? 5.5 : 4.5)} fill={INK.flame} />
        ))}
        <path
          d="M898 333H934Q934 339.5 927.5 339.5H904.5Q898 339.5 898 333Z"
          fill={INK.fill}
          stroke={INK.line}
          strokeWidth={sw(1.25)}
        />
        <line x1={934} y1={334} x2={953} y2={330} stroke={INK.line} strokeWidth={3} strokeLinecap="round" />
      </g>
      <rect x={966} y={346} width={112} height={4} rx={1} fill={INK.poche} stroke={INK.line} strokeWidth={sw(1)} />
      <rect x={970} y={350} width={106} height={62} fill={INK.fill} stroke={INK.line} strokeWidth={sw(1)} />
      <line x1={1005} x2={1005} y1={354} y2={408} stroke={INK.soft} strokeWidth={sw(1)} />
      <line x1={1041} x2={1041} y1={354} y2={408} stroke={INK.soft} strokeWidth={sw(1)} />
      <path
        d="M1032 346V337Q1032 333 1036 333H1042"
        fill="none"
        stroke={INK.line}
        strokeWidth={sw(1.25)}
        strokeLinecap="round"
      />

      {/* Zone labels */}
      <g className="font-mono" fontSize={11 * u} letterSpacing="0.12em" fill={chart.text.muted} textAnchor="middle">
        <text x={105} y={376}>
          STREET
        </text>
        <text x={434} y={214}>
          FRONT ROOM
        </text>
        <text x={683} y={214}>
          MIDDLE ROOM
        </text>
        <text x={944} y={214}>
          KITCHEN
        </text>
      </g>

      <Emissions u={u} filter={filter} reduce={reduce} />
    </g>
  )
}

/* ─── Emissions: plumes rising from each source, outdoor air seeping in ─── */

const FLOWS: { p: PollutantId; d: string; end: Pt }[] = [
  { p: 'bc', d: 'M200 435C238 433 262 405 300 399L336 397', end: { x: 336, y: 397 } },
  { p: 'no2', d: 'M200 430C234 426 258 383 300 375L336 373', end: { x: 336, y: 373 } },
]

function Emissions({ u, filter, reduce }: { u: number; filter: Filter; reduce: boolean }) {
  return (
    <g pointerEvents="none">
      {FLOWS.filter((f) => filter === 'all' || filter === f.p).map((f) => {
        const color = POLLUTANTS[f.p].color
        const gas = POLLUTANTS[f.p].kind === 'gas'
        // particles stream as dots, the gas as short dashes
        const dash = gas ? [4 * u, 5 * u] : [0.01 * u, 6 * u]
        const period = dash[0] + dash[1]
        return (
          <g key={f.p}>
            <motion.path
              d={f.d}
              fill="none"
              stroke={color}
              strokeWidth={2 * u}
              strokeLinecap={gas ? 'butt' : 'round'}
              strokeDasharray={dash.join(' ')}
              initial={{ strokeDashoffset: 0 }}
              animate={reduce ? { strokeDashoffset: 0 } : { strokeDashoffset: [0, -period * 3] }}
              transition={reduce ? { duration: 0 } : { duration: 1.6, repeat: Infinity, ease: 'linear' }}
            />
            <path
              d={`M${f.end.x - 5 * u} ${f.end.y - 4 * u}L${f.end.x} ${f.end.y}L${f.end.x - 5 * u} ${f.end.y + 4 * u}`}
              fill="none"
              stroke={color}
              strokeWidth={1.5 * u}
              strokeLinecap="round"
              strokeLinejoin="round"
            />
          </g>
        )
      })}
      {SOURCES.map((src) => (
        <Plume key={src.id} src={src} u={u} filter={filter} reduce={reduce} />
      ))}
    </g>
  )
}

function Plume({ src, u, filter, reduce }: { src: Source; u: number; filter: Filter; reduce: boolean }) {
  if (!sourceMatches(src, filter)) return null
  const kinds = filter === 'all' ? src.emits : [filter as PollutantId]
  const { x, y, dx, dy } = src.plume
  const PER = 3
  const DUR = 3.6
  const total = kinds.length * PER
  return (
    <g>
      {kinds.map((p, pi) =>
        Array.from({ length: PER }, (_, i) => {
          const k = pi * PER + i
          const color = POLLUTANTS[p].color
          const gas = POLLUTANTS[p].kind === 'gas'
          const side = ((k % 3) - 1) * 5 + (pi ? 2.5 : -2.5)
          const wobble = (k % 2 ? 1 : -1) * 5
          const style = gas
            ? { fill: 'none', stroke: color, strokeWidth: 1.3 * u }
            : { fill: color, stroke: 'none', strokeWidth: 0 }
          const r = (gas ? 2.7 : 2.3) * u
          if (reduce) {
            const t = [0.3, 0.58, 0.86][i]
            return (
              <circle
                key={`${p}${i}`}
                cx={x + side + dx * t + wobble * 0.4}
                cy={y + dy * t}
                r={r}
                opacity={1 - t * 0.6}
                {...style}
              />
            )
          }
          return (
            <motion.circle
              key={`${p}${i}`}
              r={r}
              {...style}
              initial={{ cx: x + side, cy: y, opacity: 0 }}
              animate={{
                cx: [x + side, x + side + wobble + dx * 0.5, x + side + dx],
                cy: [y, y + dy * 0.5, y + dy],
                opacity: [0, 0.95, 0],
              }}
              transition={{
                duration: DUR,
                delay: (k / total) * DUR,
                repeat: Infinity,
                ease: 'easeOut',
                times: [0, 0.4, 1],
              }}
            />
          )
        }),
      )}
    </g>
  )
}
