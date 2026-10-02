'use client'

import { AnimatePresence, motion, useReducedMotion } from 'framer-motion'
import { useEffect, useMemo, useState } from 'react'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'
import ModuleIcon from './ModuleIcon'
import { EDGES, MODULES, moduleById, handoffLabel, type EdgeId, type ModuleId } from './erpFlow'

type Pt = [number, number]
interface Rect {
  x: number
  y: number
  w: number
  h: number
}
interface EdgeGeom {
  pts: Pt[]
  kind: 'h' | 'v' | 'bracket'
  lx: number
  ly: number
  anchor: 'start' | 'middle' | 'end'
}
interface Layout {
  mode: 'grid' | 'stack'
  height: number
  tiles: Record<ModuleId, Rect>
  edges: Record<EdgeId, EdgeGeom>
}

const PAD = 8 // gap between a tile and its connector
const STEP_MS = 1300
const HOLD_MS = 1300

/* Desktop: a serpentine. Demand runs left→right across the top (Sales,
 * Inventory, MRP), procurement and cash come back right→left underneath
 * (Purchase, Accounting, Portal); delivery drops straight down to invoicing. */
function gridLayout(W: number): Layout {
  const tileW = Math.min(184, Math.floor((W - 2 * 150) / 3))
  const gap = (W - 3 * tileW) / 2
  const tileH = 104
  const rowGap = 124
  const at = (c: number, r: number): Rect => ({ x: c * (tileW + gap), y: r * (tileH + rowGap), w: tileW, h: tileH })
  const tiles: Record<ModuleId, Rect> = {
    sales: at(0, 0),
    inventory: at(1, 0),
    mrp: at(2, 0),
    purchase: at(2, 1),
    accounting: at(1, 1),
    portal: at(0, 1),
  }
  const h = (a: Rect, b: Rect): EdgeGeom => {
    const cy = a.y + a.h / 2
    const pts: Pt[] = b.x > a.x ? [[a.x + a.w + PAD, cy], [b.x - PAD, cy]] : [[a.x - PAD, cy], [b.x + b.w + PAD, cy]]
    return { pts, kind: 'h', lx: (pts[0][0] + pts[1][0]) / 2, ly: cy, anchor: 'middle' }
  }
  const v = (a: Rect, b: Rect, side: 'left' | 'right'): EdgeGeom => {
    const cx = a.x + a.w / 2
    const pts: Pt[] = [[cx, a.y + a.h + PAD], [cx, b.y - PAD]]
    return {
      pts,
      kind: 'v',
      lx: cx + (side === 'right' ? 12 : -12),
      ly: (pts[0][1] + pts[1][1]) / 2,
      anchor: side === 'right' ? 'start' : 'end',
    }
  }
  return {
    mode: 'grid',
    height: 2 * tileH + rowGap,
    tiles,
    edges: {
      reserve: h(tiles.sales, tiles.inventory),
      manufacture: h(tiles.inventory, tiles.mrp),
      purchase: v(tiles.mrp, tiles.purchase, 'left'),
      bill: h(tiles.purchase, tiles.accounting),
      invoice: v(tiles.inventory, tiles.accounting, 'right'),
      payment: h(tiles.accounting, tiles.portal),
    },
  }
}

/* Mobile: one column in flow order; delivery → invoice runs as a bracket
 * down the right margin because it skips MRP and Purchase. */
function stackLayout(W: number): Layout {
  const tileH = 64
  const gap = 76
  const tileW = W - 52
  const tiles = Object.fromEntries(
    MODULES.map((m, i) => [m.id, { x: 0, y: i * (tileH + gap), w: tileW, h: tileH }])
  ) as Record<ModuleId, Rect>
  const v = (a: Rect, b: Rect): EdgeGeom => {
    const pts: Pt[] = [[28, a.y + a.h + PAD], [28, b.y - PAD]]
    return { pts, kind: 'v', lx: 44, ly: (pts[0][1] + pts[1][1]) / 2, anchor: 'start' }
  }
  const inv = tiles.inventory
  const acc = tiles.accounting
  const bx = W - 30
  const iy = inv.y + inv.h / 2
  const ay = acc.y + acc.h / 2
  return {
    mode: 'stack',
    height: MODULES.length * tileH + (MODULES.length - 1) * gap,
    tiles,
    edges: {
      reserve: v(tiles.sales, inv),
      manufacture: v(inv, tiles.mrp),
      purchase: v(tiles.mrp, tiles.purchase),
      bill: v(tiles.purchase, acc),
      payment: v(acc, tiles.portal),
      invoice: {
        pts: [[tileW + PAD, iy], [bx, iy], [bx, ay], [tileW + PAD, ay]],
        kind: 'bracket',
        lx: W - 12,
        ly: (iy + ay) / 2,
        anchor: 'middle',
      },
    },
  }
}

function pathD(pts: Pt[], radius = 10) {
  if (pts.length === 2) return `M${pts[0][0]},${pts[0][1]}L${pts[1][0]},${pts[1][1]}`
  // Rounded orthogonal corners
  let d = `M${pts[0][0]},${pts[0][1]}`
  for (let i = 1; i < pts.length - 1; i++) {
    const [px, py] = pts[i - 1]
    const [cx, cy] = pts[i]
    const [nx, ny] = pts[i + 1]
    const inX = Math.sign(cx - px) * radius
    const inY = Math.sign(cy - py) * radius
    const outX = Math.sign(nx - cx) * radius
    const outY = Math.sign(ny - cy) * radius
    d += `L${cx - inX},${cy - inY}Q${cx},${cy} ${cx + outX},${cy + outY}`
  }
  const last = pts[pts.length - 1]
  return `${d}L${last[0]},${last[1]}`
}

function pointAt(pts: Pt[], t: number): Pt {
  const segs = pts.slice(1).map((p, i) => Math.hypot(p[0] - pts[i][0], p[1] - pts[i][1]))
  let dist = segs.reduce((a, b) => a + b, 0) * t
  for (let i = 0; i < segs.length; i++) {
    if (dist <= segs[i] || i === segs.length - 1) {
      const k = segs[i] === 0 ? 0 : Math.min(1, dist / segs[i])
      return [pts[i][0] + (pts[i + 1][0] - pts[i][0]) * k, pts[i][1] + (pts[i + 1][1] - pts[i][1]) * k]
    }
    dist -= segs[i]
  }
  return pts[pts.length - 1]
}

const easeInOut = (p: number) => (p < 0.5 ? 2 * p * p : 1 - Math.pow(-2 * p + 2, 2) / 2)

export default function OrderToCashFlow() {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduceMotion = !!useReducedMotion()
  const [selected, setSelected] = useState<ModuleId | null>(null)
  const [step, setStep] = useState<number | null>(null) // index into EDGES; EDGES.length = complete
  const [auto, setAuto] = useState(false)
  const [progress, setProgress] = useState(1)

  const layout = useMemo(() => (width >= 700 ? gridLayout(width) : width > 0 ? stackLayout(width) : null), [width])

  // Drive the token along the current edge; auto-advance unless reduced motion
  useEffect(() => {
    if (step === null || step >= EDGES.length) return
    if (reduceMotion) {
      setProgress(1)
      return
    }
    let raf = 0
    let timer: ReturnType<typeof setTimeout> | undefined
    const t0 = performance.now()
    setProgress(0)
    const tick = (now: number) => {
      const p = Math.min(1, (now - t0) / STEP_MS)
      setProgress(easeInOut(p))
      if (p < 1) raf = requestAnimationFrame(tick)
      else if (auto) timer = setTimeout(() => setStep((s) => (s === null ? null : s + 1)), HOLD_MS)
    }
    raf = requestAnimationFrame(tick)
    return () => {
      cancelAnimationFrame(raf)
      if (timer) clearTimeout(timer)
    }
  }, [step, auto, reduceMotion])

  useEffect(() => {
    if (step !== null && step >= EDGES.length) setAuto(false)
  }, [step])

  const tracing = step !== null
  const current = step !== null && step < EDGES.length ? EDGES[step] : null

  const startTrace = () => {
    setSelected(null)
    setAuto(!reduceMotion)
    setStep(0)
  }
  const stopTrace = () => {
    setStep(null)
    setAuto(false)
  }
  const go = (delta: number) => {
    setAuto(false)
    setStep((s) => Math.min(EDGES.length, Math.max(0, (s ?? 0) + delta)))
  }

  const edgeState = (id: EdgeId, i: number): 'active' | 'done' | 'linked' | 'idle' => {
    if (tracing) {
      if (current?.id === id) return 'active'
      if (step !== null && i < step) return 'done'
      return 'idle'
    }
    if (selected) {
      const e = EDGES[i]
      if (e.from === selected || e.to === selected) return 'linked'
    }
    return 'idle'
  }

  const tileState = (id: ModuleId): 'selected' | 'active' | 'idle' => {
    if (current && (current.from === id || current.to === id)) return 'active'
    if (!tracing && selected === id) return 'selected'
    return 'idle'
  }

  const token = current && layout ? pointAt(layout.edges[current.id].pts, progress) : null
  const detail = selected ? moduleById[selected] : null

  return (
    <div className="grid grid-cols-1 xl:grid-cols-3 gap-6 xl:gap-8">
      {/* Diagram card */}
      <div className="xl:col-span-2 rounded-2xl border border-white/[0.08] bg-surface">
        <div className="flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between p-5 md:p-8 pb-0 md:pb-0">
          <div className="min-w-0">
            <h3 className="font-sans text-base md:text-lg font-medium text-white">Order to cash, module by module</h3>
            <p className="mt-1 text-sm text-muted max-w-xl">
              Each arrow is a hand-off: what arrives from one module and what it becomes in the next.
            </p>
          </div>
          <div className="shrink-0">
            {!tracing ? (
              <button
                type="button"
                onClick={startTrace}
                className="inline-flex items-center gap-2 rounded-lg border border-copper/40 px-4 py-2 text-sm font-medium text-copper-light hover:bg-copper/10 hover:border-copper/60 transition-colors focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-copper"
              >
                <svg viewBox="0 0 16 16" className="w-3.5 h-3.5" fill="currentColor" aria-hidden="true">
                  <path d="M4.5 2.8v10.4a.6.6 0 0 0 .92.5l8-5.2a.6.6 0 0 0 0-1L5.42 2.3a.6.6 0 0 0-.92.5Z" />
                </svg>
                Trace an order
              </button>
            ) : (
              <div className="flex items-center gap-2">
                <TraceButton onClick={() => go(-1)} disabled={step === 0} label="Previous step">
                  <path d="M10 3 5 8l5 5" />
                </TraceButton>
                <TraceButton onClick={() => go(1)} disabled={step === EDGES.length} label="Next step">
                  <path d="m6 3 5 5-5 5" />
                </TraceButton>
                <button
                  type="button"
                  onClick={stopTrace}
                  className="rounded-lg border border-white/[0.08] px-3 py-2 text-xs font-medium text-muted hover:text-white hover:border-white/20 transition-colors"
                >
                  {step === EDGES.length ? 'Close' : 'Stop'}
                </button>
              </div>
            )}
          </div>
        </div>

        <div className="p-5 md:p-8">
          <div ref={ref} className="relative w-full" style={{ height: layout?.height ?? 480 }}>
            {layout && (
              <>
                <svg width={width} height={layout.height} className="absolute inset-0 overflow-visible" aria-hidden="true">
                  <defs>
                    {(['idle', 'hot'] as const).map((k) => (
                      <marker
                        key={k}
                        id={`o2c-arrow-${k}`}
                        viewBox="0 0 10 10"
                        refX="8"
                        refY="5"
                        markerWidth="8"
                        markerHeight="8"
                        markerUnits="userSpaceOnUse"
                        orient="auto-start-reverse"
                      >
                        <path d="M1,1.5 8,5 1,8.5" fill="none" stroke={k === 'hot' ? chart.copper : '#56646E'} strokeWidth="1.6" strokeLinecap="round" strokeLinejoin="round" />
                      </marker>
                    ))}
                  </defs>

                  {EDGES.map((e, i) => {
                    const g = layout.edges[e.id]
                    const st = edgeState(e.id, i)
                    const hot = st !== 'idle'
                    const d = pathD(g.pts)
                    return (
                      <g key={e.id}>
                        <path
                          d={d}
                          fill="none"
                          stroke={hot ? chart.copper : '#56646E'}
                          strokeOpacity={st === 'done' ? 0.55 : 1}
                          strokeWidth={hot ? 2 : 1.5}
                          strokeLinecap="round"
                          markerEnd={`url(#o2c-arrow-${hot ? 'hot' : 'idle'})`}
                          className="transition-[stroke] duration-300"
                        />
                        {st === 'active' && !reduceMotion && (
                          <motion.path
                            d={d}
                            fill="none"
                            stroke="#F0C9A0"
                            strokeWidth={2}
                            strokeLinecap="round"
                            strokeDasharray="1 11"
                            animate={{ strokeDashoffset: [0, -24] }}
                            transition={{ duration: 0.8, repeat: Infinity, ease: 'linear' }}
                          />
                        )}
                        <EdgeLabel geom={g} trigger={e.trigger} action={e.action} emphasis={hot} />
                      </g>
                    )
                  })}

                  {token && (
                    <g transform={`translate(${token[0]},${token[1]})`} pointerEvents="none">
                      <circle r={12} fill={chart.copper} opacity={0.18} />
                      <circle r={6} fill={chart.copper} stroke={chart.surface} strokeWidth={2} />
                    </g>
                  )}
                </svg>

                {MODULES.map((m, i) => {
                  const r = layout.tiles[m.id]
                  const st = tileState(m.id)
                  const lit = st !== 'idle'
                  return (
                    <button
                      key={m.id}
                      type="button"
                      aria-pressed={selected === m.id}
                      aria-label={`${m.label}${m.generic ? ' (generic step)' : ''}: ${m.role}. Show details`}
                      onClick={() => {
                        if (tracing) stopTrace()
                        setSelected((s) => (s === m.id ? null : m.id))
                      }}
                      className={`absolute rounded-xl border text-left transition-[border-color,background-color,box-shadow] duration-300 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-copper ${
                        m.generic ? 'border-dashed' : ''
                      } ${
                        lit
                          ? 'border-copper/60 bg-[#1E2224] shadow-[0_0_0_3px_rgba(184,115,51,0.12)]'
                          : m.generic
                            ? 'border-white/[0.14] bg-transparent hover:border-white/25'
                            : 'border-white/[0.08] bg-surface-raised hover:border-white/20 hover:bg-[#1F282C]'
                      } ${layout.mode === 'grid' ? 'p-3.5' : 'flex items-center gap-3 px-3'}`}
                      style={{ left: r.x, top: r.y, width: r.w, height: r.h }}
                    >
                      <span
                        className={`flex w-8 h-8 shrink-0 items-center justify-center rounded-lg transition-colors duration-300 ${
                          lit ? 'bg-copper/15 text-copper-light' : 'bg-white/[0.05] text-titanium'
                        }`}
                      >
                        <ModuleIcon id={m.id} className="w-[18px] h-[18px]" />
                      </span>
                      <span className={`block min-w-0 ${layout.mode === 'grid' ? 'mt-3' : ''}`}>
                        <span className="block text-sm font-semibold leading-5 text-white whitespace-nowrap">{m.label}</span>
                        <span className="block text-xs leading-4 text-muted whitespace-nowrap">{m.role}</span>
                      </span>
                      <span
                        aria-hidden="true"
                        className={`absolute font-mono text-[11px] text-faint ${layout.mode === 'grid' ? 'top-3.5 right-3.5' : 'right-3 top-1/2 -translate-y-1/2'}`}
                      >
                        {String(i + 1).padStart(2, '0')}
                      </span>
                    </button>
                  )
                })}
              </>
            )}
          </div>
        </div>

        {/* Step caption, synced with the token */}
        <div
          className="border-t border-white/[0.06] px-5 md:px-8 py-4 min-h-[76px] flex items-center"
          aria-live="polite"
        >
          {tracing ? (
            <div className="flex flex-col gap-1.5 sm:flex-row sm:items-start sm:gap-4">
              <span className="font-mono text-[11px] tracking-wider uppercase text-copper-light whitespace-nowrap pt-0.5">
                {current ? `Step ${step! + 1} / ${EDGES.length}` : 'Complete'}
              </span>
              <p className="text-sm text-titanium leading-relaxed">
                {current ? (
                  <>
                    <span className="font-medium text-white">{handoffLabel(current)}.</span> {current.caption}
                  </>
                ) : (
                  <>
                    <span className="font-medium text-white">Order to cash.</span> One sales order, followed through each
                    module from quotation to payment.
                  </>
                )}
              </p>
            </div>
          ) : (
            <p className="text-sm text-muted">
              Select a module for my implementation notes, or trace an order through the flow.
            </p>
          )}
        </div>
      </div>

      {/* Detail panel */}
      <div className="rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-8 min-h-[280px]">
        <AnimatePresence mode="wait" initial={false}>
          {detail ? (
            <motion.div
              key={detail.id}
              initial={reduceMotion ? false : { opacity: 0, y: 8 }}
              animate={{ opacity: 1, y: 0 }}
              exit={reduceMotion ? undefined : { opacity: 0, y: -6 }}
              transition={{ duration: 0.25 }}
            >
              <div className="flex items-center gap-3">
                <span className="flex w-10 h-10 items-center justify-center rounded-xl bg-copper/15 text-copper-light">
                  <ModuleIcon id={detail.id} className="w-5 h-5" />
                </span>
                <div>
                  <span className="block font-mono text-[11px] tracking-widest uppercase text-copper-light">
                    {detail.generic ? 'Generic step' : 'Implementation notes'}
                  </span>
                  <h4 className="font-sans text-lg font-medium text-white leading-tight">{detail.label}</h4>
                </div>
              </div>
              <p className="mt-5 text-sm text-titanium leading-relaxed">{detail.description}</p>

              <dl className="mt-6 pt-5 border-t border-white/[0.06] space-y-4">
                {(
                  [
                    ['Receives', EDGES.filter((e) => e.to === detail.id), 'from'],
                    ['Hands off', EDGES.filter((e) => e.from === detail.id), 'to'],
                  ] as const
                ).map(([title, list, dir]) =>
                  list.length ? (
                    <div key={title}>
                      <dt className="font-mono text-[11px] tracking-widest uppercase text-muted mb-2">{title}</dt>
                      {list.map((e) => {
                        const other = moduleById[dir === 'from' ? e.from : e.to]
                        return (
                          <dd key={e.id} className="flex items-baseline justify-between gap-3 text-sm py-1">
                            <span className="text-white">{handoffLabel(e)}</span>
                            <button
                              type="button"
                              onClick={() => setSelected(other.id)}
                              className="shrink-0 text-xs text-muted hover:text-copper-light underline-offset-2 hover:underline"
                            >
                              {dir} {other.label}
                            </button>
                          </dd>
                        )
                      })}
                    </div>
                  ) : null
                )}
              </dl>
            </motion.div>
          ) : (
            <motion.div
              key="empty"
              initial={reduceMotion ? false : { opacity: 0 }}
              animate={{ opacity: 1 }}
              exit={reduceMotion ? undefined : { opacity: 0 }}
              className="h-full flex flex-col justify-center"
            >
              <span className="font-mono text-[11px] tracking-widest uppercase text-muted">Module detail</span>
              <p className="mt-3 text-white text-base leading-relaxed">
                Choose a module to see how I implemented it for clients.
              </p>
              <ul className="mt-6 flex flex-wrap gap-2">
                {MODULES.filter((m) => !m.generic).map((m) => (
                  <li key={m.id}>
                    <button
                      type="button"
                      onClick={() => {
                        stopTrace()
                        setSelected(m.id)
                      }}
                      className="flex items-center gap-2 rounded-lg border border-white/[0.06] px-3 py-2 text-xs whitespace-nowrap text-titanium hover:text-white hover:border-white/20 transition-colors"
                    >
                      <ModuleIcon id={m.id} className="w-4 h-4 shrink-0" />
                      <span>{m.label}</span>
                    </button>
                  </li>
                ))}
              </ul>
            </motion.div>
          )}
        </AnimatePresence>
      </div>
    </div>
  )
}

function EdgeLabel({
  geom,
  trigger,
  action,
  emphasis,
}: {
  geom: EdgeGeom
  trigger: string
  action: string
  emphasis: boolean
}) {
  const main = emphasis ? chart.text.primary : chart.text.secondary
  if (geom.kind === 'bracket') {
    return (
      <text
        x={geom.lx}
        y={geom.ly}
        transform={`rotate(-90 ${geom.lx} ${geom.ly})`}
        textAnchor="middle"
        dy="0.35em"
        fontSize={12}
        fill={main}
      >
        {trigger} → {action}
      </text>
    )
  }
  const [y1, y2] = geom.kind === 'h' ? [geom.ly - 9, geom.ly + 19] : [geom.ly - 4, geom.ly + 14]
  return (
    <g fontSize={12}>
      <text x={geom.lx} y={y1} textAnchor={geom.anchor} fill={main} fontWeight={500}>
        {trigger}
      </text>
      <text x={geom.lx} y={y2} textAnchor={geom.anchor} fill={chart.text.muted}>
        {geom.kind === 'h' ? action : `→ ${action}`}
      </text>
    </g>
  )
}

function TraceButton({
  onClick,
  disabled,
  label,
  children,
}: {
  onClick: () => void
  disabled: boolean
  label: string
  children: React.ReactNode
}) {
  return (
    <button
      type="button"
      onClick={onClick}
      disabled={disabled}
      aria-label={label}
      className="inline-flex w-9 h-9 items-center justify-center rounded-lg border border-white/[0.08] text-titanium hover:text-white hover:border-white/20 disabled:opacity-35 disabled:pointer-events-none transition-colors"
    >
      <svg viewBox="0 0 16 16" className="w-4 h-4" fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
        {children}
      </svg>
    </button>
  )
}
