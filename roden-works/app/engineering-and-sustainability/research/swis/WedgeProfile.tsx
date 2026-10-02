'use client'

import { animate, useReducedMotion } from 'framer-motion'
import { useEffect, useMemo, useRef, useState } from 'react'
import ChartTooltip from '@/components/charts/ChartTooltip'
import { chart } from '@/components/charts/tokens'
import { linearScale } from '@/components/charts/scale'
import { useElementSize } from '@/components/charts/useElementSize'
import StatusGlyph, { glyphShapes, sillGlyph, type Glyph } from './StatusGlyph'
import {
  FLOW_MAX,
  FLOW_MIN,
  LANDMARKS,
  RM_DOWNSTREAM,
  RM_UPSTREAM,
  SILL_RM,
  SILL_TEXT,
  SOURCES,
  STATUS_TEXT,
  WEDGE_LENGTH,
  bedDepth,
  intakeStatus,
  saltFraction,
  type Landmark,
  type WedgeState,
} from './wedgePhysics'

const D_MAX = 160 // ft shown on the depth axis
const STEP = 0.25 // sampling interval, river miles
const DEPTH_TICKS = [0, 50, 100, 150]

const BED_TOP = '#1C2428'
const BED_BOTTOM = '#131A1D'
const BED_EDGE = '#4A5761'
const SILL_FILL = '#2B353A'
const HALO = 'rgba(21,28,31,0.9)'

const clamp = (v: number, lo: number, hi: number) => Math.min(hi, Math.max(lo, v))
const fmtRm = (rm: number) => (Number.isInteger(rm) ? String(rm) : rm.toFixed(1))

/** Eases a number toward its target (instantly under reduced motion). */
function useTween(target: number, reduce: boolean, duration = 0.75) {
  const [value, setValue] = useState(target)
  const current = useRef(target)
  useEffect(() => {
    if (reduce) {
      current.current = target
      setValue(target)
      return
    }
    const controls = animate(current.current, target, {
      duration,
      ease: [0.16, 1, 0.3, 1],
      onUpdate: (v) => {
        current.current = v
        setValue(v)
      },
    })
    return () => controls.stop()
  }, [target, reduce, duration])
  return value
}

let measureCtx: CanvasRenderingContext2D | null | undefined
function textWidth(text: string, size: number, weight: number, family: string) {
  if (typeof document === 'undefined') return text.length * size * 0.56
  if (measureCtx === undefined) measureCtx = document.createElement('canvas').getContext('2d')
  if (!measureCtx) return text.length * size * 0.56
  measureCtx.font = `${weight} ${size}px ${family}`
  return measureCtx.measureText(text).width
}

interface BandItem {
  lm: Landmark
  x: number
  text: string
  w: number
}
interface BandLabel extends BandItem {
  row: number
  left: number
}

/** Greedy row packing: no two labels overlap, and no leader line passes
 *  through a label in a lower row. Minor landmarks drop out if they can't fit. */
function layoutBand(items: BandItem[], minX: number, maxX: number, rows: number): BandLabel[] {
  const GAP = 12
  const placed: BandLabel[] = []
  const order = [...items].sort((a, b) => a.lm.priority - b.lm.priority || a.x - b.x)
  for (const it of order) {
    const left = clamp(it.x - it.w / 2, minX, maxX - it.w)
    const right = left + it.w
    let row = -1
    for (let r = 0; r < rows && row < 0; r++) {
      const clash = placed.some(
        (p) =>
          (p.row === r && left < p.left + p.w + GAP && right + GAP > p.left) ||
          (p.row < r && it.x > p.left - 5 && it.x < p.left + p.w + 5) ||
          (p.row > r && p.x > left - 5 && p.x < right + 5)
      )
      if (!clash) row = r
    }
    if (row < 0) continue
    placed.push({ ...it, row, left })
  }
  return placed
}

function markerStyle(glyph: Glyph) {
  switch (glyph) {
    case 'salty':
      return { fill: chart.copper, stroke: chart.surface, strokeWidth: 2 }
    case 'below':
      return { fill: chart.surface, stroke: chart.copper, strokeWidth: 1.5 }
    case 'watch':
      return { fill: chart.surface, stroke: '#8A9BA8', strokeWidth: 1.5 }
    default:
      return { fill: chart.deemph, stroke: chart.surface, strokeWidth: 2 }
  }
}

export default function WedgeProfile({ state }: { state: WedgeState }) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const reduce = useReducedMotion() ?? false
  const [family, setFamily] = useState('system-ui, sans-serif')
  const [fontsReady, setFontsReady] = useState(0)
  const [hoverId, setHoverId] = useState<string | null>(null)
  const [focusId, setFocusId] = useState<string | null>(null)

  useEffect(() => {
    if (ref.current) setFamily(getComputedStyle(ref.current).fontFamily)
    document.fonts?.ready.then(() => setFontsReady((n) => n + 1))
  }, [ref])

  const deployed = state.sill !== 'none'
  const holding = state.sill === 'holding'
  const toe = useTween(clamp(state.toe, -30, 135), reduce)
  const free = useTween(clamp(state.free, -30, 135), reduce)
  const crest = useTween(state.crest, reduce)
  const rise = useTween(deployed ? 1 : 0, reduce, 0.6)
  const ghost = useTween(holding ? 1 : 0, reduce, 0.6)

  /* ─── Layout ─── */
  const compact = width > 0 && width < 640
  const fs = compact ? 11 : 12
  const rowH = fs + 7
  const MAX_ROWS = 4
  const margin = { top: 6, right: compact ? 10 : 20, bottom: compact ? 52 : 56, left: compact ? 44 : 58 }
  const plotLeft = margin.left
  const plotRight = Math.max(plotLeft + 10, width - margin.right)

  const x = useMemo(() => linearScale([RM_UPSTREAM, RM_DOWNSTREAM], [plotLeft, plotRight]), [plotLeft, plotRight])
  const pxPerMile = (plotRight - plotLeft) / (RM_UPSTREAM - RM_DOWNSTREAM)

  /* ─── Landmark labels: rows are packed first, the plot starts below them ─── */
  const band = useMemo(() => {
    if (width === 0) return []
    const items: BandItem[] = LANDMARKS.filter((l) => !compact || l.priority === 1).map((lm) => {
      const text = compact ? lm.short : lm.name
      const icon = lm.kind === 'intake' ? 16 : 0
      return { lm, x: x(lm.rm), text, w: icon + textWidth(text, fs, 500, family) + 2 }
    })
    return layoutBand(items, 2, width - 2, MAX_ROWS)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [width, compact, fs, family, fontsReady, x])
  const rowsUsed = Math.max(1, ...band.map((b) => b.row + 1))

  const plotTop = margin.top + rowsUsed * rowH + 12
  const plotH = compact ? 290 : 350
  const H = plotTop + plotH + margin.bottom
  const plotBottom = plotTop + plotH
  const y = useMemo(() => linearScale([0, D_MAX], [plotTop, plotBottom]), [plotTop, plotBottom])

  // Vertical exaggeration of this drawing: horizontal ft/px ÷ vertical ft/px
  const exaggeration = pxPerMile > 0 ? (5280 / pxPerMile) / (D_MAX / (plotBottom - plotTop)) : 0
  const exaggerationLabel = exaggeration > 0 ? `${Math.round(exaggeration / 100) * 100}` : ''

  /* ─── Geometry ─── */
  const samples = useMemo(() => {
    const out: { rm: number; bed: number }[] = []
    for (let rm = RM_UPSTREAM; rm >= RM_DOWNSTREAM - 1e-9; rm -= STEP) out.push({ rm, bed: bedDepth(rm) })
    return out
  }, [])

  const saltTop = (rm: number, bed: number, toeRm: number) => {
    const f = saltFraction(toeRm - rm)
    return f > 0 ? bed * (1 - f) : bed
  }

  const pt = (rm: number, depth: number) => `${x(rm).toFixed(1)},${y(depth).toFixed(1)}`

  const bedLine = samples.map((s, i) => `${i ? 'L' : 'M'}${pt(s.rm, s.bed)}`).join('')
  const bedArea = `${bedLine}L${plotRight},${plotBottom}L${plotLeft},${plotBottom}Z`
  const water = `M${plotLeft},${plotTop}L${plotRight},${plotTop}` + [...samples].reverse().map((s) => `L${pt(s.rm, s.bed)}`).join('') + 'Z'

  const tops = samples.map((s) => saltTop(s.rm, s.bed, toe))
  const fresh =
    `M${plotLeft},${plotTop}L${plotRight},${plotTop}` +
    [...samples]
      .map((s, i) => ({ s, t: tops[i] }))
      .reverse()
      .map(({ s, t }) => `L${pt(s.rm, t)}`)
      .join('') +
    'Z'

  const saltIdx = samples.map((s, i) => (s.rm <= toe ? i : -1)).filter((i) => i >= 0)
  const hasWedge = saltIdx.length > 1
  const toeRmVisible = Math.min(toe, RM_UPSTREAM)
  const wedge = hasWedge
    ? `M${pt(toeRmVisible, bedDepth(toeRmVisible))}` +
      saltIdx.map((i) => `L${pt(samples[i].rm, tops[i])}`).join('') +
      [...saltIdx].reverse().map((i) => `L${pt(samples[i].rm, samples[i].bed)}`).join('') +
      'Z'
    : ''
  const haloclineIdx = saltIdx.filter((i) => tops[i] > 0.5 && tops[i] < samples[i].bed - 0.5)
  const halocline = haloclineIdx.length
    ? `M${pt(toeRmVisible, bedDepth(toeRmVisible))}` + haloclineIdx.map((i) => `L${pt(samples[i].rm, tops[i])}`).join('')
    : ''

  // Where the toe would settle with no sill — drawn only while the sill holds
  const ghostIdx = samples.map((s, i) => (s.rm <= free && s.rm >= SILL_RM ? i : -1)).filter((i) => i >= 0)
  const ghostTops = ghostIdx.map((i) => saltTop(samples[i].rm, samples[i].bed, free))
  const freeVisible = Math.min(free, RM_UPSTREAM)
  const ghostLine =
    ghost > 0.01 && ghostIdx.length > 1
      ? `M${pt(freeVisible, bedDepth(freeVisible))}` + ghostIdx.map((i, k) => `L${pt(samples[i].rm, ghostTops[k])}`).join('')
      : ''
  const ghostArea = ghostLine
    ? ghostLine + [...ghostIdx].reverse().map((i) => `L${pt(samples[i].rm, samples[i].bed)}`).join('') + 'Z'
    : ''

  // Sill mound (schematic width, real crest depth)
  const sillBed = bedDepth(SILL_RM)
  const crestDepth = sillBed - (sillBed - crest) * rise
  const baseHalf = Math.max(1.6, (compact ? 12 : 26) / Math.max(pxPerMile, 0.01))
  const crestHalf = Math.max(0.45, (compact ? 3 : 5) / Math.max(pxPerMile, 0.01))
  const sillPath =
    rise > 0.01
      ? `M${pt(SILL_RM + baseHalf, bedDepth(SILL_RM + baseHalf))}L${pt(SILL_RM + crestHalf, crestDepth)}L${pt(SILL_RM - crestHalf, crestDepth)}L${pt(SILL_RM - baseHalf, bedDepth(SILL_RM - baseHalf))}Z`
      : ''

  /* ─── Markers & annotations ─── */
  const toeOnChart = toe >= RM_DOWNSTREAM && toe <= RM_UPSTREAM
  const toeX = x(clamp(toe, RM_DOWNSTREAM, RM_UPSTREAM))
  const toeY = y(bedDepth(clamp(toe, RM_DOWNSTREAM, RM_UPSTREAM)))
  const surfaceRm = toe - WEDGE_LENGTH
  const surfaceOnChart = surfaceRm >= RM_DOWNSTREAM + 1 && surfaceRm <= RM_UPSTREAM - 1
  const freeOnChart = free > SILL_RM + 2 && free <= RM_UPSTREAM

  const toeLabel = holding ? 'Toe held at the sill' : `Toe ≈ RM ${Math.round(state.toe)}`
  const toeLabelW = textWidth(toeLabel, 12, 600, family) + 4
  const toeLabelLeftSide = toeX - 12 - toeLabelW > plotLeft + 4
  const freshW = textWidth('Fresh river water', fs, 500, family)
  const freshRight = plotLeft + 12 + freshW + 30
  const crowdX = Math.min(
    hasWedge && toeOnChart ? (holding ? x(SILL_RM) - toeLabelW / 2 : toeX - (toeLabelLeftSide ? toeLabelW + 12 : 0)) : Infinity,
    ghost > 0.01 && freeOnChart ? x(free) : Infinity
  )
  const showFreshLabel = freshRight + 10 < crowdX
  const ghostLabel = `Without sill ≈ RM ${Math.round(state.free)}`
  const ghostLabelW = textWidth(ghostLabel, 11, 500, family)

  // "Saltwater wedge" goes in the widest stretch where salt fills ≥ 60% of the column
  const wedgeLabel = (() => {
    if (!hasWedge) return null
    const label = compact ? 'Salt wedge' : 'Saltwater wedge'
    const w = textWidth(label, fs, 500, family) + 16
    let best: { a: number; b: number } | null = null
    let runStart = -1
    saltIdx.forEach((i, k) => {
      const thick = tops[i] <= samples[i].bed * 0.4 && samples[i].rm > RM_DOWNSTREAM + 1
      if (thick && runStart < 0) runStart = k
      const end = !thick || k === saltIdx.length - 1
      if (end && runStart >= 0) {
        const a = x(samples[saltIdx[runStart]].rm)
        const b = x(samples[saltIdx[thick ? k : k - 1]].rm)
        if (!best || b - a > best.b - best.a) best = { a, b }
        runStart = -1
      }
    })
    const run = best as { a: number; b: number } | null
    if (!run || run.b - run.a < w) return null
    const cx = (run.a + run.b) / 2
    const rm = x.invert(cx)
    const bed = bedDepth(rm)
    return { label, x: cx, y: y(bed * 0.56) }
  })()

  const flowFraction = (state.flow - FLOW_MIN) / (FLOW_MAX - FLOW_MIN)
  const streakSpeed = 10 + 52 * clamp(flowFraction, 0, 1) // px per second, faster with more flow
  const streakDur = 40 / streakSpeed
  const streakDepths = compact ? [10, 26] : [9, 21, 33]

  const activeId = focusId ?? hoverId
  const active = LANDMARKS.find((l) => l.id === activeId) ?? null

  const statusFor = (lm: Landmark): Glyph | null =>
    lm.kind === 'intake' ? intakeStatus(lm.rm, state) : lm.kind === 'sill' ? sillGlyph(state.sill) : null

  const ariaLabel =
    `Longitudinal profile of the lower Mississippi River from Head of Passes to New Orleans at ${state.flow.toLocaleString('en-US')} cubic feet per second. ` +
    (state.toe <= 0
      ? 'The saltwater wedge is held below Head of Passes.'
      : `Modeled wedge toe near river mile ${Math.round(state.toe)}; salt reaches the surface below river mile ${Math.round(Math.max(0, state.surface))}. `) +
    (deployed ? `Emergency sill: ${SILL_TEXT[state.sill]}.` : '')

  return (
    <div ref={ref} className="relative w-full select-none" style={{ height: H }}>
      {width > 0 && (
        <svg width={width} height={H} role="group" aria-label={ariaLabel} className="block overflow-visible">
          <defs>
            <linearGradient id="swis-fresh" gradientUnits="userSpaceOnUse" x1="0" y1={plotTop} x2="0" y2={plotBottom}>
              <stop offset="0" stopColor={chart.steel} stopOpacity="0.30" />
              <stop offset="1" stopColor={chart.steel} stopOpacity="0.07" />
            </linearGradient>
            <linearGradient id="swis-salt" gradientUnits="userSpaceOnUse" x1="0" y1={plotTop} x2="0" y2={y(D_MAX * 0.8)}>
              <stop offset="0" stopColor={chart.copper} stopOpacity="0.14" />
              <stop offset="1" stopColor={chart.copper} stopOpacity="0.46" />
            </linearGradient>
            <linearGradient id="swis-bed" gradientUnits="userSpaceOnUse" x1="0" y1={plotTop} x2="0" y2={plotBottom}>
              <stop offset="0" stopColor={BED_TOP} />
              <stop offset="1" stopColor={BED_BOTTOM} />
            </linearGradient>
            <filter id="swis-glow" x="-5%" y="-50%" width="110%" height="200%">
              <feGaussianBlur stdDeviation="2.4" />
            </filter>
            <clipPath id="swis-water">
              <path d={water} />
            </clipPath>
            <clipPath id="swis-freshclip">
              <path d={fresh} />
            </clipPath>
            <style>{'@keyframes swis-flow { to { stroke-dashoffset: -40; } }'}</style>
          </defs>

          {/* Fresh river water fills the whole channel; the wedge paints over it */}
          <path d={water} fill={chart.surface} />
          <path d={water} fill="url(#swis-fresh)" />

          {/* Depth gridlines, inside the water only */}
          <g clipPath="url(#swis-water)">
            {DEPTH_TICKS.slice(1).map((d) => (
              <line key={d} x1={plotLeft} x2={plotRight} y1={y(d)} y2={y(d)} stroke="rgba(255,255,255,0.05)" strokeWidth={1} shapeRendering="crispEdges" />
            ))}
          </g>

          {/* Current streaks — speed scales with river flow */}
          <g clipPath="url(#swis-freshclip)" pointerEvents="none">
            {streakDepths.map((d, i) => (
              <line
                key={d}
                x1={plotLeft}
                x2={plotRight}
                y1={y(d)}
                y2={y(d)}
                stroke={chart.steel}
                strokeOpacity={0.45 - i * 0.1}
                strokeWidth={1.25}
                strokeLinecap="round"
                strokeDasharray={`${10 + i * 4} ${30 - i * 4}`}
                strokeDashoffset={i * 13}
                style={reduce ? undefined : { animation: `swis-flow ${(streakDur * (1 + i * 0.35)).toFixed(2)}s linear infinite` }}
              />
            ))}
          </g>

          {/* Unchecked wedge (no sill), dashed — only while the sill is holding */}
          {ghostLine && (
            <g opacity={ghost} pointerEvents="none">
              <path d={ghostArea} fill={chart.copper} fillOpacity={0.07} />
              <path d={ghostLine} fill="none" stroke={chart.copper} strokeOpacity={0.8} strokeWidth={1.25} strokeDasharray="4 4" />
            </g>
          )}

          {/* Saltwater wedge: an opaque base erases the fresh wash, then the density gradient */}
          {hasWedge && (
            <g pointerEvents="none">
              <path d={wedge} fill={chart.surface} />
              <path d={wedge} fill="url(#swis-salt)" />
              {halocline && (
                <>
                  <path d={halocline} fill="none" stroke={chart.copper} strokeOpacity={0.55} strokeWidth={5} filter="url(#swis-glow)" />
                  <path d={halocline} fill="none" stroke={chart.copper} strokeWidth={1.5} strokeLinecap="round" strokeLinejoin="round" />
                </>
              )}
            </g>
          )}

          {/* Channel bed (schematic) */}
          <path d={bedArea} fill="url(#swis-bed)" />
          <path d={bedLine} fill="none" stroke={BED_EDGE} strokeWidth={1.25} strokeLinejoin="round" />

          {/* Emergency sill */}
          {sillPath && <path d={sillPath} fill={SILL_FILL} stroke={chart.deemph} strokeWidth={1.25} strokeLinejoin="round" />}

          {/* Landmark verticals through the water */}
          {band.map((b) => {
            const lm = b.lm
            const bottom = lm.kind === 'sill' && rise > 0.01 ? crestDepth : bedDepth(lm.rm)
            return (
              <line
                key={lm.id}
                x1={b.x}
                x2={b.x}
                y1={plotTop}
                y2={y(bottom)}
                stroke="rgba(255,255,255,0.10)"
                strokeWidth={1}
                shapeRendering="crispEdges"
              />
            )
          })}

          {/* Water surface */}
          <line x1={plotLeft} x2={plotRight} y1={plotTop} y2={plotTop} stroke="rgba(242,245,247,0.35)" strokeWidth={1} shapeRendering="crispEdges" />

          {/* In-water labels (skipped when the toe annotations would crowd them) */}
          {showFreshLabel && (
            <>
              <text x={plotLeft + 12} y={plotTop + (compact ? 20 : 24)} fontSize={fs} fontWeight={500} fill={chart.text.secondary} stroke={HALO} strokeWidth={3} paintOrder="stroke">
                Fresh river water
              </text>
              <g transform={`translate(${plotLeft + 12 + freshW + 8},${plotTop + (compact ? 16 : 20)})`}>
                <line x1={0} x2={18} y1={0} y2={0} stroke={chart.text.secondary} strokeWidth={1.25} />
                <path d="M14 -3.5 L19 0 L14 3.5" fill="none" stroke={chart.text.secondary} strokeWidth={1.25} strokeLinecap="round" strokeLinejoin="round" />
              </g>
            </>
          )}

          {wedgeLabel && (
            <text x={wedgeLabel.x} y={wedgeLabel.y} textAnchor="middle" fontSize={fs} fontWeight={500} fill={chart.text.primary} stroke={HALO} strokeWidth={3} paintOrder="stroke">
              {wedgeLabel.label}
            </text>
          )}

          {/* Salt reaches the surface */}
          {hasWedge && surfaceOnChart && (
            <g pointerEvents="none">
              <path d={`M${x(surfaceRm) - 4.5},${plotTop - 1} L${x(surfaceRm) + 4.5},${plotTop - 1} L${x(surfaceRm)},${plotTop + 5} Z`} fill={chart.copper} />
              {!compact && (
                <text x={x(surfaceRm) + 8} y={plotTop + 15} fontSize={11} fill={chart.text.secondary} stroke={HALO} strokeWidth={3} paintOrder="stroke">
                  Salt reaches surface
                </text>
              )}
            </g>
          )}

          {/* Sill crest depth */}
          {rise > 0.5 && !compact && (
            <text
              x={x(SILL_RM - crestHalf) + 7}
              y={y(crestDepth) + 16}
              fontSize={11}
              fill={chart.text.secondary}
              stroke={HALO}
              strokeWidth={3}
              paintOrder="stroke"
            >
              −{Math.round(crest)} ft
            </text>
          )}

          {/* Ghost toe */}
          {ghostLine && freeOnChart && (
            <g opacity={ghost} pointerEvents="none">
              <circle cx={x(free)} cy={y(bedDepth(free))} r={4.5} fill={chart.surface} stroke={chart.copper} strokeWidth={1.5} />
              <text
                x={clamp(x(free), plotLeft + ghostLabelW / 2 + 2, plotRight - ghostLabelW / 2)}
                y={y(bedDepth(free)) - 12}
                textAnchor="middle"
                fontSize={11}
                fill={chart.text.secondary}
                stroke={HALO}
                strokeWidth={3}
                paintOrder="stroke"
              >
                {ghostLabel}
              </text>
            </g>
          )}

          {/* Toe marker */}
          {hasWedge && toeOnChart && (
            <g pointerEvents="none">
              <circle cx={toeX} cy={toeY} r={5} fill={chart.copper} stroke={chart.surface} strokeWidth={2} />
              {holding ? (
                <text x={x(SILL_RM)} y={y(crestDepth) - (compact ? 10 : 14)} textAnchor="middle" fontSize={12} fontWeight={600} fill={chart.text.primary} stroke={HALO} strokeWidth={3} paintOrder="stroke">
                  {toeLabel}
                </text>
              ) : (
                <text
                  x={toeLabelLeftSide ? toeX - 12 : toeX + 12}
                  y={toeY - 8}
                  textAnchor={toeLabelLeftSide ? 'end' : 'start'}
                  fontSize={12}
                  fontWeight={600}
                  fill={chart.text.primary}
                  stroke={HALO}
                  strokeWidth={3}
                  paintOrder="stroke"
                >
                  {toeLabel}
                </text>
              )}
            </g>
          )}

          {/* Toe beyond the chart (very low flows) */}
          {toe > RM_UPSTREAM + 0.5 && (
            <g pointerEvents="none" transform={`translate(${plotLeft + 8},${y(bedDepth(RM_UPSTREAM)) - 10})`}>
              <path d="M6 -4.5 L0 0 L6 4.5" fill="none" stroke={chart.copper} strokeWidth={1.75} strokeLinecap="round" strokeLinejoin="round" />
              <text x={12} y={4} fontSize={12} fontWeight={600} fill={chart.text.primary} stroke={HALO} strokeWidth={3} paintOrder="stroke">
                Toe ≈ RM {Math.round(state.toe)}, off chart
              </text>
            </g>
          )}

          {/* Wedge pushed out of the river */}
          {state.toe < RM_DOWNSTREAM && (
            <text
              x={plotRight - 8}
              y={y(bedDepth(RM_DOWNSTREAM)) - 12}
              textAnchor="end"
              fontSize={fs}
              fill={chart.text.secondary}
              stroke={HALO}
              strokeWidth={3}
              paintOrder="stroke"
            >
              {compact ? 'Salt held in the Gulf →' : 'No salt in the river; wedge held out in the Gulf →'}
            </text>
          )}

          {/* Bed caption */}
          <text x={plotLeft + 10} y={plotBottom - 10} fontSize={11} fill={chart.text.muted}>
            {compact ? 'Channel bed · schematic' : `Channel bed · schematic · vertical scale exaggerated ≈${Number(exaggerationLabel).toLocaleString('en-US')}×`}
          </text>

          {/* Depth axis */}
          {DEPTH_TICKS.map((d) => (
            <text key={d} x={plotLeft - 8} y={y(d)} dy="0.32em" textAnchor="end" fontSize={11} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
              {d === 0 ? (compact ? '0 ft' : 'Surface') : `${d} ft`}
            </text>
          ))}

          {/* River-mile axis */}
          <line x1={plotLeft} x2={plotRight} y1={plotBottom} y2={plotBottom} stroke={chart.axis} strokeWidth={1} shapeRendering="crispEdges" />
          {Array.from({ length: 12 }, (_, i) => 110 - i * 10)
            .filter((t) => !compact || t % 20 === 0)
            .map((t) => (
              <g key={t} transform={`translate(${x(t)},${plotBottom})`}>
                <line y1={0} y2={4} stroke={chart.axis} strokeWidth={1} shapeRendering="crispEdges" />
                <text y={17} textAnchor="middle" fontSize={11} fill={chart.text.muted} style={{ fontVariantNumeric: 'tabular-nums' }}>
                  {t}
                </text>
              </g>
            ))}
          <text x={plotLeft} y={plotBottom + 38} fontSize={11} fill={chart.text.muted}>
            ← Upstream
          </text>
          {!compact && (
            <text x={(plotLeft + plotRight) / 2} y={plotBottom + 38} textAnchor="middle" fontSize={11} fill={chart.text.muted}>
              River miles above Head of Passes
            </text>
          )}
          <text x={plotRight} y={plotBottom + 38} textAnchor="end" fontSize={11} fill={chart.text.muted}>
            {compact ? 'Gulf →' : 'Gulf of Mexico →'}
          </text>

          {/* Landmark labels + leaders */}
          {band.map((b) => {
            const baseline = plotTop - 10 - b.row * rowH
            const glyph = statusFor(b.lm)
            const isActive = activeId === b.lm.id
            const hasIcon = b.lm.kind === 'intake'
            const m = b.lm.kind === 'intake' && glyph ? markerStyle(glyph) : null
            return (
              <g key={b.lm.id} pointerEvents="none">
                <line x1={b.x} x2={b.x} y1={baseline + 5} y2={plotTop} stroke={isActive ? 'rgba(242,245,247,0.5)' : 'rgba(242,245,247,0.18)'} strokeWidth={1} shapeRendering="crispEdges" />
                {m && <circle cx={b.x} cy={plotTop} r={3.75} {...m} />}
                {b.lm.kind === 'reference' && <rect x={b.x - 3} y={plotTop - 3} width={6} height={6} transform={`rotate(45 ${b.x} ${plotTop})`} fill="#8A9BA8" />}
                {hasIcon && glyph && (
                  <g transform={`translate(${b.left},${baseline - fs + 1}) scale(${fs / 14})`}>{glyphShapes(glyph)}</g>
                )}
                <text x={b.left + (hasIcon ? 16 : 0)} y={baseline} fontSize={fs} fontWeight={500} fill={isActive ? chart.text.primary : chart.text.secondary}>
                  {b.text}
                </text>
                {focusId === b.lm.id && (
                  <rect x={b.left - 4} y={baseline - fs - 3} width={b.w + 8} height={fs + 9} rx={4} fill="none" stroke={chart.copper} strokeWidth={1.5} />
                )}
              </g>
            )
          })}

          {/* Hit targets (hover, tap, keyboard) */}
          {band.map((b) => {
            const glyph = statusFor(b.lm)
            const statusText =
              b.lm.kind === 'intake' && glyph && glyph in STATUS_TEXT
                ? STATUS_TEXT[glyph as keyof typeof STATUS_TEXT].label
                : b.lm.kind === 'sill'
                  ? SILL_TEXT[state.sill]
                  : ''
            const top = plotTop - 10 - b.row * rowH - fs - 4
            return (
              <g
                key={b.lm.id}
                role="button"
                tabIndex={0}
                aria-label={`${b.lm.name}, river mile ${fmtRm(b.lm.rm)}${statusText ? `: ${statusText}` : ''}`}
                onPointerEnter={() => setHoverId(b.lm.id)}
                onPointerLeave={() => setHoverId(null)}
                onClick={() => setHoverId((id) => (id === b.lm.id ? null : b.lm.id))}
                onFocus={() => setFocusId(b.lm.id)}
                onBlur={() => setFocusId(null)}
                onKeyDown={(e) => {
                  if (e.key === 'Escape') (e.currentTarget as SVGGElement).blur()
                }}
                className="outline-none"
                style={{ cursor: 'pointer' }}
              >
                <rect x={Math.min(b.left, b.x - 12)} y={top} width={Math.max(b.w, 24)} height={plotTop - top + 6} fill="transparent" />
                <rect x={b.x - 12} y={plotTop} width={24} height={Math.max(0, y(bedDepth(b.lm.rm)) - plotTop)} fill="transparent" />
              </g>
            )
          })}
        </svg>
      )}

      {active && width > 0 && (
        <ChartTooltip x={x(active.rm)} y={plotTop + 64} containerWidth={width} title={active.name}>
          <div className="max-w-[240px] space-y-1.5 whitespace-normal">
            <div className="flex items-baseline gap-2 text-xs">
              <span className="font-semibold text-white tabular-nums">RM {fmtRm(active.rm)}</span>
              <span className="text-muted">above Head of Passes</span>
            </div>
            {(() => {
              const glyph = statusFor(active)
              if (!glyph) return null
              const text =
                active.kind === 'sill' ? SILL_TEXT[state.sill] : STATUS_TEXT[glyph as keyof typeof STATUS_TEXT].label
              return (
                <div className="flex items-center gap-2 text-xs text-white">
                  <StatusGlyph glyph={glyph} />
                  {text}
                </div>
              )
            })()}
            <p className="text-xs leading-snug text-titanium">{active.note}</p>
            <p className="text-[11px] leading-snug text-muted">Source: {SOURCES[active.source].label}</p>
          </div>
        </ChartTooltip>
      )}
    </div>
  )
}
