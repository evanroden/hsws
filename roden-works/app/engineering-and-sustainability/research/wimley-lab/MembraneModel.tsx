'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useMemo, useState } from 'react'
import { useInView } from '@/lib/hooks'
import ChartFrame from '@/components/charts/ChartFrame'
import Legend from '@/components/charts/Legend'
import RangeSlider from '@/components/ui/RangeSlider'
import SegmentedControl from '@/components/ui/SegmentedControl'
import { useElementSize } from '@/components/charts/useElementSize'

/* ─────────────────────────────────────────────────────────────────────────────
 * Interactive lipid-bilayer cross-section.
 *
 * Every behaviour shown is drawn only from facts already on this page:
 *   • The pHD peptide family forms nanopores that activate at pH < 6 and stay
 *     inactive at physiological pH (7.4).
 *   • Macrolittins form stable pores at nanomolar concentrations regardless of pH.
 * The geometry (helix count, spacing, lumen width) is a schematic — labelled as
 * "not to scale" — not measured structural data.
 * ────────────────────────────────────────────────────────────────────────── */

const COL = {
  head: '#6A8CDB', // steel — phospholipid headgroup
  headFill: 'rgba(106,140,219,0.14)',
  tail: '#48555E', // recessive acyl tails
  core: '#5E6C76', // hydrophobic-core label (3.2:1 on surface)
  coreBand: 'rgba(94,108,118,0.10)',
  peptide: '#3DA887', // verdigris — the active molecule
  peptideFill: 'rgba(61,168,135,0.16)',
  peptideBack: 'rgba(61,168,135,0.09)',
  lumen: '#B87333', // copper — the channel
  lumenFill: 'rgba(184,115,51,0.08)',
  cargo: '#D08C4F', // copper-light — cargo / dye
  surface: '#151C1F',
  text: '#7E8F9B',
  textStrong: '#A9B6BF',
  white: '#F2F5F7',
}

type Family = 'phd' | 'macro'

const PH_MIN = 4.5
const PH_MAX = 8.0
const PH_THRESHOLD = 6.0 // page: pHD nanopores activate at pH < 6

const H = 384

interface HelixSpec {
  id: string
  cx: number
  cy: number
  len: number
  thick: number
  angle: number
  fill: string
  strokeOpacity?: number
}

function Helix({
  spec,
  stripe = true,
}: {
  spec: HelixSpec
  stripe?: boolean
}) {
  const { id, cx, cy, len, thick, angle, fill, strokeOpacity = 1 } = spec
  const half = len / 2
  const ht = thick / 2
  const nStripes = Math.max(3, Math.round(len / 13))
  const clipId = `hx-${id}`
  return (
    <g transform={`translate(${cx},${cy}) rotate(${angle})`}>
      <clipPath id={clipId}>
        <rect x={-half} y={-ht} width={len} height={thick} rx={ht} />
      </clipPath>
      <rect
        x={-half}
        y={-ht}
        width={len}
        height={thick}
        rx={ht}
        fill={fill}
        stroke={COL.peptide}
        strokeOpacity={strokeOpacity}
        strokeWidth={1.4}
      />
      {stripe && (
        <g clipPath={`url(#${clipId})`}>
          {Array.from({ length: nStripes }, (_, i) => {
            const x = -half + (i + 0.5) * (len / nStripes)
            return (
              <line
                key={i}
                x1={x - ht * 0.85}
                y1={-ht}
                x2={x + ht * 0.85}
                y2={ht}
                stroke={COL.peptide}
                strokeOpacity={0.5 * strokeOpacity}
                strokeWidth={1.2}
                strokeLinecap="round"
              />
            )
          })}
        </g>
      )}
    </g>
  )
}

export default function MembraneModel() {
  const { ref: inRef, isInView } = useInView(0.15)
  const { ref: sizeRef, width } = useElementSize<HTMLDivElement>()
  const reduce = useReducedMotion() ?? false

  const [pH, setPH] = useState(5.5) // default: acidic — pore assembled, a compelling first view
  const [family, setFamily] = useState<Family>('phd')

  const poreOpen = family === 'macro' || pH < PH_THRESHOLD
  const active = isInView
  const animate = active && !reduce

  const trans = { duration: reduce ? 0 : 0.7, ease: [0.16, 1, 0.3, 1] as const }

  const geo = useMemo(() => {
    const W = width
    const compact = W < 480
    const MX = 14
    const upperHeadY = 118
    const lowerHeadY = 262
    const midY = (upperHeadY + lowerHeadY) / 2
    const headR = 7
    const coreTop = upperHeadY + headR + 1
    const coreBot = lowerHeadY - headR - 1
    const xC = W / 2
    const poleTop = upperHeadY - 22
    const poleBot = lowerHeadY + 22

    const step = compact ? 18 : 19
    const usable = W - MX * 2
    const n = Math.max(10, Math.round(usable / step))
    const dx = usable / n
    const heads = Array.from({ length: n }, (_, i) => MX + (i + 0.5) * dx)

    const poreHalf = 30 // lipids inside this band are displaced by the pore

    return { W, compact, MX, upperHeadY, lowerHeadY, midY, headR, coreTop, coreBot, xC, poleTop, poleBot, heads, poreHalf }
  }, [width])

  const { compact, upperHeadY, lowerHeadY, midY, headR, coreTop, coreBot, xC, poleTop, poleBot, heads, poreHalf } = geo
  const W = width

  // Tail path: a gently curved acyl chain from a headgroup toward the midplane.
  const tail = (x: number, y0: number, y1: number, off: number) => {
    const mid = (y0 + y1) / 2
    return `M${x + off},${y0} C${x + off * 1.7},${(y0 + mid) / 2} ${x + off * 0.3},${(mid + y1) / 2} ${x + off * 0.15},${y1}`
  }

  // Surface-bound peptides (inactive): resting on the outer leaflet.
  const surfaceHelices: HelixSpec[] = useMemo(() => {
    if (!W) return []
    const fracs = compact ? [0.36, 0.64] : [0.28, 0.5, 0.72]
    const len = compact ? 54 : 66
    return fracs.map((f, i) => ({
      id: `surf-${i}`,
      cx: W * f,
      cy: upperHeadY - 5,
      len,
      thick: 15,
      angle: i % 2 === 0 ? -6 : 5,
      fill: COL.peptideFill,
    }))
  }, [W, compact, upperHeadY])

  // Transmembrane barrel (active): two faint back helices + two front-wall helices.
  const poreHelices: HelixSpec[] = useMemo(() => {
    if (!W) return []
    const len = poleBot - poleTop
    return [
      { id: 'back-l', cx: xC - 9, cy: midY, len: len - 24, thick: 13, angle: 90, fill: COL.peptideBack, strokeOpacity: 0.5 },
      { id: 'back-r', cx: xC + 9, cy: midY, len: len - 24, thick: 13, angle: 90, fill: COL.peptideBack, strokeOpacity: 0.5 },
      { id: 'front-l', cx: xC - 25, cy: midY, len, thick: 18, angle: 90, fill: COL.peptideFill },
      { id: 'front-r', cx: xC + 25, cy: midY, len, thick: 18, angle: 90, fill: COL.peptideFill },
    ]
  }, [W, xC, midY, poleTop, poleBot])

  const lumenHalf = 15
  const cargoX = [0, -6, 5, -3, 7]
  const staticCargoY = [poleTop + 26, midY - 14, midY + 34, poleBot - 20]

  const label = (
    x: number,
    y: number,
    text: string,
    opts: { anchor?: 'start' | 'middle' | 'end'; color?: string; size?: number; weight?: number } = {}
  ) => (
    <text
      x={x}
      y={y}
      textAnchor={opts.anchor ?? 'start'}
      fontSize={opts.size ?? 11}
      fontWeight={opts.weight ?? 400}
      fill={opts.color ?? COL.text}
      style={{ paintOrder: 'stroke', stroke: COL.surface, strokeWidth: 3.5, strokeLinejoin: 'round' }}
    >
      {text}
    </text>
  )

  return (
    <div ref={inRef}>
      <ChartFrame
        title="Peptide–membrane interaction, by environment pH"
        subtitle="Cross-section of a lipid bilayer. Drag pH or switch peptide family to assemble or dissolve the transmembrane pore."
        actions={
          <SegmentedControl<Family>
            label="Peptide family"
            value={family}
            onChange={setFamily}
            options={[
              { value: 'phd', label: 'pHD (pH-gated)' },
              { value: 'macro', label: 'Macrolittin' },
            ]}
          />
        }
        legend={
          <Legend
            items={[
              { label: 'Lipid headgroup', color: COL.head, shape: 'dot' },
              { label: 'Hydrophobic core', color: COL.core, shape: 'rect' },
              { label: 'Peptide α-helix', color: COL.peptide, shape: 'rect' },
              { label: 'Pore lumen', color: COL.lumen, shape: 'rect' },
              { label: 'Cargo / dye', color: COL.cargo, shape: 'dot' },
            ]}
          />
        }
        note={
          <>
            Schematic model — not to scale. State follows facts on this page: pHD peptides gate at pH&nbsp;&lt;&nbsp;6;
            macrolittins hold a stable pore regardless of pH.
          </>
        }
      >
        {/* Live state readout */}
        <div
          className="mb-4 flex items-center gap-2.5 rounded-lg border border-white/[0.08] bg-white/[0.02] px-3.5 py-2.5"
          aria-live="polite"
        >
          <span
            className="relative flex h-2.5 w-2.5 shrink-0"
            aria-hidden="true"
          >
            <span
              className="absolute inline-flex h-full w-full rounded-full opacity-60"
              style={{ background: poreOpen ? COL.lumen : COL.peptide }}
            />
            <span
              className="relative inline-flex h-2.5 w-2.5 rounded-full"
              style={{ background: poreOpen ? COL.lumen : COL.peptide }}
            />
          </span>
          <span className="text-sm text-white">
            {poreOpen ? 'Pore assembled — cargo leaking' : 'Peptides surface-bound — membrane intact'}
          </span>
        </div>

        {/* Cross-section */}
        <div ref={sizeRef} className="relative w-full" style={{ height: H }}>
          {W > 0 && (
            <>
              <svg
                width={W}
                height={H}
                viewBox={`0 0 ${W} ${H}`}
                role="img"
                aria-label={
                  poreOpen
                    ? 'Cross-section of a lipid bilayer: peptide alpha-helices span the membrane as a transmembrane barrel and cargo molecules pass through the central pore lumen into the cytoplasm.'
                    : 'Cross-section of an intact lipid bilayer: peptide alpha-helices rest on the outer surface and cargo molecules remain in the extracellular space above the membrane.'
                }
                className="block"
              >
                <defs>
                  <linearGradient id="mm-water-top" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="0" stopColor="#6A8CDB" stopOpacity="0.06" />
                    <stop offset="1" stopColor="#6A8CDB" stopOpacity="0" />
                  </linearGradient>
                  <linearGradient id="mm-water-bot" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="0" stopColor="#6A8CDB" stopOpacity="0" />
                    <stop offset="1" stopColor="#6A8CDB" stopOpacity="0.06" />
                  </linearGradient>
                  <linearGradient id="mm-lumen" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="0" stopColor={COL.lumen} stopOpacity="0.02" />
                    <stop offset="0.5" stopColor={COL.lumen} stopOpacity="0.14" />
                    <stop offset="1" stopColor={COL.lumen} stopOpacity="0.02" />
                  </linearGradient>
                </defs>

                {/* Aqueous regions */}
                <rect x="0" y="0" width={W} height={coreTop} fill="url(#mm-water-top)" />
                <rect x="0" y={coreBot} width={W} height={H - coreBot} fill="url(#mm-water-bot)" />

                {/* Hydrophobic core band */}
                <rect x="0" y={coreTop} width={W} height={coreBot - coreTop} fill={COL.coreBand} />
                <line x1="0" y1={midY} x2={W} y2={midY} stroke={COL.core} strokeOpacity="0.18" strokeWidth="1" />

                {/* Region + core labels */}
                {label(13, 20, 'Extracellular space', { color: COL.textStrong })}
                {label(13, 34, poreOpen ? 'acidic microenvironment' : 'physiological pH 7.4', {
                  color: COL.text,
                  size: 10,
                })}
                {label(13, H - 10, 'Cytoplasm', { color: COL.textStrong })}

                {/* Lipid bilayer */}
                {heads.map((x, i) => {
                  const displaced = poreOpen && Math.abs(x - xC) < poreHalf
                  return (
                    <g
                      key={i}
                      style={{
                        opacity: displaced ? 0.12 : 1,
                        transition: reduce ? 'none' : 'opacity 0.6s cubic-bezier(0.16,1,0.3,1)',
                      }}
                    >
                      {/* upper leaflet */}
                      <path d={tail(x, upperHeadY + headR, midY - 3, -2.6)} stroke={COL.tail} strokeWidth="1" fill="none" strokeLinecap="round" />
                      <path d={tail(x, upperHeadY + headR, midY - 3, 2.6)} stroke={COL.tail} strokeWidth="1" fill="none" strokeLinecap="round" />
                      <circle cx={x} cy={upperHeadY} r={headR} fill={COL.headFill} stroke={COL.head} strokeWidth="1.1" />
                      {/* lower leaflet */}
                      <path d={tail(x, lowerHeadY - headR, midY + 3, -2.6)} stroke={COL.tail} strokeWidth="1" fill="none" strokeLinecap="round" />
                      <path d={tail(x, lowerHeadY - headR, midY + 3, 2.6)} stroke={COL.tail} strokeWidth="1" fill="none" strokeLinecap="round" />
                      <circle cx={x} cy={lowerHeadY} r={headR} fill={COL.headFill} stroke={COL.head} strokeWidth="1.1" />
                    </g>
                  )
                })}

                {/* core label drawn over the tails */}
                {label(13, midY - 6, 'Hydrophobic core', { color: COL.core, size: 10 })}
                {/* ── Surface-bound state (inactive) ── */}
                <motion.g
                  initial={{ opacity: 1, y: 0 }}
                  animate={active ? (poreOpen ? { opacity: 0, y: 12 } : { opacity: 1, y: 0 }) : { opacity: 1, y: 0 }}
                  transition={trans}
                  style={{ pointerEvents: 'none' }}
                >
                  {surfaceHelices.map((s) => (
                    <Helix key={s.id} spec={s} />
                  ))}
                  {/* contained cargo: trapped above an intact membrane */}
                  {[
                    [0.42, 58],
                    [0.56, 44],
                    [0.66, 66],
                  ].map(([fx, cy], i) => (
                    <circle key={i} cx={W * fx} cy={cy} r={4.5} fill={COL.cargo} stroke={COL.surface} strokeWidth={2} />
                  ))}
                  {!compact &&
                    !poreOpen &&
                    (() => {
                      const s = surfaceHelices[Math.floor(surfaceHelices.length / 2)]
                      return (
                        <>
                          <line x1={s.cx} y1={s.cy - 12} x2={s.cx} y2={s.cy - 24} stroke={COL.peptide} strokeOpacity="0.5" strokeWidth="1" />
                          {label(s.cx, s.cy - 28, 'Surface-bound peptide', { anchor: 'middle', color: COL.textStrong })}
                        </>
                      )
                    })()}
                </motion.g>

                {/* ── Transmembrane pore state (active) ── */}
                <motion.g
                  initial={{ opacity: 0, y: 14 }}
                  animate={active && poreOpen ? { opacity: 1, y: 0 } : { opacity: 0, y: 14 }}
                  transition={trans}
                  style={{ pointerEvents: 'none' }}
                >
                  {/* lumen channel */}
                  <rect
                    x={xC - lumenHalf}
                    y={poleTop}
                    width={lumenHalf * 2}
                    height={poleBot - poleTop}
                    rx={lumenHalf}
                    fill="url(#mm-lumen)"
                    stroke={COL.lumen}
                    strokeOpacity="0.45"
                    strokeWidth="1.2"
                  />
                  {/* back wall (behind lumen), then front walls */}
                  {poreHelices.map((s) => (
                    <Helix key={s.id} spec={s} />
                  ))}

                  {/* streaming cargo through the lumen */}
                  {(animate && poreOpen
                    ? cargoX
                    : poreOpen
                      ? staticCargoY.map(() => 0)
                      : []
                  ).map((_, i) =>
                    animate ? (
                      <motion.circle
                        key={i}
                        cx={xC + cargoX[i]}
                        r={4.5}
                        fill={COL.cargo}
                        stroke={COL.surface}
                        strokeWidth={2}
                        initial={{ cy: poleTop - 8, opacity: 0 }}
                        animate={{ cy: [poleTop - 8, poleBot + 8], opacity: [0, 1, 1, 0] }}
                        transition={{
                          duration: 2.6,
                          delay: i * 0.42,
                          repeat: Infinity,
                          ease: 'linear',
                          times: [0, 0.14, 0.86, 1],
                        }}
                      />
                    ) : (
                      <circle key={i} cx={xC + (cargoX[i] ?? 0)} cy={staticCargoY[i]} r={4.5} fill={COL.cargo} stroke={COL.surface} strokeWidth={2} />
                    )
                  )}

                  {/* leader labels (desktop) */}
                  {!compact && (
                    <>
                      <line x1={xC + 25} y1={upperHeadY - 2} x2={xC + 54} y2={upperHeadY - 14} stroke={COL.peptide} strokeOpacity="0.5" strokeWidth="1" />
                      {label(xC + 58, upperHeadY - 12, 'Peptide barrel', { color: COL.textStrong })}

                      <line x1={xC - lumenHalf} y1={midY} x2={xC - 46} y2={midY} stroke={COL.lumen} strokeOpacity="0.55" strokeWidth="1" />
                      {label(xC - 50, midY + 4, 'Pore lumen', { anchor: 'end', color: COL.textStrong })}

                      <line x1={xC + 6} y1={poleBot - 4} x2={xC + 46} y2={poleBot + 6} stroke={COL.cargo} strokeOpacity="0.6" strokeWidth="1" />
                      {label(xC + 50, poleBot + 10, 'Cargo leaks through', { color: COL.textStrong })}
                    </>
                  )}
                </motion.g>
              </svg>

              {/* pH badge overlay (crisp HTML text) */}
              <div className="pointer-events-none absolute right-3 top-3 rounded-lg border border-white/[0.08] bg-slate-950/70 px-3 py-2 text-right backdrop-blur-sm">
                <div className="font-sans text-lg font-semibold tracking-tight text-white leading-none">
                  pH {pH.toFixed(1)}
                </div>
                <div className="mt-1 text-[10px] uppercase tracking-wider" style={{ color: pH < PH_THRESHOLD ? COL.cargo : COL.text }}>
                  {pH < PH_THRESHOLD ? 'Acidic' : pH < 7 ? 'Near-neutral' : 'Physiological'}
                </div>
              </div>
            </>
          )}
        </div>

        {/* pH control */}
        <div className="mt-6">
          <RangeSlider
            label="Environment pH"
            value={pH}
            min={PH_MIN}
            max={PH_MAX}
            step={0.1}
            onChange={setPH}
            valueText={`pH ${pH.toFixed(1)}`}
            accent={COL.lumen}
            ticks={[
              { value: PH_THRESHOLD, label: '6.0' },
              { value: 7.4, label: '7.4' },
            ]}
          />
          <p className="mt-3 text-xs text-muted">
            {family === 'macro' ? (
              <>Macrolittins hold a stable pore at nanomolar concentrations — pH-independent, so the channel stays open across the whole range.</>
            ) : (
              <>pH&nbsp;6.0 — pHD nanopore threshold&nbsp;·&nbsp;pH&nbsp;7.4 — physiological. Below&nbsp;6.0 the peptides insert and assemble a pore.</>
            )}
          </p>
        </div>
      </ChartFrame>
    </div>
  )
}
