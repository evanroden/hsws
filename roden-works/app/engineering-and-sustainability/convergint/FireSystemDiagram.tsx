'use client'

import { motion, useReducedMotion } from 'framer-motion'
import { useEffect, useId, useState, type KeyboardEvent, type ReactNode } from 'react'
import ChartFrame from '@/components/charts/ChartFrame'
import { chart } from '@/components/charts/tokens'
import { useElementSize } from '@/components/charts/useElementSize'

/* ─── Sources for the generic system behaviour shown here ──────────────────
 * • Circuit types (IDC / NAC / SLC), alarm > supervisory > trouble priority,
 *   annunciator near the responders' entrance, DACT sending alarm, supervisory
 *   and trouble signals: UFGS 28 31 76 "Fire Alarm and Mass Notification"
 *   (Aug 2020), paras 1.6.1, 1.6.2, 2.7, 2.24.2 — wbdg.org.
 * • Clean agent sequence (releasing control unit, pre-discharge alarm, time
 *   delay ≤ 60 s with abort, releasing circuit, cylinder low-pressure switch
 *   reporting a supervisory signal): UFGS 21 22 00 "Clean Agent Fire
 *   Extinguishing Systems", paras 2.2.7, 2.3 and sequence of operation.
 * • Supervising station retransmits fire alarm signals immediately to the
 *   communications center: NFPA 72 §26.2.1.2 (NFSA, Mar 2024).
 * • Waterflow switch → alarm; valve tamper switch → supervisory:
 *   firesystems.net, "Waterflow, Tamper, and Supervisory Switches" (2026).
 */

type CircuitType = 'slc' | 'nac' | 'release' | 'supv' | 'monitor'

const CIRCUITS: Record<
  CircuitType,
  { label: string; tag: string; color: string; width: number; dash?: string; arrow: boolean }
> = {
  slc: { label: 'Signaling line circuit (SLC)', tag: 'SLC', color: chart.copper, width: 1.5, arrow: false },
  nac: { label: 'Notification appliance circuit (NAC)', tag: 'NAC', color: chart.verdigris, width: 1.5, arrow: true },
  // Added hues for the extra categories: coral 5.7:1 and amber 7.7:1 on #151C1F.
  // Each also carries its own stroke pattern + text tag, never color alone.
  release: { label: 'Releasing circuit', tag: 'RELEASE', color: '#E5735F', width: 2.5, arrow: true },
  supv: { label: 'Supervisory signal', tag: 'SUPERVISORY', color: '#D9A441', width: 1.5, dash: '5 4', arrow: true },
  monitor: { label: 'Monitoring link', tag: 'MONITORING', color: chart.steel, width: 1.5, dash: '9 4', arrow: true },
}

type Group = 'Initiating device' | 'Control' | 'Notification' | 'Off-site monitoring' | 'Suppression'
type IconName =
  | 'smoke'
  | 'pull'
  | 'riser'
  | 'facp'
  | 'horn'
  | 'annunciator'
  | 'communicator'
  | 'central'
  | 'releasing'
  | 'cylinders'

interface Device {
  id: string
  label: string
  sub: string
  group: Group
  icon: IconName
  description: string
}

const DEVICES: Device[] = [
  {
    id: 'smoke',
    label: 'Smoke detector',
    sub: 'Addressable',
    group: 'Initiating device',
    icon: 'smoke',
    description:
      'Photoelectric and ionization detectors placed throughout a building to sense smoke particles. Addressable devices report their exact location to the fire alarm control panel for rapid response. NFPA 72 dictates spacing, placement heights, and maintenance intervals.',
  },
  {
    id: 'pull',
    label: 'Pull station',
    sub: 'Manual · at exits',
    group: 'Initiating device',
    icon: 'pull',
    description:
      'Manual fire alarm boxes located at building exits per NFPA 72 requirements. When activated, they send an alarm signal to the FACP triggering building-wide notification. Double-action stations reduce false alarms in high-traffic environments.',
  },
  {
    id: 'riser',
    label: 'Sprinkler riser',
    sub: 'Waterflow + tamper',
    group: 'Suppression',
    icon: 'riser',
    description:
      'Vertical pipes that connect the water supply to the sprinkler system. Each riser serves a zone and includes a tamper switch and flow switch that reports to the FACP. Wet, dry, pre-action, and deluge systems are selected based on the hazard classification per NFPA 13.',
  },
  {
    id: 'facp',
    label: 'FACP',
    sub: 'Fire alarm control panel',
    group: 'Control',
    icon: 'facp',
    description:
      'The brain of the fire protection system. The FACP receives signals from every initiating device, processes alarm/trouble/supervisory conditions, activates notification appliances, and communicates with the central monitoring station. Programming defines system behavior — sequences, priorities, and interlocks.',
  },
  {
    id: 'horn',
    label: 'Horn/strobes',
    sub: 'Audible + visible',
    group: 'Notification',
    icon: 'horn',
    description:
      'Notification appliances wired on the notification appliance circuits. Horns sound the evacuation signal and strobes flash for occupants who may not hear it; the FACP drives both whenever it enters alarm. NFPA 72 sets their audibility and visibility requirements.',
  },
  {
    id: 'annunciator',
    label: 'Annunciator',
    sub: 'At main entrance',
    group: 'Notification',
    icon: 'annunciator',
    description:
      'The command center for fire response. Graphic annunciator panels display a floor-by-floor map showing the exact zone in alarm. Firefighters use these to pinpoint the origin and direct evacuation. Required at main entrances per AHJ specifications.',
  },
  {
    id: 'communicator',
    label: 'Communicator',
    sub: 'Off-site transmitter',
    group: 'Off-site monitoring',
    icon: 'communicator',
    description:
      'A transmitter at the panel — commonly a digital alarm communicator (DACT), cellular, or IP unit — that sends alarm, supervisory, and trouble signals off-site to a supervising station.',
  },
  {
    id: 'central',
    label: 'Central station',
    sub: 'Supervising station',
    group: 'Off-site monitoring',
    icon: 'central',
    description:
      'A continuously staffed supervising station. Under NFPA 72 it immediately retransmits fire alarm signals to the fire department’s communications center, then notifies the building’s contacts.',
  },
  {
    id: 'releasing',
    label: 'Releasing panel',
    sub: 'Clean agent control',
    group: 'Suppression',
    icon: 'releasing',
    description:
      'A releasing control unit dedicated to the clean agent system. It watches its own detectors plus manual release and abort stations, runs the pre-discharge alarm and time delay, then energizes the releasing circuit — and reports alarm, supervisory, and release status to the FACP.',
  },
  {
    id: 'cylinders',
    label: 'Agent cylinders',
    sub: 'Clean agent storage',
    group: 'Suppression',
    icon: 'cylinders',
    description:
      'Gaseous suppression systems (FM-200, Novec 1230, or Inergen) designed for spaces where water would cause more damage than fire — data centers, museum archives, telecom rooms. The agent suppresses fire by removing heat or displacing oxygen without leaving residue. Governed by NFPA 2001. A low-pressure switch on each cylinder sends a supervisory signal if the agent leaks down.',
  },
]

const deviceById = (id: string) => DEVICES.find((d) => d.id === id)!

interface Edge {
  id: string
  type: CircuitType
  from: string
  to: string
}

const EDGES: Edge[] = [
  { id: 'slc-smoke', type: 'slc', from: 'smoke', to: 'facp' },
  { id: 'slc-pull', type: 'slc', from: 'pull', to: 'facp' },
  { id: 'slc-riser', type: 'slc', from: 'riser', to: 'facp' },
  { id: 'supv-riser', type: 'supv', from: 'riser', to: 'facp' },
  { id: 'nac', type: 'nac', from: 'facp', to: 'horn' },
  { id: 'slc-annunciator', type: 'slc', from: 'facp', to: 'annunciator' },
  { id: 'facp-comm', type: 'monitor', from: 'facp', to: 'communicator' },
  { id: 'monitor', type: 'monitor', from: 'communicator', to: 'central' },
  { id: 'slc-releasing', type: 'slc', from: 'releasing', to: 'facp' },
  { id: 'release', type: 'release', from: 'releasing', to: 'cylinders' },
  { id: 'supv-cyl', type: 'supv', from: 'cylinders', to: 'releasing' },
]

/* ─── Layouts: real coordinates in viewBox units, two arrangements ───────── */

type Pt = [number, number]
interface Box {
  x: number
  y: number
  w: number
  h: number
}
interface Layout {
  name: 'wide' | 'narrow'
  vb: [number, number, number, number]
  font: { label: number; sub: number; tag: number; group: number; hub: number }
  icon: number
  nodes: Record<string, Box>
  edges: Record<string, Pt[]>
  /** Where each edge's text tag sits (omitted where the run is too short) */
  tags: Partial<Record<string, Pt>>
  groups: { text: string; x: number; y: number; anchor?: 'start' | 'end' }[]
  region?: Box
}

const W = 172
const WIDE: Layout = {
  name: 'wide',
  vb: [0, 0, 1100, 560],
  font: { label: 14, sub: 12, tag: 12, group: 12, hub: 22 },
  icon: 24,
  nodes: {
    smoke: { x: 24, y: 56, w: W, h: 60 },
    pull: { x: 24, y: 168, w: W, h: 60 },
    riser: { x: 24, y: 344, w: W, h: 64 },
    facp: { x: 440, y: 166, w: 200, h: 140 },
    horn: { x: 904, y: 56, w: W, h: 60 },
    annunciator: { x: 904, y: 184, w: W, h: 60 },
    communicator: { x: 612, y: 368, w: W, h: 60 },
    central: { x: 904, y: 368, w: W, h: 60 },
    releasing: { x: 356, y: 480, w: W, h: 60 },
    cylinders: { x: 656, y: 480, w: W, h: 60 },
  },
  edges: {
    'slc-smoke': [[196, 86], [256, 86], [256, 220], [440, 220]],
    'slc-pull': [[196, 198], [256, 198], [256, 220], [440, 220]],
    'slc-riser': [[196, 364], [256, 364], [256, 220], [440, 220]],
    'supv-riser': [[196, 396], [312, 396], [312, 276], [440, 276]],
    nac: [[640, 184], [772, 184], [772, 86], [904, 86]],
    'slc-annunciator': [[640, 214], [904, 214]],
    'facp-comm': [[590, 306], [590, 398], [612, 398]],
    monitor: [[784, 398], [904, 398]],
    'slc-releasing': [[470, 480], [470, 306]],
    release: [[528, 500], [656, 500]],
    'supv-cyl': [[656, 524], [528, 524]],
  },
  tags: {
    'slc-smoke': [348, 220],
    'supv-riser': [376, 276],
    nac: [838, 86],
    'slc-annunciator': [838, 214],
    monitor: [844, 398],
    'slc-releasing': [470, 400],
    release: [592, 500],
    'supv-cyl': [592, 524],
  },
  groups: [
    { text: 'INITIATING DEVICES', x: 24, y: 38 },
    { text: 'NOTIFICATION', x: 904, y: 38 },
    { text: 'OFF-SITE', x: 904, y: 354 },
  ],
  region: { x: 340, y: 444, w: 504, h: 108 },
}

const N = 152
const NARROW: Layout = {
  name: 'narrow',
  vb: [-2, -2, 324, 552],
  font: { label: 13, sub: 11.75, tag: 11.75, group: 11.75, hub: 18 },
  icon: 20,
  nodes: {
    smoke: { x: 0, y: 24, w: N, h: 52 },
    pull: { x: 168, y: 24, w: N, h: 52 },
    riser: { x: 0, y: 92, w: N, h: 52 },
    annunciator: { x: 168, y: 92, w: N, h: 52 },
    facp: { x: 60, y: 176, w: 200, h: 84 },
    horn: { x: 0, y: 308, w: N, h: 52 },
    communicator: { x: 168, y: 308, w: N, h: 52 },
    central: { x: 168, y: 392, w: N, h: 52 },
    releasing: { x: 0, y: 484, w: N, h: 52 },
    cylinders: { x: 168, y: 484, w: N, h: 52 },
  },
  edges: {
    'slc-smoke': [[152, 50], [160, 50], [160, 176]],
    'slc-pull': [[168, 50], [160, 50], [160, 176]],
    'slc-riser': [[152, 112], [160, 112], [160, 176]],
    'supv-riser': [[40, 144], [40, 218], [60, 218]],
    nac: [[110, 260], [110, 284], [76, 284], [76, 308]],
    'slc-annunciator': [[160, 176], [160, 124], [168, 124]],
    'facp-comm': [[210, 260], [210, 284], [244, 284], [244, 308]],
    monitor: [[244, 360], [244, 392]],
    'slc-releasing': [[76, 484], [76, 456], [160, 456], [160, 260]],
    release: [[152, 502], [168, 502]],
    'supv-cyl': [[168, 522], [152, 522]],
  },
  tags: {
    'slc-smoke': [160, 158],
    nac: [93, 284],
    'slc-releasing': [160, 400],
  },
  groups: [
    { text: 'INITIATING DEVICES', x: 0, y: 14 },
    { text: 'CLEAN AGENT · NFPA 2001', x: 320, y: 476, anchor: 'end' },
  ],
}

/* ─── Alarm sequence (generic, simplified) ────────────────────────────────── */

interface Step {
  title: string
  body: string
  nodes: string[]
  edges: string[]
}

const STEPS: Step[] = [
  {
    title: 'Smoke detector senses smoke',
    body: 'A detector in the fire area sees smoke particles cross its threshold and reports an alarm at its own address.',
    nodes: ['smoke'],
    edges: [],
  },
  {
    title: 'Signal travels the SLC',
    body: 'The addressable alarm reaches the FACP over the signaling line circuit, identifying the exact device and location.',
    nodes: ['smoke', 'facp'],
    edges: ['slc-smoke'],
  },
  {
    title: 'FACP enters alarm',
    body: 'The panel processes the input and runs its programmed sequence. Alarm signals take priority over supervisory and trouble conditions.',
    nodes: ['facp'],
    edges: [],
  },
  {
    title: 'Occupants and responders are alerted',
    body: 'NACs sound the horns and flash the strobes for evacuation; the annunciator at the entrance shows the zone in alarm.',
    nodes: ['facp', 'horn', 'annunciator'],
    edges: ['nac', 'slc-annunciator'],
  },
  {
    title: 'Central station is signaled',
    body: 'The communicator transmits the alarm off-site. The central station retransmits it to the fire department’s communications center.',
    nodes: ['facp', 'communicator', 'central'],
    edges: ['facp-comm', 'monitor'],
  },
  {
    title: 'Suppression operates',
    body: 'Where heat opens a sprinkler, water flows up the riser and its waterflow switch reports an alarm. In a clean-agent room, the releasing panel runs a pre-discharge alarm and time delay (an abort window), then energizes the releasing circuit to discharge agent.',
    nodes: ['riser', 'releasing', 'cylinders'],
    edges: ['slc-riser', 'release'],
  },
]

const STEP_MS = 3400
const MIN_WIDE = 1080

/* ─── Component ───────────────────────────────────────────────────────────── */

export default function FireSystemDiagram() {
  const reduceMotion = useReducedMotion() ?? false
  const [selected, setSelected] = useState<string | null>(null)
  const [step, setStep] = useState<number | null>(null)
  const [playing, setPlaying] = useState(false)

  // Advance the sequence on a timer; under reduced motion the steps still
  // advance, the diagram just swaps states without pulses.
  useEffect(() => {
    if (!playing || step === null) return
    if (step >= STEPS.length - 1) {
      setPlaying(false)
      return
    }
    const t = setTimeout(() => setStep((s) => (s === null ? 0 : s + 1)), STEP_MS)
    return () => clearTimeout(t)
  }, [playing, step])

  const finished = step === STEPS.length - 1 && !playing
  const run = () => {
    setSelected(null)
    if (step === null || finished) setStep(0)
    setPlaying(true)
  }
  const reset = () => {
    setPlaying(false)
    setStep(null)
  }

  const primaryLabel = playing ? 'Pause' : step === null ? 'Run alarm sequence' : finished ? 'Replay sequence' : 'Resume'

  return (
    <ChartFrame
      title="Fire alarm system architecture"
      subtitle="Select any device to see its role, or run the sequence to follow one alarm from detection to suppression."
      actions={
        <>
          <button
            type="button"
            onClick={playing ? () => setPlaying(false) : run}
            className="inline-flex items-center gap-2 rounded-lg bg-copper px-3.5 py-1.5 text-xs font-semibold text-slate-950 hover:bg-copper-light transition-colors"
          >
            <svg className="w-3.5 h-3.5" viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">
              {playing ? (
                <path d="M4 3h3v10H4zM9 3h3v10H9z" />
              ) : (
                <path d="M4.5 2.8v10.4a.6.6 0 00.9.5l8.2-5.2a.6.6 0 000-1L5.4 2.3a.6.6 0 00-.9.5z" />
              )}
            </svg>
            {primaryLabel}
          </button>
          {step !== null && (
            <button
              type="button"
              onClick={reset}
              className="inline-flex items-center rounded-lg border border-white/[0.08] px-3 py-1.5 text-xs font-medium text-muted hover:text-white hover:border-white/20 transition-colors"
            >
              Reset
            </button>
          )}
        </>
      }
      legend={
        <ul className="flex flex-wrap items-center gap-x-5 gap-y-2">
          {(Object.keys(CIRCUITS) as CircuitType[]).map((k) => (
            <li key={k} className="flex items-center gap-2 text-xs text-titanium">
              <svg width="26" height="8" aria-hidden="true">
                <line
                  x1="1"
                  y1="4"
                  x2="25"
                  y2="4"
                  stroke={CIRCUITS[k].color}
                  strokeWidth={Math.min(CIRCUITS[k].width, 2.5)}
                  strokeDasharray={CIRCUITS[k].dash}
                />
              </svg>
              {CIRCUITS[k].label}
            </li>
          ))}
        </ul>
      }
      note="Simplified, generic schematic — real designs vary by occupancy, code edition, and the authority having jurisdiction. Sequence per NFPA 72 and UFGS 21 22 00 / 28 31 76."
      table={{
        caption: 'Fire alarm system devices, roles, and connections',
        columns: ['Device', 'Group', 'Connects to'],
        rows: DEVICES.map((d) => [
          d.id === 'facp' ? 'Fire alarm control panel (FACP)' : d.label,
          d.group,
          EDGES.filter((e) => e.from === d.id || e.to === d.id)
            .map((e) => `${deviceById(e.from === d.id ? e.to : e.from).label} (${CIRCUITS[e.type].tag.toLowerCase()})`)
            .join(', '),
        ]),
      }}
    >
      <Diagram
        selected={selected}
        onSelect={(id) => {
          setSelected((cur) => (cur === id ? null : id))
        }}
        step={step}
        reduceMotion={reduceMotion}
      />

      <div className="mt-6 grid grid-cols-1 lg:grid-cols-2 gap-4">
        <DetailPanel selected={selected} onSelect={setSelected} />
        <SequencePanel
          step={step}
          playing={playing}
          reduceMotion={reduceMotion}
          onStep={(i) => {
            setPlaying(false)
            setSelected(null)
            setStep(i)
          }}
        />
      </div>
    </ChartFrame>
  )
}

/* ─── The SVG ─────────────────────────────────────────────────────────────── */

function Diagram({
  selected,
  onSelect,
  step,
  reduceMotion,
}: {
  selected: string | null
  onSelect: (id: string) => void
  step: number | null
  reduceMotion: boolean
}) {
  const { ref, width } = useElementSize<HTMLDivElement>()
  const uid = useId().replace(/:/g, '')
  const [focused, setFocused] = useState<string | null>(null)
  const [hovered, setHovered] = useState<string | null>(null)

  const L = width >= MIN_WIDE ? WIDE : NARROW
  const [, , vbW, vbH] = L.vb
  // Narrow layout scales up to 1.5× on tablets, never down below the container
  const scale = L.name === 'wide' ? width / vbW : Math.min(width / vbW, 1.5)
  const svgW = vbW * scale
  const svgH = vbH * scale

  const seq = step !== null ? STEPS[step] : null
  const activeNodes = new Set<string>(seq ? seq.nodes : selected ? [selected] : [])
  const activeEdges = new Set<string>(
    seq ? seq.edges : selected ? EDGES.filter((e) => e.from === selected || e.to === selected).map((e) => e.id) : []
  )
  const dimming = activeNodes.size > 0
  const inAlarm = step !== null && step >= 2

  const onKey = (id: string) => (e: KeyboardEvent<SVGGElement>) => {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault()
      onSelect(id)
    }
  }

  // Draw inactive edges first so active runs sit on top of shared trunks
  const orderedEdges = [...EDGES].sort((a, b) => Number(activeEdges.has(a.id)) - Number(activeEdges.has(b.id)))

  return (
    <div ref={ref} className="w-full">
      {width > 0 && (
        <svg
          width={svgW}
          height={svgH}
          viewBox={L.vb.join(' ')}
          role="group"
          aria-label="Fire alarm system schematic. Initiating devices report to the fire alarm control panel over the signaling line circuit; the panel drives horn/strobes on notification appliance circuits, updates the annunciator, signals the central station through a communicator, and monitors the sprinkler riser and a clean agent releasing panel."
          className="block mx-auto select-none"
        >
          <defs>
            {(Object.keys(CIRCUITS) as CircuitType[]).map((k) => (
              <marker
                key={k}
                id={`${uid}-arr-${k}`}
                viewBox="0 0 10 10"
                refX="9"
                refY="5"
                markerWidth="7"
                markerHeight="7"
                markerUnits="userSpaceOnUse"
                orient="auto"
              >
                <path d="M0 0L10 5L0 10z" fill={CIRCUITS[k].color} />
              </marker>
            ))}
          </defs>

          {/* Clean agent zone */}
          {L.region && (
            <g>
              <rect
                x={L.region.x}
                y={L.region.y}
                width={L.region.w}
                height={L.region.h}
                rx={10}
                fill="rgba(255,255,255,0.015)"
                stroke={chart.axis}
                strokeWidth={1}
              />
              <text
                x={L.region.x + L.region.w - 12}
                y={L.region.y + 17}
                textAnchor="end"
                fontSize={L.font.group}
                fill={chart.text.muted}
                className="font-mono"
                letterSpacing="0.06em"
              >
                CLEAN AGENT · NFPA 2001
              </text>
            </g>
          )}

          {L.groups.map((g) => (
            <text
              key={g.text}
              x={g.x}
              y={g.y}
              textAnchor={g.anchor ?? 'start'}
              fontSize={L.font.group}
              fill={chart.text.muted}
              className="font-mono"
              letterSpacing="0.06em"
            >
              {g.text}
            </text>
          ))}

          {/* Circuits */}
          {orderedEdges.map((e) => {
            const c = CIRCUITS[e.type]
            const pts = L.edges[e.id]
            const d = pts.map((p, i) => `${i ? 'L' : 'M'}${p[0]},${p[1]}`).join('')
            const on = activeEdges.has(e.id)
            const pulse = on && seq !== null
            return (
              <g key={e.id} opacity={dimming && !on ? 0.28 : 1} style={{ transition: 'opacity 0.3s' }}>
                <path
                  d={d}
                  fill="none"
                  stroke={c.color}
                  strokeWidth={on ? c.width + 0.75 : c.width}
                  strokeDasharray={c.dash}
                  strokeLinejoin="round"
                  markerEnd={c.arrow ? `url(#${uid}-arr-${e.type})` : undefined}
                />
                {pulse && !reduceMotion && (
                  <motion.path
                    d={d}
                    fill="none"
                    stroke="#F2F5F7"
                    strokeWidth={c.width + 1}
                    strokeLinecap="round"
                    strokeDasharray="3 13"
                    initial={{ strokeDashoffset: 0 }}
                    animate={{ strokeDashoffset: -32 }}
                    transition={{ duration: 0.7, repeat: Infinity, ease: 'linear' }}
                  />
                )}
              </g>
            )
          })}

          {/* Circuit tags */}
          {EDGES.map((e) => {
            const at = L.tags[e.id]
            if (!at) return null
            const c = CIRCUITS[e.type]
            const tw = c.tag.length * 0.6 * L.font.tag + 12
            const th = L.font.tag + 6
            const on = activeEdges.has(e.id)
            return (
              <g key={e.id} opacity={dimming && !on ? 0.4 : 1} style={{ transition: 'opacity 0.3s' }}>
                <rect x={at[0] - tw / 2} y={at[1] - th / 2} width={tw} height={th} rx={th / 2} fill={chart.surface} stroke={c.color} strokeWidth={1} />
                <text
                  x={at[0]}
                  y={at[1]}
                  dy="0.35em"
                  textAnchor="middle"
                  fontSize={L.font.tag}
                  fill={chart.text.secondary}
                  className="font-mono"
                >
                  {c.tag}
                </text>
              </g>
            )
          })}

          {/* Devices */}
          {DEVICES.map((dev) => {
            const b = L.nodes[dev.id]
            const on = activeNodes.has(dev.id)
            const isSel = selected === dev.id
            const hub = dev.id === 'facp'
            const alarmed = seq !== null && on
            const stroke = isSel
              ? chart.text.primary
              : alarmed
                ? '#D08C4F'
                : hovered === dev.id
                  ? 'rgba(255,255,255,0.34)'
                  : 'rgba(255,255,255,0.14)'
            const iconColor = alarmed || isSel ? '#D08C4F' : chart.text.secondary
            return (
              <g
                key={dev.id}
                role="button"
                tabIndex={0}
                aria-pressed={isSel}
                aria-label={`${hub ? 'Fire alarm control panel' : dev.label}, ${dev.group.toLowerCase()}`}
                data-cursor="Select"
                onClick={() => onSelect(dev.id)}
                onKeyDown={onKey(dev.id)}
                onFocus={(e) => e.currentTarget.matches(':focus-visible') && setFocused(dev.id)}
                onBlur={() => setFocused(null)}
                onPointerEnter={() => setHovered(dev.id)}
                onPointerLeave={() => setHovered(null)}
                className="cursor-pointer outline-none"
                opacity={dimming && !on ? 0.55 : 1}
                style={{ transition: 'opacity 0.3s' }}
              >
                {alarmed && !reduceMotion && (
                  <motion.rect
                    x={b.x}
                    y={b.y}
                    width={b.w}
                    height={b.h}
                    rx={10}
                    fill="none"
                    stroke="#D08C4F"
                    strokeWidth={1.5}
                    initial={{ opacity: 0.7, scale: 1 }}
                    animate={{ opacity: 0, scale: 1.08 }}
                    transition={{ duration: 1.4, repeat: Infinity, ease: 'easeOut' }}
                    style={{ transformBox: 'fill-box', transformOrigin: 'center' }}
                  />
                )}
                {focused === dev.id && (
                  <rect x={b.x - 4} y={b.y - 4} width={b.w + 8} height={b.h + 8} rx={13} fill="none" stroke={chart.copper} strokeWidth={2} />
                )}
                <rect
                  x={b.x}
                  y={b.y}
                  width={b.w}
                  height={b.h}
                  rx={10}
                  fill={alarmed || isSel ? '#221E1A' : '#1B2327'}
                  stroke={stroke}
                  strokeWidth={isSel || alarmed ? 1.5 : 1}
                  style={{ transition: 'stroke 0.2s, fill 0.2s' }}
                />
                {hub ? (
                  <HubContent b={b} L={L} color={iconColor} inAlarm={inAlarm} />
                ) : (
                  <>
                    <Icon
                      name={dev.icon}
                      x={b.x + (L.name === 'wide' ? 14 : 10)}
                      y={b.y + (b.h - L.icon) / 2}
                      size={L.icon}
                      color={iconColor}
                    />
                    <text
                      x={b.x + (L.name === 'wide' ? 14 : 10) + L.icon + (L.name === 'wide' ? 12 : 8)}
                      y={b.y + b.h / 2 - 3}
                      fontSize={L.font.label}
                      fontWeight={500}
                      fill={chart.text.primary}
                      className="font-sans"
                    >
                      {dev.label}
                    </text>
                    <text
                      x={b.x + (L.name === 'wide' ? 14 : 10) + L.icon + (L.name === 'wide' ? 12 : 8)}
                      y={b.y + b.h / 2 + L.font.sub + 1}
                      fontSize={L.font.sub}
                      fill={chart.text.muted}
                      className="font-sans"
                    >
                      {dev.sub}
                    </text>
                  </>
                )}
              </g>
            )
          })}
        </svg>
      )}
    </div>
  )
}

function HubContent({ b, L, color, inAlarm }: { b: Box; L: Layout; color: string; inAlarm: boolean }) {
  const status = (
    <>
      <circle r={3.5} fill={inAlarm ? '#E5735F' : chart.verdigris} />
      <text x={9} dy="0.35em" fontSize={L.font.tag} fill={chart.text.secondary} className="font-mono" letterSpacing="0.06em">
        {inAlarm ? 'FIRE ALARM' : 'SYSTEM NORMAL'}
      </text>
    </>
  )
  if (L.name === 'wide') {
    const cx = b.x + b.w / 2
    return (
      <>
        <Icon name="facp" x={cx - 14} y={b.y + 16} size={28} color={color} />
        <text x={cx} y={b.y + 74} textAnchor="middle" fontSize={L.font.hub} fontWeight={600} fill={chart.text.primary} className="font-sans" letterSpacing="-0.01em">
          FACP
        </text>
        <text x={cx} y={b.y + 94} textAnchor="middle" fontSize={L.font.sub} fill={chart.text.muted} className="font-sans">
          Fire alarm control panel
        </text>
        <g transform={`translate(${cx - (inAlarm ? 42 : 52)},${b.y + 118})`}>{status}</g>
      </>
    )
  }
  const tx = b.x + 50
  return (
    <>
      <Icon name="facp" x={b.x + 14} y={b.y + 18} size={24} color={color} />
      <text x={tx} y={b.y + 28} fontSize={L.font.hub} fontWeight={600} fill={chart.text.primary} className="font-sans">
        FACP
      </text>
      <text x={tx} y={b.y + 47} fontSize={L.font.sub} fill={chart.text.muted} className="font-sans">
        Fire alarm control panel
      </text>
      <g transform={`translate(${tx + 3},${b.y + 67})`}>{status}</g>
    </>
  )
}

/* ─── Line icons (24px grid, 1.5px stroke) ────────────────────────────────── */

const ICON_PATHS: Record<IconName, ReactNode> = {
  smoke: (
    <>
      <circle cx="12" cy="12" r="8.5" />
      <circle cx="12" cy="12" r="3" />
      <path d="M12 3.5v2.5M12 18v2.5M3.5 12H6M18 12h2.5" />
    </>
  ),
  pull: (
    <>
      <rect x="5" y="2.5" width="14" height="19" rx="2" />
      <path d="M8.5 8h7M12 8v6.5M9.5 17.5h5" />
    </>
  ),
  riser: (
    <>
      <path d="M12 2.5v4M12 17.5v4M8 21.5h8M7.5 2.5h9" />
      <path d="M7 9l10 6V9L7 15z" />
    </>
  ),
  facp: (
    <>
      <rect x="4" y="2.5" width="16" height="19" rx="1.5" />
      <rect x="7" y="6" width="10" height="5" rx="0.5" />
      <path d="M7.5 15h1M11.5 15h1M15.5 15h1M7 18.5h10" />
    </>
  ),
  horn: (
    <>
      <path d="M3.5 9.5v5h3.5l5 4v-13l-5 4z" />
      <path d="M15.5 9a4 4 0 010 6M18 6.5a7.5 7.5 0 010 11" />
    </>
  ),
  annunciator: (
    <>
      <rect x="3" y="3.5" width="18" height="13" rx="1.5" />
      <path d="M8 21h8M12 16.5V21M6.5 8h5M6.5 11.5h8" />
      <circle cx="17" cy="8" r="1.3" />
    </>
  ),
  communicator: (
    <>
      <path d="M12 12v9.5M9 21.5h6" />
      <circle cx="12" cy="10" r="1.5" />
      <path d="M8.5 6.5a5 5 0 000 7M15.5 6.5a5 5 0 010 7M5.8 3.8a8.8 8.8 0 000 12.4M18.2 3.8a8.8 8.8 0 010 12.4" />
    </>
  ),
  central: (
    <>
      <path d="M3.5 21.5V9l8.5-5.5L20.5 9v12.5z" />
      <path d="M9.5 21.5v-5h5v5M8 11.5h2M14 11.5h2" />
    </>
  ),
  releasing: (
    <>
      <rect x="4" y="2.5" width="16" height="19" rx="2" />
      <path d="M13 6.5l-3.5 5.5h5l-3.5 5.5" />
    </>
  ),
  cylinders: (
    <>
      <rect x="4.5" y="7" width="6.5" height="14.5" rx="3.25" />
      <rect x="13" y="7" width="6.5" height="14.5" rx="3.25" />
      <path d="M7.75 7V3.5M16.25 7V3.5M6 3.5h3.5M14.5 3.5H18" />
    </>
  ),
}

function Icon({ name, x, y, size, color }: { name: IconName; x: number; y: number; size: number; color: string }) {
  return (
    <svg
      x={x}
      y={y}
      width={size}
      height={size}
      viewBox="0 0 24 24"
      fill="none"
      stroke={color}
      strokeWidth={1.5}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      style={{ transition: 'stroke 0.2s' }}
    >
      {ICON_PATHS[name]}
    </svg>
  )
}

/* ─── Detail + sequence panels ────────────────────────────────────────────── */

function DetailPanel({ selected, onSelect }: { selected: string | null; onSelect: (id: string | null) => void }) {
  const dev = selected ? deviceById(selected) : null
  return (
    <div className="rounded-xl border border-white/[0.08] bg-surface-raised p-5 md:p-6" aria-live="polite">
      {dev ? (
        <motion.div key={dev.id} initial={{ opacity: 0, y: 6 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.25 }}>
          <div className="flex items-start justify-between gap-4">
            <div>
              <span className="font-mono text-[11px] tracking-widest uppercase text-copper-light">{dev.group}</span>
              <h4 className="mt-1.5 font-sans text-lg font-semibold tracking-tight text-white">
                {dev.id === 'facp' ? 'Fire alarm control panel' : dev.label}
              </h4>
            </div>
            <button
              type="button"
              onClick={() => onSelect(null)}
              className="shrink-0 rounded-md px-2 py-1 text-xs text-muted hover:text-white border border-white/[0.08] hover:border-white/20 transition-colors"
            >
              Clear
            </button>
          </div>
          <p className="mt-3 text-sm leading-relaxed text-titanium">{dev.description}</p>
          <h5 className="mt-5 font-mono text-[11px] tracking-widest uppercase text-muted">Connections</h5>
          <ul className="mt-2 space-y-1.5">
            {EDGES.filter((e) => e.from === dev.id || e.to === dev.id).map((e) => {
              const other = deviceById(e.from === dev.id ? e.to : e.from)
              const c = CIRCUITS[e.type]
              return (
                <li key={e.id} className="flex items-center gap-2.5 text-sm">
                  <svg width="22" height="8" aria-hidden="true" className="shrink-0">
                    <line x1="1" y1="4" x2="21" y2="4" stroke={c.color} strokeWidth={Math.min(c.width, 2.5)} strokeDasharray={c.dash} />
                  </svg>
                  <span className="text-white">{other.id === 'facp' ? 'FACP' : other.label}</span>
                  <span className="text-muted">· {c.label}</span>
                </li>
              )
            })}
          </ul>
        </motion.div>
      ) : (
        <div>
          <span className="font-mono text-[11px] tracking-widest uppercase text-muted">Device detail</span>
          <p className="mt-2 text-sm leading-relaxed text-titanium">
            Select a device in the diagram — or pick one here — to see what it does and how it connects.
          </p>
          <div className="mt-4 flex flex-wrap gap-2">
            {DEVICES.map((d) => (
              <button
                key={d.id}
                type="button"
                onClick={() => onSelect(d.id)}
                className="rounded-md border border-white/[0.08] px-2.5 py-1 text-xs text-titanium hover:text-white hover:border-white/25 transition-colors"
              >
                {d.id === 'facp' ? 'FACP' : d.label}
              </button>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}

function SequencePanel({
  step,
  playing,
  reduceMotion,
  onStep,
}: {
  step: number | null
  playing: boolean
  reduceMotion: boolean
  onStep: (i: number) => void
}) {
  return (
    <div className="rounded-xl border border-white/[0.08] bg-surface-raised p-5 md:p-6">
      <div className="flex items-baseline justify-between gap-4">
        <span className="font-mono text-[11px] tracking-widest uppercase text-muted">Alarm sequence</span>
        {step !== null && (
          <span className="font-mono text-[11px] text-muted tabular-nums" aria-live="polite">
            Step {step + 1} of {STEPS.length}
            {playing ? '' : ' · paused'}
          </span>
        )}
      </div>
      <ol className="mt-3 space-y-1">
        {STEPS.map((s, i) => {
          const current = step === i
          const done = step !== null && i < step
          return (
            <li key={s.title}>
              <button
                type="button"
                onClick={() => onStep(i)}
                aria-current={current ? 'step' : undefined}
                className={`group w-full text-left rounded-lg px-3 py-2 transition-colors ${
                  current ? 'bg-white/[0.05]' : 'hover:bg-white/[0.03]'
                }`}
              >
                <div className="flex items-start gap-3">
                  <span
                    className={`mt-px flex h-5 w-5 shrink-0 items-center justify-center rounded-full border text-[11px] font-semibold tabular-nums transition-colors ${
                      current
                        ? 'border-copper bg-copper text-slate-950'
                        : done
                          ? 'border-copper/60 text-copper-light'
                          : 'border-white/15 text-muted'
                    }`}
                  >
                    {i + 1}
                  </span>
                  <div className="min-w-0 flex-1">
                    <span className={`block text-sm font-medium ${current || done ? 'text-white' : 'text-titanium'}`}>
                      {s.title}
                    </span>
                    {current && (
                      <motion.p
                        initial={reduceMotion ? false : { opacity: 0, height: 0 }}
                        animate={{ opacity: 1, height: 'auto' }}
                        transition={{ duration: 0.25 }}
                        className="mt-1 text-sm leading-relaxed text-titanium"
                      >
                        {s.body}
                      </motion.p>
                    )}
                    {current && playing && !reduceMotion && i < STEPS.length - 1 && (
                      <div className="mt-2 h-0.5 w-full overflow-hidden rounded-full bg-white/[0.08]">
                        <motion.div
                          key={i}
                          className="h-full bg-copper"
                          initial={{ width: '0%' }}
                          animate={{ width: '100%' }}
                          transition={{ duration: STEP_MS / 1000, ease: 'linear' }}
                        />
                      </div>
                    )}
                  </div>
                </div>
              </button>
            </li>
          )
        })}
      </ol>
      {step === null && (
        <p className="mt-3 px-3 text-xs text-muted">Press “Run alarm sequence” to animate each step, or select a step to jump to it.</p>
      )}
    </div>
  )
}
