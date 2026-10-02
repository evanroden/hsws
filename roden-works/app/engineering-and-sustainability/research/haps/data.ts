import { chart } from '@/components/charts/tokens'

/* ─── Pollutants ─────────────────────────────────────────────────────────────
 * Identity follows the validated categorical order (copper → verdigris → steel).
 * Particles are drawn as filled dots and the gas (NO₂) as a ring, so identity
 * never rests on hue alone.
 */
export type PollutantId = 'pm25' | 'bc' | 'no2'

export interface Pollutant {
  id: PollutantId
  label: string
  color: string
  /** Drawn as a filled dot (particles) or a ring (gas) */
  kind: 'particles' | 'gas'
  description: string
}

export const POLLUTANT_ORDER: PollutantId[] = ['pm25', 'bc', 'no2']

export const POLLUTANTS: Record<PollutantId, Pollutant> = {
  pm25: {
    id: 'pm25',
    label: 'PM2.5',
    color: chart.copper,
    kind: 'particles',
    description:
      'Fine particulate matter smaller than 2.5 micrometers in diameter. The particles reach deep into the lung alveoli and can enter the bloodstream, causing inflammation throughout the body. Indoor sources include cooking, candles, incense, and tobacco smoke.',
  },
  bc: {
    id: 'bc',
    label: 'Black carbon',
    color: chart.verdigris,
    kind: 'particles',
    description:
      'A component of soot from incomplete combustion of fossil fuels, biomass, and cooking fuels. In the study, participants in the highest quartile of black carbon exposure had systolic blood pressure about 2 mmHg higher.',
  },
  no2: {
    id: 'no2',
    label: 'NO₂',
    color: chart.steel,
    kind: 'gas',
    description:
      'Indoors, nitrogen dioxide comes mostly from gas stoves and space heaters. NO₂ irritates the airways, worsens asthma, and contributes to chronic respiratory disease. Poorly ventilated homes are hit hardest.',
  },
}

/* ─── WHO global air quality guidelines (2021) ──────────────────────────────
 * WHO global air quality guidelines: particulate matter (PM2.5 and PM10), ozone,
 * nitrogen dioxide, sulfur dioxide and carbon monoxide. Geneva: WHO; 2021.
 * Table 0.1 (p. xvii), AQG levels in µg/m³; 24-hour levels are 99th percentiles
 * (3–4 exceedance days per year). "The present guidelines are applicable to both
 * outdoor and indoor environments globally" (p. xx). Black carbon / elemental
 * carbon gets good-practice statements only — "the available information is
 * insufficient to derive AQG levels" (p. xvi).
 * https://www.who.int/publications/i/item/9789240034228
 */
export const WHO_AQG: Record<PollutantId, { annual: number; day: number } | null> = {
  pm25: { annual: 5, day: 15 },
  bc: null,
  no2: { annual: 10, day: 25 },
}

/* ─── Diagram geometry ───────────────────────────────────────────────────────
 * Base drawing units for a longitudinal section of a shotgun house, street on
 * the left. `anchor` is the visual centre of each element (hotspot ring);
 * `badge` is where its numbered marker sits in the compact (mobile) drawing.
 */
export interface Pt {
  x: number
  y: number
}

export interface Source {
  id: string
  n: number
  name: string
  location: string
  emits: PollutantId[]
  detail: string
  anchor: Pt
  badge: Pt
  /** Emission origin and drift vector for the particle plume */
  plume: { x: number; y: number; dx: number; dy: number }
}

// Source → pollutant pairings follow the pollutant descriptions above.
export const SOURCES: Source[] = [
  {
    id: 'traffic',
    n: 1,
    name: 'Traffic exhaust',
    location: 'Street',
    emits: ['bc', 'no2'],
    detail:
      'Tailpipe exhaust drifts in through doors, windows and gaps in the walls, carrying black carbon and NO₂ indoors.',
    anchor: { x: 186, y: 426 },
    badge: { x: 196, y: 368 },
    plume: { x: 198, y: 433, dx: 34, dy: -16 },
  },
  {
    id: 'heater',
    n: 2,
    name: 'Gas space heater',
    location: 'Front room',
    emits: ['no2', 'pm25'],
    detail:
      'Burning gas releases NO₂ and fine particles straight into the room, a bigger problem in poorly ventilated homes.',
    anchor: { x: 368, y: 388 },
    badge: { x: 368, y: 290 },
    plume: { x: 368, y: 364, dx: 0, dy: -52 },
  },
  {
    id: 'tobacco',
    n: 3,
    name: 'Tobacco smoke',
    location: 'Front room',
    emits: ['pm25', 'bc'],
    detail:
      'Smoking is a significant indoor source of fine particles, and as incomplete combustion it adds black carbon too.',
    anchor: { x: 445, y: 364 },
    badge: { x: 440, y: 300 },
    plume: { x: 453, y: 358, dx: 8, dy: -56 },
  },
  {
    id: 'candles',
    n: 4,
    name: 'Candles & incense',
    location: 'Middle room',
    emits: ['pm25'],
    detail: 'Burning candles and incense adds fine particles to the room air.',
    anchor: { x: 620, y: 334 },
    badge: { x: 620, y: 262 },
    plume: { x: 622, y: 320, dx: 0, dy: -54 },
  },
  {
    id: 'stove',
    n: 5,
    name: 'Gas stove & cooking',
    location: 'Kitchen',
    emits: ['no2', 'pm25'],
    detail:
      'The gas flame emits NO₂ and fine particles; frying, searing and other cooking add more PM2.5.',
    anchor: { x: 918, y: 339 },
    badge: { x: 918, y: 276 },
    plume: { x: 916, y: 328, dx: 0, dy: -58 },
  },
]

export interface Monitor {
  id: string
  key: 'A' | 'B' | 'C' | 'D'
  name: string
  /** Label used inside the drawing */
  short: string
  measures: string
  pollutant: PollutantId | null
  placement: string
  detail: string
  anchor: Pt
  badge: Pt
}

// Instruments as listed in the page's methodology section. Positions in the
// drawing are schematic (the page states monitors were deployed in homes).
export const MONITORS: Monitor[] = [
  {
    id: 'pdr',
    key: 'A',
    name: 'pDR-1500',
    short: 'pDR-1500',
    measures: 'Real-time PM2.5',
    pollutant: 'pm25',
    placement: 'In home',
    detail: 'Logs fine-particle (PM2.5) concentrations in real time.',
    anchor: { x: 700, y: 336 },
    badge: { x: 688, y: 262 },
  },
  {
    id: 'ae51',
    key: 'B',
    name: 'MicroAeth AE51',
    short: 'MicroAeth AE51',
    measures: 'Black carbon',
    pollutant: 'bc',
    placement: 'In home',
    detail: 'Measures black carbon in the room air.',
    anchor: { x: 737, y: 342 },
    badge: { x: 730, y: 262 },
  },
  {
    id: 'ogawa',
    key: 'C',
    name: 'Ogawa Samplers',
    short: 'Ogawa sampler',
    measures: 'Passive NO₂',
    pollutant: 'no2',
    placement: 'In home',
    detail: 'Collects NO₂ passively over the sampling period.',
    anchor: { x: 760, y: 305 },
    badge: { x: 772, y: 262 },
  },
  {
    id: 'abpm',
    key: 'D',
    name: 'Ambulatory BP Monitor',
    short: 'Ambulatory BP monitor',
    measures: '24-hr blood pressure',
    pollutant: null,
    placement: 'Worn by participant',
    detail: 'Worn by the participant to record blood pressure over 24 hours.',
    anchor: { x: 512, y: 336 },
    badge: { x: 556, y: 290 },
  },
]

export type Filter = 'all' | PollutantId

export const sourceMatches = (s: Source, f: Filter) => f === 'all' || s.emits.includes(f)
export const monitorMatches = (m: Monitor, f: Filter) => f === 'all' || m.pollutant === f

export const sourcesFor = (p: PollutantId) => SOURCES.filter((s) => s.emits.includes(p))
export const monitorFor = (p: PollutantId) => MONITORS.find((m) => m.pollutant === p)
