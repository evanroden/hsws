/* ─────────────────────────────────────────────────────────────────────────────
 * Saltwater wedge — an ILLUSTRATIVE steady-state model, not a forecast.
 *
 * Every fixed number below is a published fact (sources at the bottom); the
 * only modelled pieces are (1) the straight-line toe-vs-flow fit between two
 * Corps reference points, (2) the sill's overtopping flows, and (3) the
 * schematic channel-bed profile. Those three are labelled on the page.
 *
 * Coordinates: x = river miles above Head of Passes (AHP, RM 0 at Head of
 * Passes; negative = below it, down Southwest Pass). y = feet below surface.
 * ────────────────────────────────────────────────────────────────────────── */

export const FLOW_MIN = 100_000
export const FLOW_MAX = 600_000
export const FLOW_STEP = 1_000

/** Below ~300,000 cfs salt moves upriver; at or above it the river pushes the
 *  wedge back out — "the magic number", per a Corps spokesperson [csm; also
 *  wj0929]. The fit puts the toe at Head of Passes at this flow. */
export const THRESHOLD_FLOW = 300_000

/** Calibration point: with only the original −55 ft sill, USACE models put the
 *  worst-case 2023 toe at RM 103.7 [dvids]. That projection was made for the
 *  fall 2023 low flow, forecast to bottom out near 130,000 cfs [wwno0919]. */
const REF_FLOW = 130_000
const REF_TOE = 103.7

const MILES_PER_CFS = REF_TOE / (THRESHOLD_FLOW - REF_FLOW) // ≈ 0.61 mi per 1,000 cfs

/** The model's flow floor for "ever observed": 1988's record low was about
 *  120,000 cfs, when the wedge reached Kenner, upstream of New Orleans
 *  [wwno0919]. The line gives ~RM 110 there — inside the Harahan–Kenner reach
 *  (Harahan RM 108.9; port limit incl. Kenner RM 115 [noaa]). Below it the
 *  line is extrapolation. */
export const RECORD_LOW_FLOW = 120_000

/** Toe → "salt at the surface" distance. USACE reported the 2023 toe at RM 69.4
 *  with surface water inundated at RM 54.4, and projected a toe of RM 103.7
 *  with surface inundation at RM 88.7 — 15 miles both times [dvids]. */
export const WEDGE_LENGTH = 15

/** Emergency sill site — "River Mile 64, near Myrtle Grove", used in 1988,
 *  1999, 2012, 2022 and 2023 [gohsep]. */
export const SILL_RM = 64

/** Bed at the sill site: the July 2023 sill crest sat at −55 ft [dvids] and
 *  raised the bed ~35 ft [enr2022, csm], so the natural bed there is ~90 ft. */
export const SILL_BED_FT = 90

/** The sill is ordered as the toe nears Myrtle Grove: at the close of June 2023
 *  the toe was at RM 54.4 [wj0710]; the sill was announced July 5 and built in
 *  July [wj0929]. The model builds it once the toe would pass this mile. */
const SILL_BUILD_TOE = 54.4

export type SillCrest = 55 | 30

/** Flow below which each crest height is overtopped (model assumption,
 *  calibrated to 2023):
 *  −55 ft: overtopped around Sept 20, 2023 [wj0929, enr2023] as flow sat near
 *          140,000–148,000 cfs [enr2023, nbc] → 150,000 cfs.
 *  −30 ft: raised from Sept 24 [wj0929]; held the toe at or below RM 69.4
 *          through fall flows of ~130,000–150,000 cfs [dvids, wwnolive] → so
 *          the model lets it fail only below that range, at 125,000 cfs. */
export const OVERTOP_FLOW: Record<SillCrest, number> = { 55: 150_000, 30: 125_000 }

/** Intakes this close upstream of the toe are flagged "watch" (display rule). */
export const WATCH_MILES = 10

/** Horizontal extent of the profile: past the New Orleans port limit to a few
 *  miles down Southwest Pass. */
export const RM_UPSTREAM = 115
export const RM_DOWNSTREAM = -8

/* ─── Model ──────────────────────────────────────────────────────────────── */

/** Steady-state toe with no sill in the way (RM; negative = below HOP). */
export function unobstructedToe(flow: number) {
  return (THRESHOLD_FLOW - flow) * MILES_PER_CFS
}

export type SillState = 'none' | 'approaching' | 'holding' | 'overtopped'

export interface WedgeState {
  flow: number
  crest: SillCrest
  /** Where the toe would settle without a sill */
  free: number
  /** Where the toe settles with the sill (if built) */
  toe: number
  /** Downstream of this mile the salt reaches the surface */
  surface: number
  sill: SillState
}

export function solveWedge(flow: number, crest: SillCrest): WedgeState {
  const free = unobstructedToe(flow)
  let sill: SillState = 'none'
  let toe = free
  if (free >= SILL_BUILD_TOE) {
    if (free < SILL_RM) sill = 'approaching'
    else if (flow >= OVERTOP_FLOW[crest]) {
      sill = 'holding'
      toe = SILL_RM
    } else sill = 'overtopped'
  }
  return { flow, crest, free, toe, surface: toe - WEDGE_LENGTH, sill }
}

export type IntakeStatus = 'salty' | 'below' | 'watch' | 'clear'

export function intakeStatus(rm: number, s: Pick<WedgeState, 'toe' | 'surface'>): IntakeStatus {
  if (rm <= s.surface) return 'salty'
  if (rm <= s.toe) return 'below'
  if (rm <= s.toe + WATCH_MILES) return 'watch'
  return 'clear'
}

export const STATUS_TEXT: Record<IntakeStatus, { label: string; detail: string }> = {
  salty: { label: 'Salty at intake', detail: 'Salt reaches the surface here' },
  below: { label: 'Salt on the bed below', detail: 'Wedge underneath; surface still fresh' },
  watch: { label: 'Watch', detail: `Toe within ${WATCH_MILES} miles downstream` },
  clear: { label: 'Clear', detail: 'Wedge well downstream' },
}

export const SILL_TEXT: Record<SillState, string> = {
  none: 'Not needed: the wedge is well below Myrtle Grove',
  approaching: 'Built: the wedge is approaching from downstream',
  holding: 'Holding: the toe is stopped at the sill',
  overtopped: 'Overtopped: salt is spilling past the crest',
}

/* ─── Landmarks ──────────────────────────────────────────────────────────── */

export type LandmarkKind = 'reference' | 'intake' | 'sill'

export interface Landmark {
  id: string
  name: string
  /** Label used on narrow screens */
  short: string
  rm: number
  kind: LandmarkKind
  /** 1 = always drawn; 2 = drawn when there is room (always in the list) */
  priority: 1 | 2
  note: string
  source: SourceId
}

export const LANDMARKS: Landmark[] = [
  {
    id: 'hop',
    name: 'Head of Passes',
    short: 'Head of Passes',
    rm: 0,
    kind: 'reference',
    priority: 1,
    note: 'The river splits into its passes here; river miles are counted upstream from this point.',
    source: 'noaa',
  },
  {
    id: 'pointe',
    name: 'Pointe à la Hache',
    short: 'Pte. à la Hache',
    rm: 49,
    kind: 'intake',
    priority: 1,
    note: 'Plaquemines Parish water plant. Salt reached it in fall 2023; the Corps barged in fresh water to blend.',
    source: 'noaa',
  },
  {
    id: 'sill',
    name: '2023 sill',
    short: 'Sill',
    rm: SILL_RM,
    kind: 'sill',
    priority: 1,
    note: 'Emergency underwater sill near Myrtle Grove: built to −55 ft in July 2023, raised to −30 ft from Sept 24 with a 620-ft navigation notch left at −55 ft.',
    source: 'dvids',
  },
  {
    id: 'belle',
    name: 'Belle Chasse',
    short: 'Belle Chasse',
    rm: 75.5,
    kind: 'intake',
    priority: 1,
    note: 'Plaquemines Parish intake, the next plant upriver when the 2023 toe peaked at RM 69.4.',
    source: 'dvids',
  },
  {
    id: 'dalcour',
    name: 'Dalcour',
    short: 'Dalcour',
    rm: 80.9,
    kind: 'intake',
    priority: 2,
    note: 'Plaquemines Parish east-bank intake.',
    source: 'fox8',
  },
  {
    id: 'stbernard',
    name: 'St. Bernard',
    short: 'St. Bernard',
    rm: 88,
    kind: 'intake',
    priority: 2,
    note: 'St. Bernard Parish intake.',
    source: 'fox8',
  },
  {
    id: 'algiers',
    name: 'Algiers',
    short: 'Algiers',
    rm: 95.7,
    kind: 'intake',
    priority: 1,
    note: 'New Orleans Sewerage & Water Board plant serving the West Bank.',
    source: 'fox8',
  },
  {
    id: 'carrollton',
    name: 'Carrollton · New Orleans',
    short: 'Carrollton',
    rm: 104.7,
    kind: 'intake',
    priority: 1,
    note: 'New Orleans’ main treatment plant (East Bank).',
    source: 'dvids',
  },
]

export const INTAKES = LANDMARKS.filter((l) => l.kind === 'intake')

/** Plain-language position of a toe relative to the landmarks. */
export function describeToe(toe: number): string {
  if (toe < -1.2) return 'Pushed out past Head of Passes into the Gulf'
  if (toe > RM_UPSTREAM) return 'Upstream of New Orleans, past this chart'
  const sorted = [...LANDMARKS].sort((a, b) => a.rm - b.rm)
  const at = sorted.find((l) => Math.abs(l.rm - toe) < 1.2)
  if (at) return at.kind === 'sill' ? 'At the sill, near Myrtle Grove' : `At ${at.name}`
  const down = [...sorted].reverse().find((l) => l.rm < toe)
  const up = sorted.find((l) => l.rm > toe)
  if (down && up) return `Between ${down.kind === 'sill' ? 'the sill' : down.name} and ${up.name}`
  return down ? `Upstream of ${down.name}` : `Below ${up?.name}`
}

/* ─── Schematic channel bed ──────────────────────────────────────────────── */

/** Schematic thalweg (ft below surface). Shape only — NOT a survey. Keeps the
 *  published facts: charted depths of 31–194 ft between Head of Passes and New
 *  Orleans, deepest in the bends [noaa]; ~90 ft at the sill site (above); the
 *  50-ft project channel through Southwest Pass [noaa]. */
const BED: [number, number][] = [
  [118, 96],
  [112, 104],
  [106, 138],
  [102, 112],
  [98.5, 104],
  [95, 150],
  [91.5, 108],
  [87, 92],
  [83.5, 98],
  [80.5, 124],
  [77, 96],
  [71, 88],
  [SILL_RM, SILL_BED_FT],
  [58, 84],
  [52, 94],
  [47, 104],
  [42, 86],
  [34, 92],
  [26, 80],
  [18, 76],
  [11, 70],
  [5, 64],
  [0, 58],
  [-4, 54],
  [-10, 50],
]

// Monotone cubic (Fritsch–Carlson) through the control points — no overshoot.
const BX = BED.map((p) => -p[0]) // ascending x for interpolation
const BY = BED.map((p) => p[1])
const BM = (() => {
  const n = BX.length
  const d = BX.slice(0, -1).map((_, i) => (BY[i + 1] - BY[i]) / (BX[i + 1] - BX[i]))
  const m = new Array<number>(n)
  m[0] = d[0]
  m[n - 1] = d[n - 2]
  for (let i = 1; i < n - 1; i++) m[i] = d[i - 1] * d[i] <= 0 ? 0 : (d[i - 1] + d[i]) / 2
  for (let i = 0; i < n - 1; i++) {
    if (d[i] === 0) {
      m[i] = 0
      m[i + 1] = 0
      continue
    }
    const a = m[i] / d[i]
    const b = m[i + 1] / d[i]
    const s = a * a + b * b
    if (s > 9) {
      const t = 3 / Math.sqrt(s)
      m[i] = t * a * d[i]
      m[i + 1] = t * b * d[i]
    }
  }
  return m
})()

export function bedDepth(rm: number): number {
  const x = -rm
  if (x <= BX[0]) return BY[0]
  if (x >= BX[BX.length - 1]) return BY[BY.length - 1]
  let i = 0
  while (x > BX[i + 1]) i++
  const h = BX[i + 1] - BX[i]
  const t = (x - BX[i]) / h
  const t2 = t * t
  const t3 = t2 * t
  return (
    (2 * t3 - 3 * t2 + 1) * BY[i] +
    (t3 - 2 * t2 + t) * h * BM[i] +
    (-2 * t3 + 3 * t2) * BY[i + 1] +
    (t3 - t2) * h * BM[i + 1]
  )
}

/** Fraction of the local column that is salt, d = miles downstream of the toe.
 *  Smoothstep: a thin tongue at the toe thickening to full depth after
 *  WEDGE_LENGTH miles. Shape illustrative. */
export function saltFraction(d: number) {
  if (d <= 0) return 0
  if (d >= WEDGE_LENGTH) return 1
  const t = d / WEDGE_LENGTH
  return t * t * (3 - 2 * t)
}

/* ─── Sources ────────────────────────────────────────────────────────────── */

export type SourceId =
  | 'dvids'
  | 'fox8'
  | 'noaa'
  | 'gohsep'
  | 'wj0929'
  | 'wj0710'
  | 'wwno0919'
  | 'wwnolive'
  | 'nbc'
  | 'enr2023'
  | 'enr2022'
  | 'csm'
  | 'nps'

export const SOURCES: Record<SourceId, { label: string; url: string }> = {
  // Belle Chasse RM 75.5, Carrollton RM 104.7; sill −55 → −30 ft with 620-ft
  // notch at −55 ft; 2023 toe max RM 69.4 / surface RM 54.4; projected
  // RM 103.7 / 88.7 with the original sill.
  dvids: {
    label: 'USACE New Orleans District (DVIDS), Sept. 24, 2024',
    url: 'https://www.dvidshub.net/news/481623',
  },
  // Corps intake table from the Oct. 5, 2023 briefing: Belle Chasse 75.5,
  // Dalcour 80.9, St. Bernard 88.0, Algiers 95.7, Carrollton 104.7.
  fox8: {
    label: 'Corps intake table via FOX 8 WVUE, Oct. 5, 2023',
    url: 'https://www.fox8live.com/2023/10/05/significant-adjustment-saltwater-wedge-timeline-delays-some-impacts-by-nearly-month-if-all/',
  },
  // Head of Passes = mile 0.0 (AHP origin); Pointe a la Hache mile 49; Belle
  // Chasse 75.5; Harahan 108.9; port upper limit ~115 (incl. Kenner); depths
  // 31–194 ft from Head of Passes to New Orleans; 50-ft project channel.
  noaa: {
    label: 'NOAA U.S. Coast Pilot 5, ch. 8',
    url: 'https://nauticalcharts.noaa.gov/publications/coast-pilot/files/cp5/CPB5_C08_WEB.pdf',
  },
  // Sills at "River Mile 64, near Myrtle Grove" in 1988, 1999, 2012, 2022, 2023.
  gohsep: {
    label: 'USACE release via GOHSEP, Aug. 29, 2024',
    url: 'https://gohsep.la.gov/about/news/usace-to-construct-underwater-sill-to-arrest-saltwater-progression-into-mississippi-river/',
  },
  // 300,000 cfs threshold; July sill near Mile 64 to −55 ft; overtopped around
  // Sept 20; raised to −30 ft from Sept 24; toe RM 69.4 as of Sept 27.
  wj0929: {
    label: 'Waterways Journal, Sept. 29, 2023',
    url: 'https://www.waterwaysjournal.net/2023/09/29/corps-augments-underwater-sill-to-slow-salt-water-intrusion/',
  },
  // Toe at RM 54.4 at the close of June 2023, before the sill was built.
  wj0710: {
    label: 'Waterways Journal, July 10, 2023',
    url: 'https://www.waterwaysjournal.net/2023/07/10/corps-to-build-sill-to-block-salt-water-wedge/',
  },
  // Flow forecast to fall to 130,000 cfs by mid-October; 1988 low of 120,000
  // cfs let the wedge reach Kenner.
  wwno0919: {
    label: 'WWNO, Sept. 19, 2023',
    url: 'https://www.wwno.org/coastal-desk/2023-09-19/saltwater-threatens-south-louisiana-drinking-water-second-year-in-a-row-amid-severe-drought',
  },
  // Toe RM 63.9 on Oct 9; flows "hovering around 150,000 cfs" on Oct 12.
  wwnolive: {
    label: 'WWNO saltwater wedge live updates, fall 2023',
    url: 'https://www.wwno.org/live-blog/saltwater-wedge-updates',
  },
  // "the flow stood at 148,000 cubic feet per second" the week of Sept 18.
  nbc: {
    label: 'NBC News, Sept. 26, 2023',
    url: 'https://www.nbcnews.com/science/environment/new-orleans-braces-drinking-water-emergency-drought-stricken-mississip-rcna117218',
  },
  // Sill overtopped Sept 20; flow 140,000 cfs.
  enr2023: {
    label: 'ENR, Sept. 2023',
    url: 'https://enr.com/articles/57198-corps-raises-emergency-sill-to-halt-salt-wedge-threatening-new-orleans-water-supply',
  },
  // 2022 sill: a "35-ft sill" at the same site.
  enr2022: {
    label: 'ENR, Oct. 31, 2022',
    url: 'https://enr.com/articles/55206-underwater-levee-halts-salt-wedge-intrusion-on-lower-mississippi-river',
  },
  // July 2023 sill at RM 64 "brought the bottom of the river up nearly 35
  // feet"; Corps spokesperson: 300,000 cfs is "the magic number".
  csm: {
    label: 'Christian Science Monitor, Oct. 23, 2023',
    url: 'https://www.csmonitor.com/Environment/2023/1023/Saltwater-influx-tests-communities-near-Mississippi-s-mouth',
  },
  // "At New Orleans, the average flow rate is 600,000 cubic feet per second."
  nps: {
    label: 'National Park Service, Mississippi River facts',
    url: 'https://www.nps.gov/miss/riverfacts.htm',
  },
}
