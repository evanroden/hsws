import { chart } from '@/components/charts/tokens'

/* ─── Loops ──────────────────────────────────────────────────────────────────
 * The three validated chart hues carry the three water loops; gas and power
 * add two hues. Every loop is also identified by text (pipe labels, legend,
 * panel chips) and by line style (supply solid, return dashed), never by
 * color alone. All six colors are ≥ 3:1 against the #151C1F surface.
 */
export type LoopId = 'steam' | 'chw' | 'cw' | 'gas' | 'power' | 'controls'

export interface LoopDef {
  id: LoopId
  label: string
  /** Legend wording for the solid / dashed variants */
  supply: string
  return?: string
  color: string
  /** Lighter tint used for the moving flow particles */
  tint: string
}

export const LOOPS: Record<LoopId, LoopDef> = {
  steam: { id: 'steam', label: 'Steam & condensate', supply: 'Steam', return: 'condensate', color: chart.copper, tint: '#E2B994' },
  chw: { id: 'chw', label: 'Chilled water', supply: 'Chilled water supply', return: 'return', color: chart.steel, tint: '#B4C5EE' },
  cw: { id: 'cw', label: 'Condenser water', supply: 'Condenser water supply', return: 'return', color: chart.verdigris, tint: '#97D3C0' },
  gas: { id: 'gas', label: 'Natural gas', supply: 'Natural gas', color: '#A386E0', tint: '#D0C0F0' },
  power: { id: 'power', label: 'Electrical power', supply: 'Electrical power', color: '#D4B43C', tint: '#ECDB98' },
  controls: { id: 'controls', label: 'Controls data', supply: 'Controls (BAS) data', color: '#8A9BA8', tint: '#C3CDD4' },
}

export const LOOP_ORDER: LoopId[] = ['steam', 'chw', 'cw', 'gas', 'power', 'controls']

/* ─── Equipment groups ───────────────────────────────────────────────────────
 * Copy is carried over from the original diagram; ratings are illustrative
 * (the figure is labeled as a representative schematic). "Evan's role" lines
 * map each system to the responsibilities stated in EnfraOverview: overseeing
 * subcontractors, managing maintenance budgets, energy data analysis for
 * optimization, and ensuring continuous operation.
 */
export type GroupId = 'bas' | 'towers' | 'chillers' | 'pumps' | 'boilers' | 'generators' | 'hospital'

export interface GroupDef {
  id: GroupId
  name: string
  short: string
  loops: LoopId[]
  description: string
  params: { label: string; value: string }[]
  note?: string
  role: string
}

export const GROUPS: GroupDef[] = [
  {
    id: 'bas',
    name: 'Controls & automation',
    short: 'Monitors and sequences every system',
    loops: ['controls'],
    description:
      'The building automation system (BAS) head-end aggregates data from field sensors — temperature, pressure, flow, power — and executes optimized control sequences across all mechanical systems. ENFRA Connect® adds real-time dashboards, automated fault detection and diagnostics, and predictive maintenance alerts.',
    params: [
      { label: 'Platform', value: 'ENFRA Connect®' },
      { label: 'Integration', value: 'BACnet / Modbus' },
      { label: 'Monitoring', value: '24/7 remote' },
    ],
    role: 'Energy data analysis for optimization is one of Evan’s core responsibilities — and it runs on the trends this system records.',
  },
  {
    id: 'towers',
    name: 'Cooling towers',
    short: 'Reject the chillers’ heat outdoors',
    loops: ['cw'],
    description:
      'Induced-draft cooling towers reject condenser heat to the atmosphere through evaporative cooling. Hot condenser water cascades over fill media while fans draw ambient air upward, evaporating a small fraction and cooling the remainder. Chemical water treatment controls scale, corrosion, and biological growth in the open loop.',
    params: [
      { label: 'Type', value: '2-cell, induced draft' },
      { label: 'Condenser water', value: '85 °F supply / 95 °F return' },
      { label: 'Heat rejection', value: 'Evaporative' },
    ],
    role: 'Overseeing subcontractors and managing maintenance budgets — keeping heat-rejection equipment serviced so the chillers can run.',
  },
  {
    id: 'chillers',
    name: 'Chillers',
    short: 'Make chilled water for cooling',
    loops: ['chw', 'cw'],
    description:
      'Water-cooled centrifugal chillers produce chilled water for air conditioning, operating-room cooling, MRI suites, pharmaceutical storage, and server rooms. Refrigerant circulates through an evaporator, which cools the chilled water, and a condenser, which rejects that heat to the condenser-water loop. Variable-speed drives optimize part-load efficiency.',
    params: [
      { label: 'Units', value: '2 × 1,200-ton centrifugal' },
      { label: 'Chilled water', value: '42 °F supply / 56 °F return' },
      { label: 'Refrigerant', value: 'R-134a' },
    ],
    role: 'Energy data analysis for optimization — tracking how efficiently the plant turns electricity into chilled water.',
  },
  {
    id: 'pumps',
    name: 'Pumps & distribution',
    short: 'Circulate water through every loop',
    loops: ['chw', 'cw', 'steam'],
    description:
      'Variable-frequency-drive pumps circulate chilled water, condenser water, and condensate through the plant and the underground distribution network. A primary–secondary decoupled loop lets the chillers run at constant flow while building loads vary; differential-pressure sensors at remote buildings modulate pump speed to match real-time demand.',
    params: [
      { label: 'Arrangement', value: 'Primary / secondary, decoupled' },
      { label: 'Drives', value: 'VFD, N+1 redundancy' },
      { label: 'Distribution', value: 'Underground pipe network' },
    ],
    note: 'Tags: CHWP chilled-water pumps · CWP condenser-water pumps · CP condensate pumps',
    role: 'Energy data analysis for optimization — pump speed follows demand, so flow and power trends show where energy can be saved.',
  },
  {
    id: 'boilers',
    name: 'Steam boilers',
    short: 'Make steam for heating and sterilization',
    loops: ['steam', 'gas'],
    description:
      'Dual-fuel fire-tube boilers generate high-pressure steam for heating, sterilization (autoclaves), kitchens, laundry, and humidification. Steam is the lifeblood of hospital operations, so redundant units provide N+1 reliability for life safety.',
    params: [
      { label: 'Units', value: '2 × 600 HP fire-tube' },
      { label: 'Steam', value: '150 psi / 350 °F' },
      { label: 'Fuel', value: 'Natural gas + #2 fuel oil' },
    ],
    role: 'Ensuring continuous operation of the steam the hospital depends on for heating and sterilization.',
  },
  {
    id: 'generators',
    name: 'Emergency generators & ATS',
    short: 'Carry critical loads when the grid fails',
    loops: ['power'],
    description:
      'Diesel generators and an automatic transfer switch (ATS) keep life-safety loads — ICUs, operating rooms, ventilators, and fire alarm systems — powered through a utility outage. When the utility feed fails, the generators start and the ATS transfers critical loads to them within 10 seconds; on-site diesel storage keeps them running through an extended outage.',
    params: [
      { label: 'Units', value: '2 × 2 MW diesel gensets' },
      { label: 'Transfer', value: 'ATS, under 10 seconds' },
      { label: 'Standards', value: 'NEC 700 / NFPA 110 Type 10' },
    ],
    role: 'Ensuring continuous operation — this is the equipment that keeps critical care powered when the grid is not.',
  },
  {
    id: 'hospital',
    name: 'Distribution to the hospital',
    short: 'Heating, cooling, sterilization, power',
    loops: ['steam', 'chw', 'power'],
    description:
      'Underground distribution carries the plant’s output to the hospital campus: steam for heating, humidification, kitchens, laundry, and sterilization; chilled water for the air handlers that cool operating rooms, MRI suites, pharmaceutical storage, and server rooms; and generator-backed power for life-safety and critical loads.',
    params: [
      { label: 'Heating', value: 'Steam' },
      { label: 'Sterilization', value: 'Steam to autoclaves' },
      { label: 'Cooling', value: 'Chilled water to air handlers' },
      { label: 'Critical power', value: 'ICUs, ORs, life safety' },
    ],
    role: 'Ensuring the continuous operation of the infrastructure the hospital depends on for heating, cooling, sterilization, and power.',
  },
]

export const GROUP_BY_ID = Object.fromEntries(GROUPS.map((g) => [g.id, g])) as Record<GroupId, GroupDef>

/* ─── Utility-outage sequence (simulated; timing not to scale) ──────────── */
export const OUTAGE_STEPS = [
  { title: 'Utility feed lost', detail: 'The normal source drops out and unprotected loads go dark.' },
  { title: 'Generators start', detail: 'The standby diesel gensets start and come up to speed.' },
  { title: 'ATS transfers', detail: 'The transfer switch moves critical loads to the emergency source — within 10 seconds (NFPA 110 Type 10).' },
  { title: 'Critical care stays powered', detail: 'ICUs, operating rooms, ventilators, and fire alarm systems run on generator power.' },
] as const

export type Mode = 'normal' | 'outage'
