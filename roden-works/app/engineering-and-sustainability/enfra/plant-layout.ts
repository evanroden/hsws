import type { GroupId, LoopId } from './plant-model'

export type Pt = readonly [number, number]
export interface Rect {
  x: number
  y: number
  w: number
  h: number
}
type Anchor = 'start' | 'middle' | 'end'

export interface PipeDef {
  id: string
  loop: LoopId
  /** Return lines (condensate, CHWR, CWR) are dashed */
  dashed?: boolean
  /** Polyline authored in the direction of flow */
  pts: Pt[]
  /** Selecting any of these groups lights the pipe up */
  groups: GroupId[]
  /** Electrical role, used by the utility-outage simulation */
  power?: 'utility' | 'emergency' | 'critical'
  /** Static direction chevrons (points on the polyline) */
  arrows?: Pt[]
}

export interface PipeLabel {
  pipe: string
  text: string
  x: number
  y: number
  anchor?: Anchor
}

export interface PumpDef {
  id: string
  pipe: string
  x: number
  y: number
  dir: 'up' | 'down' | 'left' | 'right'
  tag: string
  tagX: number
  tagY: number
  tagAnchor: Anchor
}

export interface LaneLabel {
  group: GroupId
  name: string
  spec: string
  x: number
  y: number
}

export type CardId = 'cooling' | 'heating' | 'sterilization' | 'power'
export interface HospitalCard {
  id: CardId
  label: string
  sub: string
  rect: Rect
}

export interface Layout {
  id: 'wide' | 'tall'
  W: number
  H: number
  /** Narrowest rendered width this layout is designed for (text ≥ 11px) */
  minWidth: number
  maxWidth: number
  plant: Rect
  plantLabel: Pt
  hospital: Rect
  hospitalLabel: Pt
  bas: Rect
  tower: Rect
  chiller: Rect
  boiler: Rect
  generator: Rect
  ats: Rect
  meter: Pt
  transformer: Pt
  utilityLabels: { gas: Pt; power: Pt; lost: Pt }
  outageMark: Pt
  pumps: PumpDef[]
  pipes: PipeDef[]
  pipeLabels: PipeLabel[]
  signals: Pt[][]
  lanes: LaneLabel[]
  status: Pt
  cards: HospitalCard[]
  hits: Record<GroupId, Rect[]>
  brackets: Record<GroupId, Rect[]>
  markers: Record<GroupId, Pt>
}

/* ─── Wide layout — plant left, hospital right, one lane per system ─────── */
export const WIDE: Layout = {
  id: 'wide',
  W: 880,
  H: 640,
  minWidth: 720,
  maxWidth: 960,
  plant: { x: 112, y: 8, w: 564, h: 624 },
  plantLabel: [128, 34],
  hospital: { x: 704, y: 272, w: 168, h: 360 },
  hospitalLabel: [718, 298],

  bas: { x: 300, y: 44, w: 116, h: 48 },
  tower: { x: 300, y: 116, w: 160, h: 80 },
  chiller: { x: 300, y: 260, w: 160, h: 84 },
  boiler: { x: 300, y: 424, w: 152, h: 40 },
  generator: { x: 316, y: 560, w: 128, h: 40 },
  ats: { x: 580, y: 512, w: 72, h: 92 },
  meter: [52, 444],
  transformer: [52, 532],
  utilityLabels: { gas: [12, 428], power: [12, 516], lost: [12, 556] },
  outageMark: [88, 532],

  pumps: [
    { id: 'cwp', pipe: 'cws', x: 284, y: 238, dir: 'down', tag: 'CWP', tagX: 298, tagY: 243, tagAnchor: 'start' },
    { id: 'chwp', pipe: 'chws', x: 508, y: 331, dir: 'right', tag: 'CHWP', tagX: 508, tagY: 313, tagAnchor: 'middle' },
    { id: 'cp', pipe: 'cond', x: 544, y: 496, dir: 'left', tag: 'CP', tagX: 544, tagY: 478, tagAnchor: 'middle' },
  ],

  pipes: [
    { id: 'cws', loop: 'cw', pts: [[296, 192], [284, 192], [284, 274], [300, 274]], groups: ['towers', 'chillers', 'pumps'], arrows: [[284, 208]] },
    { id: 'cwr', loop: 'cw', dashed: true, pts: [[460, 274], [492, 274], [492, 146], [460, 146]], groups: ['towers', 'chillers'], arrows: [[492, 178]] },
    { id: 'chws', loop: 'chw', pts: [[460, 331], [716, 331]], groups: ['chillers', 'pumps', 'hospital'], arrows: [[560, 331]] },
    { id: 'chwr', loop: 'chw', dashed: true, pts: [[716, 368], [336, 368], [336, 344]], groups: ['chillers', 'pumps', 'hospital'], arrows: [[560, 368]] },
    { id: 'steam', loop: 'steam', pts: [[428, 424], [428, 404], [716, 404]], groups: ['boilers', 'hospital'], arrows: [[620, 404]] },
    { id: 'steam2', loop: 'steam', pts: [[688, 404], [688, 464], [716, 464]], groups: ['boilers', 'hospital'] },
    { id: 'cond', loop: 'steam', dashed: true, pts: [[716, 496], [476, 496], [476, 452], [452, 452]], groups: ['boilers', 'pumps', 'hospital'], arrows: [[692, 496]] },
    { id: 'gas', loop: 'gas', pts: [[0, 444], [284, 444]], groups: ['boilers'], arrows: [[88, 444]] },
    { id: 'utility', loop: 'power', power: 'utility', pts: [[0, 532], [580, 532]], groups: ['generators'], arrows: [[88, 532]] },
    { id: 'genfeed', loop: 'power', power: 'emergency', pts: [[444, 584], [580, 584]], groups: ['generators'], arrows: [[520, 584]] },
    { id: 'critical', loop: 'power', power: 'critical', pts: [[652, 558], [716, 558]], groups: ['generators', 'hospital'], arrows: [[690, 558]] },
  ],

  pipeLabels: [
    { pipe: 'cws', text: 'CWS 85°F', x: 298, y: 219 },
    { pipe: 'cwr', text: 'CWR 95°F', x: 502, y: 214 },
    { pipe: 'chws', text: 'CHWS 42°F', x: 596, y: 325 },
    { pipe: 'chwr', text: 'CHWR 56°F', x: 596, y: 362 },
    { pipe: 'steam', text: 'Steam 150 psi', x: 468, y: 398 },
    { pipe: 'cond', text: 'Condensate', x: 590, y: 490 },
  ],

  signals: [
    [[300, 68], [268, 68], [268, 584]],
    [[268, 159], [300, 159]],
    [[268, 302], [334, 302]],
    [[268, 478], [340, 478], [340, 464]],
    [[268, 584], [316, 584]],
  ],

  lanes: [
    { group: 'bas', name: 'Controls', spec: 'ENFRA Connect®', x: 128, y: 64 },
    { group: 'towers', name: 'Cooling towers', spec: 'Induced draft', x: 128, y: 150 },
    { group: 'chillers', name: 'Chillers', spec: '2 × 1,200 ton', x: 128, y: 296 },
    { group: 'boilers', name: 'Steam boilers', spec: '2 × 600 HP', x: 128, y: 408 },
    { group: 'generators', name: 'Generators', spec: '2 × 2 MW diesel', x: 128, y: 566 },
  ],
  status: [128, 604],

  cards: [
    { id: 'cooling', label: 'Cooling', sub: 'Air handlers', rect: { x: 716, y: 312, w: 144, h: 72 } },
    { id: 'heating', label: 'Heating', sub: 'Humidification', rect: { x: 716, y: 392, w: 144, h: 48 } },
    { id: 'sterilization', label: 'Sterilization', sub: 'Autoclaves', rect: { x: 716, y: 448, w: 144, h: 60 } },
    { id: 'power', label: 'Critical power', sub: 'ICUs · ORs', rect: { x: 716, y: 520, w: 144, h: 76 } },
  ],

  hits: {
    bas: [{ x: 120, y: 42, w: 300, h: 56 }],
    towers: [{ x: 120, y: 110, w: 346, h: 90 }],
    chillers: [{ x: 120, y: 254, w: 350, h: 94 }],
    pumps: [
      { x: 272, y: 224, w: 54, h: 28 },
      { x: 490, y: 300, w: 36, h: 44 },
      { x: 528, y: 465, w: 32, h: 44 },
    ],
    boilers: [{ x: 120, y: 396, w: 342, h: 76 }],
    generators: [
      { x: 120, y: 550, w: 332, h: 62 },
      { x: 576, y: 508, w: 80, h: 100 },
    ],
    hospital: [{ x: 704, y: 272, w: 168, h: 360 }],
  },
  brackets: {
    bas: [{ x: 300, y: 44, w: 116, h: 48 }],
    towers: [{ x: 296, y: 116, w: 168, h: 80 }],
    chillers: [{ x: 300, y: 252, w: 168, h: 92 }],
    pumps: [
      { x: 274, y: 228, w: 20, h: 20 },
      { x: 498, y: 321, w: 20, h: 20 },
      { x: 534, y: 486, w: 20, h: 20 },
    ],
    boilers: [{ x: 284, y: 408, w: 176, h: 56 }],
    generators: [
      { x: 316, y: 552, w: 136, h: 48 },
      { x: 580, y: 512, w: 72, h: 92 },
    ],
    hospital: [{ x: 704, y: 272, w: 168, h: 360 }],
  },
  markers: {
    bas: [416, 44],
    towers: [464, 116],
    chillers: [468, 252],
    pumps: [518, 321],
    boilers: [460, 408],
    generators: [452, 552],
    hospital: [860, 284],
  },
}
